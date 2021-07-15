(* Second round of resolution tackles references and forward paths *)
open Odoc_model
open Lang
module Id = Paths.Identifier

(* for < 4.03 *)
(* let kasprintf k fmt =
   Format.(kfprintf (fun _ -> k (flush_str_formatter ())) str_formatter fmt) *)

module Opt = struct
  let map f = function Some x -> Some (f x) | None -> None
end

(** Equivalent to {!Comment.synopsis}. *)
let synopsis_from_comment (docs : Component.CComment.docs) =
  match docs with
  | ({ value = #Comment.nestable_block_element; _ } as e) :: _ ->
      (* Only the first element is considered. *)
      Comment.synopsis [ e ]
  | _ -> None

let synopsis_of_module env (m : Component.Module.t) =
  match synopsis_from_comment m.doc with
  | Some _ as s -> s
  | None -> (
      (* If there is no doc, look at the expansion. *)
      match Tools.signature_of_module env m with
      | Ok sg -> synopsis_from_comment (Component.extract_signature_doc sg)
      | Error _ -> None)

exception Loop

let rec is_forward : Paths.Path.Module.t -> bool = function
  | `Resolved _ -> false
  | `Root _ -> false
  | `Forward _ -> true
  | `Identifier _ -> false
  | `Dot (p, _) -> is_forward p
  | `Apply (p1, p2) -> is_forward p1 || is_forward p2

let rec should_reresolve : Paths.Path.Resolved.t -> bool =
 fun p ->
  let open Paths.Path.Resolved in
  match p with
  | `Identifier _ -> false
  | `Subst (x, y) -> should_reresolve (x :> t) || should_reresolve (y :> t)
  | `Hidden p -> should_reresolve (p :> t)
  | `Canonical (x, y) ->
      should_reresolve (x :> t) || should_resolve (y :> Paths.Path.t)
  | `CanonicalModuleType (x, y) ->
      should_reresolve (x :> t) || should_resolve (y :> Paths.Path.t)
  | `CanonicalType (x, y) ->
      should_reresolve (x :> t) || should_resolve (y :> Paths.Path.t)
  | `Apply (x, y) ->
      should_reresolve (x :> t) || should_reresolve (y :> Paths.Path.Resolved.t)
  | `SubstT (x, y) -> should_reresolve (x :> t) || should_reresolve (y :> t)
  | `Alias (x, y) -> should_reresolve (x :> t) || should_reresolve (y :> t)
  | `AliasModuleType (x, y) ->
      should_reresolve (x :> t) || should_reresolve (y :> t)
  | `Type (p, _)
  | `Class (p, _)
  | `ClassType (p, _)
  | `ModuleType (p, _)
  | `Module (p, _) ->
      should_reresolve (p :> t)
  | `OpaqueModule m -> should_reresolve (m :> t)
  | `OpaqueModuleType m -> should_reresolve (m :> t)

and should_resolve : Paths.Path.t -> bool =
 fun p -> match p with `Resolved p -> should_reresolve p | _ -> true

let type_path : Env.t -> Paths.Path.Type.t -> Paths.Path.Type.t =
 fun env p ->
  if not (should_resolve (p :> Paths.Path.t)) then p
  else
    let cp = Component.Of_Lang.(type_path empty p) in
    match cp with
    | `Resolved p ->
        let result = Tools.reresolve_type env p in
        `Resolved (result |> Cpath.resolved_type_path_of_cpath)
    | _ -> (
        match Tools.resolve_type_path env cp with
        | Ok p' ->
            let result = Tools.reresolve_type env p' in
            `Resolved (Cpath.resolved_type_path_of_cpath result)
        | Error e ->
            Errors.report ~what:(`Type_path cp) ~tools_error:e `Lookup;
            Cpath.type_path_of_cpath cp)

and module_type_path :
    Env.t -> Paths.Path.ModuleType.t -> Paths.Path.ModuleType.t =
 fun env p ->
  if not (should_resolve (p :> Paths.Path.t)) then p
  else
    let cp = Component.Of_Lang.(module_type_path empty p) in
    match cp with
    | `Resolved p ->
        `Resolved
          (Tools.reresolve_module_type env p
          |> Cpath.resolved_module_type_path_of_cpath)
    | _ -> (
        match Tools.resolve_module_type_path env cp with
        | Ok p' ->
            let result = Tools.reresolve_module_type env p' in
            `Resolved (Cpath.resolved_module_type_path_of_cpath result)
        | Error e ->
            Errors.report ~what:(`Module_type_path cp) ~tools_error:e `Resolve;
            Cpath.module_type_path_of_cpath cp)

and module_path : Env.t -> Paths.Path.Module.t -> Paths.Path.Module.t =
 fun env p ->
  if not (should_resolve (p :> Paths.Path.t)) then p
  else
    let cp = Component.Of_Lang.(module_path empty p) in
    match cp with
    | `Resolved p ->
        let after = Tools.reresolve_module env p in
        `Resolved (Cpath.resolved_module_path_of_cpath after)
    | _ -> (
        match Tools.resolve_module_path env cp with
        | Ok p' ->
            let result = Tools.reresolve_module env p' in
            `Resolved (Cpath.resolved_module_path_of_cpath result)
        | Error _ when is_forward p -> p
        | Error e ->
            Errors.report ~what:(`Module_path cp) ~tools_error:e `Resolve;
            Cpath.module_path_of_cpath cp)

let rec comment_inline_element :
    Env.t -> Comment.inline_element -> Comment.inline_element =
 fun env x ->
  match x with
  | `Styled (s, ls) ->
      `Styled (s, List.map (with_location (comment_inline_element env)) ls)
  | `Reference (r, []) -> (
      (* Format.fprintf Format.err_formatter "XXXXXXXXXX about to resolve reference: %a\n%!" (Component.Fmt.model_reference) r; *)
      match Ref_tools.resolve_reference env r with
      | Some (`Identifier (#Id.Label.t as i) as r) ->
          (* Format.fprintf Format.err_formatter "XXXXXXXXXX resolved reference: %a\n%!" (Component.Fmt.model_resolved_reference) r; *)
          let content =
            match Env.lookup_section_title i env with Some x -> x | None -> []
          in
          `Reference (`Resolved r, content)
      | Some x ->
          (* Format.fprintf Format.err_formatter "XXXXXXXXXX resolved reference: %a\n%!" (Component.Fmt.model_resolved_reference) x; *)
          `Reference (`Resolved x, [])
      | None ->
          (* Format.fprintf Format.err_formatter "XXXXXXXXXX FAILED to resolve reference: %a\n%!" (Component.Fmt.model_reference) r; *)
          `Reference (r, []))
  | `Reference (r, content) as orig -> (
      (* Format.fprintf Format.err_formatter "XXXXXXXXXX about to resolve contentful reference: %a\n" (Component.Fmt.model_reference) r; *)
      match Ref_tools.resolve_reference env r with
      | Some x -> `Reference (`Resolved x, content)
      | None -> orig)
  | y -> y

and paragraph env elts =
  List.map (with_location (comment_inline_element env)) elts

and resolve_external_synopsis env synopsis =
  let env = Env.inherit_resolver env in
  paragraph env synopsis

and comment_nestable_block_element env parent
    (x : Comment.nestable_block_element) =
  match x with
  | `Paragraph elts -> `Paragraph (paragraph env elts)
  | (`Code_block _ | `Verbatim _) as x -> x
  | `List (x, ys) ->
      `List
        ( x,
          List.map
            (List.map
               (with_location (comment_nestable_block_element env parent)))
            ys )
  | `Modules refs ->
      let refs =
        List.map
          (fun (r : Comment.module_reference) ->
            match Ref_tools.resolve_module_reference env r.module_reference with
            | Some (r, _, m) ->
                let module_synopsis =
                  Opt.map
                    (resolve_external_synopsis env)
                    (synopsis_of_module env m)
                in
                { Comment.module_reference = `Resolved r; module_synopsis }
            | None -> r)
          refs
      in
      `Modules refs

and comment_block_element env parent (x : Comment.block_element) =
  match x with
  | #Comment.nestable_block_element as x ->
      (comment_nestable_block_element env parent x :> Comment.block_element)
  | `Heading _ as x -> x
  | `Tag _ as x -> x

and with_location :
    type a. (a -> a) -> a Location_.with_location -> a Location_.with_location =
 fun fn x ->
  let value = Lookup_failures.with_location x.location (fun () -> fn x.value) in
  { x with value }

and comment_docs env parent d =
  List.map
    (with_location (comment_block_element env (parent :> Id.LabelParent.t)))
    d

and comment env parent = function
  | `Stop -> `Stop
  | `Docs d -> `Docs (comment_docs env parent d)

let rec unit env t =
  let open Compilation_unit in
  let content =
    match t.content with
    | Module sg -> Module (signature env (t.id :> Id.Signature.t) sg)
    | Pack _ as p -> p
  in
  { t with content; linked = true }

and value_ env parent t =
  let open Value in
  (* Format.fprintf Format.err_formatter "Handling %a\n%!" Component.Fmt.model_identifier (t.id :> Id.t); *)
  let result =
    {
      t with
      doc = comment_docs env parent t.doc;
      type_ = type_expression env parent [] t.type_;
    }
  in
  (* Format.fprintf Format.err_formatter "Done\n%!"; *)
  result

and exception_ env parent e =
  let open Exception in
  let res = Opt.map (type_expression env parent []) e.res in
  let args = type_decl_constructor_argument env parent e.args in
  let doc = comment_docs env parent e.doc in
  { e with res; args; doc }

and extension env parent t =
  let open Extension in
  let constructor c =
    let open Constructor in
    {
      c with
      args = type_decl_constructor_argument env parent c.args;
      res = Opt.map (type_expression env parent []) c.res;
      doc = comment_docs env parent c.doc;
    }
  in
  let type_path = type_path env t.type_path in
  let constructors = List.map constructor t.constructors in
  let doc = comment_docs env parent t.doc in
  { t with type_path; constructors; doc }

and class_type_expr env parent =
  let open ClassType in
  function
  | Constr (path, texps) ->
      Constr (path, List.map (type_expression env parent []) texps)
  | Signature s -> Signature (class_signature env parent s)

and class_type env parent c =
  let open ClassType in
  let doc = comment_docs env parent c.doc in
  { c with expr = class_type_expr env parent c.expr; doc }

and class_signature env parent c =
  let open ClassSignature in
  let env = Env.open_class_signature c env in
  let map_item = function
    | Method m -> Method (method_ env parent m)
    | InstanceVariable i -> InstanceVariable (instance_variable env parent i)
    | Constraint (t1, t2) ->
        Constraint
          (type_expression env parent [] t1, type_expression env parent [] t2)
    | Inherit c -> Inherit (class_type_expr env parent c)
    | Comment c -> Comment c
  in
  {
    self = Opt.map (type_expression env parent []) c.self;
    items = List.map map_item c.items;
    doc = comment_docs env parent c.doc;
  }

and method_ env parent m =
  let open Method in
  let doc = comment_docs env parent m.doc in
  { m with type_ = type_expression env parent [] m.type_; doc }

and instance_variable env parent i =
  let open InstanceVariable in
  let doc = comment_docs env parent i.doc in
  { i with type_ = type_expression env parent [] i.type_; doc }

and class_ env parent c =
  let open Class in
  let rec map_decl = function
    | ClassType expr -> ClassType (class_type_expr env parent expr)
    | Arrow (lbl, expr, decl) ->
        Arrow (lbl, type_expression env parent [] expr, map_decl decl)
  in
  let doc = comment_docs env parent c.doc in
  { c with type_ = map_decl c.type_; doc }

and module_substitution env parent m =
  let open ModuleSubstitution in
  let doc = comment_docs env parent m.doc in
  { m with manifest = module_path env m.manifest; doc }

and signature : Env.t -> Id.Signature.t -> Signature.t -> _ =
 fun env id s ->
  let env = Env.open_signature s env |> Env.add_docs s.doc in
  let items = signature_items env id s.items
  and doc = comment_docs env id s.doc in
  { s with items; doc }

and signature_items :
    Env.t -> Id.Signature.t -> Signature.item list -> Signature.item list =
 fun env id s ->
  let open Signature in
  List.map
    (fun item ->
      match item with
      | Module (r, m) -> Module (r, module_ env m)
      | ModuleSubstitution m ->
          ModuleSubstitution (module_substitution env id m)
      | Type (r, t) -> Type (r, type_decl env id t)
      | TypeSubstitution t -> TypeSubstitution (type_decl env id t)
      | ModuleType mt -> ModuleType (module_type env mt)
      | ModuleTypeSubstitution mts ->
          ModuleTypeSubstitution (module_type_substitution env mts)
      | Value v -> Value (value_ env id v)
      | Comment c -> Comment (comment env id c)
      | TypExt t -> TypExt (extension env id t)
      | Exception e -> Exception (exception_ env id e)
      | Class (r, c) -> Class (r, class_ env id c)
      | ClassType (r, c) -> ClassType (r, class_type env id c)
      | Include i -> Include (include_ env i)
      | Open o -> Open o)
    s

and simple_expansion :
    Env.t ->
    Id.Signature.t ->
    ModuleType.simple_expansion ->
    ModuleType.simple_expansion =
 fun env id m ->
  match m with
  | Signature sg -> Signature (signature env id sg)
  | Functor (arg, sg) ->
      let env' = Env.add_functor_parameter arg env in
      Functor (functor_argument env arg, simple_expansion env' id sg)

and module_ : Env.t -> Module.t -> Module.t =
 fun env m ->
  let open Module in
  let sg_id = (m.id :> Id.Signature.t) in
  (* Format.fprintf Format.err_formatter "Processing Module %a\n%!"
     Component.Fmt.model_identifier
     (m.id :> Id.t); *)
  if m.hidden then m
  else
    let type_ = module_decl env sg_id m.type_ in
    let type_ =
      match type_ with
      | Alias (`Resolved p, _) ->
          let hidden_alias =
            Paths.Path.is_hidden (`Resolved (p :> Paths.Path.Resolved.t))
          in
          let self_canonical =
            let i = Paths.Path.Resolved.Module.identifier p in
            i = (m.id :> Paths.Identifier.Path.Module.t)
          in
          let expansion_needed = self_canonical || hidden_alias in
          if expansion_needed then
            let cp = Component.Of_Lang.(resolved_module_path empty p) in
            match
              Expand_tools.expansion_of_module_alias env m.id (`Resolved cp)
            with
            | Ok (_, _, e) ->
                let le = Lang_of.(simple_expansion empty sg_id e) in
                Alias (`Resolved p, Some (simple_expansion env sg_id le))
            | Error _ -> type_
          else type_
      | Alias _ | ModuleType _ -> type_
    in
    { m with doc = comment_docs env sg_id m.doc; type_ }

and module_decl : Env.t -> Id.Signature.t -> Module.decl -> Module.decl =
 fun env id decl ->
  let open Module in
  match decl with
  | ModuleType expr -> ModuleType (module_type_expr env id expr)
  | Alias (p, e) ->
      Alias (module_path env p, Opt.map (simple_expansion env id) e)

and include_decl : Env.t -> Id.Signature.t -> Include.decl -> Include.decl =
 fun env id decl ->
  let open Include in
  match decl with
  | ModuleType expr -> ModuleType (u_module_type_expr env id expr)
  | Alias p -> Alias (module_path env p)

and module_type : Env.t -> ModuleType.t -> ModuleType.t =
 fun env m ->
  let sg_id = (m.id :> Id.Signature.t) in
  let open ModuleType in
  let expr' =
    match m.expr with
    | None -> None
    | Some expr -> Some (module_type_expr env sg_id expr)
  in
  (* let self_canonical =
       match m.expr with
       | Some (Path (`Resolved p)) when Paths.Path.Resolved.ModuleType.canonical_ident p = Some m.id ->
         true
       | _ -> false
     in*)
  let doc = comment_docs env sg_id m.doc in
  { m with expr = expr'; doc }

and module_type_substitution :
    Env.t -> ModuleTypeSubstitution.t -> ModuleTypeSubstitution.t =
 fun env m ->
  let sg_id = (m.id :> Id.Signature.t) in
  let open ModuleTypeSubstitution in
  let manifest' = module_type_expr env sg_id m.manifest in
  let doc = comment_docs env sg_id m.doc in
  { m with manifest = manifest'; doc }

and include_ : Env.t -> Include.t -> Include.t =
 fun env i ->
  let open Include in
  let decl = include_decl env i.parent i.decl in
  (* Format.eprintf "include_: %a\n%!" Component.Fmt.module_decl
        (Component.Of_Lang.(module_decl empty i.decl)); *)
  let doc = comment_docs env i.parent i.doc in
  let expansion =
    let content = signature env i.parent i.expansion.content in
    { i.expansion with content }
  in
  { i with decl; expansion; doc }

and functor_parameter_parameter :
    Env.t -> FunctorParameter.parameter -> FunctorParameter.parameter =
 fun env a ->
  let sg_id = (a.id :> Id.Signature.t) in
  let expr = module_type_expr env sg_id a.expr in
  { a with expr }

and functor_argument env a =
  match a with
  | FunctorParameter.Unit -> FunctorParameter.Unit
  | Named arg -> Named (functor_parameter_parameter env arg)

and handle_fragments env id sg subs =
  let open ModuleType in
  (* Format.fprintf Format.err_formatter
     "Handling `With` expression for %a (expr=%a) [%a]\n%!"
     Component.Fmt.model_identifier
     (id :> Id.t)
     Component.Fmt.module_type_expr cexpr Component.Fmt.substitution_list
     (List.map Component.Of_Lang.(module_type_substitution empty) subs);*)
  List.fold_left
    (fun (sg_res, subs) lsub ->
      (* Format.fprintf Format.err_formatter "Signature is: %a\n%!"
         Component.Fmt.signature sg; *)
      (* Format.fprintf Format.err_formatter "Handling sub: %a\n%!"
         Component.Fmt.substitution
         Component.Of_Lang.(module_type_substitution empty sub); *)
      match (sg_res, lsub) with
      | Result.Ok sg, ModuleEq (frag, decl) ->
          let frag' =
            match frag with
            | `Resolved f ->
                let cfrag =
                  Component.Of_Lang.(resolved_module_fragment empty f)
                in
                `Resolved
                  (Tools.reresolve_module_fragment env cfrag
                  |> Lang_of.(Path.resolved_module_fragment empty))
            | _ -> frag
          in
          let sg' =
            Tools.fragmap ~mark_substituted:true env
              Component.Of_Lang.(with_module_type_substitution empty lsub)
              sg
          in
          (sg', ModuleEq (frag', module_decl env id decl) :: subs)
      | Ok sg, TypeEq (frag, eqn) ->
          let frag' =
            match frag with
            | `Resolved f ->
                let cfrag =
                  Component.Of_Lang.(resolved_type_fragment empty f)
                in
                `Resolved
                  (Tools.reresolve_type_fragment env cfrag
                  |> Lang_of.(Path.resolved_type_fragment empty))
            | _ -> frag
          in
          let sg' =
            Tools.fragmap ~mark_substituted:true env
              Component.Of_Lang.(with_module_type_substitution empty lsub)
              sg
          in
          (sg', TypeEq (frag', type_decl_equation env id eqn) :: subs)
      | Ok sg, ModuleTypeEq (frag, eqn) ->
          let frag' =
            match frag with
            | `Resolved f ->
                let cfrag =
                  Component.Of_Lang.(resolved_module_type_fragment empty f)
                in
                `Resolved
                  (Tools.reresolve_module_type_fragment env cfrag
                  |> Lang_of.(Path.resolved_module_type_fragment empty))
            | _ -> frag
          in
          let sg' =
            Tools.fragmap ~mark_substituted:true env
              Component.Of_Lang.(with_module_type_substitution empty lsub)
              sg
          in
          (sg', ModuleTypeEq (frag', module_type_expr env id eqn) :: subs)
      | Ok sg, ModuleSubst (frag, mpath) ->
          let frag' =
            match frag with
            | `Resolved f ->
                let cfrag =
                  Component.Of_Lang.(resolved_module_fragment empty f)
                in
                `Resolved
                  (Tools.reresolve_module_fragment env cfrag
                  |> Lang_of.(Path.resolved_module_fragment empty))
            | _ -> frag
          in
          let sg' =
            Tools.fragmap ~mark_substituted:true env
              Component.Of_Lang.(with_module_type_substitution empty lsub)
              sg
          in
          (sg', ModuleSubst (frag', module_path env mpath) :: subs)
      | Ok sg, TypeSubst (frag, eqn) ->
          let frag' =
            match frag with
            | `Resolved f ->
                let cfrag =
                  Component.Of_Lang.(resolved_type_fragment empty f)
                in
                `Resolved
                  (Tools.reresolve_type_fragment env cfrag
                  |> Lang_of.(Path.resolved_type_fragment empty))
            | _ -> frag
          in
          let sg' =
            Tools.fragmap ~mark_substituted:true env
              Component.Of_Lang.(with_module_type_substitution empty lsub)
              sg
          in
          (sg', TypeSubst (frag', type_decl_equation env id eqn) :: subs)
      | Ok sg, ModuleTypeSubst (frag, eqn) ->
          let frag' =
            match frag with
            | `Resolved f ->
                let cfrag =
                  Component.Of_Lang.(resolved_module_type_fragment empty f)
                in
                `Resolved
                  (Tools.reresolve_module_type_fragment env cfrag
                  |> Lang_of.(Path.resolved_module_type_fragment empty))
            | _ -> frag
          in
          let sg' =
            Tools.fragmap ~mark_substituted:true env
              Component.Of_Lang.(with_module_type_substitution empty lsub)
              sg
          in
          (sg', ModuleTypeSubst (frag', module_type_expr env id eqn) :: subs)
      | (Error _ as e), lsub -> (e, lsub :: subs))
    (Ok sg, []) subs
  |> snd |> List.rev

and u_module_type_expr :
    Env.t -> Id.Signature.t -> ModuleType.U.expr -> ModuleType.U.expr =
 fun env id expr ->
  match expr with
  | Signature s -> Signature (signature env id s)
  | Path p -> Path (module_type_path env p)
  | With (subs, expr) as unresolved -> (
      let cexpr = Component.Of_Lang.(u_module_type_expr empty expr) in
      match
        Tools.signature_of_u_module_type_expr ~mark_substituted:true env cexpr
      with
      | Ok sg ->
          With (handle_fragments env id sg subs, u_module_type_expr env id expr)
      | Error e ->
          Errors.report ~what:(`Module_type_U cexpr) ~tools_error:e `Resolve;
          unresolved)
  | TypeOf { t_desc = StructInclude p; t_expansion } ->
      TypeOf { t_desc = StructInclude (module_path env p); t_expansion }
  | TypeOf { t_desc = ModPath p; t_expansion } ->
      TypeOf { t_desc = ModPath (module_path env p); t_expansion }

and module_type_expr :
    Env.t -> Id.Signature.t -> ModuleType.expr -> ModuleType.expr =
 fun env id expr ->
  let open ModuleType in
  let do_expn cur (e : Paths.Path.ModuleType.t option) =
    match (cur, e) with
    | Some e, _ ->
        Some (simple_expansion env (id :> Paths.Identifier.Signature.t) e)
    | None, Some (`Resolved p_path) ->
        let hidden_alias =
          Paths.Path.is_hidden (`Resolved (p_path :> Paths.Path.Resolved.t))
        in
        let self_canonical =
          let i = Paths.Path.Resolved.ModuleType.identifier p_path in
          (i :> Id.Signature.t) = id
        in
        let expansion_needed = self_canonical || hidden_alias in
        if expansion_needed then
          let cp = Component.Of_Lang.(resolved_module_type_path empty p_path) in
          match
            Expand_tools.expansion_of_module_type_expr env id
              (Path { p_path = `Resolved cp; p_expansion = None })
          with
          | Ok (_, _, e) ->
              let le = Lang_of.(simple_expansion empty id e) in
              Some (simple_expansion env id le)
          | Error _ -> None
        else None
    | None, _ -> None
  in
  match expr with
  | Signature s -> Signature (signature env id s)
  | Path { p_path; p_expansion } ->
      let p_path = module_type_path env p_path in
      Path { p_path; p_expansion = do_expn p_expansion (Some p_path) }
  | With { w_substitutions; w_expansion; w_expr } as unresolved -> (
      let cexpr = Component.Of_Lang.(u_module_type_expr empty w_expr) in
      match
        Tools.signature_of_u_module_type_expr ~mark_substituted:true env cexpr
      with
      | Ok sg ->
          With
            {
              w_substitutions = handle_fragments env id sg w_substitutions;
              w_expansion = do_expn w_expansion None;
              w_expr = u_module_type_expr env id w_expr;
            }
      | Error e ->
          Errors.report ~what:(`Module_type_U cexpr) ~tools_error:e `Expand;
          unresolved)
  | Functor (arg, res) ->
      let arg' = functor_argument env arg in
      let env = Env.add_functor_parameter arg env in
      let res' = module_type_expr env (`Result id) res in
      Functor (arg', res')
  | TypeOf { t_desc = StructInclude p; t_expansion } ->
      TypeOf
        {
          t_desc = StructInclude (module_path env p);
          t_expansion = do_expn t_expansion None;
        }
  | TypeOf { t_desc = ModPath p; t_expansion } ->
      TypeOf
        {
          t_desc = ModPath (module_path env p);
          t_expansion = do_expn t_expansion None;
        }

and type_decl_representation :
    Env.t ->
    Id.Signature.t ->
    TypeDecl.Representation.t ->
    TypeDecl.Representation.t =
 fun env parent r ->
  let open TypeDecl.Representation in
  match r with
  | Variant cs -> Variant (List.map (type_decl_constructor env parent) cs)
  | Record fs -> Record (List.map (type_decl_field env parent) fs)
  | Extensible -> Extensible

and type_decl : Env.t -> Id.Signature.t -> TypeDecl.t -> TypeDecl.t =
 fun env parent t ->
  let open TypeDecl in
  (* Format.eprintf "Handling type decl %a\n%!" Component.Fmt.model_identifier
            (t.id :> Paths.Identifier.t); *)
  let equation = type_decl_equation env parent t.equation in
  let doc = comment_docs env parent t.doc in
  let hidden_path =
    match equation.Equation.manifest with
    | Some (Constr (`Resolved path, params))
      when Paths.Path.Resolved.Type.is_hidden path
           || Paths.Path.Resolved.Type.canonical_ident path
              = (Some t.id :> Paths.Identifier.Path.Type.t option) ->
        Some (path, params)
    | _ -> None
  in
  let representation =
    Opt.map (type_decl_representation env parent) t.representation
  in
  let default = { t with equation; doc; representation } in
  let result =
    match hidden_path with
    | Some (p, params) -> (
        let p' =
          Component.Of_Lang.resolved_type_path Component.Of_Lang.empty p
        in
        (* Format.eprintf "found hidden path: %a\n%!"
           Component.Fmt.resolved_type_path p'; *)
        match Tools.lookup_type env p' with
        | Ok (`FType (_, t')) ->
            let equation =
              try
                Expand_tools.collapse_eqns default.equation
                  (Lang_of.type_decl_equation Lang_of.empty
                     (parent :> Id.Parent.t)
                     t'.equation)
                  params
              with _ -> default.equation
            in
            { default with equation = type_decl_equation env parent equation }
        | Ok (`FClass _ | `FClassType _ | `FType_removed _) | Error _ -> default
        )
    | None -> default
  in
  (* Format.fprintf Format.err_formatter "type_decl result: %a\n%!"
        Component.Fmt.type_decl (Component.Of_Lang.(type_decl empty result)); *)
  result

and type_decl_equation env parent t =
  let open TypeDecl.Equation in
  let manifest = Opt.map (type_expression env parent []) t.manifest in
  let constraints =
    List.map
      (fun (tex1, tex2) ->
        (type_expression env parent [] tex1, type_expression env parent [] tex2))
      t.constraints
  in
  { t with manifest; constraints }

and type_decl_field env parent f =
  let open TypeDecl.Field in
  let doc = comment_docs env parent f.doc in
  { f with type_ = type_expression env parent [] f.type_; doc }

and type_decl_constructor_argument env parent c =
  let open TypeDecl.Constructor in
  match c with
  | Tuple ts -> Tuple (List.map (type_expression env parent []) ts)
  | Record fs -> Record (List.map (type_decl_field env parent) fs)

and type_decl_constructor env parent c =
  let open TypeDecl.Constructor in
  let doc = comment_docs env parent c.doc in
  let args = type_decl_constructor_argument env parent c.args in
  let res = Opt.map (type_expression env parent []) c.res in
  { c with doc; args; res }

and type_expression_polyvar env parent visited v =
  let open TypeExpr.Polymorphic_variant in
  let constructor c =
    let open Constructor in
    let doc = comment_docs env parent c.doc in
    {
      c with
      arguments = List.map (type_expression env parent visited) c.arguments;
      doc;
    }
  in
  let element = function
    | Type t ->
        Type
          (match type_expression env parent visited t with
          | Constr _ as x -> x
          | _ -> t)
        (* These have to remain Constrs *)
    | Constructor c -> Constructor (constructor c)
  in
  { v with elements = List.map element v.elements }

and type_expression_object env parent visited o =
  let open TypeExpr.Object in
  let method_ m =
    { m with type_ = type_expression env parent visited m.type_ }
  in
  let field = function
    | Method m -> Method (method_ m)
    | Inherit t -> Inherit (type_expression env parent visited t)
  in
  { o with fields = List.map field o.fields }

and type_expression_package env parent visited p =
  let open TypeExpr.Package in
  let substitution (frag, t) =
    let cfrag = Component.Of_Lang.(type_fragment empty frag) in
    let frag' =
      match cfrag with
      | `Resolved f -> `Resolved (Tools.reresolve_type_fragment env f)
      | _ -> cfrag
    in
    ( Lang_of.(Path.type_fragment empty frag'),
      type_expression env parent visited t )
  in
  {
    path = module_type_path env p.path;
    substitutions = List.map substitution p.substitutions;
  }

and type_expression : Env.t -> Id.Signature.t -> _ -> _ =
 fun env parent visited texpr ->
  let open TypeExpr in
  match texpr with
  | Var _ | Any -> texpr
  | Alias (t, str) -> Alias (type_expression env parent visited t, str)
  | Arrow (lbl, t1, t2) ->
      Arrow
        ( lbl,
          type_expression env parent visited t1,
          type_expression env parent visited t2 )
  | Tuple ts -> Tuple (List.map (type_expression env parent visited) ts)
  | Constr (path', ts') -> (
      let path = type_path env path' in
      let ts = List.map (type_expression env parent visited) ts' in
      if not (Paths.Path.is_hidden (path :> Paths.Path.t)) then Constr (path, ts)
      else
        let cp = Component.Of_Lang.(type_path empty path') in
        match Tools.resolve_type env ~add_canonical:true cp with
        | Ok (cp', `FType (_, t)) ->
            let cp' = Tools.reresolve_type env cp' in
            let p = Cpath.resolved_type_path_of_cpath cp' in
            if List.mem p visited then raise Loop
            else if Cpath.is_resolved_type_hidden cp' then
              match t.Component.TypeDecl.equation with
              | { manifest = Some expr; params; _ } -> (
                  try
                    let map =
                      List.fold_left2
                        (fun acc param sub ->
                          match param.Lang.TypeDecl.desc with
                          | Lang.TypeDecl.Var x -> (x, sub) :: acc
                          | Any -> acc)
                        [] params ts
                    in
                    let t' =
                      Expand_tools.type_expr map
                        Lang_of.(type_expr empty (parent :> Id.Parent.t) expr)
                    in
                    type_expression env parent (p :: visited) t'
                  with
                  | Loop -> Constr (`Resolved p, ts)
                  | e ->
                      Format.eprintf
                        "Caught unexpected exception when expanding type \
                         declaration (%s)\n\
                         %!"
                        (Printexc.to_string e);
                      Constr (`Resolved p, ts))
              | _ -> Constr (`Resolved p, ts)
            else Constr (`Resolved p, ts)
        | Ok (cp', (`FClass _ | `FClassType _)) ->
            let p = Cpath.resolved_type_path_of_cpath cp' in
            Constr (`Resolved p, ts)
        | Ok (_cp, `FType_removed (_, x, _eq)) ->
            (* Type variables ? *)
            Lang_of.(type_expr empty (parent :> Id.Parent.t) x)
        | Error _ -> Constr (Cpath.type_path_of_cpath cp, ts))
  | Polymorphic_variant v ->
      Polymorphic_variant (type_expression_polyvar env parent visited v)
  | Object o -> Object (type_expression_object env parent visited o)
  | Class (path, ts) ->
      Class (path, List.map (type_expression env parent visited) ts)
  | Poly (strs, t) -> Poly (strs, type_expression env parent visited t)
  | Package p -> Package (type_expression_package env parent visited p)

let link ~filename x y =
  Lookup_failures.catch_failures ~filename (fun () ->
      if y.Lang.Compilation_unit.linked then y else unit x y)

let page env page =
  let children =
    List.fold_right
      (fun child res ->
        match Ref_tools.resolve_reference env child with
        | Some r -> `Resolved r :: res
        | None ->
            Errors.report ~what:(`Child child) `Resolve;
            res)
      page.Odoc_model.Lang.Page.children []
  in
  {
    page with
    Page.content = comment_docs env page.Page.name page.content;
    children;
    linked = true;
  }

let resolve_page ~filename env p =
  Lookup_failures.catch_failures ~filename (fun () ->
      if p.Lang.Page.linked then p else page env p)

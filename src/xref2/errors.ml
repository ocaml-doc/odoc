open Odoc_model

module Tools_error = struct
  open Paths
  (** Errors raised by Tools *)

  type handle_subs_error =
    [ `UnresolvedPath of
      [ `Module of Cpath.module_ ]
      (* Failed to resolve a module path when applying a fragment item *) ]

  type signature_of_module_error =
    [ `OpaqueModule (* The module does not have an expansion *)
    | `UnresolvedForwardPath
      (* The module signature depends upon a forward path *)
    | `UnresolvedPath of
      [ `Module of Cpath.module_ * simple_module_lookup_error
      | `ModuleType of Cpath.module_type * simple_module_type_lookup_error ]
      (* The path to the module or module type could not be resolved *)
    | `UnexpandedTypeOf of
      Component.ModuleType.type_of_desc
      (* The `module type of` expression could not be expanded *) ]

  and simple_module_lookup_error =
    [ `Local of
      Env.t * Ident.path_module
      (* Internal error: Found local path during lookup *)
    | `Unresolved_apply (* [`Apply] argument is not [`Resolved] *)
    | `Find_failure
    | (* Internal error: the module was not found in the parent signature *)
      `Lookup_failure of
      Identifier.Path.Module.t
      (* Could not find the module in the environment *)
    | `Lookup_failure_root of string (* Could not find the root module *)
    | `Parent of parent_lookup_error ]

  and simple_module_type_expr_of_module_error =
    [ `ApplyNotFunctor
      (* Internal error: attempt made to apply a module that's not a functor *)
    | `OpaqueModule (* The module does not have an expansion *)
    | `UnresolvedForwardPath
      (* The module signature depends upon a forward path *)
    | `UnresolvedPath of
      [ `Module of Cpath.module_ * simple_module_lookup_error
      | `ModuleType of Cpath.module_type * simple_module_type_lookup_error ]
    | `Parent of parent_lookup_error ]

  and simple_module_type_lookup_error =
    [ `LocalMT of
      Env.t * Ident.module_type
      (* Internal error: Found local path during lookup *)
    | `Find_failure
      (* Internal error: the module was not found in the parent signature *)
    | `Lookup_failureMT of
      Identifier.ModuleType.t
      (* Could not find the module in the environment *)
    | `Parent of parent_lookup_error ]

  and simple_type_lookup_error =
    [ `LocalType of
      Env.t * Ident.path_type
      (* Internal error: Found local path during lookup *)
    | `Class_replaced
      (* Class was replaced with a destructive substitution and we're not sure
          what to do now *)
    | `Find_failure
      (* Internal error: the type was not found in the parent signature *)
    | `Lookup_failureT of
      Identifier.Path.Type.t
      (* Could not find the module in the environment *)
    | `Parent of parent_lookup_error ]

  and parent_lookup_error =
    [ `Parent_sig of
      signature_of_module_error
      (* Error found while calculating the parent signature *)
    | `Parent_module_type of
      simple_module_type_lookup_error
      (* Error found while looking up parent module type *)
    | `Parent_expr of
      simple_module_type_expr_of_module_error
      (* Error found while evaluating parent module expression *)
    | `Parent_module of
      simple_module_lookup_error
      (* Error found while looking up parent module *)
    | `Fragment_root (* Encountered unexpected fragment root *)
    | `Parent of parent_lookup_error ]

  type any =
    [ simple_type_lookup_error
    | simple_module_type_lookup_error
    | simple_module_type_expr_of_module_error
    | simple_module_lookup_error
    | signature_of_module_error
    | parent_lookup_error ]

  let rec pp : Format.formatter -> any -> unit =
   fun fmt err ->
    match err with
    | `OpaqueModule -> Format.fprintf fmt "OpaqueModule"
    | `UnresolvedForwardPath -> Format.fprintf fmt "Unresolved forward path"
    | `UnresolvedPath (`Module (p, e)) ->
        Format.fprintf fmt "Unresolved module path %a (%a)"
          Component.Fmt.module_path p pp
          (e :> any)
    | `UnresolvedPath (`ModuleType (p, e)) ->
        Format.fprintf fmt "Unresolved module type path %a (%a)"
          Component.Fmt.module_type_path p pp
          (e :> any)
    | `LocalMT (_, id) -> Format.fprintf fmt "Local id found: %a" Ident.fmt id
    | `Local (_, id) -> Format.fprintf fmt "Local id found: %a" Ident.fmt id
    | `LocalType (_, id) -> Format.fprintf fmt "Local id found: %a" Ident.fmt id
    | `Unresolved_apply -> Format.fprintf fmt "Unresolved apply"
    | `Find_failure -> Format.fprintf fmt "Find failure"
    | `Lookup_failure m ->
        Format.fprintf fmt "Lookup failure (module): %a"
          Component.Fmt.model_identifier
          (m :> Odoc_model.Paths.Identifier.t)
    | `Lookup_failure_root r ->
        Format.fprintf fmt "Lookup failure (root module): %s" r
    | `Lookup_failureMT m ->
        Format.fprintf fmt "Lookup failure (module type): %a"
          Component.Fmt.model_identifier
          (m :> Odoc_model.Paths.Identifier.t)
    | `Lookup_failureT m ->
        Format.fprintf fmt "Lookup failure (type): %a"
          Component.Fmt.model_identifier
          (m :> Odoc_model.Paths.Identifier.t)
    | `ApplyNotFunctor -> Format.fprintf fmt "Apply module is not a functor"
    | `Class_replaced -> Format.fprintf fmt "Class replaced"
    | `Parent p -> pp fmt (p :> any)
    | `UnexpandedTypeOf t ->
        Format.fprintf fmt "Unexpanded `module type of` expression: %a"
          Component.Fmt.module_type_type_of_desc t
    | `Parent_sig e -> Format.fprintf fmt "Parent_sig: %a" pp (e :> any)
    | `Parent_module_type e ->
        Format.fprintf fmt "Parent_module_type: %a" pp (e :> any)
    | `Parent_expr e -> Format.fprintf fmt "Parent_expr: %a" pp (e :> any)
    | `Parent_module e -> Format.fprintf fmt "Parent_module: %a" pp (e :> any)
    | `Fragment_root -> Format.fprintf fmt "Fragment root"
end

(* Ugh. we need to determine whether this was down to an unexpanded module type error. This is horrendous. *)
let is_unexpanded_module_type_of =
  let open Tools_error in
  let rec inner : any -> bool = function
    | `Local _ -> false
    | `Find_failure -> false
    | `Lookup_failure _ -> false
    | `Unresolved_apply -> false
    | `Lookup_failure_root _ -> false
    | `Parent p -> inner (p :> any)
    | `Parent_sig p -> inner (p :> any)
    | `Parent_module_type p -> inner (p :> any)
    | `Parent_expr p -> inner (p :> any)
    | `Parent_module p -> inner (p :> any)
    | `Fragment_root -> false
    | `OpaqueModule -> false
    | `UnresolvedForwardPath -> false
    | `UnexpandedTypeOf _ -> true (* woo *)
    | `LocalMT _ -> false
    | `Lookup_failureMT _ -> false
    | `ApplyNotFunctor -> false
    | `UnresolvedPath (`Module (_, e)) -> inner (e :> any)
    | `UnresolvedPath (`ModuleType (_, e)) -> inner (e :> any)
    | `Lookup_failureT _ -> false
    | `LocalType _ -> false
    | `Class_replaced -> false
  in
  inner

(** To use as [Lookup_failures.kind]. *)
let rec kind_of_module_cpath = function
  | `Root _ -> Some `Root
  | `Substituted p' | `Dot (p', _) -> kind_of_module_cpath p'
  | `Apply (a, b) -> (
      match kind_of_module_cpath a with
      | Some _ as a -> a
      | None -> kind_of_module_cpath b)
  | _ -> None

let rec kind_of_module_type_cpath = function
  | `Substituted p' -> kind_of_module_type_cpath p'
  | `Dot (p', _) -> kind_of_module_cpath p'
  | _ -> None

let rec kind_of_error = function
  | `UnresolvedPath (`Module (cp, _)) -> kind_of_module_cpath cp
  | `UnresolvedPath (`ModuleType (cp, _)) -> kind_of_module_type_cpath cp
  | `Lookup_failure (`Root _) | `Lookup_failure_root _ -> Some `Root
  | `Parent (`Parent_sig e) -> kind_of_error (e :> Tools_error.any)
  | `Parent (`Parent_module_type e) -> kind_of_error (e :> Tools_error.any)
  | `Parent (`Parent_expr e) -> kind_of_error (e :> Tools_error.any)
  | `Parent (`Parent_module e) -> kind_of_error (e :> Tools_error.any)
  | `Parent (`Parent _ as e) -> kind_of_error (e :> Tools_error.any)
  | _ -> None

open Paths

type what =
  [ `Functor_parameter of Identifier.FunctorParameter.t
  | `Value of Identifier.Value.t
  | `Class of Identifier.Class.t
  | `Class_type of Identifier.ClassType.t
  | `Module of Identifier.Module.t
  | `Module_type of Identifier.Signature.t
  | `Module_path of Cpath.module_
  | `Module_type_path of Cpath.module_type
  | `Module_type_U of Component.ModuleType.U.expr
  | `Include of Component.Include.decl
  | `Package of Cpath.module_type
  | `Type of Cfrag.type_
  | `Type_path of Cpath.type_
  | `With_module of Cfrag.module_
  | `With_type of Cfrag.type_
  | `Module_type_expr of Component.ModuleType.expr
  | `Module_type_u_expr of Component.ModuleType.U.expr
  | `Child of Reference.t ]

let report ~(what : what) ?tools_error action =
  let kind =
    match tools_error with
    | Some e -> kind_of_error (e :> Tools_error.any)
    | None -> (
        match what with
        | `Include (Component.Include.Alias cp) -> kind_of_module_cpath cp
        | `Module (`Root _) -> Some `Root
        | _ -> None)
  in
  let action =
    match action with
    | `Lookup -> "lookup"
    | `Expand -> "compile expansion for"
    | `Resolve_module_type -> "resolve type of"
    | `Resolve -> "resolve"
    | `Compile -> "compile"
  in
  let pp_tools_error fmt = function
    | Some e -> Format.fprintf fmt " %a" Tools_error.pp (e :> Tools_error.any)
    | None -> ()
  in
  let r ?(kind = kind) subject pp_a a =
    Lookup_failures.report ?kind "Failed to %s %s %a%a" action subject pp_a a
      pp_tools_error tools_error
  in
  let open Component.Fmt in
  let fmt_id fmt id = model_identifier fmt (id :> Paths.Identifier.t) in
  match what with
  | `Functor_parameter id -> r "functor parameter" fmt_id id
  | `Value id -> r "value" fmt_id id
  | `Class id -> r "class" fmt_id id
  | `Class_type id -> r "class type" fmt_id id
  | `Module id -> r "module" fmt_id id
  | `Module_type id -> r "module type" fmt_id id
  | `Module_path path -> r "module path" module_path path
  | `Module_type_path path -> r "module type path" module_type_path path
  | `Module_type_U expr -> r "module type expr" u_module_type_expr expr
  | `Include decl -> r "include" include_decl decl
  | `Package path ->
      r "module package" module_type_path (path :> Cpath.module_type)
  | `Type cfrag -> r "type" type_fragment cfrag
  | `Type_path path -> r "type" type_path path
  | `With_module frag -> r "module substitution" module_fragment frag
  | `With_type frag -> r "type substitution" type_fragment frag
  | `Module_type_expr cexpr -> r "module type expression" module_type_expr cexpr
  | `Module_type_u_expr cexpr ->
      r "module type u expression" u_module_type_expr cexpr
  | `Child rf -> r "child reference" model_reference rf

open Component

exception Invalidated

exception MTOInvalidated

type ('a, 'b) or_replaced = Not_replaced of 'a | Replaced of 'b

type 'a type_or_replaced = ('a, TypeExpr.t * TypeDecl.Equation.t) or_replaced

type 'a module_type_or_replaced = ('a, ModuleType.expr) or_replaced

let map_replaced f = function
  | Not_replaced p -> Not_replaced (f p)
  | Replaced _ as r -> r

open Component
open Substitution

type nonrec t = t

let identity =
  {
    module_ = PathModuleMap.empty;
    module_type = ModuleTypeMap.empty;
    module_type_replacement = ModuleTypeMap.empty;
    type_ = PathTypeMap.empty;
    class_type = PathClassTypeMap.empty;
    type_replacement = PathTypeMap.empty;
    path_invalidating_modules = [];
    module_type_of_invalidating_modules = [];
    unresolve_opaque_paths = false;
  }

let unresolve_opaque_paths s = { s with unresolve_opaque_paths = true }

let path_invalidate_module id t =
  { t with path_invalidating_modules = id :: t.path_invalidating_modules }

let mto_invalidate_module id t =
  {
    t with
    module_type_of_invalidating_modules =
      id :: t.module_type_of_invalidating_modules;
  }

let add_module id p rp t =
  { t with module_ = PathModuleMap.add id (`Prefixed (p, rp)) t.module_ }

let add_module_type id p rp t =
  {
    t with
    module_type = ModuleTypeMap.add id (`Prefixed (p, rp)) t.module_type;
  }

let add_type : Ident.type_ -> Cpath.type_ -> Cpath.Resolved.type_ -> t -> t =
 fun id p rp t ->
  {
    t with
    type_ = PathTypeMap.add (id :> Ident.path_type) (`Prefixed (p, rp)) t.type_;
  }

let add_class :
    Ident.class_ -> Cpath.class_type -> Cpath.Resolved.class_type -> t -> t =
 fun id p rp t ->
  {
    t with
    type_ =
      PathTypeMap.add
        (id :> Ident.path_type)
        (`Prefixed ((p :> Cpath.type_), (rp :> Cpath.Resolved.type_)))
        t.type_;
    class_type =
      PathClassTypeMap.add
        (id :> Ident.path_class_type)
        (`Prefixed (p, rp))
        t.class_type;
  }

let add_class_type :
    Ident.class_type -> Cpath.class_type -> Cpath.Resolved.class_type -> t -> t
    =
 fun id p rp t ->
  {
    t with
    type_ =
      PathTypeMap.add
        (id :> Ident.path_type)
        (`Prefixed ((p :> Cpath.type_), (rp :> Cpath.Resolved.type_)))
        t.type_;
    class_type =
      PathClassTypeMap.add
        (id :> Ident.path_class_type)
        (`Prefixed (p, rp))
        t.class_type;
  }

let add_type_replacement id texp equation t =
  {
    t with
    type_replacement = PathTypeMap.add id (texp, equation) t.type_replacement;
  }

let add_module_type_replacement path mty t =
  {
    t with
    module_type_replacement =
      ModuleTypeMap.add path mty t.module_type_replacement;
  }

let add_module_substitution : Ident.path_module -> t -> t =
 fun id t ->
  {
    t with
    module_type_of_invalidating_modules =
      id :: t.module_type_of_invalidating_modules;
    path_invalidating_modules = id :: t.path_invalidating_modules;
    module_ = PathModuleMap.add id `Substituted t.module_;
  }

let rename_module : Ident.path_module -> Ident.path_module -> t -> t =
 fun id id' t ->
  { t with module_ = PathModuleMap.add id (`Renamed id') t.module_ }

let rename_module_type : Ident.module_type -> Ident.module_type -> t -> t =
 fun id id' t ->
  { t with module_type = ModuleTypeMap.add id (`Renamed id') t.module_type }

let rename_type : Ident.path_type -> Ident.path_type -> t -> t =
 fun id id' t -> { t with type_ = PathTypeMap.add id (`Renamed id') t.type_ }

let rename_class_type : Ident.path_class_type -> Ident.path_class_type -> t -> t
    =
 fun id id' t ->
  {
    t with
    class_type = PathClassTypeMap.add id (`Renamed id') t.class_type;
    type_ =
      PathTypeMap.add
        (id :> Ident.path_type)
        (`Renamed (id' :> Ident.path_type))
        t.type_;
  }

let rec substitute_vars vars t =
  let open TypeExpr in
  match t with
  | Var s -> ( try List.assoc s vars with Not_found -> t)
  | Any -> Any
  | Alias (t, str) -> Alias (substitute_vars vars t, str)
  | Arrow (lbl, t1, t2) ->
      Arrow (lbl, substitute_vars vars t1, substitute_vars vars t2)
  | Tuple ts -> Tuple (List.map (substitute_vars vars) ts)
  | Constr (p, ts) -> Constr (p, List.map (substitute_vars vars) ts)
  | Polymorphic_variant v ->
      Polymorphic_variant (substitute_vars_poly_variant vars v)
  | Object o -> Object (substitute_vars_type_object vars o)
  | Class (p, ts) -> Class (p, List.map (substitute_vars vars) ts)
  | Poly (strs, ts) -> Poly (strs, substitute_vars vars ts)
  | Package p -> Package (substitute_vars_package vars p)

and substitute_vars_package vars p =
  let open TypeExpr.Package in
  let subst_subst (p, t) = (p, substitute_vars vars t) in
  { p with substitutions = List.map subst_subst p.substitutions }

and substitute_vars_type_object vars o =
  let open TypeExpr.Object in
  let subst_field = function
    | Method m -> Method { m with type_ = substitute_vars vars m.type_ }
    | Inherit t -> Inherit (substitute_vars vars t)
  in
  { o with fields = List.map subst_field o.fields }

and substitute_vars_poly_variant vars v =
  let open TypeExpr.Polymorphic_variant in
  let subst_element = function
    | Type t -> Type (substitute_vars vars t)
    | Constructor c ->
        let arguments =
          List.map (substitute_vars vars) c.Constructor.arguments
        in
        Constructor { c with arguments }
  in
  { v with elements = List.map subst_element v.elements }

let rec resolved_module_path :
    t -> Cpath.Resolved.module_ -> Cpath.Resolved.module_ =
 fun s p ->
  match p with
  | `Local id -> (
      if List.mem id s.path_invalidating_modules then raise Invalidated;
      match
        try Some (PathModuleMap.find (id :> Ident.path_module) s.module_)
        with _ -> None
      with
      | Some (`Renamed x) -> `Local x
      | Some (`Prefixed (_p, rp)) -> rp
      | Some `Substituted -> `Substituted p
      | None -> p)
  | `Identifier _ -> p
  | `Apply (p1, p2) ->
      `Apply (resolved_module_path s p1, resolved_module_path s p2)
  | `Substituted p -> `Substituted (resolved_module_path s p)
  | `Module (p, n) -> `Module (resolved_parent_path s p, n)
  | `Alias (p1, p2) ->
      `Alias (resolved_module_path s p1, resolved_module_path s p2)
  | `Subst (p1, p2) ->
      let p1 =
        match resolved_module_type_path s p1 with
        | Replaced _ ->
            (* the left hand side of Subst is a named module type inside a module,
               it cannot be substituted away *)
            assert false
        | Not_replaced p1 -> p1
      in
      `Subst (p1, resolved_module_path s p2)
  | `Hidden p1 -> `Hidden (resolved_module_path s p1)
  | `Canonical (p1, p2) ->
      `Canonical (resolved_module_path s p1, module_path s p2)
  | `OpaqueModule m ->
      if s.unresolve_opaque_paths then raise Invalidated
      else `OpaqueModule (resolved_module_path s m)

and resolved_parent_path s = function
  | `Module m -> `Module (resolved_module_path s m)
  | `ModuleType m ->
      let p =
        match resolved_module_type_path s m with
        | Replaced _ -> assert false
        | Not_replaced p1 -> p1
      in
      `ModuleType p
  | `FragmentRoot as x -> x

and module_path : t -> Cpath.module_ -> Cpath.module_ =
 fun s p ->
  match p with
  | `Resolved p' -> (
      try `Resolved (resolved_module_path s p')
      with Invalidated ->
        let path' = Cpath.unresolve_resolved_module_path p' in
        module_path s path')
  | `Dot (p', str) -> `Dot (module_path s p', str)
  | `Module (p', str) -> `Module (resolved_parent_path s p', str)
  | `Apply (p1, p2) -> `Apply (module_path s p1, module_path s p2)
  | `Local (id, b) -> (
      match
        try Some (PathModuleMap.find (id :> Ident.path_module) s.module_)
        with _ -> None
      with
      | Some (`Prefixed (p, _rp)) -> p
      | Some (`Renamed x) -> `Local (x, b)
      | Some `Substituted -> `Substituted p
      | None -> `Local (id, b))
  | `Identifier _ -> p
  | `Substituted p -> `Substituted (module_path s p)
  | `Forward _ -> p
  | `Root _ -> p

and resolved_module_type_path :
    t ->
    Cpath.Resolved.module_type ->
    (Cpath.Resolved.module_type, ModuleType.expr) or_replaced =
 fun s p ->
  match p with
  | `Local id -> (
      if ModuleTypeMap.mem id s.module_type_replacement then
        Replaced (ModuleTypeMap.find id s.module_type_replacement)
      else
        match ModuleTypeMap.find id s.module_type with
        | `Prefixed (_p, rp) -> Not_replaced rp
        | `Renamed x -> Not_replaced (`Local x)
        | exception Not_found -> Not_replaced (`Local id))
  | `Identifier _ -> Not_replaced p
  | `Substituted p ->
      resolved_module_type_path s p |> map_replaced (fun p -> `Substituted p)
  | `ModuleType (p, n) ->
      Not_replaced (`ModuleType (resolved_parent_path s p, n))
  | `CanonicalModuleType (mt1, mt2) -> (
      match (resolved_module_type_path s mt1, module_type_path s mt2) with
      | Not_replaced mt1', Not_replaced mt2' ->
          Not_replaced (`CanonicalModuleType (mt1', mt2'))
      | x, _ -> x)
  | `OpaqueModuleType m ->
      if s.unresolve_opaque_paths then raise Invalidated
      else
        resolved_module_type_path s m
        |> map_replaced (fun x -> `OpaqueModuleType x)
  | `SubstT (p1, p2) -> (
      match
        (resolved_module_type_path s p1, resolved_module_type_path s p2)
      with
      | Not_replaced p1, Not_replaced p2 -> Not_replaced (`SubstT (p1, p2))
      | Replaced mt, _ | _, Replaced mt -> Replaced mt)
  | `AliasModuleType (p1, p2) -> (
      match
        (resolved_module_type_path s p1, resolved_module_type_path s p2)
      with
      | Not_replaced p1, Not_replaced p2 ->
          Not_replaced (`AliasModuleType (p1, p2))
      | Replaced mt, _ | _, Replaced mt -> Replaced mt)

and module_type_path :
    t -> Cpath.module_type -> Cpath.module_type module_type_or_replaced =
 fun s p ->
  match p with
  | `Resolved r -> (
      try resolved_module_type_path s r |> map_replaced (fun r -> `Resolved r)
      with Invalidated ->
        let path' = Cpath.unresolve_resolved_module_type_path r in
        module_type_path s path')
  | `Substituted p ->
      module_type_path s p |> map_replaced (fun r -> `Substituted r)
  | `Local (id, b) ->
      if ModuleTypeMap.mem id s.module_type_replacement then
        Replaced (ModuleTypeMap.find id s.module_type_replacement)
      else
        let r =
          match
            try Some (ModuleTypeMap.find id s.module_type) with _ -> None
          with
          | Some (`Prefixed (p, _rp)) -> p
          | Some (`Renamed x) -> `Local (x, b)
          | None -> `Local (id, b)
        in
        Not_replaced r
  | `Identifier _ -> Not_replaced p
  | `Dot (p, n) -> Not_replaced (`Dot (module_path s p, n))
  | `ModuleType (p', str) ->
      Not_replaced (`ModuleType (resolved_parent_path s p', str))

and resolved_type_path :
    t ->
    Cpath.Resolved.type_ ->
    (Cpath.Resolved.type_, TypeExpr.t * TypeDecl.Equation.t) or_replaced =
 fun s p ->
  match p with
  | `Local id -> (
      if PathTypeMap.mem id s.type_replacement then
        Replaced (PathTypeMap.find id s.type_replacement)
      else
        match
          try Some (PathTypeMap.find id s.type_) with Not_found -> None
        with
        | Some (`Prefixed (_p, rp)) -> Not_replaced rp
        | Some (`Renamed x) -> Not_replaced (`Local x)
        | None -> Not_replaced (`Local id))
  | `CanonicalType (t1, t2) -> (
      match (resolved_type_path s t1, type_path s t2) with
      | Not_replaced t1', Not_replaced t2' ->
          Not_replaced (`CanonicalType (t1', t2'))
      | x, _ -> x)
  | `Identifier _ -> Not_replaced p
  | `Substituted p ->
      resolved_type_path s p |> map_replaced (fun p -> `Substituted p)
  | `Type (p, n) -> Not_replaced (`Type (resolved_parent_path s p, n))
  | `ClassType (p, n) -> Not_replaced (`ClassType (resolved_parent_path s p, n))
  | `Class (p, n) -> Not_replaced (`Class (resolved_parent_path s p, n))

and type_path : t -> Cpath.type_ -> Cpath.type_ type_or_replaced =
 fun s p ->
  match p with
  | `Resolved r -> (
      try resolved_type_path s r |> map_replaced (fun r -> `Resolved r)
      with Invalidated ->
        let path' = Cpath.unresolve_resolved_type_path r in
        type_path s path')
  | `Substituted p -> type_path s p |> map_replaced (fun r -> `Substituted r)
  | `Local (id, b) -> (
      if PathTypeMap.mem id s.type_replacement then
        Replaced (PathTypeMap.find id s.type_replacement)
      else
        match
          try Some (PathTypeMap.find id s.type_) with Not_found -> None
        with
        | Some (`Prefixed (p, _rp)) -> Not_replaced p
        | Some (`Renamed x) -> Not_replaced (`Local (x, b))
        | None -> Not_replaced (`Local (id, b)))
  | `Identifier _ -> Not_replaced p
  | `Dot (p, n) -> Not_replaced (`Dot (module_path s p, n))
  | `Type (p, n) -> Not_replaced (`Type (resolved_parent_path s p, n))
  | `Class (p, n) -> Not_replaced (`Class (resolved_parent_path s p, n))
  | `ClassType (p, n) -> Not_replaced (`ClassType (resolved_parent_path s p, n))

and resolved_class_type_path :
    t -> Cpath.Resolved.class_type -> Cpath.Resolved.class_type =
 fun s p ->
  match p with
  | `Local id -> (
      match
        try Some (PathClassTypeMap.find id s.class_type) with _ -> None
      with
      | Some (`Prefixed (_p, rp)) -> rp
      | Some (`Renamed x) -> `Local x
      | None -> `Local id)
  | `Identifier _ -> p
  | `Substituted p -> `Substituted (resolved_class_type_path s p)
  | `ClassType (p, n) -> `ClassType (resolved_parent_path s p, n)
  | `Class (p, n) -> `Class (resolved_parent_path s p, n)

and class_type_path : t -> Cpath.class_type -> Cpath.class_type =
 fun s p ->
  match p with
  | `Resolved r -> (
      try `Resolved (resolved_class_type_path s r)
      with Invalidated ->
        let path' = Cpath.unresolve_resolved_class_type_path r in
        class_type_path s path')
  | `Local (id, b) -> (
      match
        try Some (PathClassTypeMap.find id s.class_type) with _ -> None
      with
      | Some (`Prefixed (p, _rp)) -> p
      | Some (`Renamed x) -> `Local (x, b)
      | None -> `Local (id, b))
  | `Identifier _ -> p
  | `Substituted p -> `Substituted (class_type_path s p)
  | `Dot (p, n) -> `Dot (module_path s p, n)
  | `Class (p, n) -> `Class (resolved_parent_path s p, n)
  | `ClassType (p, n) -> `ClassType (resolved_parent_path s p, n)

let rec resolved_signature_fragment :
    t -> Cfrag.resolved_signature -> Cfrag.resolved_signature =
 fun t r ->
  match r with
  | `Root (`ModuleType p) ->
      let p =
        match resolved_module_type_path t p with
        | Not_replaced p -> p
        | Replaced _ -> assert false
      in
      `Root (`ModuleType p)
  | `Root (`Module p) -> `Root (`Module (resolved_module_path t p))
  | (`Subst _ | `Alias _ | `OpaqueModule _ | `Module _) as x ->
      (resolved_module_fragment t x :> Cfrag.resolved_signature)

and resolved_module_fragment :
    t -> Cfrag.resolved_module -> Cfrag.resolved_module =
 fun t r ->
  match r with
  | `Subst (mty, f) ->
      let p =
        match resolved_module_type_path t mty with
        | Not_replaced p -> p
        | Replaced _ ->
            (* the left hand side of subst is a named module type inside a module,
               it cannot be substituted *)
            assert false
      in
      `Subst (p, resolved_module_fragment t f)
  | `Alias (m, f) ->
      `Alias (resolved_module_path t m, resolved_module_fragment t f)
  | `Module (sg, n) -> `Module (resolved_signature_fragment t sg, n)
  | `OpaqueModule m -> `OpaqueModule (resolved_module_fragment t m)

and resolved_module_type_fragment :
    t -> Cfrag.resolved_module_type -> Cfrag.resolved_module_type =
 fun t r ->
  match r with
  | `ModuleType (s, n) -> `ModuleType (resolved_signature_fragment t s, n)

and resolved_type_fragment : t -> Cfrag.resolved_type -> Cfrag.resolved_type =
 fun t r ->
  match r with
  | `Type (s, n) -> `Type (resolved_signature_fragment t s, n)
  | `ClassType (s, n) -> `ClassType (resolved_signature_fragment t s, n)
  | `Class (s, n) -> `Class (resolved_signature_fragment t s, n)

let rec signature_fragment : t -> Cfrag.signature -> Cfrag.signature =
 fun t r ->
  match r with
  | `Resolved f -> `Resolved (resolved_signature_fragment t f)
  | `Dot (sg, n) -> `Dot (signature_fragment t sg, n)
  | `Root -> `Root

let rec module_fragment : t -> Cfrag.module_ -> Cfrag.module_ =
 fun t r ->
  match r with
  | `Resolved r -> (
      try `Resolved (resolved_module_fragment t r)
      with Invalidated ->
        let frag' = Cfrag.unresolve_module r in
        module_fragment t frag')
  | `Dot (sg, n) -> `Dot (signature_fragment t sg, n)

let rec module_type_fragment : t -> Cfrag.module_type -> Cfrag.module_type =
 fun t r ->
  match r with
  | `Resolved r -> (
      try `Resolved (resolved_module_type_fragment t r)
      with Invalidated ->
        let frag' = Cfrag.unresolve_module_type r in
        module_type_fragment t frag')
  | `Dot (sg, n) -> `Dot (signature_fragment t sg, n)

let rec type_fragment : t -> Cfrag.type_ -> Cfrag.type_ =
 fun t r ->
  match r with
  | `Resolved r -> (
      try `Resolved (resolved_type_fragment t r)
      with Invalidated ->
        let frag' = Cfrag.unresolve_type r in
        type_fragment t frag')
  | `Dot (sg, n) -> `Dot (signature_fragment t sg, n)

let option_ conv s x = match x with Some x -> Some (conv s x) | None -> None

let option_bind conv s x = match x with Some x -> conv s x | None -> None

let list conv s xs = List.map (conv s) xs

let rec type_ s t =
  let open Component.TypeDecl in
  let representation = option_ type_decl_representation s t.representation in
  let canonical =
    match t.canonical with
    | Some p -> (
        match type_path s p with
        | Not_replaced p' -> Some p'
        | Replaced _ -> None)
    | None -> None
  in
  {
    equation = type_decl_equation s t.equation;
    representation;
    doc = t.doc;
    canonical;
  }

and type_decl_representation s t =
  let open Component.TypeDecl.Representation in
  match t with
  | Variant cs -> Variant (List.map (type_decl_constructor s) cs)
  | Record fs -> Record (List.map (type_decl_field s) fs)
  | Extensible -> t

and type_decl_constructor s t =
  let open Component.TypeDecl.Constructor in
  let args = type_decl_constructor_arg s t.args in
  let res = option_ type_expr s t.res in
  { t with args; res }

and type_poly_var s v =
  let open Component.TypeExpr.Polymorphic_variant in
  let map_constr c =
    let open Constructor in
    {
      name = c.name;
      constant = c.constant;
      arguments = List.map (type_expr s) c.arguments;
      doc = c.doc;
    }
  in
  let map_element = function
    | Type t -> (
        match type_expr s t with
        | Polymorphic_variant v -> v.elements
        | x -> [ Type x ])
    | Constructor c -> [ Constructor (map_constr c) ]
  in

  { kind = v.kind; elements = List.flatten (List.map map_element v.elements) }

and type_object s o =
  let open Component.TypeExpr.Object in
  let map_field = function
    | Method m -> Method { m with type_ = type_expr s m.type_ }
    | Inherit t -> Inherit (type_expr s t)
  in
  { fields = List.map map_field o.fields; open_ = o.open_ }

and type_package s p =
  let open Component.TypeExpr.Package in
  let sub (x, y) = (type_fragment s x, type_expr s y) in
  {
    path =
      (match module_type_path s p.path with
      | Not_replaced p -> p
      | Replaced (Path p) -> p.p_path
      | Replaced _ ->
          (* substituting away a packed module type by a non-path module type is a type error *)
          assert false);
    substitutions = List.map sub p.substitutions;
  }

and type_expr s t =
  let open Component.TypeExpr in
  match t with
  | Var s -> Var s
  | Any -> Any
  | Alias (t, str) -> Alias (type_expr s t, str)
  | Arrow (lbl, t1, t2) -> Arrow (lbl, type_expr s t1, type_expr s t2)
  | Tuple ts -> Tuple (List.map (type_expr s) ts)
  | Constr (p, ts) -> (
      match type_path s p with
      | Replaced (t, eq) ->
          let mk_var acc pexpr param =
            match param.Odoc_model.Lang.TypeDecl.desc with
            | Any -> acc
            | Var n -> (n, type_expr s pexpr) :: acc
          in
          let vars = List.fold_left2 mk_var [] ts eq.params in
          substitute_vars vars t
      | Not_replaced p -> Constr (p, List.map (type_expr s) ts))
  | Polymorphic_variant v -> Polymorphic_variant (type_poly_var s v)
  | Object o -> Object (type_object s o)
  | Class (p, ts) -> Class (class_type_path s p, List.map (type_expr s) ts)
  | Poly (strs, ts) -> Poly (strs, type_expr s ts)
  | Package p -> Package (type_package s p)

and simple_expansion :
    t ->
    Component.ModuleType.simple_expansion ->
    Component.ModuleType.simple_expansion =
 fun s t ->
  let open Component.ModuleType in
  match t with
  | Signature sg -> Signature (signature s sg)
  | Functor (arg, sg) -> Functor (functor_parameter s arg, simple_expansion s sg)

and module_type s t =
  let open Component.ModuleType in
  let expr =
    match t.expr with Some m -> Some (module_type_expr s m) | None -> None
  in
  let maybe_path s t =
    match module_type_path s t with
    | Not_replaced p -> Some p
    | Replaced (Path p) -> Some p.p_path
    | Replaced _ -> None
  in
  { expr; doc = t.doc; canonical = option_bind maybe_path s t.canonical }

and module_type_substitution s t =
  let open Component.ModuleTypeSubstitution in
  let manifest = module_type_expr s t.manifest in
  { manifest; doc = t.doc }

and functor_parameter s t =
  let open Component.FunctorParameter in
  match t with
  | Named arg -> Named { arg with expr = module_type_expr s arg.expr }
  | Unit -> Unit

and module_type_type_of_desc s t =
  let open Component.ModuleType in
  match t with
  | ModPath p ->
      if mto_module_path_invalidated s p then raise MTOInvalidated
      else ModPath (module_path s p)
  | StructInclude p ->
      if mto_module_path_invalidated s p then raise MTOInvalidated
      else StructInclude (module_path s p)

and module_type_type_of_desc_noexn s t =
  let open Component.ModuleType in
  match t with
  | ModPath p -> ModPath (module_path s p)
  | StructInclude p -> StructInclude (module_path s p)

and mto_module_path_invalidated : t -> Cpath.module_ -> bool =
 fun s p ->
  match p with
  | `Resolved p' -> mto_resolved_module_path_invalidated s p'
  | `Substituted p' | `Dot (p', _) -> mto_module_path_invalidated s p'
  | `Module (`Module p', _) -> mto_resolved_module_path_invalidated s p'
  | `Module (_, _) -> false
  | `Apply (p1, p2) ->
      mto_module_path_invalidated s p1 || mto_module_path_invalidated s p2
  | `Local (id, _) -> List.mem id s.module_type_of_invalidating_modules
  | `Identifier _ -> false
  | `Forward _ -> false
  | `Root _ -> false

and mto_resolved_module_path_invalidated s p =
  match p with
  | `Local id -> List.mem id s.module_type_of_invalidating_modules
  | `Identifier _ -> false
  | `Apply (p1, p2) ->
      mto_resolved_module_path_invalidated s p1
      || mto_resolved_module_path_invalidated s p2
  | `Module (`Module p, _) | `Substituted p ->
      mto_resolved_module_path_invalidated s p
  | `Module (_, _) -> false
  | `Alias (p1, _p2) -> mto_resolved_module_path_invalidated s p1
  | `Subst (_p1, p2) -> mto_resolved_module_path_invalidated s p2
  | `Hidden p -> mto_resolved_module_path_invalidated s p
  | `Canonical (p1, _p2) -> mto_resolved_module_path_invalidated s p1
  | `OpaqueModule p -> mto_resolved_module_path_invalidated s p

and u_module_type_expr s t =
  let open Component.ModuleType.U in
  match t with
  | Path p -> (
      match module_type_path s p with
      | Not_replaced p -> Path p
      | Replaced eqn -> (
          match eqn with
          | Path p -> Path p.p_path
          | Signature s -> Signature s
          | TypeOf t -> TypeOf t
          | With w -> With (w.w_substitutions, w.w_expr)
          | Functor _ ->
              (* non functor cannot be substituted away to a functor *)
              assert false))
  | Signature sg -> Signature (signature s sg)
  | With (subs, e) ->
      With
        (List.map (with_module_type_substitution s) subs, u_module_type_expr s e)
  | TypeOf { t_desc; t_expansion = Some (Signature e) } -> (
      try
        TypeOf
          {
            t_desc = module_type_type_of_desc s t_desc;
            t_expansion = Some (Signature (apply_sig_map_sg s e));
          }
      with MTOInvalidated -> u_module_type_expr s (Signature e))
  | TypeOf { t_expansion = Some (Functor _); _ } -> assert false
  | TypeOf { t_desc; t_expansion = None } ->
      TypeOf
        { t_desc = module_type_type_of_desc_noexn s t_desc; t_expansion = None }

and module_type_of_simple_expansion :
    Component.ModuleType.simple_expansion -> Component.ModuleType.expr =
  function
  | Signature sg -> Signature sg
  | Functor (arg, e) -> Functor (arg, module_type_of_simple_expansion e)

and module_type_expr s t =
  let open Component.ModuleType in
  match t with
  | Path { p_path; p_expansion } -> (
      match module_type_path s p_path with
      | Not_replaced p_path ->
          Path { p_path; p_expansion = option_ simple_expansion s p_expansion }
      | Replaced s -> s)
  | Signature sg -> Signature (signature s sg)
  | Functor (arg, expr) ->
      Functor (functor_parameter s arg, module_type_expr s expr)
  | With { w_substitutions; w_expansion; w_expr } ->
      With
        {
          w_substitutions =
            List.map (with_module_type_substitution s) w_substitutions;
          w_expansion = option_ simple_expansion s w_expansion;
          w_expr = u_module_type_expr s w_expr;
        }
  | TypeOf { t_desc; t_expansion = Some e } -> (
      try
        TypeOf
          {
            t_desc = module_type_type_of_desc s t_desc;
            t_expansion = Some (simple_expansion s e);
          }
      with MTOInvalidated ->
        module_type_expr s (module_type_of_simple_expansion e))
  | TypeOf { t_desc; t_expansion = None } ->
      TypeOf
        { t_desc = module_type_type_of_desc_noexn s t_desc; t_expansion = None }

and with_module_type_substitution s sub =
  let open Component.ModuleType in
  match sub with
  | ModuleEq (f, m) -> ModuleEq (module_fragment s f, module_decl s m)
  | ModuleSubst (f, p) -> ModuleSubst (module_fragment s f, module_path s p)
  | TypeEq (f, eq) -> TypeEq (type_fragment s f, type_decl_equation s eq)
  | TypeSubst (f, eq) -> TypeSubst (type_fragment s f, type_decl_equation s eq)
  | ModuleTypeEq (f, eq) ->
      ModuleTypeEq (module_type_fragment s f, module_type_expr s eq)
  | ModuleTypeSubst (f, eq) ->
      ModuleTypeSubst (module_type_fragment s f, module_type_expr s eq)

and module_decl s t =
  match t with
  | Alias (p, e) -> Alias (module_path s p, option_ simple_expansion s e)
  | ModuleType t -> ModuleType (module_type_expr s t)

and include_decl s t =
  match t with
  | Include.Alias p -> Include.Alias (module_path s p)
  | ModuleType t -> ModuleType (u_module_type_expr s t)

and module_ s t =
  let open Component.Module in
  let type_ = module_decl s t.type_ in
  let canonical = option_ (fun s m1 -> module_path s m1) s t.canonical in
  { t with type_; canonical }

and module_substitution s m =
  let open Component.ModuleSubstitution in
  let manifest = module_path s m.manifest in
  { manifest; doc = m.doc }

and type_decl_field s f =
  let open Component.TypeDecl.Field in
  { f with type_ = type_expr s f.type_ }

and type_decl_constructor_arg s a =
  let open Component.TypeDecl.Constructor in
  match a with
  | Tuple ts -> Tuple (list type_expr s ts)
  | Record fs -> Record (list type_decl_field s fs)

and type_decl_equation s t =
  let open Component.TypeDecl.Equation in
  {
    t with
    manifest = option_ type_expr s t.manifest;
    constraints =
      List.map (fun (x, y) -> (type_expr s x, type_expr s y)) t.constraints;
  }

and exception_ s e =
  let open Component.Exception in
  let res = option_ type_expr s e.res in
  let args = type_decl_constructor_arg s e.args in
  { args; res; doc = e.doc }

and extension_constructor s c =
  let open Component.Extension.Constructor in
  {
    c with
    args = type_decl_constructor_arg s c.args;
    res = option_ type_expr s c.res;
  }

and extension s e =
  let open Component.Extension in
  let type_path =
    match type_path s e.type_path with
    | Not_replaced p -> p
    | Replaced (TypeExpr.Constr (p, _), _) -> p
    | Replaced _ -> (* What else is possible ? *) assert false
  and constructors = List.map (extension_constructor s) e.constructors in
  { e with type_path; constructors }

and include_ s i =
  let open Component.Include in
  {
    i with
    decl = include_decl s i.decl;
    strengthened = option_ module_path s i.strengthened;
    expansion_ = apply_sig_map_sg s i.expansion_;
  }

and open_ s o =
  let open Component.Open in
  { expansion = apply_sig_map_sg s o.expansion }

and value s v =
  let open Component.Value in
  { v with type_ = type_expr s v.type_ }

and class_ s c =
  let open Component.Class in
  let expansion = option_ class_signature s c.expansion in
  { c with type_ = class_decl s c.type_; expansion }

and class_decl s =
  let open Component.Class in
  function
  | ClassType e -> ClassType (class_type_expr s e)
  | Arrow (lbl, t, d) -> Arrow (lbl, type_expr s t, class_decl s d)

and class_type_expr s =
  let open Component.ClassType in
  function
  | Constr (p, ts) -> Constr (class_type_path s p, List.map (type_expr s) ts)
  | Signature sg -> Signature (class_signature s sg)

and class_type s c =
  let open Component.ClassType in
  let expansion = option_ class_signature s c.expansion in
  { c with expr = class_type_expr s c.expr; expansion }

and class_signature_item s =
  let open Component.ClassSignature in
  function
  | Method (id, m) -> Method (id, method_ s m)
  | InstanceVariable (id, i) -> InstanceVariable (id, instance_variable s i)
  | Constraint (t1, t2) -> Constraint (type_expr s t1, type_expr s t2)
  | Inherit e -> Inherit (class_type_expr s e)
  | Comment _ as y -> y

and class_signature s sg =
  let open Component.ClassSignature in
  {
    sg with
    self = option_ type_expr s sg.self;
    items = List.map (class_signature_item s) sg.items;
  }

and method_ s m =
  let open Component.Method in
  { m with type_ = type_expr s m.type_ }

and instance_variable s i =
  let open Component.InstanceVariable in
  { i with type_ = type_expr s i.type_ }

and rename_bound_idents s sg =
  let open Component.Signature in
  let new_module_id id =
    try
      match PathModuleMap.find (id :> Ident.path_module) s.module_ with
      | `Renamed (`LModule _ as x) -> x
      | _ -> failwith "Error"
    with Not_found -> Ident.Rename.module_ id
  in
  let new_module_type_id id =
    try
      match ModuleTypeMap.find id s.module_type with
      | `Renamed x -> x
      | _ -> failwith "Error"
    with Not_found -> Ident.Rename.module_type id
  in
  let new_type_id id =
    try
      match PathTypeMap.find (id :> Ident.path_type) s.type_ with
      | `Renamed (`LType _ as x) -> x
      | _ -> failwith "Error"
    with Not_found -> Ident.Rename.type_ id
  in
  let new_class_id id =
    try
      match
        PathClassTypeMap.find (id :> Ident.path_class_type) s.class_type
      with
      | `Renamed (`LClass _ as x) -> x
      | _ -> failwith "Error"
    with Not_found -> Ident.Rename.class_ id
  in
  let new_class_type_id id =
    try
      match
        PathClassTypeMap.find (id :> Ident.path_class_type) s.class_type
      with
      | `Renamed (`LClassType _ as x) -> x
      | _ -> failwith "Error!"
    with Not_found -> Ident.Rename.class_type id
  in
  function
  | [] -> (s, List.rev sg)
  | Module (id, r, m) :: rest ->
      let id' = new_module_id id in
      rename_bound_idents
        (rename_module (id :> Ident.path_module) (id' :> Ident.path_module) s)
        (Module (id', r, m) :: sg)
        rest
  | ModuleSubstitution (id, m) :: rest ->
      let id' = new_module_id id in
      rename_bound_idents
        (rename_module (id :> Ident.path_module) (id' :> Ident.path_module) s)
        (ModuleSubstitution (id', m) :: sg)
        rest
  | ModuleType (id, mt) :: rest ->
      let id' = new_module_type_id id in
      rename_bound_idents
        (rename_module_type id id' s)
        (ModuleType (id', mt) :: sg)
        rest
  | ModuleTypeSubstitution (id, mt) :: rest ->
      let id' = new_module_type_id id in
      rename_bound_idents
        (rename_module_type id id' s)
        (ModuleTypeSubstitution (id', mt) :: sg)
        rest
  | Type (id, r, t) :: rest ->
      let id' = new_type_id id in
      rename_bound_idents
        (rename_type (id :> Ident.path_type) (id' :> Ident.path_type) s)
        (Type (id', r, t) :: sg)
        rest
  | TypeSubstitution (id, t) :: rest ->
      let id' = new_type_id id in
      rename_bound_idents
        (rename_type (id :> Ident.path_type) (id' :> Ident.path_type) s)
        (TypeSubstitution (id', t) :: sg)
        rest
  | Exception (id, e) :: rest ->
      let id' = Ident.Rename.exception_ id in
      rename_bound_idents s (Exception (id', e) :: sg) rest
  | TypExt e :: rest -> rename_bound_idents s (TypExt e :: sg) rest
  | Value (id, v) :: rest ->
      let id' = Ident.Rename.value id in
      rename_bound_idents s (Value (id', v) :: sg) rest
  | Class (id, r, c) :: rest ->
      let id' = new_class_id id in
      rename_bound_idents
        (rename_class_type
           (id :> Ident.path_class_type)
           (id' :> Ident.path_class_type)
           s)
        (Class (id', r, c) :: sg)
        rest
  | ClassType (id, r, c) :: rest ->
      let id' = new_class_type_id id in
      rename_bound_idents
        (rename_class_type
           (id :> Ident.path_class_type)
           (id' :> Ident.path_class_type)
           s)
        (ClassType (id', r, c) :: sg)
        rest
  | Include ({ expansion_; _ } as i) :: rest ->
      let s, items = rename_bound_idents s [] expansion_.items in
      rename_bound_idents s
        (Include { i with expansion_ = { expansion_ with items; removed = [] } }
        :: sg)
        rest
  | Open { expansion } :: rest ->
      let s, items = rename_bound_idents s [] expansion.items in
      rename_bound_idents s
        (Open { expansion = { expansion with items; removed = [] } } :: sg)
        rest
  | (Comment _ as item) :: rest -> rename_bound_idents s (item :: sg) rest

and removed_items s items =
  let open Component.Signature in
  List.map
    (function
      | RModule (id, _) as x -> (
          try
            match PathModuleMap.find (id :> Ident.path_module) s.module_ with
            | `Prefixed (_, x) -> RModule (id, x)
            | _ -> x
          with Not_found -> x)
      | x -> x)
    items

and signature s sg =
  let s, items = rename_bound_idents s [] sg.items in
  let items, removed, dont_recompile = apply_sig_map s items sg.removed in
  { sg with items; removed; compiled = sg.compiled && dont_recompile }

and apply_sig_map_sg s (sg : Component.Signature.t) =
  let items, removed, dont_recompile = apply_sig_map s sg.items sg.removed in
  { sg with items; removed; compiled = sg.compiled && dont_recompile }

and apply_sig_map s items removed =
  let open Component.Signature in
  let rec inner items acc =
    match items with
    | [] -> List.rev acc
    | Module (id, r, m) :: rest ->
        inner rest
          (Module
             ( id,
               r,
               Component.Delayed.put (fun () ->
                   module_ s (Component.Delayed.get m)) )
          :: acc)
    | ModuleSubstitution (id, m) :: rest ->
        inner rest (ModuleSubstitution (id, module_substitution s m) :: acc)
    | ModuleType (id, mt) :: rest ->
        inner rest
          (ModuleType
             ( id,
               Component.Delayed.put (fun () ->
                   module_type s (Component.Delayed.get mt)) )
          :: acc)
    | ModuleTypeSubstitution (id, mt) :: rest ->
        inner rest
          (ModuleTypeSubstitution (id, module_type_substitution s mt) :: acc)
    | Type (id, r, t) :: rest ->
        inner rest
          (Type
             ( id,
               r,
               Component.Delayed.put (fun () ->
                   type_ s (Component.Delayed.get t)) )
          :: acc)
    | TypeSubstitution (id, t) :: rest ->
        inner rest (TypeSubstitution (id, type_ s t) :: acc)
    | Exception (id, e) :: rest ->
        inner rest (Exception (id, exception_ s e) :: acc)
    | TypExt e :: rest -> inner rest (TypExt (extension s e) :: acc)
    | Value (id, v) :: rest ->
        inner rest
          (Value
             ( id,
               Component.Delayed.put (fun () ->
                   value s (Component.Delayed.get v)) )
          :: acc)
    | Class (id, r, c) :: rest -> inner rest (Class (id, r, class_ s c) :: acc)
    | ClassType (id, r, c) :: rest ->
        inner rest (ClassType (id, r, class_type s c) :: acc)
    | Include i :: rest -> inner rest (Include (include_ s i) :: acc)
    | Open o :: rest -> inner rest (Open (open_ s o) :: acc)
    | Comment c :: rest -> inner rest (Comment c :: acc)
  in
  let dont_recompile =
    List.length s.path_invalidating_modules = 0
    && List.length s.module_type_of_invalidating_modules = 0
  in
  (inner items [], removed_items s removed, dont_recompile)

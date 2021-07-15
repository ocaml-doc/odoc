open Odoc_model
open Paths
open Names

type maps = {
  module_ : Identifier.Module.t Component.ModuleMap.t;
  module_type : Identifier.ModuleType.t Component.ModuleTypeMap.t;
  functor_parameter :
    (Ident.functor_parameter * Identifier.FunctorParameter.t) list;
  type_ : Identifier.Type.t Component.TypeMap.t;
  path_type : Identifier.Path.Type.t Component.PathTypeMap.t;
  class_ : (Ident.class_ * Identifier.Class.t) list;
  class_type : (Ident.class_type * Identifier.ClassType.t) list;
  path_class_type : Identifier.Path.ClassType.t Component.PathClassTypeMap.t;
  fragment_root : Cfrag.root option;
  (* Shadowed items *)
  shadowed : Lang.Include.shadowed;
}

let empty_shadow =
  let open Lang.Include in
  {
    s_modules = [];
    s_module_types = [];
    s_values = [];
    s_types = [];
    s_classes = [];
    s_class_types = [];
  }

let empty =
  {
    module_ = Component.ModuleMap.empty;
    module_type = Component.ModuleTypeMap.empty;
    functor_parameter = [];
    type_ = Component.TypeMap.empty;
    path_type = Component.PathTypeMap.empty;
    class_ = [];
    class_type = [];
    path_class_type = Component.PathClassTypeMap.empty;
    fragment_root = None;
    shadowed = empty_shadow;
  }

let with_fragment_root r = { empty with fragment_root = Some r }

(** Raises [Not_found] *)
let lookup_module map : Ident.path_module -> _ = function
  | #Ident.module_ as id ->
      (Component.ModuleMap.find id map.module_ :> Identifier.Path.Module.t)
  | #Ident.functor_parameter as id ->
      (List.assoc id map.functor_parameter :> Identifier.Path.Module.t)
  | _ -> raise Not_found

module Opt = Component.Opt

module Path = struct
  let rec module_ map (p : Cpath.module_) : Odoc_model.Paths.Path.Module.t =
    match p with
    | `Substituted x -> module_ map x
    | `Local (id, b) ->
        `Identifier
          ( (try lookup_module map id
             with Not_found ->
               failwith (Format.asprintf "Not_found: %a" Ident.fmt id)),
            b )
    | `Identifier (i, b) -> `Identifier (i, b)
    | `Resolved x -> `Resolved (resolved_module map x)
    | `Root x -> `Root x
    | `Dot (p, s) -> `Dot (module_ map p, s)
    | `Forward s -> `Forward s
    | `Apply (m1, m2) -> `Apply (module_ map m1, module_ map m2)
    | `Module (`Module p, n) ->
        `Dot (`Resolved (resolved_module map p), ModuleName.to_string n)
    | `Module (_, _) -> failwith "Probably shouldn't happen"

  and module_type map (p : Cpath.module_type) :
      Odoc_model.Paths.Path.ModuleType.t =
    match p with
    | `Substituted x -> module_type map x
    | `Identifier ((#Odoc_model.Paths.Identifier.ModuleType.t as y), b) ->
        `Identifier (y, b)
    | `Local (id, b) ->
        `Identifier
          ( (try Component.ModuleTypeMap.find id map.module_type
             with Not_found ->
               failwith (Format.asprintf "Not_found: %a" Ident.fmt id)),
            b )
    | `Resolved x -> `Resolved (resolved_module_type map x)
    | `Dot (p, n) -> `Dot (module_ map p, n)
    | `ModuleType (`Module p, n) ->
        `Dot (`Resolved (resolved_module map p), ModuleTypeName.to_string n)
    | `ModuleType (_, _) -> failwith "Probably shouldn't happen"

  and type_ map (p : Cpath.type_) : Odoc_model.Paths.Path.Type.t =
    match p with
    | `Substituted x -> type_ map x
    | `Identifier ((#Odoc_model.Paths.Identifier.Path.Type.t as y), b) ->
        `Identifier (y, b)
    | `Local (id, b) ->
        `Identifier (Component.PathTypeMap.find id map.path_type, b)
    | `Resolved x -> `Resolved (resolved_type map x)
    | `Dot (p, n) -> `Dot (module_ map p, n)
    | `Type (`Module p, n) ->
        `Dot (`Resolved (resolved_module map p), TypeName.to_string n)
    | `Class (`Module p, n) ->
        `Dot (`Resolved (resolved_module map p), ClassName.to_string n)
    | `ClassType (`Module p, n) ->
        `Dot (`Resolved (resolved_module map p), ClassTypeName.to_string n)
    | `Type _ | `Class _ | `ClassType _ -> failwith "Probably shouldn't happen"

  and class_type map (p : Cpath.class_type) : Odoc_model.Paths.Path.ClassType.t
      =
    match p with
    | `Substituted x -> class_type map x
    | `Identifier ((#Odoc_model.Paths.Identifier.Path.ClassType.t as y), b) ->
        `Identifier (y, b)
    | `Local (id, b) ->
        `Identifier (Component.PathClassTypeMap.find id map.path_class_type, b)
    | `Resolved x -> `Resolved (resolved_class_type map x)
    | `Dot (p, n) -> `Dot (module_ map p, n)
    | `Class (`Module p, n) ->
        `Dot (`Resolved (resolved_module map p), ClassName.to_string n)
    | `ClassType (`Module p, n) ->
        `Dot (`Resolved (resolved_module map p), ClassTypeName.to_string n)
    | `Class _ | `ClassType _ -> failwith "Probably shouldn't happen"

  and resolved_module map (p : Cpath.Resolved.module_) :
      Odoc_model.Paths.Path.Resolved.Module.t =
    match p with
    | `Local id ->
        `Identifier
          (try lookup_module map id
           with Not_found ->
             failwith (Format.asprintf "Not_found: %a" Ident.fmt id))
    | `Substituted x -> resolved_module map x
    | `Identifier y -> `Identifier y
    | `Subst (mty, m) ->
        `Subst (resolved_module_type map mty, resolved_module map m)
    | `Hidden h -> `Hidden (resolved_module map h)
    | `Module (p, n) -> `Module (resolved_parent map p, n)
    | `Canonical (r, m) -> `Canonical (resolved_module map r, module_ map m)
    | `Apply (m1, m2) -> `Apply (resolved_module map m1, resolved_module map m2)
    | `Alias (m1, m2) -> `Alias (resolved_module map m1, resolved_module map m2)
    | `OpaqueModule m -> `OpaqueModule (resolved_module map m)

  and resolved_parent map (p : Cpath.Resolved.parent) =
    match p with
    | `Module m -> resolved_module map m
    | `ModuleType _ -> failwith "Invalid"
    | `FragmentRoot -> (
        match map.fragment_root with
        | Some r -> resolved_parent map (r :> Cpath.Resolved.parent)
        | None -> failwith "Invalid")

  and resolved_module_type map (p : Cpath.Resolved.module_type) :
      Odoc_model.Paths.Path.Resolved.ModuleType.t =
    match p with
    | `Identifier (#Odoc_model.Paths.Identifier.ModuleType.t as y) ->
        `Identifier y
    | `Local id ->
        `Identifier
          (try Component.ModuleTypeMap.find id map.module_type
           with Not_found ->
             failwith (Format.asprintf "Not_found: %a" Ident.fmt id))
    | `ModuleType (p, name) -> `ModuleType (resolved_parent map p, name)
    | `Substituted s -> resolved_module_type map s
    | `SubstT (p1, p2) ->
        `SubstT (resolved_module_type map p1, resolved_module_type map p2)
    | `AliasModuleType (p1, p2) ->
        `AliasModuleType
          (resolved_module_type map p1, resolved_module_type map p2)
    | `CanonicalModuleType (p1, p2) ->
        `CanonicalModuleType (resolved_module_type map p1, module_type map p2)
    | `OpaqueModuleType m -> `OpaqueModuleType (resolved_module_type map m)

  and resolved_type map (p : Cpath.Resolved.type_) :
      Odoc_model.Paths.Path.Resolved.Type.t =
    match p with
    | `Identifier (#Odoc_model.Paths.Identifier.Path.Type.t as y) ->
        `Identifier y
    | `Local id -> `Identifier (Component.PathTypeMap.find id map.path_type)
    | `CanonicalType (t1, t2) ->
        `CanonicalType (resolved_type map t1, type_ map t2)
    | `Type (p, name) -> `Type (resolved_parent map p, name)
    | `Class (p, name) -> `Class (resolved_parent map p, name)
    | `ClassType (p, name) -> `ClassType (resolved_parent map p, name)
    | `Substituted s -> resolved_type map s

  and resolved_class_type map (p : Cpath.Resolved.class_type) :
      Odoc_model.Paths.Path.Resolved.ClassType.t =
    match p with
    | `Identifier (#Odoc_model.Paths.Identifier.Path.ClassType.t as y) ->
        `Identifier y
    | `Local id ->
        `Identifier (Component.PathClassTypeMap.find id map.path_class_type)
    | `Class (p, name) -> `Class (resolved_parent map p, name)
    | `ClassType (p, name) -> `ClassType (resolved_parent map p, name)
    | `Substituted s -> resolved_class_type map s

  let rec module_fragment :
      maps -> Cfrag.module_ -> Odoc_model.Paths.Fragment.Module.t =
   fun map f ->
    match f with
    | `Resolved r -> `Resolved (resolved_module_fragment map r)
    | `Dot (sg, p) -> `Dot (signature_fragment map sg, p)

  and signature_fragment :
      maps -> Cfrag.signature -> Odoc_model.Paths.Fragment.Signature.t =
   fun map f ->
    match f with
    | `Resolved r -> `Resolved (resolved_signature_fragment map r)
    | `Dot (sg, p) -> `Dot (signature_fragment map sg, p)
    | `Root -> `Root

  and type_fragment : maps -> Cfrag.type_ -> Odoc_model.Paths.Fragment.Type.t =
   fun map f ->
    match f with
    | `Resolved r -> `Resolved (resolved_type_fragment map r)
    | `Dot (sg, p) -> `Dot (signature_fragment map sg, p)

  and resolved_module_fragment :
      maps ->
      Cfrag.resolved_module ->
      Odoc_model.Paths.Fragment.Resolved.Module.t =
   fun map f ->
    match f with
    | `Subst (p, f) ->
        `Subst (resolved_module_type map p, resolved_module_fragment map f)
    | `Alias (p, f) ->
        `Alias (resolved_module map p, resolved_module_fragment map f)
    | `Module (p, n) -> `Module (resolved_signature_fragment map p, n)
    | `OpaqueModule m -> `OpaqueModule (resolved_module_fragment map m)

  and resolved_signature_fragment :
      maps ->
      Cfrag.resolved_signature ->
      Odoc_model.Paths.Fragment.Resolved.Signature.t =
   fun map f ->
    match f with
    | `Root (`ModuleType p) -> `Root (`ModuleType (resolved_module_type map p))
    | `Root (`Module p) -> `Root (`Module (resolved_module map p))
    | (`OpaqueModule _ | `Subst _ | `Alias _ | `Module _) as x ->
        (resolved_module_fragment map x
          :> Odoc_model.Paths.Fragment.Resolved.Signature.t)

  and resolved_type_fragment :
      maps -> Cfrag.resolved_type -> Odoc_model.Paths.Fragment.Resolved.Type.t =
   fun map f ->
    match f with
    | `Type (p, n) -> `Type (resolved_signature_fragment map p, n)
    | `ClassType (p, n) -> `ClassType (resolved_signature_fragment map p, n)
    | `Class (p, n) -> `Class (resolved_signature_fragment map p, n)

  let rec module_type_fragment :
      maps -> Cfrag.module_type -> Odoc_model.Paths.Fragment.ModuleType.t =
   fun map f ->
    match f with
    | `Resolved r -> `Resolved (resolved_module_type_fragment map r)
    | `Dot (sg, p) -> `Dot (signature_fragment map sg, p)

  and resolved_module_type_fragment :
      maps ->
      Cfrag.resolved_module_type ->
      Odoc_model.Paths.Fragment.Resolved.ModuleType.t =
   fun map f ->
    match f with
    | `ModuleType (p, n) -> `Module_type (resolved_signature_fragment map p, n)
end

module ExtractIDs = struct
  open Component

  let rec type_decl parent map id =
    let name = Ident.Name.type_ id in
    let identifier =
      if List.mem name map.shadowed.s_types then
        `Type (parent, Odoc_model.Names.TypeName.internal_of_string name)
      else `Type (parent, Ident.Name.typed_type id)
    in
    {
      map with
      type_ = Component.TypeMap.add id identifier map.type_;
      path_type =
        Component.PathTypeMap.add
          (id :> Ident.path_type)
          (identifier :> Identifier.Path.Type.t)
          map.path_type;
    }

  and module_ parent map id =
    let name = Ident.Name.module_ id in
    let identifier =
      if List.mem name map.shadowed.s_modules then
        `Module (parent, ModuleName.internal_of_string name)
      else `Module (parent, Ident.Name.typed_module id)
    in
    { map with module_ = Component.ModuleMap.add id identifier map.module_ }

  and module_type parent map id =
    let name = Ident.Name.module_type id in
    let identifier =
      if List.mem name map.shadowed.s_module_types then
        `ModuleType (parent, ModuleTypeName.internal_of_string name)
      else `ModuleType (parent, Ident.Name.typed_module_type id)
    in
    {
      map with
      module_type = Component.ModuleTypeMap.add id identifier map.module_type;
    }

  and class_ parent map id =
    let name = Ident.Name.class_ id in
    let identifier =
      if List.mem name map.shadowed.s_classes then
        `Class (parent, ClassName.internal_of_string name)
      else `Class (parent, Ident.Name.typed_class id)
    in
    {
      map with
      class_ = (id, identifier) :: map.class_;
      path_class_type =
        Component.PathClassTypeMap.add
          (id :> Ident.path_class_type)
          (identifier :> Identifier.Path.ClassType.t)
          map.path_class_type;
      path_type =
        Component.PathTypeMap.add
          (id :> Ident.path_type)
          (identifier :> Identifier.Path.Type.t)
          map.path_type;
    }

  and class_type parent map (id : Ident.class_type) =
    let name = Ident.Name.class_type id in
    let identifier =
      if List.mem name map.shadowed.s_class_types then
        `ClassType (parent, ClassTypeName.internal_of_string name)
      else `ClassType (parent, Ident.Name.typed_class_type id)
    in
    {
      map with
      class_type = ((id :> Ident.class_type), identifier) :: map.class_type;
      path_class_type =
        Component.PathClassTypeMap.add
          (id :> Ident.path_class_type)
          (identifier :> Identifier.Path.ClassType.t)
          map.path_class_type;
      path_type =
        Component.PathTypeMap.add
          (id :> Ident.path_type)
          (identifier :> Identifier.Path.Type.t)
          map.path_type;
    }

  and include_ parent map i = signature parent map i.Include.expansion_

  and open_ parent map o = signature parent map o.Open.expansion

  and signature_items parent map items =
    let open Signature in
    let rec inner items map =
      match items with
      | [] -> map
      | Module (id, _, _) :: rest -> inner rest (module_ parent map id)
      | ModuleSubstitution (id, _) :: rest -> inner rest (module_ parent map id)
      | ModuleType (id, _mt) :: rest -> inner rest (module_type parent map id)
      | ModuleTypeSubstitution (id, _mt) :: rest ->
          inner rest (module_type parent map id)
      | Type (id, _, _t) :: rest -> inner rest (type_decl parent map id)
      | TypeSubstitution (id, _t) :: rest ->
          inner rest (type_decl parent map id)
      | Class (id, _, _) :: rest -> inner rest (class_ parent map id)
      | ClassType (id, _, _) :: rest -> inner rest (class_type parent map id)
      | Exception (_, _) :: rest
      | Value (_, _) :: rest
      | TypExt _ :: rest
      | Comment _ :: rest ->
          inner rest map
      | Include i :: rest -> inner rest (include_ parent map i)
      | Open o :: rest -> inner rest (open_ parent map o)
    in
    inner items map

  and signature parent map sg =
    let open Signature in
    signature_items parent map sg.items
end

let rec signature_items id map items =
  let open Component.Signature in
  let map = ExtractIDs.signature_items id map items in
  let parent = id in
  let rec inner : item list -> Odoc_model.Lang.Signature.item list -> _ =
   fun items acc ->
    match items with
    | [] -> List.rev acc
    | Module (id, r, m) :: rest ->
        let m = Component.Delayed.get m in
        inner rest
          (Odoc_model.Lang.Signature.Module (r, module_ map parent id m) :: acc)
    | ModuleType (id, m) :: rest ->
        inner rest
          (Odoc_model.Lang.Signature.ModuleType (module_type map parent id m)
          :: acc)
    | ModuleTypeSubstitution (id, m) :: rest ->
        inner rest
          (Odoc_model.Lang.Signature.ModuleTypeSubstitution
             (module_type_substitution map parent id m)
          :: acc)
    | Type (id, r, t) :: rest ->
        let t = Component.Delayed.get t in
        inner rest (Type (r, type_decl map parent id t) :: acc)
    | Exception (id', e) :: rest ->
        inner rest
          (Exception
             (exception_ map
                (id :> Odoc_model.Paths.Identifier.Signature.t)
                id' e)
          :: acc)
    | TypExt t :: rest -> inner rest (TypExt (typ_ext map id t) :: acc)
    | Value (id, v) :: rest ->
        let v = Component.Delayed.get v in
        inner rest (Value (value_ map parent id v) :: acc)
    | Include i :: rest -> inner rest (Include (include_ id map i) :: acc)
    | Open o :: rest -> inner rest (Open (open_ id map o) :: acc)
    | ModuleSubstitution (id, m) :: rest ->
        inner rest
          (ModuleSubstitution (module_substitution map parent id m) :: acc)
    | TypeSubstitution (id, t) :: rest ->
        inner rest (TypeSubstitution (type_decl map parent id t) :: acc)
    | Class (id, r, c) :: rest ->
        inner rest (Class (r, class_ map parent id c) :: acc)
    | ClassType (id, r, c) :: rest ->
        inner rest (ClassType (r, class_type map parent id c) :: acc)
    | Comment c :: rest ->
        inner rest
          (Comment (docs_or_stop (id :> Identifier.LabelParent.t) c) :: acc)
  in
  inner items []

and signature :
    Paths.Identifier.Signature.t ->
    maps ->
    Component.Signature.t ->
    Lang.Signature.t =
 fun id map sg ->
  let open Component.Signature in
  (* let map = { map with shadowed = empty_shadow } in *)
  {
    items = signature_items id map sg.items;
    compiled = sg.compiled;
    doc = docs (id :> Identifier.LabelParent.t) sg.doc;
  }

and class_ map parent id c =
  let open Component.Class in
  let identifier = List.assoc id map.class_ in
  let expansion =
    Opt.map
      (class_signature map (identifier :> Identifier.ClassSignature.t))
      c.expansion
  in
  {
    id = identifier;
    doc = docs (parent :> Identifier.LabelParent.t) c.doc;
    virtual_ = c.virtual_;
    params = c.params;
    type_ =
      class_decl map (identifier :> Paths.Identifier.Path.ClassType.t) c.type_;
    expansion;
  }

and class_decl map parent c =
  match c with
  | Component.Class.ClassType expr ->
      ClassType (class_type_expr map parent expr)
  | Arrow (lbl, t, d) ->
      Arrow
        ( lbl,
          type_expr map (parent :> Identifier.Parent.t) t,
          class_decl map parent d )

and class_type_expr map parent c =
  match c with
  | Component.ClassType.Constr (p, ts) ->
      Constr
        ( Path.class_type map p,
          List.map (type_expr map (parent :> Identifier.Parent.t)) ts )
  | Signature s -> Signature (class_signature map parent s)

and class_type map parent id c =
  let open Component.ClassType in
  let identifier = List.assoc id map.class_type in
  let expansion =
    Opt.map
      (class_signature map (identifier :> Identifier.ClassSignature.t))
      c.expansion
  in
  {
    Odoc_model.Lang.ClassType.id = identifier;
    doc = docs (parent :> Identifier.LabelParent.t) c.doc;
    virtual_ = c.virtual_;
    params = c.params;
    expr =
      class_type_expr map
        (identifier :> Paths.Identifier.Path.ClassType.t)
        c.expr;
    expansion;
  }

and class_signature map parent sg =
  let open Component.ClassSignature in
  let pparent = (parent :> Identifier.Parent.t) in
  let items =
    List.map
      (function
        | Method (id, m) ->
            Odoc_model.Lang.ClassSignature.Method (method_ map parent id m)
        | InstanceVariable (id, i) ->
            InstanceVariable (instance_variable map parent id i)
        | Constraint (t1, t2) ->
            Constraint (type_expr map pparent t1, type_expr map pparent t2)
        | Inherit e -> Inherit (class_type_expr map parent e)
        | Comment c ->
            Comment (docs_or_stop (parent :> Identifier.LabelParent.t) c))
      sg.items
  and doc = docs (parent :> Identifier.LabelParent.t) sg.doc in
  { self = Opt.map (type_expr map pparent) sg.self; items; doc }

and method_ map parent id m =
  let open Component.Method in
  let identifier = `Method (parent, Ident.Name.typed_method id) in
  {
    id = identifier;
    doc = docs (parent :> Identifier.LabelParent.t) m.doc;
    private_ = m.private_;
    virtual_ = m.virtual_;
    type_ = type_expr map (parent :> Identifier.Parent.t) m.type_;
  }

and instance_variable map parent id i =
  let open Component.InstanceVariable in
  let identifier =
    `InstanceVariable (parent, Ident.Name.typed_instance_variable id)
  in
  {
    id = identifier;
    doc = docs (parent :> Identifier.LabelParent.t) i.doc;
    mutable_ = i.mutable_;
    virtual_ = i.virtual_;
    type_ = type_expr map (parent :> Identifier.Parent.t) i.type_;
  }

and simple_expansion :
    maps ->
    Identifier.Signature.t ->
    Component.ModuleType.simple_expansion ->
    Lang.ModuleType.simple_expansion =
 fun map id e ->
  let open Component.FunctorParameter in
  match e with
  | Signature sg -> Signature (signature id map sg)
  | Functor (Named arg, sg) ->
      let identifier = `Result id in
      let name = Ident.Name.typed_functor_parameter arg.id in
      let param_identifier = `Parameter (id, name) in
      let map =
        {
          map with
          functor_parameter =
            (arg.id, param_identifier) :: map.functor_parameter;
        }
      in
      let arg = functor_parameter map arg in
      Functor (Named arg, simple_expansion map identifier sg)
  | Functor (Unit, sg) -> Functor (Unit, simple_expansion map (`Result id) sg)

and combine_shadowed s1 s2 =
  let open Odoc_model.Lang.Include in
  {
    s_modules = s1.s_modules @ s2.s_modules;
    s_module_types = s1.s_module_types @ s2.s_module_types;
    s_values = s1.s_values @ s2.s_values;
    s_types = s1.s_types @ s2.s_types;
    s_classes = s1.s_classes @ s2.s_classes;
    s_class_types = s1.s_class_types @ s2.s_class_types;
  }

and include_decl :
    maps ->
    Odoc_model.Paths.Identifier.Signature.t ->
    Component.Include.decl ->
    Odoc_model.Lang.Include.decl =
 fun map identifier d ->
  match d with
  | Alias p -> Alias (Path.module_ map p)
  | ModuleType mty -> ModuleType (u_module_type_expr map identifier mty)

and include_ parent map i =
  let open Component.Include in
  {
    Odoc_model.Lang.Include.parent;
    doc = docs (parent :> Identifier.LabelParent.t) i.doc;
    decl = include_decl map parent i.decl;
    expansion =
      {
        shadowed = i.shadowed;
        content =
          signature parent
            { map with shadowed = combine_shadowed map.shadowed i.shadowed }
            i.expansion_;
      };
    status = i.status;
    strengthened = Opt.map (Path.module_ map) i.strengthened;
  }

and open_ parent map o =
  let open Component.Open in
  { Odoc_model.Lang.Open.expansion = signature parent map o.expansion }

and value_ map parent id v =
  let open Component.Value in
  let name = Ident.Name.value id in
  let identifier =
    if List.mem name map.shadowed.s_values then
      `Value (parent, ValueName.internal_of_string name)
    else `Value (parent, Ident.Name.typed_value id)
  in
  {
    id = identifier;
    doc = docs (parent :> Identifier.LabelParent.t) v.doc;
    type_ = type_expr map (parent :> Identifier.Parent.t) v.type_;
    value = v.value;
  }

and typ_ext map parent t =
  let open Component.Extension in
  {
    parent;
    type_path = Path.type_ map t.type_path;
    doc = docs (parent :> Identifier.LabelParent.t) t.doc;
    type_params = t.type_params;
    private_ = t.private_;
    constructors = List.map (extension_constructor map parent) t.constructors;
  }

and extension_constructor map parent c =
  let open Component.Extension.Constructor in
  let identifier = `Extension (parent, ExtensionName.make_std c.name) in
  {
    id = identifier;
    doc = docs (parent :> Identifier.LabelParent.t) c.doc;
    args =
      type_decl_constructor_argument map (parent :> Identifier.Parent.t) c.args;
    res = Opt.map (type_expr map (parent :> Identifier.Parent.t)) c.res;
  }

and module_ map parent id m =
  try
    let open Component.Module in
    let id =
      (Component.ModuleMap.find id map.module_ :> Paths.Identifier.Module.t)
    in
    let identifier = (id :> Odoc_model.Paths.Identifier.Signature.t) in
    let canonical = function
      | Some p -> Some (Path.module_ map p)
      | None -> None
    in
    let map = { map with shadowed = empty_shadow } in
    {
      Odoc_model.Lang.Module.id;
      doc = docs (parent :> Identifier.LabelParent.t) m.doc;
      type_ = module_decl map identifier m.type_;
      canonical = canonical m.canonical;
      hidden = m.hidden;
    }
  with e ->
    let bt = Printexc.get_backtrace () in
    Format.fprintf Format.err_formatter
      "Exception handling module: %a\nbacktrace:\n%s\n%!" Ident.fmt id bt;
    raise e

and module_substitution map parent id m =
  let open Component.ModuleSubstitution in
  {
    Odoc_model.Lang.ModuleSubstitution.id =
      (Component.ModuleMap.find id map.module_ :> Identifier.Module.t);
    doc = docs (parent :> Identifier.LabelParent.t) m.doc;
    manifest = Path.module_ map m.manifest;
  }

and module_decl :
    maps ->
    Odoc_model.Paths.Identifier.Signature.t ->
    Component.Module.decl ->
    Odoc_model.Lang.Module.decl =
 fun map identifier d ->
  match d with
  | Component.Module.Alias (p, s) ->
      Odoc_model.Lang.Module.Alias
        (Path.module_ map p, Opt.map (simple_expansion map identifier) s)
  | ModuleType mty -> ModuleType (module_type_expr map identifier mty)

and mty_substitution map identifier = function
  | Component.ModuleType.ModuleEq (frag, decl) ->
      Odoc_model.Lang.ModuleType.ModuleEq
        (Path.module_fragment map frag, module_decl map identifier decl)
  | ModuleSubst (frag, path) ->
      ModuleSubst (Path.module_fragment map frag, Path.module_ map path)
  | TypeEq (frag, eqn) ->
      TypeEq
        ( Path.type_fragment map frag,
          type_decl_equation map (identifier :> Identifier.Parent.t) eqn )
  | TypeSubst (frag, eqn) ->
      TypeSubst
        ( Path.type_fragment map frag,
          type_decl_equation map (identifier :> Identifier.Parent.t) eqn )
  | ModuleTypeEq (frag, eqn) ->
      ModuleTypeEq
        (Path.module_type_fragment map frag, module_type_expr map identifier eqn)
  | ModuleTypeSubst (frag, eqn) ->
      ModuleTypeSubst
        (Path.module_type_fragment map frag, module_type_expr map identifier eqn)

and u_module_type_expr map identifier = function
  | Component.ModuleType.U.Path p_path ->
      Odoc_model.Lang.ModuleType.U.Path (Path.module_type map p_path)
  | Signature s ->
      Signature
        (signature
           (identifier :> Odoc_model.Paths.Identifier.Signature.t)
           map s)
  | With (subs, expr) ->
      With
        ( List.map (mty_substitution map identifier) subs,
          u_module_type_expr map identifier expr )
  | TypeOf { t_desc = ModPath p; t_expansion } ->
      TypeOf
        {
          t_desc = ModPath (Path.module_ map p);
          t_expansion = Opt.map (simple_expansion map identifier) t_expansion;
        }
  | TypeOf { t_desc = StructInclude p; t_expansion } ->
      TypeOf
        {
          t_desc = StructInclude (Path.module_ map p);
          t_expansion = Opt.map (simple_expansion map identifier) t_expansion;
        }

and module_type_expr map identifier = function
  | Component.ModuleType.Path { p_path; p_expansion } ->
      Odoc_model.Lang.ModuleType.Path
        {
          p_path = Path.module_type map p_path;
          p_expansion = Opt.map (simple_expansion map identifier) p_expansion;
        }
  | Signature s ->
      Signature
        (signature
           (identifier :> Odoc_model.Paths.Identifier.Signature.t)
           map s)
  | With { w_substitutions; w_expansion; w_expr } ->
      With
        {
          w_substitutions =
            List.map (mty_substitution map identifier) w_substitutions;
          w_expansion = Opt.map (simple_expansion map identifier) w_expansion;
          w_expr = u_module_type_expr map identifier w_expr;
        }
  | Functor (Named arg, expr) ->
      let name = Ident.Name.typed_functor_parameter arg.id in
      let identifier' = `Parameter (identifier, name) in
      let map =
        {
          map with
          functor_parameter = (arg.id, identifier') :: map.functor_parameter;
        }
      in
      Functor
        ( Named (functor_parameter map arg),
          module_type_expr map (`Result identifier) expr )
  | Functor (Unit, expr) ->
      Functor (Unit, module_type_expr map (`Result identifier) expr)
  | TypeOf { t_desc = ModPath p; t_expansion } ->
      TypeOf
        {
          t_desc = ModPath (Path.module_ map p);
          t_expansion = Opt.map (simple_expansion map identifier) t_expansion;
        }
  | TypeOf { t_desc = StructInclude p; t_expansion } ->
      TypeOf
        {
          t_desc = StructInclude (Path.module_ map p);
          t_expansion = Opt.map (simple_expansion map identifier) t_expansion;
        }

and module_type :
    maps ->
    Identifier.Signature.t ->
    Ident.module_type ->
    Component.ModuleType.t Component.Delayed.t ->
    Odoc_model.Lang.ModuleType.t =
 fun map parent id mty ->
  let identifier = Component.ModuleTypeMap.find id map.module_type in
  let mty = Component.Delayed.get mty in
  let sig_id = (identifier :> Odoc_model.Paths.Identifier.Signature.t) in
  let map = { map with shadowed = empty_shadow } in
  {
    Odoc_model.Lang.ModuleType.id = identifier;
    doc = docs (parent :> Identifier.LabelParent.t) mty.doc;
    canonical = Opt.map (Path.module_type map) mty.canonical;
    expr = Opt.map (module_type_expr map sig_id) mty.expr;
  }

and module_type_substitution :
    maps ->
    Identifier.Signature.t ->
    Ident.module_type ->
    Component.ModuleTypeSubstitution.t ->
    Odoc_model.Lang.ModuleTypeSubstitution.t =
 fun map parent id mty ->
  let identifier = Component.ModuleTypeMap.find id map.module_type in
  let sig_id = (identifier :> Odoc_model.Paths.Identifier.Signature.t) in
  let map = { map with shadowed = empty_shadow } in
  {
    Odoc_model.Lang.ModuleTypeSubstitution.id = identifier;
    doc = docs (parent :> Identifier.LabelParent.t) mty.doc;
    manifest = module_type_expr map sig_id mty.manifest;
  }

and type_decl_constructor_argument :
    maps ->
    Paths.Identifier.Parent.t ->
    Component.TypeDecl.Constructor.argument ->
    Odoc_model.Lang.TypeDecl.Constructor.argument =
 fun map parent a ->
  match a with
  | Tuple ls -> Tuple (List.map (type_expr map parent) ls)
  | Record fs -> Record (List.map (type_decl_field map parent) fs)

and type_decl_field :
    maps ->
    Identifier.Parent.t ->
    Component.TypeDecl.Field.t ->
    Odoc_model.Lang.TypeDecl.Field.t =
 fun map parent f ->
  let identifier = `Field (parent, FieldName.make_std f.name) in
  {
    id = identifier;
    doc = docs (parent :> Identifier.LabelParent.t) f.doc;
    mutable_ = f.mutable_;
    type_ = type_expr map parent f.type_;
  }

and type_decl_equation map (parent : Identifier.Parent.t)
    (eqn : Component.TypeDecl.Equation.t) : Odoc_model.Lang.TypeDecl.Equation.t
    =
  {
    params = eqn.params;
    private_ = eqn.private_;
    manifest = Opt.map (type_expr map parent) eqn.manifest;
    constraints =
      List.map
        (fun (x, y) -> (type_expr map parent x, type_expr map parent y))
        eqn.constraints;
  }

and type_decl map parent id (t : Component.TypeDecl.t) :
    Odoc_model.Lang.TypeDecl.t =
  let identifier = Component.TypeMap.find id map.type_ in
  {
    id = identifier;
    equation = type_decl_equation map (parent :> Identifier.Parent.t) t.equation;
    doc = docs (parent :> Identifier.LabelParent.t) t.doc;
    canonical = Opt.map (Path.type_ map) t.canonical;
    representation =
      Opt.map (type_decl_representation map identifier) t.representation;
  }

and type_decl_representation map id (t : Component.TypeDecl.Representation.t) :
    Odoc_model.Lang.TypeDecl.Representation.t =
  match t with
  | Extensible -> Extensible
  | Variant cs -> Variant (List.map (type_decl_constructor map id) cs)
  | Record fs ->
      Record
        (List.map
           (type_decl_field map (id :> Odoc_model.Paths.Identifier.Parent.t))
           fs)

and type_decl_constructor :
    maps ->
    Odoc_model.Paths.Identifier.Type.t ->
    Component.TypeDecl.Constructor.t ->
    Odoc_model.Lang.TypeDecl.Constructor.t =
 fun map id t ->
  let identifier = `Constructor (id, ConstructorName.make_std t.name) in
  {
    id = identifier;
    doc = docs (id :> Identifier.LabelParent.t) t.doc;
    args =
      type_decl_constructor_argument map
        (id :> Odoc_model.Paths.Identifier.Parent.t)
        t.args;
    res = Opt.map (type_expr map (id :> Identifier.Parent.t)) t.res;
  }

and type_expr_package map parent t =
  {
    Lang.TypeExpr.Package.path =
      Path.module_type map t.Component.TypeExpr.Package.path;
    substitutions =
      List.map
        (fun (frag, texpr) ->
          (Path.type_fragment map frag, type_expr map parent texpr))
        t.substitutions;
  }

and type_expr map (parent : Identifier.Parent.t) (t : Component.TypeExpr.t) :
    Odoc_model.Lang.TypeExpr.t =
  try
    match t with
    | Var s -> Var s
    | Any -> Any
    | Alias (t, str) -> Alias (type_expr map parent t, str)
    | Arrow (lbl, t1, t2) ->
        Arrow (lbl, type_expr map parent t1, type_expr map parent t2)
    | Tuple ts -> Tuple (List.map (type_expr map parent) ts)
    | Constr (path, ts) ->
        Constr (Path.type_ map path, List.map (type_expr map parent) ts)
    | Polymorphic_variant v ->
        Polymorphic_variant (type_expr_polyvar map parent v)
    | Object o -> Object (type_expr_object map parent o)
    | Class (p, ts) ->
        Class (Path.class_type map p, List.map (type_expr map parent) ts)
    | Poly (strs, t) -> Poly (strs, type_expr map parent t)
    | Package p -> Package (type_expr_package map parent p)
  with e ->
    let bt = Printexc.get_backtrace () in
    Format.fprintf Format.err_formatter
      "Exception %s handling type_expr: %a\nbacktrace:\n%s\n%!"
      (Printexc.to_string e) Component.Fmt.type_expr t bt;
    raise e

and type_expr_polyvar map parent v =
  let constructor c =
    {
      Lang.TypeExpr.Polymorphic_variant.Constructor.name =
        c.Component.TypeExpr.Polymorphic_variant.Constructor.name;
      constant = c.constant;
      arguments = List.map (type_expr map parent) c.arguments;
      doc = docs (parent :> Identifier.LabelParent.t) c.doc;
    }
  in
  let element = function
    | Component.TypeExpr.Polymorphic_variant.Type t ->
        Lang.TypeExpr.Polymorphic_variant.Type (type_expr map parent t)
    | Constructor c -> Constructor (constructor c)
  in
  { kind = v.kind; elements = List.map element v.elements }

and type_expr_object map parent o =
  let method_ m =
    {
      Lang.TypeExpr.Object.name = m.Component.TypeExpr.Object.name;
      type_ = type_expr map parent m.type_;
    }
  in
  let field = function
    | Component.TypeExpr.Object.Method m ->
        Lang.TypeExpr.Object.Method (method_ m)
    | Inherit i -> Inherit (type_expr map parent i)
  in
  { Lang.TypeExpr.Object.fields = List.map field o.fields; open_ = o.open_ }

and functor_parameter map f : Odoc_model.Lang.FunctorParameter.parameter =
  let identifier = List.assoc f.id map.functor_parameter in
  {
    Odoc_model.Lang.FunctorParameter.id = identifier;
    expr =
      module_type_expr map
        (identifier :> Odoc_model.Paths.Identifier.Signature.t)
        f.expr;
  }

and exception_ map parent id (e : Component.Exception.t) :
    Odoc_model.Lang.Exception.t =
  let identifier = `Exception (parent, Ident.Name.typed_exception id) in
  {
    id = identifier;
    doc = docs (parent :> Identifier.LabelParent.t) e.doc;
    args =
      type_decl_constructor_argument map (parent :> Identifier.Parent.t) e.args;
    res = Opt.map (type_expr map (parent :> Identifier.Parent.t)) e.res;
  }

and block_element parent
    (d : Component.CComment.block_element Odoc_model.Location_.with_location) :
    Odoc_model.Comment.block_element Odoc_model.Location_.with_location =
  let value =
    match d.Odoc_model.Location_.value with
    | `Heading (l, id, content) -> (
        try `Heading (l, `Label (parent, Ident.Name.typed_label id), content)
        with Not_found ->
          Format.fprintf Format.err_formatter "Failed to find id: %a\n"
            Ident.fmt id;
          raise Not_found)
    | `Tag t -> `Tag t
    | #Odoc_model.Comment.nestable_block_element as n -> n
  in
  { d with Odoc_model.Location_.value }

and docs :
    Identifier.LabelParent.t ->
    Component.CComment.docs ->
    Odoc_model.Comment.docs =
 fun parent ds -> List.map (fun d -> block_element parent d) ds

and docs_or_stop parent (d : Component.CComment.docs_or_stop) =
  match d with `Docs d -> `Docs (docs parent d) | `Stop -> `Stop

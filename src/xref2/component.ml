module Maps = Odoc_model.Paths.Identifier.Maps

module ModuleMap = Map.Make (struct
  type t = Ident.module_

  let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any)
end)

module TypeMap = Map.Make (struct
  type t = Ident.type_

  let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any)
end)

module PathModuleMap = Map.Make (struct
  type t = Ident.path_module

  let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any)
end)

module ModuleTypeMap = Map.Make (struct
  type t = Ident.module_type

  let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any)
end)

module PathTypeMap = Map.Make (struct
  type t = Ident.path_type

  let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any)
end)

module PathClassTypeMap = Map.Make (struct
  type t = Ident.path_class_type

  let compare a b = Ident.compare (a :> Ident.any) (b :> Ident.any)
end)

module IdentMap = Map.Make (struct
  type t = Ident.any

  let compare = Ident.compare
end)

module Delayed = struct
  let eager = ref false

  type 'a t = { mutable v : 'a option; mutable get : (unit -> 'a) option }

  let get : 'a t -> 'a =
   fun x ->
    match (x.v, x.get) with
    | Some x, _ -> x
    | None, Some get ->
        let v = get () in
        x.v <- Some v;
        x.get <- None;
        v
    | _, _ -> failwith "bad delayed"

  let put : (unit -> 'a) -> 'a t =
   fun f ->
    if !eager then { v = Some (f ()); get = None }
    else { v = None; get = Some f }

  let put_val : 'a -> 'a t = fun v -> { v = Some v; get = None }
end

module Opt = struct
  let map f = function Some x -> Some (f x) | None -> None
end

module rec Module : sig
  type decl =
    | Alias of Cpath.module_ * ModuleType.simple_expansion option
    | ModuleType of ModuleType.expr

  type t = {
    doc : CComment.docs;
    type_ : decl;
    canonical : Cpath.module_ option;
    hidden : bool;
  }
end =
  Module

and ModuleSubstitution : sig
  type t = { doc : CComment.docs; manifest : Cpath.module_ }
end =
  ModuleSubstitution

and ModuleTypeSubstitution : sig
  type t = { doc : CComment.docs; manifest : ModuleType.expr }
end =
  ModuleTypeSubstitution

and TypeExpr : sig
  module Polymorphic_variant : sig
    type kind = Odoc_model.Lang.TypeExpr.Polymorphic_variant.kind

    module Constructor : sig
      type t = {
        name : string;
        constant : bool;
        arguments : TypeExpr.t list;
        doc : CComment.docs;
      }
    end

    type element = Type of TypeExpr.t | Constructor of Constructor.t

    type t = { kind : kind; elements : element list }
  end

  module Object : sig
    type method_ = { name : string; type_ : TypeExpr.t }

    type field = Method of method_ | Inherit of TypeExpr.t

    type t = { fields : field list; open_ : bool }
  end

  module Package : sig
    type substitution = Cfrag.type_ * TypeExpr.t

    type t = { path : Cpath.module_type; substitutions : substitution list }
  end

  type label = Odoc_model.Lang.TypeExpr.label

  type t =
    | Var of string
    | Any
    | Alias of t * string
    | Arrow of label option * t * t
    | Tuple of t list
    | Constr of Cpath.type_ * t list
    | Polymorphic_variant of TypeExpr.Polymorphic_variant.t
    | Object of TypeExpr.Object.t
    | Class of Cpath.class_type * t list
    | Poly of string list * t
    | Package of TypeExpr.Package.t
end =
  TypeExpr

and Extension : sig
  module Constructor : sig
    type t = {
      name : string;
      doc : CComment.docs;
      args : TypeDecl.Constructor.argument;
      res : TypeExpr.t option;
    }
  end

  type t = {
    type_path : Cpath.type_;
    doc : CComment.docs;
    type_params : TypeDecl.param list;
    private_ : bool;
    constructors : Constructor.t list;
  }
end =
  Extension

and Exception : sig
  type t = {
    doc : CComment.docs;
    args : TypeDecl.Constructor.argument;
    res : TypeExpr.t option;
  }
end =
  Exception

and FunctorParameter : sig
  type parameter = { id : Ident.functor_parameter; expr : ModuleType.expr }

  type t = Named of parameter | Unit
end =
  FunctorParameter

and ModuleType : sig
  type substitution =
    | ModuleEq of Cfrag.module_ * Module.decl
    | ModuleSubst of Cfrag.module_ * Cpath.module_
    | ModuleTypeEq of Cfrag.module_type * ModuleType.expr
    | ModuleTypeSubst of Cfrag.module_type * ModuleType.expr
    | TypeEq of Cfrag.type_ * TypeDecl.Equation.t
    | TypeSubst of Cfrag.type_ * TypeDecl.Equation.t

  type type_of_desc =
    | ModPath of Cpath.module_
    | StructInclude of Cpath.module_

  type simple_expansion =
    | Signature of Signature.t
    | Functor of FunctorParameter.t * simple_expansion

  type typeof_t = {
    t_desc : type_of_desc;
    t_expansion : simple_expansion option;
  }

  module U : sig
    type expr =
      | Path of Cpath.module_type
      | Signature of Signature.t
      | With of substitution list * expr
      | TypeOf of typeof_t
  end

  type path_t = {
    p_expansion : simple_expansion option;
    p_path : Cpath.module_type;
  }

  type with_t = {
    w_substitutions : substitution list;
    w_expansion : simple_expansion option;
    w_expr : U.expr;
  }

  type expr =
    | Path of path_t
    | Signature of Signature.t
    | With of with_t
    | Functor of FunctorParameter.t * expr
    | TypeOf of typeof_t

  type t = {
    doc : CComment.docs;
    canonical : Cpath.module_type option;
    expr : expr option;
  }
end =
  ModuleType

and TypeDecl : sig
  module Field : sig
    type t = {
      name : string;
      doc : CComment.docs;
      mutable_ : bool;
      type_ : TypeExpr.t;
    }
  end

  module Constructor : sig
    type argument = Tuple of TypeExpr.t list | Record of Field.t list

    type t = {
      name : string;
      doc : CComment.docs;
      args : argument;
      res : TypeExpr.t option;
    }
  end

  module Representation : sig
    type t =
      | Variant of Constructor.t list
      | Record of Field.t list
      | Extensible
  end

  type param = Odoc_model.Lang.TypeDecl.param

  module Equation : sig
    type t = {
      params : param list;
      private_ : bool;
      manifest : TypeExpr.t option;
      constraints : (TypeExpr.t * TypeExpr.t) list;
    }
  end

  type t = {
    doc : CComment.docs;
    canonical : Cpath.type_ option;
    equation : Equation.t;
    representation : Representation.t option;
  }
end =
  TypeDecl

and Value : sig
  type value = Odoc_model.Lang.Value.value

  type t = { doc : CComment.docs; type_ : TypeExpr.t; value : value }
end =
  Value

and Signature : sig
  type recursive = Odoc_model.Lang.Signature.recursive

  type item =
    | Module of Ident.module_ * recursive * Module.t Delayed.t
    | ModuleSubstitution of Ident.module_ * ModuleSubstitution.t
    | ModuleType of Ident.module_type * ModuleType.t Delayed.t
    | ModuleTypeSubstitution of Ident.module_type * ModuleTypeSubstitution.t
    | Type of Ident.type_ * recursive * TypeDecl.t Delayed.t
    | TypeSubstitution of Ident.type_ * TypeDecl.t
    | Exception of Ident.exception_ * Exception.t
    | TypExt of Extension.t
    | Value of Ident.value * Value.t Delayed.t
    | Class of Ident.class_ * recursive * Class.t
    | ClassType of Ident.class_type * recursive * ClassType.t
    | Include of Include.t
    | Open of Open.t
    | Comment of CComment.docs_or_stop

  (* When doing destructive substitution we keep track of the items that have been removed,
       and the path they've been substituted with *)
  type removed_item =
    | RModule of Ident.module_ * Cpath.Resolved.module_
    | RType of Ident.type_ * TypeExpr.t * TypeDecl.Equation.t
        (** [RType (_, texpr, eq)], [eq.manifest = Some texpr] *)
    | RModuleType of Ident.module_type * ModuleType.expr

  type t = {
    items : item list;
    compiled : bool;
    removed : removed_item list;
    doc : CComment.docs;
  }
end =
  Signature

and Open : sig
  type t = { expansion : Signature.t }
end =
  Open

and Include : sig
  type decl = Alias of Cpath.module_ | ModuleType of ModuleType.U.expr

  type t = {
    parent : Odoc_model.Paths.Identifier.Signature.t;
    strengthened : Cpath.module_ option;
    doc : CComment.docs;
    status : [ `Default | `Inline | `Closed | `Open ];
    shadowed : Odoc_model.Lang.Include.shadowed;
    expansion_ : Signature.t;
    decl : decl;
  }
end =
  Include

and Class : sig
  type decl =
    | ClassType of ClassType.expr
    | Arrow of TypeExpr.label option * TypeExpr.t * decl

  type t = {
    doc : CComment.docs;
    virtual_ : bool;
    params : TypeDecl.param list;
    type_ : decl;
    expansion : ClassSignature.t option;
  }
end =
  Class

and ClassType : sig
  type expr =
    | Constr of Cpath.class_type * TypeExpr.t list
    | Signature of ClassSignature.t

  type t = {
    doc : CComment.docs;
    virtual_ : bool;
    params : TypeDecl.param list;
    expr : expr;
    expansion : ClassSignature.t option;
  }
end =
  ClassType

and ClassSignature : sig
  type item =
    | Method of Ident.method_ * Method.t
    | InstanceVariable of Ident.instance_variable * InstanceVariable.t
    | Constraint of TypeExpr.t * TypeExpr.t
    | Inherit of ClassType.expr
    | Comment of CComment.docs_or_stop

  type t = { self : TypeExpr.t option; items : item list; doc : CComment.docs }
end =
  ClassSignature

and Method : sig
  type t = {
    doc : CComment.docs;
    private_ : bool;
    virtual_ : bool;
    type_ : TypeExpr.t;
  }
end =
  Method

and InstanceVariable : sig
  type t = {
    doc : CComment.docs;
    mutable_ : bool;
    virtual_ : bool;
    type_ : TypeExpr.t;
  }
end =
  InstanceVariable

and Substitution : sig
  type subst_module =
    [ `Prefixed of Cpath.module_ * Cpath.Resolved.module_
    | `Substituted
    | `Renamed of Ident.path_module ]

  type subst_module_type =
    [ `Prefixed of Cpath.module_type * Cpath.Resolved.module_type
    | `Renamed of Ident.module_type ]

  type subst_type =
    [ `Prefixed of Cpath.type_ * Cpath.Resolved.type_
    | `Renamed of Ident.path_type ]

  type subst_class_type =
    [ `Prefixed of Cpath.class_type * Cpath.Resolved.class_type
    | `Renamed of Ident.path_class_type ]

  type t = {
    module_ : subst_module PathModuleMap.t;
    module_type : subst_module_type ModuleTypeMap.t;
    type_ : subst_type PathTypeMap.t;
    class_type : subst_class_type PathClassTypeMap.t;
    type_replacement : (TypeExpr.t * TypeDecl.Equation.t) PathTypeMap.t;
    module_type_replacement : ModuleType.expr ModuleTypeMap.t;
    path_invalidating_modules : Ident.path_module list;
    module_type_of_invalidating_modules : Ident.path_module list;
    unresolve_opaque_paths : bool;
  }
end =
  Substitution

and CComment : sig
  type block_element =
    [ Odoc_model.Comment.nestable_block_element
    | `Heading of
      Odoc_model.Comment.heading_level
      * Ident.label
      * Odoc_model.Comment.link_content
    | `Tag of Odoc_model.Comment.tag ]

  type docs = block_element Odoc_model.Comment.with_location list

  type docs_or_stop = [ `Docs of docs | `Stop ]
end =
  CComment

module Element = struct
  open Odoc_model.Paths

  type module_ = [ `Module of Identifier.Path.Module.t * Module.t Delayed.t ]

  type module_type = [ `ModuleType of Identifier.ModuleType.t * ModuleType.t ]

  type type_ = [ `Type of Identifier.Type.t * TypeDecl.t ]

  type value = [ `Value of Identifier.Value.t * Value.t ]

  type label = [ `Label of Identifier.Label.t ]

  type class_ = [ `Class of Identifier.Class.t * Class.t ]

  type class_type = [ `ClassType of Identifier.ClassType.t * ClassType.t ]

  type datatype = [ type_ | class_ | class_type ]

  type signature = [ module_ | module_type ]

  type constructor =
    [ `Constructor of Identifier.Constructor.t * TypeDecl.Constructor.t ]

  type exception_ = [ `Exception of Identifier.Exception.t * Exception.t ]

  type extension =
    [ `Extension of Identifier.Extension.t * Extension.Constructor.t ]

  type field = [ `Field of Identifier.Field.t * TypeDecl.Field.t ]

  type label_parent = [ signature | datatype ]

  type any =
    [ signature
    | value
    | type_
    | label
    | class_
    | class_type
    | constructor
    | exception_
    | extension
    | field ]
end

module Fmt = struct
  open Odoc_model.Names

  let fpf = Format.fprintf

  let fpp_opt fmt pp_a ppf = function
    | Some t -> fpf ppf fmt pp_a t
    | None -> ()

  let fpp_list fmt_sep fmt_outer pp_a ppf t =
    let pp_sep ppf () = fpf ppf fmt_sep in
    match t with
    | [] -> ()
    | t -> fpf ppf fmt_outer (Format.pp_print_list ~pp_sep pp_a) t

  let rec signature ppf sg =
    let open Signature in
    Format.fprintf ppf "@[<v>";
    List.iter
      (function
        | Module (id, _, m) ->
            Format.fprintf ppf "@[<v 2>module %a %a@]@," Ident.fmt id module_
              (Delayed.get m)
        | ModuleSubstitution (id, m) ->
            Format.fprintf ppf "@[<v 2>module %a := %a@]@," Ident.fmt id
              module_path m.ModuleSubstitution.manifest
        | ModuleType (id, mt) ->
            Format.fprintf ppf "@[<v 2>module type %a %a@]@," Ident.fmt id
              module_type (Delayed.get mt)
        | ModuleTypeSubstitution (id, mts) ->
            Format.fprintf ppf "@[<v 2>module type %a := %a@]@," Ident.fmt id
              module_type_expr mts.ModuleTypeSubstitution.manifest
        | Type (id, _, t) ->
            Format.fprintf ppf "@[<v 2>type %a%a@]@," Ident.fmt id type_decl
              (Delayed.get t)
        | TypeSubstitution (id, t) ->
            Format.fprintf ppf "@[<v 2>type %a :=%a@]@," Ident.fmt id type_decl
              t
        | Exception (id, e) ->
            Format.fprintf ppf "@[<v 2>exception %a %a@]@," Ident.fmt id
              exception_ e
        | TypExt e ->
            Format.fprintf ppf "@[<v 2>type_extension %a@]@," extension e
        | Value (id, v) ->
            Format.fprintf ppf "@[<v 2>val %a %a@]@," Ident.fmt id value
              (Delayed.get v)
        | Class (id, _, c) ->
            Format.fprintf ppf "@[<v 2>class %a %a@]@," Ident.fmt id class_ c
        | ClassType (id, _, c) ->
            Format.fprintf ppf "@[<v 2>class type %a %a@]@," Ident.fmt id
              class_type c
        | Include i -> Format.fprintf ppf "@[<v 2>include %a@]@," include_ i
        | Open o -> Format.fprintf ppf "open [ %a ]" signature o.expansion
        | Comment _c -> ())
      sg.items;
    Format.fprintf ppf "@] (removed=[%a])" removed_item_list sg.removed

  and option pp ppf x =
    match x with
    | Some x -> Format.fprintf ppf "Some(%a)" pp x
    | None -> Format.fprintf ppf "None"

  and class_signature ppf sg =
    let open ClassSignature in
    Format.fprintf ppf "@[<v>self=%a@," (option type_expr) sg.self;
    List.iter
      (function
        | Method (id, m) ->
            Format.fprintf ppf "@[<v 2>method %a : %a@]@," Ident.fmt id method_
              m
        | InstanceVariable (id, i) ->
            Format.fprintf ppf "@[<v 2>instance variable %a : %a@]@," Ident.fmt
              id instance_variable i
        | Constraint (t1, t2) ->
            Format.fprintf ppf "@[<v 2>constraint %a = %a@]@," type_expr t1
              type_expr t2
        | Inherit i -> Format.fprintf ppf "@[<v 2>inherit %a" class_type_expr i
        | Comment _ -> ())
      sg.items

  and method_ ppf m =
    let open Method in
    Format.fprintf ppf "%s%s%a"
      (if m.private_ then "private " else "")
      (if m.virtual_ then "virtual " else "")
      type_expr m.type_

  and instance_variable ppf i =
    let open InstanceVariable in
    Format.fprintf ppf "%s%s%a"
      (if i.mutable_ then "mutable " else "")
      (if i.virtual_ then "virtual " else "")
      type_expr i.type_

  and list pp ppf ls =
    match ls with
    | x :: y :: rest -> Format.fprintf ppf "%a, %a" pp x (list pp) (y :: rest)
    | [ x ] -> Format.fprintf ppf "%a" pp x
    | [] -> ()

  and class_type_expr ppf c =
    let open ClassType in
    match c with
    | Constr (p, ts) ->
        Format.fprintf ppf "constr(%a,%a)" class_type_path p (list type_expr) ts
    | Signature sg -> Format.fprintf ppf "(%a)" class_signature sg

  and removed_item ppf r =
    let open Signature in
    match r with
    | RModule (id, path) ->
        Format.fprintf ppf "module %a (%a)" Ident.fmt id resolved_module_path
          path
    | RType (id, texpr, eq) ->
        Format.fprintf ppf "type %a %a = (%a)" type_params eq.params Ident.fmt
          id type_expr texpr
    | RModuleType (id, mty) ->
        Format.fprintf ppf "module type %a = %a" Ident.fmt id module_type_expr
          mty

  and removed_item_list ppf r =
    match r with
    | [] -> ()
    | [ x ] -> Format.fprintf ppf "%a" removed_item x
    | x :: ys -> Format.fprintf ppf "%a;%a" removed_item x removed_item_list ys

  and class_decl ppf c =
    let open Class in
    match c with
    | ClassType cty -> Format.fprintf ppf "%a" class_type_expr cty
    | Arrow (lbl, ty, decl) ->
        Format.fprintf ppf "%a%a -> %a" type_expr_label lbl type_expr ty
          class_decl decl

  and class_ ppf c = Format.fprintf ppf "%a" class_decl c.type_

  and class_type ppf _c = Format.fprintf ppf "<todo>"

  and include_ ppf i =
    Format.fprintf ppf "%a (sig = %a)" include_decl i.decl signature
      i.expansion_

  and include_decl ppf =
    let open Include in
    function
    | Alias p -> Format.fprintf ppf "= %a" module_path p
    | ModuleType mt -> Format.fprintf ppf ": %a" u_module_type_expr mt

  and value ppf v =
    let open Value in
    Format.fprintf ppf ": %a" type_expr v.type_

  and module_decl ppf d =
    let open Module in
    match d with
    | Alias (p, _) -> Format.fprintf ppf "= %a" module_path p
    | ModuleType mt -> Format.fprintf ppf ": %a" module_type_expr mt

  and module_ ppf m = Format.fprintf ppf "%a" module_decl m.type_

  and simple_expansion ppf (m : ModuleType.simple_expansion) =
    match m with
    | ModuleType.Signature sg -> Format.fprintf ppf "sig: %a" signature sg
    | Functor (arg, sg) ->
        Format.fprintf ppf "functor: (%a) -> %a" functor_parameter arg
          simple_expansion sg

  and module_type ppf mt =
    match mt.expr with
    | Some x -> Format.fprintf ppf "= %a" module_type_expr x
    | None -> ()

  and module_type_type_of_desc ppf t =
    match t with
    | ModuleType.ModPath p ->
        Format.fprintf ppf "module type of %a" module_path p
    | StructInclude p ->
        Format.fprintf ppf "module type of struct include %a end" module_path p

  and u_module_type_expr ppf mt =
    let open ModuleType.U in
    match mt with
    | Path p -> module_type_path ppf p
    | Signature sg -> Format.fprintf ppf "sig@,@[<v 2>%a@]end" signature sg
    | With (subs, e) ->
        Format.fprintf ppf "%a with [%a]" u_module_type_expr e substitution_list
          subs
    | TypeOf { t_desc; _ } -> module_type_type_of_desc ppf t_desc

  and module_type_expr ppf mt =
    let open ModuleType in
    match mt with
    | Path { p_path; _ } -> module_type_path ppf p_path
    | Signature sg -> Format.fprintf ppf "sig@,@[<v 2>%a@]end" signature sg
    | With { w_substitutions = subs; w_expr; _ } ->
        Format.fprintf ppf "%a with [%a]" u_module_type_expr w_expr
          substitution_list subs
    | Functor (arg, res) ->
        Format.fprintf ppf "(%a) -> %a" functor_parameter arg module_type_expr
          res
    | TypeOf { t_desc = ModPath p; _ } ->
        Format.fprintf ppf "module type of %a" module_path p
    | TypeOf { t_desc = StructInclude p; _ } ->
        Format.fprintf ppf "module type of struct include %a end" module_path p

  and functor_parameter ppf x =
    let open FunctorParameter in
    match x with
    | Unit -> ()
    | Named x -> Format.fprintf ppf "%a" functor_parameter_parameter x

  and functor_parameter_parameter ppf x =
    Format.fprintf ppf "%a : %a" Ident.fmt x.FunctorParameter.id
      module_type_expr x.FunctorParameter.expr

  and type_decl ppf t =
    let open TypeDecl in
    match t.representation with
    | Some repr ->
        Format.fprintf ppf "%a = %a"
          (fpp_opt " : %a" type_expr)
          t.equation.Equation.manifest type_decl_repr repr
    | None -> (fpp_opt " = %a" type_expr) ppf t.equation.Equation.manifest

  and type_decl_repr ppf =
    let open TypeDecl.Representation in
    function
    | Variant cs -> fpp_list " | " "%a" type_decl_constructor ppf cs
    | Record fs -> type_decl_fields ppf fs
    | Extensible -> Format.fprintf ppf ".."

  and type_decl_constructor ppf t =
    let open TypeDecl.Constructor in
    match t.res with
    | Some res ->
        fpf ppf "%s : %a -> %a" t.name type_decl_constructor_arg t.args
          type_expr res
    | None -> fpf ppf "%s of %a" t.name type_decl_constructor_arg t.args

  and type_decl_constructor_arg ppf =
    let open TypeDecl.Constructor in
    function
    | Tuple ts -> type_tuple ppf ts | Record fs -> type_decl_fields ppf fs

  and type_decl_field ppf t =
    let open TypeDecl.Field in
    let mutable_ = if t.mutable_ then "mutable " else "" in
    fpf ppf "%s%s : %a" mutable_ t.name type_expr t.type_

  and type_decl_fields ppf fs = fpp_list "; " "{ %a }" type_decl_field ppf fs

  and type_tuple ppf ts = fpp_list " * " "%a" type_expr ppf ts

  and type_param ppf t =
    let desc =
      match t.Odoc_model.Lang.TypeDecl.desc with Any -> "_" | Var n -> n
    and variance =
      match t.variance with Some Pos -> "+" | Some Neg -> "-" | None -> ""
    and injectivity = if t.injectivity then "!" else "" in
    Format.fprintf ppf "%s%s%s" variance injectivity desc

  and type_params ppf ts =
    let pp_sep ppf () = Format.fprintf ppf ", " in
    Format.fprintf ppf "(%a)" (Format.pp_print_list ~pp_sep type_param) ts

  and type_equation ppf t =
    match t.TypeDecl.Equation.manifest with
    | None -> ()
    | Some m -> Format.fprintf ppf " = %a" type_expr m

  and exception_ _ppf _e = ()

  and extension ppf e = Format.fprintf ppf "%a" type_path e.Extension.type_path

  and substitution ppf t =
    let open ModuleType in
    match t with
    | ModuleEq (frag, decl) ->
        Format.fprintf ppf "%a = %a" module_fragment frag module_decl decl
    | ModuleSubst (frag, mpath) ->
        Format.fprintf ppf "%a := %a" module_fragment frag module_path mpath
    | ModuleTypeEq (frag, mty) ->
        Format.fprintf ppf "%a = %a" module_type_fragment frag module_type_expr
          mty
    | ModuleTypeSubst (frag, mty) ->
        Format.fprintf ppf "%a := %a" module_type_fragment frag module_type_expr
          mty
    | TypeEq (frag, decl) ->
        Format.fprintf ppf "%a%a" type_fragment frag type_equation decl
    | TypeSubst (frag, decl) ->
        Format.fprintf ppf "%a%a" type_fragment frag type_equation decl

  and substitution_list ppf l =
    match l with
    | [ sub ] -> Format.fprintf ppf "%a" substitution sub
    | sub :: subs ->
        Format.fprintf ppf "%a; %a" substitution sub substitution_list subs
    | [] -> ()

  and type_expr_label ppf l =
    match l with
    | Some (Odoc_model.Lang.TypeExpr.Label l) -> Format.fprintf ppf "%s:" l
    | Some (Optional o) -> Format.fprintf ppf "?%s:" o
    | None -> ()

  and type_expr_list ppf l =
    match l with
    | [ t ] -> Format.fprintf ppf "%a" type_expr t
    | t :: ts -> Format.fprintf ppf "%a * %a" type_expr t type_expr_list ts
    | [] -> ()

  and type_object ppf _o = Format.fprintf ppf "(object)"

  and type_class ppf (x, ys) =
    Format.fprintf ppf "(class %a %a)" class_type_path x type_expr_list ys

  and type_package ppf _p = Format.fprintf ppf "(package)"

  and type_expr_polymorphic_variant ppf p =
    let open TypeExpr.Polymorphic_variant in
    let pp_element ppf = function
      | Type t -> type_expr ppf t
      | Constructor c ->
          fpf ppf "`%s%a" c.Constructor.name
            (fpp_list " * " " of %a" type_expr)
            c.arguments
    in
    let pp_elements = fpp_list " | " "%a" pp_element in
    match p.kind with
    | Fixed -> fpf ppf "[ %a ]" pp_elements p.elements
    | Closed xs ->
        fpf ppf "[ %a > %a ]" pp_elements p.elements
          (fpp_list " " "%a" Format.pp_print_string)
          xs
    | Open -> fpf ppf "[> %a ]" pp_elements p.elements

  and type_expr ppf e =
    let open TypeExpr in
    match e with
    | Var x -> Format.fprintf ppf "%s" x
    | Any -> Format.fprintf ppf "_"
    | Alias (x, y) -> Format.fprintf ppf "(alias %a %s)" type_expr x y
    | Arrow (l, t1, t2) ->
        Format.fprintf ppf "%a(%a) -> %a" type_expr_label l type_expr t1
          type_expr t2
    | Tuple ts -> Format.fprintf ppf "(%a)" type_expr_list ts
    | Constr (p, args) -> (
        match args with
        | [] -> Format.fprintf ppf "%a" type_path p
        | _ -> Format.fprintf ppf "[%a] %a" type_expr_list args type_path p)
    | Polymorphic_variant poly ->
        Format.fprintf ppf "(poly_var %a)" type_expr_polymorphic_variant poly
    | Object x -> type_object ppf x
    | Class (x, y) -> type_class ppf (x, y)
    | Poly (_ss, _t) -> Format.fprintf ppf "(poly)"
    | Package x -> type_package ppf x

  and resolved_module_path : Format.formatter -> Cpath.Resolved.module_ -> unit
      =
   fun ppf p ->
    match p with
    | `Local ident -> Format.fprintf ppf "%a" Ident.fmt ident
    | `Apply (p1, p2) ->
        Format.fprintf ppf "%a(%a)" resolved_module_path p1 resolved_module_path
          p2
    | `Identifier p ->
        Format.fprintf ppf "%a" model_identifier
          (p :> Odoc_model.Paths.Identifier.t)
    | `Substituted p ->
        Format.fprintf ppf "substituted(%a)" resolved_module_path p
    | `Module (p, m) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.ModuleName.to_string m)
    | `Alias (p1, p2) ->
        Format.fprintf ppf "alias(%a,%a)" resolved_module_path p1
          resolved_module_path p2
    | `Subst (p1, p2) ->
        Format.fprintf ppf "subst(%a,%a)" resolved_module_type_path p1
          resolved_module_path p2
    | `Hidden p1 -> Format.fprintf ppf "hidden(%a)" resolved_module_path p1
    | `Canonical (p1, p2) ->
        Format.fprintf ppf "canonical(%a,%a)" resolved_module_path p1
          module_path p2
    | `OpaqueModule m ->
        Format.fprintf ppf "opaquemodule(%a)" resolved_module_path m

  and module_path : Format.formatter -> Cpath.module_ -> unit =
   fun ppf p ->
    match p with
    | `Resolved p -> Format.fprintf ppf "r(%a)" resolved_module_path p
    | `Dot (p, str) -> Format.fprintf ppf "%a.%s" module_path p str
    | `Module (p, n) ->
        Format.fprintf ppf "%a.%a" resolved_parent_path p ModuleName.fmt n
    | `Apply (p1, p2) ->
        Format.fprintf ppf "%a(%a)" module_path p1 module_path p2
    | `Identifier (id, b) ->
        Format.fprintf ppf "identifier(%a, %b)" model_identifier
          (id :> Odoc_model.Paths.Identifier.t)
          b
    | `Local (id, b) -> Format.fprintf ppf "local(%a,%b)" Ident.fmt id b
    | `Substituted p -> Format.fprintf ppf "substituted(%a)" module_path p
    | `Forward s -> Format.fprintf ppf "forward(%s)" s
    | `Root r -> Format.fprintf ppf "unresolvedroot(%s)" r

  and resolved_module_type_path :
      Format.formatter -> Cpath.Resolved.module_type -> unit =
   fun ppf p ->
    match p with
    | `Local id -> Format.fprintf ppf "%a" Ident.fmt id
    | `Identifier id ->
        Format.fprintf ppf "%a" model_identifier
          (id :> Odoc_model.Paths.Identifier.t)
    | `Substituted x ->
        Format.fprintf ppf "substituted(%a)" resolved_module_type_path x
    | `ModuleType (p, m) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (ModuleTypeName.to_string m)
    | `CanonicalModuleType (m1, m2) ->
        Format.fprintf ppf "canonicalt(%a,%a)" resolved_module_type_path m1
          module_type_path m2
    | `OpaqueModuleType m ->
        Format.fprintf ppf "opaquemoduletype(%a)" resolved_module_type_path m
    | `AliasModuleType (mt1, mt2) ->
        Format.fprintf ppf "aliasmoduletype(%a,%a)" resolved_module_type_path
          mt1 resolved_module_type_path mt2
    | `SubstT (mt1, mt2) ->
        Format.fprintf ppf "subst(%a,%a)" resolved_module_type_path mt1
          resolved_module_type_path mt2

  and module_type_path : Format.formatter -> Cpath.module_type -> unit =
   fun ppf m ->
    match m with
    | `Resolved p -> Format.fprintf ppf "r(%a)" resolved_module_type_path p
    | `Identifier (id, b) ->
        Format.fprintf ppf "identifier(%a, %b)" model_identifier
          (id :> Odoc_model.Paths.Identifier.t)
          b
    | `Local (id, b) -> Format.fprintf ppf "local(%a,%b)" Ident.fmt id b
    | `Substituted s -> Format.fprintf ppf "substituted(%a)" module_type_path s
    | `Dot (m, s) -> Format.fprintf ppf "%a.%s" module_path m s
    | `ModuleType (m, n) ->
        Format.fprintf ppf "%a.%a" resolved_parent_path m ModuleTypeName.fmt n

  and resolved_type_path : Format.formatter -> Cpath.Resolved.type_ -> unit =
   fun ppf p ->
    match p with
    | `Local id -> Format.fprintf ppf "%a" Ident.fmt id
    | `Identifier id ->
        Format.fprintf ppf "%a" model_identifier
          (id :> Odoc_model.Paths.Identifier.t)
    | `Substituted x ->
        Format.fprintf ppf "substituted(%a)" resolved_type_path x
    | `CanonicalType (t1, t2) ->
        Format.fprintf ppf "canonicalty(%a,%a)" resolved_type_path t1 type_path
          t2
    | `Class (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.ClassName.to_string t)
    | `ClassType (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.ClassTypeName.to_string t)
    | `Type (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.TypeName.to_string t)

  and resolved_parent_path : Format.formatter -> Cpath.Resolved.parent -> unit =
   fun ppf p ->
    match p with
    | `Module m -> resolved_module_path ppf m
    | `ModuleType m -> Format.fprintf ppf ">>%a<<" resolved_module_type_path m
    | `FragmentRoot -> Format.fprintf ppf "FragmentRoot"

  and type_path : Format.formatter -> Cpath.type_ -> unit =
   fun ppf p ->
    match p with
    | `Resolved r -> Format.fprintf ppf "r(%a)" resolved_type_path r
    | `Identifier (id, b) ->
        Format.fprintf ppf "identifier(%a, %b)" model_identifier
          (id :> Odoc_model.Paths.Identifier.t)
          b
    | `Local (id, b) -> Format.fprintf ppf "local(%a,%b)" Ident.fmt id b
    | `Substituted s -> Format.fprintf ppf "substituted(%a)" type_path s
    | `Dot (m, s) -> Format.fprintf ppf "%a.%s" module_path m s
    | `Class (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.ClassName.to_string t)
    | `ClassType (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.ClassTypeName.to_string t)
    | `Type (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.TypeName.to_string t)

  and resolved_class_type_path :
      Format.formatter -> Cpath.Resolved.class_type -> unit =
   fun ppf p ->
    match p with
    | `Local id -> Format.fprintf ppf "%a" Ident.fmt id
    | `Identifier id ->
        Format.fprintf ppf "%a" model_identifier
          (id :> Odoc_model.Paths.Identifier.t)
    | `Substituted s ->
        Format.fprintf ppf "substituted(%a)" resolved_class_type_path s
    | `Class (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.ClassName.to_string t)
    | `ClassType (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.ClassTypeName.to_string t)

  and class_type_path : Format.formatter -> Cpath.class_type -> unit =
   fun ppf p ->
    match p with
    | `Resolved r -> Format.fprintf ppf "%a" resolved_class_type_path r
    | `Identifier (id, b) ->
        Format.fprintf ppf "identifier(%a, %b)" model_identifier
          (id :> Odoc_model.Paths.Identifier.t)
          b
    | `Local (id, b) -> Format.fprintf ppf "local(%a,%b)" Ident.fmt id b
    | `Substituted s -> Format.fprintf ppf "substituted(%a)" class_type_path s
    | `Dot (m, s) -> Format.fprintf ppf "%a.%s" module_path m s
    | `Class (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.ClassName.to_string t)
    | `ClassType (p, t) ->
        Format.fprintf ppf "%a.%s" resolved_parent_path p
          (Odoc_model.Names.ClassTypeName.to_string t)

  and model_path : Format.formatter -> Odoc_model.Paths.Path.t -> unit =
   fun ppf (p : Odoc_model.Paths.Path.t) ->
    match p with
    | `Resolved rp -> Format.fprintf ppf "r(%a)" model_resolved_path rp
    | `Identifier (id, b) ->
        Format.fprintf ppf "identifier(%a, %b)" model_identifier
          (id :> Odoc_model.Paths.Identifier.t)
          b
    | `Root s -> Format.fprintf ppf "root(%s)" s
    | `Forward s -> Format.fprintf ppf "forward(%s)" s
    | `Dot (parent, s) ->
        Format.fprintf ppf "%a.%s" model_path
          (parent :> Odoc_model.Paths.Path.t)
          s
    | `Apply (func, arg) ->
        Format.fprintf ppf "%a(%a)" model_path
          (func :> Odoc_model.Paths.Path.t)
          model_path
          (arg :> Odoc_model.Paths.Path.t)

  and model_resolved_path ppf (p : Odoc_model.Paths.Path.Resolved.t) =
    let open Odoc_model.Paths.Path.Resolved in
    match p with
    | `Identifier id ->
        Format.fprintf ppf "%a" model_identifier
          (id :> Odoc_model.Paths.Identifier.t)
    | `Module (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_path
          (parent :> t)
          (Odoc_model.Names.ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_path
          (parent :> t)
          (Odoc_model.Names.ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_path
          (parent :> t)
          (Odoc_model.Names.TypeName.to_string name)
    | `Alias (path, realpath) ->
        Format.fprintf ppf "alias(%a,%a)" model_resolved_path
          (path :> t)
          model_resolved_path
          (realpath :> t)
    | `AliasModuleType (path, realpath) ->
        Format.fprintf ppf "aliasmoduletype(%a,%a)" model_resolved_path
          (path :> t)
          model_resolved_path
          (realpath :> t)
    | `Subst (modty, m) ->
        Format.fprintf ppf "subst(%a,%a)" model_resolved_path
          (modty :> t)
          model_resolved_path
          (m :> t)
    | `SubstT (t1, t2) ->
        Format.fprintf ppf "substt(%a,%a)" model_resolved_path
          (t1 :> t)
          model_resolved_path
          (t2 :> t)
    | `CanonicalModuleType (t1, t2) ->
        Format.fprintf ppf "canonicalt(%a,%a)" model_resolved_path
          (t1 :> t)
          model_path
          (t2 :> Odoc_model.Paths.Path.t)
    | `CanonicalType (t1, t2) ->
        Format.fprintf ppf "canonicalty(%a,%a)" model_resolved_path
          (t1 :> t)
          model_path
          (t2 :> Odoc_model.Paths.Path.t)
    | `Apply (funct, arg) ->
        Format.fprintf ppf "%a(%a)" model_resolved_path
          (funct :> t)
          model_resolved_path
          (arg :> t)
    | `Canonical (p1, p2) ->
        Format.fprintf ppf "canonical(%a,%a)" model_resolved_path
          (p1 :> t)
          model_path
          (p2 :> Odoc_model.Paths.Path.t)
    | `Hidden p -> Format.fprintf ppf "hidden(%a)" model_resolved_path (p :> t)
    | `Class (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_path
          (parent :> t)
          (Odoc_model.Names.ClassName.to_string name)
    | `ClassType (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_path
          (parent :> t)
          (Odoc_model.Names.ClassTypeName.to_string name)
    | `OpaqueModule m ->
        Format.fprintf ppf "opaquemodule(%a)" model_resolved_path (m :> t)
    | `OpaqueModuleType m ->
        Format.fprintf ppf "opaquemoduletype(%a)" model_resolved_path (m :> t)

  and model_identifier ppf (p : Odoc_model.Paths.Identifier.t) =
    match p with
    | `Root (_, unit_name) ->
        Format.fprintf ppf "(root %s)"
          (Odoc_model.Names.ModuleName.to_string unit_name)
    | `Module (parent, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (parent :> Odoc_model.Paths.Identifier.t)
          (Odoc_model.Names.ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (parent :> Odoc_model.Paths.Identifier.t)
          (Odoc_model.Names.ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (parent :> Odoc_model.Paths.Identifier.t)
          (Odoc_model.Names.TypeName.to_string name)
    | `Parameter (parent, name) ->
        Format.fprintf ppf "(param %a %s)" model_identifier
          (parent :> Odoc_model.Paths.Identifier.t)
          (Odoc_model.Names.ParameterName.to_string name)
    | `Result parent ->
        Format.fprintf ppf "%a.result" model_identifier
          (parent :> Odoc_model.Paths.Identifier.t)
    | `CoreType name ->
        Format.fprintf ppf "%s" (Odoc_model.Names.TypeName.to_string name)
    | `Constructor (ty, x) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (ty :> Odoc_model.Paths.Identifier.t)
          (ConstructorName.to_string x)
    | `Value (parent, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (parent :> Odoc_model.Paths.Identifier.t)
          (ValueName.to_string name)
    | `CoreException name ->
        Format.fprintf ppf "%s" (ExceptionName.to_string name)
    | `Class (sg, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (sg :> Odoc_model.Paths.Identifier.t)
          (ClassName.to_string name)
    | `ClassType (sg, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (sg :> Odoc_model.Paths.Identifier.t)
          (ClassTypeName.to_string name)
    | `InstanceVariable (sg, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (sg :> Odoc_model.Paths.Identifier.t)
          (InstanceVariableName.to_string name)
    | `Method (sg, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (sg :> Odoc_model.Paths.Identifier.t)
          (MethodName.to_string name)
    | `Label (parent, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (parent :> Odoc_model.Paths.Identifier.t)
          (LabelName.to_string name)
    | `Field (ty, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (ty :> Odoc_model.Paths.Identifier.t)
          (FieldName.to_string name)
    | `Exception (p, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (p :> Odoc_model.Paths.Identifier.t)
          (ExceptionName.to_string name)
    | `Extension (p, name) ->
        Format.fprintf ppf "%a.%s" model_identifier
          (p :> Odoc_model.Paths.Identifier.t)
          (ExtensionName.to_string name)
    | `Page (_, name) | `LeafPage (_, name) ->
        Format.fprintf ppf "%s" (PageName.to_string name)

  and model_fragment ppf (f : Odoc_model.Paths.Fragment.t) =
    match f with
    | `Resolved rf -> model_resolved_fragment ppf rf
    | `Dot (sg, d) ->
        Format.fprintf ppf "*%a.%s" model_fragment
          (sg :> Odoc_model.Paths.Fragment.t)
          d
    | `Root -> ()

  and model_resolved_fragment ppf (f : Odoc_model.Paths.Fragment.Resolved.t) =
    let open Odoc_model.Paths.Fragment.Resolved in
    match f with
    | `Root (`ModuleType p) ->
        Format.fprintf ppf "root(%a)" model_resolved_path
          (p :> Odoc_model.Paths.Path.Resolved.t)
    | `Root (`Module p) ->
        Format.fprintf ppf "root(%a)" model_resolved_path
          (p :> Odoc_model.Paths.Path.Resolved.t)
    | `Module (sg, m) ->
        Format.fprintf ppf "%a.%s" model_resolved_fragment
          (sg :> t)
          (Odoc_model.Names.ModuleName.to_string m)
    | `Module_type (sg, mty) ->
        Format.fprintf ppf "%a.%s" model_resolved_fragment
          (sg :> t)
          (Odoc_model.Names.ModuleTypeName.to_string mty)
    | `Type (sg, t) ->
        Format.fprintf ppf "%a.%s" model_resolved_fragment
          (sg :> t)
          (Odoc_model.Names.TypeName.to_string t)
    | `Subst (path, m) ->
        Format.fprintf ppf "(%a subst -> %a)" model_resolved_path
          (path :> Odoc_model.Paths.Path.Resolved.t)
          model_resolved_fragment
          (m :> t)
    | `Alias (_, _) -> Format.fprintf ppf "UNIMPLEMENTED subst alias!?"
    | `Class (sg, c) ->
        Format.fprintf ppf "%a.%s" model_resolved_fragment
          (sg :> t)
          (Odoc_model.Names.ClassName.to_string c)
    | `ClassType (sg, c) ->
        Format.fprintf ppf "%a.%s" model_resolved_fragment
          (sg :> t)
          (Odoc_model.Names.ClassTypeName.to_string c)
    | `OpaqueModule m ->
        Format.fprintf ppf "opaquemodule(%a)" model_resolved_fragment
          (m :> Odoc_model.Paths.Fragment.Resolved.t)

  and resolved_root_fragment ppf (f : Cfrag.root) =
    match f with
    | `ModuleType p -> Format.fprintf ppf "root(%a)" resolved_module_type_path p
    | `Module p -> Format.fprintf ppf "root(%a)" resolved_module_path p

  and resolved_signature_fragment ppf (f : Cfrag.resolved_signature) =
    match f with
    | `Root r -> Format.fprintf ppf "%a" resolved_root_fragment r
    | (`Subst _ | `Alias _ | `Module _) as x -> resolved_module_fragment ppf x
    | `OpaqueModule m ->
        Format.fprintf ppf "opaquemodule(%a)" resolved_module_fragment m

  and resolved_module_fragment ppf (f : Cfrag.resolved_module) =
    match f with
    | `Subst (s, f) ->
        Format.fprintf ppf "subst(%a,%a)" resolved_module_type_path s
          resolved_module_fragment f
    | `Alias (m, f) ->
        Format.fprintf ppf "substalias(%a,%a)" resolved_module_path m
          resolved_module_fragment f
    | `Module (p, n) ->
        Format.fprintf ppf "%a.%s" resolved_signature_fragment p
          (ModuleName.to_string n)
    | `OpaqueModule m ->
        Format.fprintf ppf "opaquemodule(%a)" resolved_module_fragment m

  and resolved_module_type_fragment ppf (f : Cfrag.resolved_module_type) =
    match f with
    | `ModuleType (p, n) ->
        Format.fprintf ppf "%a.%s" resolved_signature_fragment p
          (ModuleTypeName.to_string n)

  and resolved_type_fragment ppf (f : Cfrag.resolved_type) =
    match f with
    | `Type (s, n) ->
        Format.fprintf ppf "%a.%s" resolved_signature_fragment s
          (TypeName.to_string n)
    | `Class (s, n) ->
        Format.fprintf ppf "%a.%s" resolved_signature_fragment s
          (ClassName.to_string n)
    | `ClassType (s, n) ->
        Format.fprintf ppf "%a.%s" resolved_signature_fragment s
          (ClassTypeName.to_string n)

  and signature_fragment ppf (f : Cfrag.signature) =
    match f with
    | `Resolved r -> Format.fprintf ppf "r(%a)" resolved_signature_fragment r
    | `Dot (s, n) -> Format.fprintf ppf "%a.%s" signature_fragment s n
    | `Root -> Format.fprintf ppf "root"

  and module_fragment ppf (f : Cfrag.module_) =
    match f with
    | `Resolved r -> Format.fprintf ppf "r(%a)" resolved_module_fragment r
    | `Dot (s, n) -> Format.fprintf ppf "%a.%s" signature_fragment s n

  and module_type_fragment ppf (f : Cfrag.module_type) =
    match f with
    | `Resolved r ->
        Format.fprintf ppf "resolved(%a)" resolved_module_type_fragment r
    | `Dot (s, n) -> Format.fprintf ppf "%a.%s" signature_fragment s n

  and type_fragment ppf (f : Cfrag.type_) =
    match f with
    | `Resolved r -> Format.fprintf ppf "r(%a)" resolved_type_fragment r
    | `Dot (s, n) -> Format.fprintf ppf "%a.%s" signature_fragment s n

  and model_resolved_reference ppf (r : Odoc_model.Paths.Reference.Resolved.t) =
    let open Odoc_model.Paths.Reference.Resolved in
    match r with
    | `Identifier id -> Format.fprintf ppf "%a" model_identifier id
    | `Hidden p ->
        Format.fprintf ppf "hidden(%a)" model_resolved_reference (p :> t)
    | `Module (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference
          (parent :> t)
          (ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference
          (parent :> t)
          (ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference
          (parent :> t)
          (TypeName.to_string name)
    | `Constructor (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference
          (parent :> t)
          (ConstructorName.to_string name)
    | `Field (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference
          (parent :> t)
          (FieldName.to_string name)
    | `Extension (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference
          (parent :> t)
          (ExtensionName.to_string name)
    | `Exception (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference
          (parent :> t)
          (ExceptionName.to_string name)
    | `Value (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference
          (parent :> t)
          (ValueName.to_string name)
    | `Class (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference
          (parent :> t)
          (ClassName.to_string name)
    | `ClassType (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference
          (parent :> t)
          (ClassTypeName.to_string name)
    | `Method (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference
          (parent :> t)
          (MethodName.to_string name)
    | `InstanceVariable (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference
          (parent :> t)
          (InstanceVariableName.to_string name)
    | `Alias (x, y) ->
        Format.fprintf ppf "alias(%a,%a)" model_resolved_path
          (x :> Odoc_model.Paths.Path.Resolved.t)
          model_resolved_reference
          (y :> Odoc_model.Paths.Reference.Resolved.t)
    | `AliasModuleType (x, y) ->
        Format.fprintf ppf "aliasmoduletype(%a,%a)" model_resolved_path
          (x :> Odoc_model.Paths.Path.Resolved.t)
          model_resolved_reference
          (y :> Odoc_model.Paths.Reference.Resolved.t)
    | `Canonical (x, y) ->
        Format.fprintf ppf "canonical(%a,%a)" model_resolved_reference
          (x :> t)
          model_reference
          (y :> Odoc_model.Paths.Reference.t)
    | `Label (parent, name) ->
        Format.fprintf ppf "%a.%s" model_resolved_reference
          (parent :> t)
          (LabelName.to_string name)

  and model_reference ppf (r : Odoc_model.Paths.Reference.t) =
    let open Odoc_model.Paths.Reference in
    match r with
    | `Resolved r' -> Format.fprintf ppf "r(%a)" model_resolved_reference r'
    | `Root (name, _) -> Format.fprintf ppf "unresolvedroot(%s)" name
    | `Dot (parent, str) ->
        Format.fprintf ppf "%a.%s" model_reference (parent :> t) str
    | `Module (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference
          (parent :> t)
          (ModuleName.to_string name)
    | `ModuleType (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference
          (parent :> t)
          (ModuleTypeName.to_string name)
    | `Type (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference
          (parent :> t)
          (TypeName.to_string name)
    | `Constructor (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference
          (parent :> t)
          (ConstructorName.to_string name)
    | `Field (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference
          (parent :> t)
          (FieldName.to_string name)
    | `Extension (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference
          (parent :> t)
          (ExtensionName.to_string name)
    | `Exception (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference
          (parent :> t)
          (ExceptionName.to_string name)
    | `Value (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference
          (parent :> t)
          (ValueName.to_string name)
    | `Class (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference
          (parent :> t)
          (ClassName.to_string name)
    | `ClassType (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference
          (parent :> t)
          (ClassTypeName.to_string name)
    | `Method (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference
          (parent :> t)
          (MethodName.to_string name)
    | `InstanceVariable (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference
          (parent :> t)
          (InstanceVariableName.to_string name)
    | `Label (parent, name) ->
        Format.fprintf ppf "%a.%s" model_reference
          (parent :> t)
          (LabelName.to_string name)
end

module LocalIdents = struct
  open Odoc_model
  (** The purpose of this module is to extract identifiers
      that could be referenced in Paths - that is, modules,
      module types, types, classes and class types. That way
      we can assign them an Ident.t ahead of time and be
      self-consistent. Because we don't need _all_ of the
      identifiers we don't traverse the entire structure.
      Additionally, we stop at (class_)signature boundaries
      since identifiers within these won't be referenced 
      except within them, so we only do that on demand. *)

  type t = {
    modules : Paths.Identifier.Sets.Module.t;
    module_types : Paths.Identifier.Sets.ModuleType.t;
    types : Paths.Identifier.Sets.Type.t;
    classes : Paths.Identifier.Sets.Class.t;
    class_types : Paths.Identifier.Sets.ClassType.t;
  }

  let empty =
    let open Paths.Identifier.Sets in
    {
      modules = Module.empty;
      module_types = ModuleType.empty;
      types = Type.empty;
      classes = Class.empty;
      class_types = ClassType.empty;
    }

  open Lang

  let rec signature_items s ids =
    let open Paths in
    let open Signature in
    List.fold_right
      (fun c ids ->
        match c with
        | Module (_, { Module.id; _ }) ->
            { ids with modules = Identifier.Sets.Module.add id ids.modules }
        | ModuleType m ->
            {
              ids with
              module_types =
                Identifier.Sets.ModuleType.add m.ModuleType.id ids.module_types;
            }
        | ModuleSubstitution { ModuleSubstitution.id; _ } ->
            { ids with modules = Identifier.Sets.Module.add id ids.modules }
        | ModuleTypeSubstitution { ModuleTypeSubstitution.id; _ } ->
            {
              ids with
              module_types = Identifier.Sets.ModuleType.add id ids.module_types;
            }
        | Type (_, t) ->
            {
              ids with
              types = Identifier.Sets.Type.add t.TypeDecl.id ids.types;
            }
        | TypeSubstitution t ->
            {
              ids with
              types = Identifier.Sets.Type.add t.TypeDecl.id ids.types;
            }
        | Class (_, c) ->
            {
              ids with
              classes = Identifier.Sets.Class.add c.Class.id ids.classes;
            }
        | ClassType (_, c) ->
            {
              ids with
              class_types =
                Identifier.Sets.ClassType.add c.ClassType.id ids.class_types;
            }
        | TypExt _ | Exception _ | Value _ | Comment _ -> ids
        | Include i -> signature i.Include.expansion.content ids
        | Open o -> signature o.Open.expansion ids)
      s ids

  and signature s ids = signature_items s.items ids
end

module Of_Lang = struct
  open Odoc_model

  type map = {
    modules : Ident.module_ Paths.Identifier.Maps.Module.t;
    module_types : Ident.module_type Paths.Identifier.Maps.ModuleType.t;
    functor_parameters :
      Ident.functor_parameter Paths.Identifier.Maps.FunctorParameter.t;
    types : Ident.type_ Paths.Identifier.Maps.Type.t;
    path_types : Ident.path_type Paths.Identifier.Maps.Path.Type.t;
    path_class_types :
      Ident.path_class_type Paths.Identifier.Maps.Path.ClassType.t;
    classes : Ident.class_ Paths.Identifier.Maps.Class.t;
    class_types : Ident.class_type Paths.Identifier.Maps.ClassType.t;
  }

  let empty =
    let open Paths.Identifier.Maps in
    {
      modules = Module.empty;
      module_types = ModuleType.empty;
      functor_parameters = FunctorParameter.empty;
      types = Type.empty;
      path_types = Path.Type.empty;
      path_class_types = Path.ClassType.empty;
      classes = Class.empty;
      class_types = ClassType.empty;
    }

  let map_of_idents ids map =
    let open Paths.Identifier in
    let types_new =
      Sets.Type.fold
        (fun i acc -> Maps.Type.add i (Ident.Of_Identifier.type_ i) acc)
        ids.LocalIdents.types Maps.Type.empty
    in
    let classes_new =
      Sets.Class.fold
        (fun i acc -> Maps.Class.add i (Ident.Of_Identifier.class_ i) acc)
        ids.LocalIdents.classes Maps.Class.empty
    in
    let class_types_new =
      Sets.ClassType.fold
        (fun i acc ->
          Maps.ClassType.add i (Ident.Of_Identifier.class_type i) acc)
        ids.LocalIdents.class_types Maps.ClassType.empty
    in
    let modules_new =
      Sets.Module.fold
        (fun i acc ->
          Maps.Module.add (i :> Module.t) (Ident.Of_Identifier.module_ i) acc)
        ids.LocalIdents.modules Maps.Module.empty
    in
    let module_types_new =
      Sets.ModuleType.fold
        (fun i acc ->
          Maps.ModuleType.add i (Ident.Of_Identifier.module_type i) acc)
        ids.LocalIdents.module_types Maps.ModuleType.empty
    in
    let path_class_types_new =
      Maps.Path.ClassType.empty
      |> Maps.ClassType.fold
           (fun key v acc ->
             Maps.Path.ClassType.add
               (key :> Path.ClassType.t)
               (v :> Ident.path_class_type)
               acc)
           class_types_new
      |> Maps.Class.fold
           (fun key v acc ->
             Maps.Path.ClassType.add
               (key :> Path.ClassType.t)
               (v :> Ident.path_class_type)
               acc)
           classes_new
    in
    let path_types_new =
      Maps.Path.Type.empty
      |> Maps.Path.ClassType.fold
           (fun key v acc ->
             Maps.Path.Type.add (key :> Path.Type.t) (v :> Ident.path_type) acc)
           path_class_types_new
      |> Maps.Type.fold
           (fun key v acc ->
             Maps.Path.Type.add (key :> Path.Type.t) (v :> Ident.path_type) acc)
           types_new
    in
    let merge_fn _k v1 v2 =
      match (v1, v2) with
      | _, Some x -> Some x
      | None, None -> None
      | Some x, None -> Some x
    in
    let modules = Maps.Module.merge merge_fn modules_new map.modules in
    let module_types =
      Maps.ModuleType.merge merge_fn module_types_new map.module_types
    in
    let functor_parameters = map.functor_parameters in
    let types = Maps.Type.merge merge_fn types_new map.types in
    let classes = Maps.Class.merge merge_fn classes_new map.classes in
    let class_types =
      Maps.ClassType.merge merge_fn class_types_new map.class_types
    in
    let path_types =
      Maps.Path.Type.merge merge_fn path_types_new map.path_types
    in
    let path_class_types =
      Maps.Path.ClassType.merge merge_fn path_class_types_new
        map.path_class_types
    in
    {
      modules;
      module_types;
      functor_parameters;
      types;
      classes;
      class_types;
      path_types;
      path_class_types;
    }

  let option conv ident_map x =
    match x with None -> None | Some x' -> Some (conv ident_map x')

  let identifier lookup map i =
    match lookup i map with
    | x -> `Local x
    | exception Not_found -> `Identifier i

  let find_any_module i ident_map =
    match i with
    | #Paths.Identifier.Module.t as id ->
        (Maps.Module.find id ident_map.modules :> Ident.path_module)
    | #Paths.Identifier.FunctorParameter.t as id ->
        (Maps.FunctorParameter.find id ident_map.functor_parameters
          :> Ident.path_module)
    | _ -> raise Not_found

  let rec resolved_module_path :
      _ -> Odoc_model.Paths.Path.Resolved.Module.t -> Cpath.Resolved.module_ =
   fun ident_map p ->
    let recurse p = resolved_module_path ident_map p in
    match p with
    | `Identifier i -> identifier find_any_module ident_map i
    | `Module (p, name) -> `Module (`Module (recurse p), name)
    | `Apply (p1, p2) -> `Apply (recurse p1, recurse p2)
    | `Alias (p1, p2) -> `Alias (recurse p1, recurse p2)
    | `Subst (p1, p2) ->
        `Subst (resolved_module_type_path ident_map p1, recurse p2)
    | `Canonical (p1, p2) -> `Canonical (recurse p1, module_path ident_map p2)
    | `Hidden p1 -> `Hidden (recurse p1)
    | `OpaqueModule m -> `OpaqueModule (recurse m)

  and resolved_module_type_path :
      _ ->
      Odoc_model.Paths.Path.Resolved.ModuleType.t ->
      Cpath.Resolved.module_type =
   fun ident_map p ->
    match p with
    | `Identifier i -> identifier Maps.ModuleType.find ident_map.module_types i
    | `ModuleType (p, name) ->
        `ModuleType (`Module (resolved_module_path ident_map p), name)
    | `CanonicalModuleType (p1, p2) ->
        `CanonicalModuleType
          (resolved_module_type_path ident_map p1, module_type_path ident_map p2)
    | `OpaqueModuleType m ->
        `OpaqueModuleType (resolved_module_type_path ident_map m)
    | `AliasModuleType (m1, m2) ->
        `AliasModuleType
          ( resolved_module_type_path ident_map m1,
            resolved_module_type_path ident_map m2 )
    | `SubstT (p1, p2) ->
        `SubstT
          ( resolved_module_type_path ident_map p1,
            resolved_module_type_path ident_map p2 )

  and resolved_type_path :
      _ -> Odoc_model.Paths.Path.Resolved.Type.t -> Cpath.Resolved.type_ =
   fun ident_map p ->
    match p with
    | `Identifier i -> identifier Maps.Path.Type.find ident_map.path_types i
    | `CanonicalType (p1, p2) ->
        `CanonicalType (resolved_type_path ident_map p1, type_path ident_map p2)
    | `Type (p, name) -> `Type (`Module (resolved_module_path ident_map p), name)
    | `Class (p, name) ->
        `Class (`Module (resolved_module_path ident_map p), name)
    | `ClassType (p, name) ->
        `ClassType (`Module (resolved_module_path ident_map p), name)

  and resolved_class_type_path :
      _ ->
      Odoc_model.Paths.Path.Resolved.ClassType.t ->
      Cpath.Resolved.class_type =
   fun ident_map p ->
    match p with
    | `Identifier i ->
        identifier Maps.Path.ClassType.find ident_map.path_class_types i
    | `Class (p, name) ->
        `Class (`Module (resolved_module_path ident_map p), name)
    | `ClassType (p, name) ->
        `ClassType (`Module (resolved_module_path ident_map p), name)

  and module_path : _ -> Odoc_model.Paths.Path.Module.t -> Cpath.module_ =
   fun ident_map p ->
    match p with
    | `Resolved r -> `Resolved (resolved_module_path ident_map r)
    | `Identifier (i, b) -> (
        match identifier find_any_module ident_map i with
        | `Identifier i -> `Identifier (i, b)
        | `Local i -> `Local (i, b))
    | `Dot (path', x) -> `Dot (module_path ident_map path', x)
    | `Apply (p1, p2) ->
        `Apply (module_path ident_map p1, module_path ident_map p2)
    | `Forward str -> `Forward str
    | `Root str -> `Root str

  and module_type_path :
      _ -> Odoc_model.Paths.Path.ModuleType.t -> Cpath.module_type =
   fun ident_map p ->
    match p with
    | `Resolved r -> `Resolved (resolved_module_type_path ident_map r)
    | `Identifier (i, b) -> (
        match identifier Maps.ModuleType.find ident_map.module_types i with
        | `Identifier i -> `Identifier (i, b)
        | `Local i -> `Local (i, b))
    | `Dot (path', x) -> `Dot (module_path ident_map path', x)

  and type_path : _ -> Odoc_model.Paths.Path.Type.t -> Cpath.type_ =
   fun ident_map p ->
    match p with
    | `Resolved r -> `Resolved (resolved_type_path ident_map r)
    | `Identifier (i, b) -> (
        match identifier Maps.Path.Type.find ident_map.path_types i with
        | `Identifier i -> `Identifier (i, b)
        | `Local i -> `Local (i, b))
    | `Dot (path', x) -> `Dot (module_path ident_map path', x)

  and class_type_path :
      _ -> Odoc_model.Paths.Path.ClassType.t -> Cpath.class_type =
   fun ident_map p ->
    match p with
    | `Resolved r -> `Resolved (resolved_class_type_path ident_map r)
    | `Identifier (i, b) -> (
        match
          identifier Maps.Path.ClassType.find ident_map.path_class_types i
        with
        | `Identifier i -> `Identifier (i, b)
        | `Local i -> `Local (i, b))
    | `Dot (path', x) -> `Dot (module_path ident_map path', x)

  let rec resolved_signature_fragment :
      map ->
      Odoc_model.Paths.Fragment.Resolved.Signature.t ->
      Cfrag.resolved_signature =
   fun ident_map ty ->
    match ty with
    | `Root (`ModuleType path) ->
        `Root (`ModuleType (resolved_module_type_path ident_map path))
    | `Root (`Module path) ->
        `Root (`Module (resolved_module_path ident_map path))
    | (`Alias _ | `Subst _ | `Module _ | `OpaqueModule _) as x ->
        (resolved_module_fragment ident_map x :> Cfrag.resolved_signature)

  and resolved_module_fragment :
      _ -> Odoc_model.Paths.Fragment.Resolved.Module.t -> Cfrag.resolved_module
      =
   fun ident_map ty ->
    match ty with
    | `Subst (p, m) ->
        `Subst
          ( resolved_module_type_path ident_map p,
            resolved_module_fragment ident_map m )
    | `Alias (p, m) ->
        `Alias
          ( resolved_module_path ident_map p,
            resolved_module_fragment ident_map m )
    | `Module (p, m) -> `Module (resolved_signature_fragment ident_map p, m)
    | `OpaqueModule m -> `OpaqueModule (resolved_module_fragment ident_map m)

  and resolved_module_type_fragment :
      _ ->
      Odoc_model.Paths.Fragment.Resolved.ModuleType.t ->
      Cfrag.resolved_module_type =
   fun ident_map ty ->
    match ty with
    | `Module_type (p, m) ->
        `ModuleType (resolved_signature_fragment ident_map p, m)

  and resolved_type_fragment :
      _ -> Odoc_model.Paths.Fragment.Resolved.Type.t -> Cfrag.resolved_type =
   fun ident_map ty ->
    match ty with
    | `Type (p, n) -> `Type (resolved_signature_fragment ident_map p, n)
    | `Class (p, n) -> `Class (resolved_signature_fragment ident_map p, n)
    | `ClassType (p, n) ->
        `ClassType (resolved_signature_fragment ident_map p, n)

  let rec signature_fragment :
      _ -> Odoc_model.Paths.Fragment.Signature.t -> Cfrag.signature =
   fun ident_map ty ->
    match ty with
    | `Resolved r -> `Resolved (resolved_signature_fragment ident_map r)
    | `Dot (p, n) -> `Dot (signature_fragment ident_map p, n)
    | `Root -> `Root

  let module_fragment : _ -> Odoc_model.Paths.Fragment.Module.t -> Cfrag.module_
      =
   fun ident_map ty ->
    match ty with
    | `Resolved r -> `Resolved (resolved_module_fragment ident_map r)
    | `Dot (p, n) -> `Dot (signature_fragment ident_map p, n)

  let module_type_fragment :
      _ -> Odoc_model.Paths.Fragment.ModuleType.t -> Cfrag.module_type =
   fun ident_map ty ->
    match ty with
    | `Resolved r -> `Resolved (resolved_module_type_fragment ident_map r)
    | `Dot (p, n) -> `Dot (signature_fragment ident_map p, n)

  let type_fragment : _ -> Odoc_model.Paths.Fragment.Type.t -> Cfrag.type_ =
   fun ident_map ty ->
    match ty with
    | `Resolved r -> `Resolved (resolved_type_fragment ident_map r)
    | `Dot (p, n) -> `Dot (signature_fragment ident_map p, n)

  let rec type_decl ident_map ty =
    let open Odoc_model.Lang.TypeDecl in
    {
      TypeDecl.doc = docs ident_map ty.doc;
      canonical = Opt.map (type_path ident_map) ty.canonical;
      equation = type_equation ident_map ty.equation;
      representation =
        Opt.map (type_decl_representation ident_map) ty.representation;
    }

  and type_decl_representation ident_map r =
    let open Odoc_model.Lang.TypeDecl.Representation in
    match r with
    | Variant cs ->
        TypeDecl.Representation.Variant
          (List.map (type_decl_constructor ident_map) cs)
    | Record fs -> Record (List.map (type_decl_field ident_map) fs)
    | Extensible -> Extensible

  and type_decl_constructor ident_map t =
    let open Odoc_model.Lang.TypeDecl.Constructor in
    let args = type_decl_constructor_argument ident_map t.args in
    let res = Opt.map (type_expression ident_map) t.res in
    {
      TypeDecl.Constructor.name = Paths.Identifier.name t.id;
      doc = docs ident_map t.doc;
      args;
      res;
    }

  and type_decl_constructor_argument ident_map a =
    let open Odoc_model.Lang.TypeDecl.Constructor in
    match a with
    | Tuple ts ->
        TypeDecl.Constructor.Tuple (List.map (type_expression ident_map) ts)
    | Record fs -> Record (List.map (type_decl_field ident_map) fs)

  and type_decl_field ident_map f =
    let open Odoc_model.Lang.TypeDecl.Field in
    let type_ = type_expression ident_map f.type_ in
    {
      TypeDecl.Field.name = Paths.Identifier.name f.id;
      doc = docs ident_map f.doc;
      mutable_ = f.mutable_;
      type_;
    }

  and type_equation ident_map teq =
    let open Odoc_model.Lang.TypeDecl.Equation in
    {
      TypeDecl.Equation.params = teq.params;
      private_ = teq.private_;
      manifest = option type_expression ident_map teq.manifest;
      constraints =
        List.map
          (fun (x, y) ->
            (type_expression ident_map x, type_expression ident_map y))
          teq.constraints;
    }

  and type_expr_polyvar ident_map v =
    let open Odoc_model.Lang.TypeExpr.Polymorphic_variant in
    let map_element = function
      | Type expr ->
          TypeExpr.Polymorphic_variant.Type (type_expression ident_map expr)
      | Constructor c ->
          Constructor
            TypeExpr.Polymorphic_variant.Constructor.
              {
                name = c.name;
                constant = c.constant;
                arguments = List.map (type_expression ident_map) c.arguments;
                doc = docs ident_map c.doc;
              }
    in
    {
      TypeExpr.Polymorphic_variant.kind = v.kind;
      elements = List.map map_element v.elements;
    }

  and type_object ident_map o =
    let open Odoc_model.Lang.TypeExpr.Object in
    let map_field = function
      | Method m ->
          TypeExpr.(
            Object.Method
              {
                Object.name = m.name;
                type_ = type_expression ident_map m.type_;
              })
      | Inherit i -> Inherit (type_expression ident_map i)
    in
    { TypeExpr.Object.open_ = o.open_; fields = List.map map_field o.fields }

  and type_package ident_map pkg =
    let open Odoc_model.Lang.TypeExpr.Package in
    {
      TypeExpr.Package.path = module_type_path ident_map pkg.path;
      substitutions =
        List.map
          (fun (x, y) ->
            let f = type_fragment ident_map x in
            (f, type_expression ident_map y))
          pkg.substitutions;
    }

  and type_expression ident_map expr =
    let open Odoc_model.Lang.TypeExpr in
    match expr with
    | Var s -> TypeExpr.Var s
    | Any -> Any
    | Constr (p, xs) ->
        Constr (type_path ident_map p, List.map (type_expression ident_map) xs)
    | Arrow (lbl, t1, t2) ->
        Arrow (lbl, type_expression ident_map t1, type_expression ident_map t2)
    | Tuple ts -> Tuple (List.map (type_expression ident_map) ts)
    | Polymorphic_variant v ->
        Polymorphic_variant (type_expr_polyvar ident_map v)
    | Poly (s, ts) -> Poly (s, type_expression ident_map ts)
    | Alias (t, s) -> Alias (type_expression ident_map t, s)
    | Class (p, ts) ->
        Class
          (class_type_path ident_map p, List.map (type_expression ident_map) ts)
    | Object o -> Object (type_object ident_map o)
    | Package p -> Package (type_package ident_map p)

  and module_decl ident_map m =
    match m with
    | Odoc_model.Lang.Module.Alias (p, e) ->
        Module.Alias
          (module_path ident_map p, option simple_expansion ident_map e)
    | Odoc_model.Lang.Module.ModuleType s ->
        Module.ModuleType (module_type_expr ident_map s)

  and include_decl ident_map m =
    match m with
    | Odoc_model.Lang.Include.Alias p -> Include.Alias (module_path ident_map p)
    | ModuleType s -> ModuleType (u_module_type_expr ident_map s)

  and canonical ident_map (canonical : Odoc_model.Paths.Path.Module.t option) =
    match canonical with
    | Some p -> Some (module_path ident_map p)
    | None -> None

  and simple_expansion ident_map
      (f : Odoc_model.Lang.ModuleType.simple_expansion) :
      ModuleType.simple_expansion =
    let open Odoc_model.Lang.ModuleType in
    let open Odoc_model.Lang.FunctorParameter in
    match f with
    | Signature t -> Signature (signature ident_map t)
    | Functor (arg, sg) -> (
        match arg with
        | Named arg ->
            let identifier = arg.Odoc_model.Lang.FunctorParameter.id in
            let id = Ident.Of_Identifier.functor_parameter identifier in
            let ident_map' =
              {
                ident_map with
                functor_parameters =
                  Maps.FunctorParameter.add identifier id
                    ident_map.functor_parameters;
              }
            in
            let arg' = functor_parameter ident_map' id arg in
            Functor (FunctorParameter.Named arg', simple_expansion ident_map' sg)
        | Unit -> Functor (FunctorParameter.Unit, simple_expansion ident_map sg)
        )

  and module_ ident_map m =
    let type_ = module_decl ident_map m.Odoc_model.Lang.Module.type_ in
    let canonical = canonical ident_map m.Odoc_model.Lang.Module.canonical in
    { Module.doc = docs ident_map m.doc; type_; canonical; hidden = m.hidden }

  and with_module_type_substitution ident_map m =
    let open Odoc_model.Lang.ModuleType in
    match m with
    | ModuleEq (frag, decl) ->
        ModuleType.ModuleEq
          (module_fragment ident_map frag, module_decl ident_map decl)
    | ModuleSubst (frag, p) ->
        ModuleType.ModuleSubst
          (module_fragment ident_map frag, module_path ident_map p)
    | ModuleTypeEq (frag, mty) ->
        ModuleType.ModuleTypeEq
          (module_type_fragment ident_map frag, module_type_expr ident_map mty)
    | ModuleTypeSubst (frag, mty) ->
        ModuleType.ModuleTypeSubst
          (module_type_fragment ident_map frag, module_type_expr ident_map mty)
    | TypeEq (frag, eqn) ->
        ModuleType.TypeEq
          (type_fragment ident_map frag, type_equation ident_map eqn)
    | TypeSubst (frag, eqn) ->
        ModuleType.TypeSubst
          (type_fragment ident_map frag, type_equation ident_map eqn)

  and functor_parameter ident_map id a =
    let expr' =
      module_type_expr ident_map a.Odoc_model.Lang.FunctorParameter.expr
    in
    { FunctorParameter.id; expr = expr' }

  and extension ident_map e =
    let open Odoc_model.Lang.Extension in
    let type_path = type_path ident_map e.type_path in
    let constructors =
      List.map (extension_constructor ident_map) e.constructors
    in
    {
      Extension.type_path;
      doc = docs ident_map e.doc;
      type_params = e.type_params;
      private_ = e.private_;
      constructors;
    }

  and extension_constructor ident_map c =
    let open Odoc_model.Lang.Extension.Constructor in
    let args = type_decl_constructor_argument ident_map c.args in
    let res = Opt.map (type_expression ident_map) c.res in
    {
      Extension.Constructor.name = Paths.Identifier.name c.id;
      doc = docs ident_map c.doc;
      args;
      res;
    }

  and exception_ ident_map e =
    let open Odoc_model.Lang.Exception in
    let args = type_decl_constructor_argument ident_map e.args in
    let res = Opt.map (type_expression ident_map) e.res in
    { Exception.doc = docs ident_map e.doc; args; res }

  and u_module_type_expr ident_map m =
    let open Odoc_model in
    match m with
    | Lang.ModuleType.U.Signature s ->
        let s = signature ident_map s in
        ModuleType.U.Signature s
    | Path p ->
        let p' = module_type_path ident_map p in
        Path p'
    | With (w, e) ->
        let w' = List.map (with_module_type_substitution ident_map) w in
        With (w', u_module_type_expr ident_map e)
    | TypeOf { t_desc; t_expansion } ->
        let t_desc =
          match t_desc with
          | ModPath p -> ModuleType.ModPath (module_path ident_map p)
          | StructInclude p -> StructInclude (module_path ident_map p)
        in
        let t_expansion = Opt.map (simple_expansion ident_map) t_expansion in
        TypeOf { t_desc; t_expansion }

  and module_type_expr ident_map m =
    let open Odoc_model in
    let open Paths in
    match m with
    | Lang.ModuleType.Signature s ->
        let s = signature ident_map s in
        ModuleType.Signature s
    | Lang.ModuleType.Path p ->
        let p' =
          ModuleType.
            {
              p_path = module_type_path ident_map p.p_path;
              p_expansion = option simple_expansion ident_map p.p_expansion;
            }
        in
        ModuleType.Path p'
    | Lang.ModuleType.With w ->
        let w' =
          ModuleType.
            {
              w_substitutions =
                List.map
                  (with_module_type_substitution ident_map)
                  w.w_substitutions;
              w_expansion = option simple_expansion ident_map w.w_expansion;
              w_expr = u_module_type_expr ident_map w.w_expr;
            }
        in
        ModuleType.With w'
    | Lang.ModuleType.Functor (Named arg, expr) ->
        let identifier = arg.Lang.FunctorParameter.id in
        let id = Ident.Of_Identifier.functor_parameter identifier in
        let ident_map' =
          {
            ident_map with
            functor_parameters =
              Identifier.Maps.FunctorParameter.add identifier id
                ident_map.functor_parameters;
          }
        in
        let arg' = functor_parameter ident_map' id arg in
        let expr' = module_type_expr ident_map' expr in
        ModuleType.Functor (Named arg', expr')
    | Lang.ModuleType.Functor (Unit, expr) ->
        let expr' = module_type_expr ident_map expr in
        ModuleType.Functor (Unit, expr')
    | Lang.ModuleType.TypeOf { t_desc; t_expansion } ->
        let t_desc =
          match t_desc with
          | ModPath p -> ModuleType.ModPath (module_path ident_map p)
          | StructInclude p -> StructInclude (module_path ident_map p)
        in
        let t_expansion = option simple_expansion ident_map t_expansion in
        ModuleType.(TypeOf { t_desc; t_expansion })

  and module_type ident_map m =
    let expr =
      Opt.map (module_type_expr ident_map) m.Odoc_model.Lang.ModuleType.expr
    in
    {
      ModuleType.doc = docs ident_map m.doc;
      canonical = option module_type_path ident_map m.canonical;
      expr;
    }

  and value ident_map v =
    let type_ = type_expression ident_map v.Lang.Value.type_ in
    { Value.type_; doc = docs ident_map v.doc; value = v.value }

  and include_ ident_map i =
    let open Odoc_model.Lang.Include in
    let decl = include_decl ident_map i.decl in
    {
      Include.parent = i.parent;
      doc = docs ident_map i.doc;
      shadowed = i.expansion.shadowed;
      expansion_ = apply_sig_map ident_map i.expansion.content;
      status = i.status;
      strengthened = option module_path ident_map i.strengthened;
      decl;
    }

  and class_ ident_map c =
    let open Odoc_model.Lang.Class in
    let expansion = Opt.map (class_signature ident_map) c.expansion in
    {
      Class.doc = docs ident_map c.doc;
      virtual_ = c.virtual_;
      params = c.params;
      type_ = class_decl ident_map c.type_;
      expansion;
    }

  and class_decl ident_map c =
    let open Odoc_model.Lang.Class in
    match c with
    | ClassType e -> Class.ClassType (class_type_expr ident_map e)
    | Arrow (lbl, e, d) ->
        Arrow (lbl, type_expression ident_map e, class_decl ident_map d)

  and class_type_expr ident_map e =
    let open Odoc_model.Lang.ClassType in
    match e with
    | Constr (p, ts) ->
        ClassType.Constr
          (class_type_path ident_map p, List.map (type_expression ident_map) ts)
    | Signature s -> Signature (class_signature ident_map s)

  and class_type ident_map t =
    let open Odoc_model.Lang.ClassType in
    let expansion = Opt.map (class_signature ident_map) t.expansion in
    {
      ClassType.doc = docs ident_map t.doc;
      virtual_ = t.virtual_;
      params = t.params;
      expr = class_type_expr ident_map t.expr;
      expansion;
    }

  and class_signature ident_map sg =
    let open Odoc_model.Lang.ClassSignature in
    let items =
      List.map
        (function
          | Method m ->
              let id = Ident.Of_Identifier.method_ m.id in
              let m' = method_ ident_map m in
              ClassSignature.Method (id, m')
          | InstanceVariable i ->
              let id = Ident.Of_Identifier.instance_variable i.id in
              let i' = instance_variable ident_map i in
              ClassSignature.InstanceVariable (id, i')
          | Constraint (t1, t2) ->
              Constraint
                (type_expression ident_map t1, type_expression ident_map t2)
          | Inherit e -> Inherit (class_type_expr ident_map e)
          | Comment c -> Comment (docs_or_stop ident_map c))
        sg.items
    in
    {
      ClassSignature.self = Opt.map (type_expression ident_map) sg.self;
      items;
      doc = docs ident_map sg.doc;
    }

  and method_ ident_map m =
    let open Odoc_model.Lang.Method in
    {
      Method.doc = docs ident_map m.doc;
      private_ = m.private_;
      virtual_ = m.virtual_;
      type_ = type_expression ident_map m.type_;
    }

  and instance_variable ident_map i =
    {
      InstanceVariable.doc = docs ident_map i.doc;
      mutable_ = i.mutable_;
      virtual_ = i.virtual_;
      type_ = type_expression ident_map i.type_;
    }

  and module_substitution ident_map (t : Odoc_model.Lang.ModuleSubstitution.t) =
    {
      ModuleSubstitution.doc = docs ident_map t.doc;
      manifest = module_path ident_map t.manifest;
    }

  and module_type_substitution ident_map
      (t : Odoc_model.Lang.ModuleTypeSubstitution.t) =
    {
      ModuleTypeSubstitution.doc = docs ident_map t.doc;
      manifest = module_type_expr ident_map t.manifest;
    }

  and module_of_module_substitution ident_map
      (t : Odoc_model.Lang.ModuleSubstitution.t) =
    let manifest = module_path ident_map t.manifest in
    let canonical = Some manifest in
    {
      Module.doc = docs ident_map t.doc;
      type_ = Alias (manifest, None);
      canonical;
      hidden = true;
    }

  and signature : _ -> Odoc_model.Lang.Signature.t -> Signature.t =
   fun ident_map items ->
    (* First we construct a list of brand new [Ident.t]s
                for each item in the signature *)
    let ident_map =
      map_of_idents (LocalIdents.signature items LocalIdents.empty) ident_map
    in
    (* Now we construct the Components for each item,
                converting all paths containing Identifiers pointing at
                our elements to local paths *)
    apply_sig_map ident_map items

  and open_ ident_map o =
    Open.
      { expansion = apply_sig_map ident_map o.Odoc_model.Lang.Open.expansion }

  and apply_sig_map ident_map sg =
    let items =
      List.map
        (let open Odoc_model.Lang.Signature in
        let open Odoc_model.Paths in
        function
        | Type (r, t) ->
            let id = Identifier.Maps.Type.find t.id ident_map.types in
            let t' = Delayed.put (fun () -> type_decl ident_map t) in
            Signature.Type (id, r, t')
        | TypeSubstitution t ->
            let id = Identifier.Maps.Type.find t.id ident_map.types in
            let t' = type_decl ident_map t in
            Signature.TypeSubstitution (id, t')
        | Module (r, m) ->
            let id =
              Identifier.Maps.Module.find
                (m.id :> Identifier.Module.t)
                ident_map.modules
            in
            let m' = Delayed.put (fun () -> module_ ident_map m) in
            Signature.Module (id, r, m')
        | ModuleSubstitution m ->
            let id = Identifier.Maps.Module.find m.id ident_map.modules in
            let m' = module_substitution ident_map m in
            Signature.ModuleSubstitution (id, m')
        | ModuleTypeSubstitution m ->
            let id =
              Identifier.Maps.ModuleType.find m.id ident_map.module_types
            in
            let m' = module_type_substitution ident_map m in
            Signature.ModuleTypeSubstitution (id, m')
        | ModuleType m ->
            let id =
              Identifier.Maps.ModuleType.find m.id ident_map.module_types
            in
            let m' = Delayed.put (fun () -> module_type ident_map m) in
            Signature.ModuleType (id, m')
        | Value v ->
            let id = Ident.Of_Identifier.value v.id in
            let v' = Delayed.put (fun () -> value ident_map v) in
            Signature.Value (id, v')
        | Comment c -> Comment (docs_or_stop ident_map c)
        | TypExt e -> TypExt (extension ident_map e)
        | Exception e ->
            let id = Ident.Of_Identifier.exception_ e.id in
            Exception (id, exception_ ident_map e)
        | Class (r, c) ->
            let id = Identifier.Maps.Class.find c.id ident_map.classes in
            Class (id, r, class_ ident_map c)
        | ClassType (r, c) ->
            let id =
              Identifier.Maps.ClassType.find c.id ident_map.class_types
            in
            ClassType (id, r, class_type ident_map c)
        | Open o -> Open (open_ ident_map o)
        | Include i -> Include (include_ ident_map i))
        sg.items
    in
    { items; removed = []; compiled = sg.compiled; doc = docs ident_map sg.doc }

  and with_location :
        'a 'b.
        (map -> 'a -> 'b) ->
        map ->
        'a Location_.with_location ->
        'b Location_.with_location =
   fun conv ident_map v -> { v with value = conv ident_map v.Location_.value }

  and block_element :
      _ -> Odoc_model.Comment.block_element -> CComment.block_element =
   fun _ b ->
    match b with
    | `Heading (l, id, content) ->
        `Heading (l, Ident.Of_Identifier.label id, content)
    | `Tag t -> `Tag t
    | #Odoc_model.Comment.nestable_block_element as n -> n

  and docs ident_map d = List.map (with_location block_element ident_map) d

  and docs_or_stop ident_map = function
    | `Docs d -> `Docs (docs ident_map d)
    | `Stop -> `Stop
end

let module_of_functor_argument (arg : FunctorParameter.parameter) =
  {
    Module.doc = [];
    type_ = ModuleType arg.expr;
    canonical = None;
    hidden = false;
  }

(** This is equivalent to {!Lang.extract_signature_doc}. *)
let extract_signature_doc (s : Signature.t) =
  match (s.doc, s.items) with
  | [], Include { expansion_; status = `Inline; _ } :: _ -> expansion_.doc
  | doc, _ -> doc

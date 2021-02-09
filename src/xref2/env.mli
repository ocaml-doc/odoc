(* Env.mli *)

type lookup_result_found = { root : Odoc_model.Root.t; hidden : bool }

type lookup_unit_result =
  | Forward_reference
  | Found of lookup_result_found
  | Not_found

type root =
  | Resolved of
      (Digest.t
      * Odoc_model.Paths.Identifier.Module.t
      * bool
      * Component.Module.t Component.Delayed.t)
  | Forward

type resolver = {
  open_units : string list;
  lookup_unit : string -> lookup_unit_result;
  resolve_unit : Odoc_model.Root.t -> Odoc_model.Lang.Compilation_unit.t;
  lookup_page : string -> Odoc_model.Root.t option;
  resolve_page : Odoc_model.Root.t -> Odoc_model.Lang.Page.t;
}

type lookup_type =
  | Module of Odoc_model.Paths_types.Identifier.reference_module
  | ModuleType of Odoc_model.Paths_types.Identifier.module_type
  | RootModule of string * [ `Forward | `Resolved of Digest.t ] option
  | ModuleByName of string * Odoc_model.Paths_types.Identifier.reference_module
  | FragmentRoot of int

val pp_lookup_type_list : Format.formatter -> lookup_type list -> unit

type t

val with_recorded_lookups : t -> (t -> 'a) -> lookup_type list * 'a

val set_resolver : t -> resolver -> t

val has_resolver : t -> bool

val id : t -> int

val empty : t

val add_fragment_root : Component.Signature.t -> t -> t

val add_module :
  Odoc_model.Paths_types.Identifier.reference_module ->
  Component.Module.t Component.Delayed.t ->
  Component.CComment.docs ->
  t ->
  t

val add_type :
  Odoc_model.Paths_types.Identifier.type_ -> Component.TypeDecl.t -> t -> t

val add_module_type :
  Odoc_model.Paths_types.Identifier.reference_module_type ->
  Component.ModuleType.t ->
  t ->
  t

val add_value :
  Odoc_model.Paths_types.Identifier.value -> Component.Value.t -> t -> t

val add_external :
  Odoc_model.Paths_types.Identifier.value -> Component.External.t -> t -> t

val add_label : Odoc_model.Paths_types.Identifier.reference_label -> t -> t

val add_label_title :
  Odoc_model.Paths_types.Identifier.reference_label ->
  Odoc_model.Comment.link_content ->
  t ->
  t

val add_class :
  Odoc_model.Paths_types.Identifier.reference_class ->
  Component.Class.t ->
  t ->
  t

val add_class_type :
  Odoc_model.Paths_types.Identifier.class_type ->
  Component.ClassType.t ->
  t ->
  t

val add_exception :
  Odoc_model.Paths_types.Identifier.exception_ ->
  Component.Exception.t ->
  t ->
  t

val add_extension_constructor :
  Odoc_model.Paths_types.Identifier.extension ->
  Component.Extension.Constructor.t ->
  t ->
  t

val add_docs : Odoc_model.Comment.docs -> t -> t

val add_comment : Odoc_model.Comment.docs_or_stop -> t -> t

val add_method :
  Odoc_model.Paths_types.Identifier.reference_method ->
  Component.Method.t ->
  t ->
  t

val add_module_functor_args :
  Component.Module.t -> Odoc_model.Paths_types.Identifier.path_module -> t -> t

val add_module_type_functor_args :
  Component.ModuleType.t ->
  Odoc_model.Paths_types.Identifier.module_type ->
  t ->
  t

val lookup_fragment_root : t -> (int * Component.Signature.t) option

val lookup_section_title :
  Odoc_model.Paths_types.Identifier.reference_label ->
  t ->
  Odoc_model.Comment.link_content option

val lookup_page : string -> t -> Odoc_model.Lang.Page.t option

val module_of_unit : Odoc_model.Lang.Compilation_unit.t -> Component.Module.t

val lookup_root_module : string -> t -> root option

type value_or_external =
  [ `External of Odoc_model.Paths_types.Identifier.value * Component.External.t
  | `Value of Odoc_model.Paths_types.Identifier.value * Component.Value.t ]

type 'a scope constraint 'a = [< Component.Element.any ]
(** Target of a lookup *)

type 'a maybe_ambiguous =
  ('a, [ `Ambiguous of 'a * 'a list | `Not_found ]) Result.result

val lookup_by_name : 'a scope -> string -> t -> 'a maybe_ambiguous
(** Lookup an element in Env depending on the given [scope].
    Return [Error (`Ambiguous _)] when two or more elements match the given scope and name. *)

val lookup_by_id :
  'a scope -> [< Odoc_model.Paths_types.Identifier.any ] -> t -> 'a option
(** Like [lookup_by_name] but use an identifier as key. *)

val s_any : Component.Element.any scope

val s_signature : Component.Element.signature scope

val s_module : Component.Element.module_ scope

val s_module_type : Component.Element.module_type scope

val s_datatype : Component.Element.datatype scope

val s_type : Component.Element.type_ scope

val s_class : Component.Element.class_ scope

val s_class_type : Component.Element.class_type scope

val s_value : value_or_external scope

val s_label : Component.Element.label scope

val s_constructor : Component.Element.constructor scope

val s_exception : Component.Element.exception_ scope

val s_extension : Component.Element.extension scope

val s_field : Component.Element.field scope

val s_label_parent : Component.Element.label_parent scope

(* val open_component_signature :
  Odoc_model.Paths_types.Identifier.signature -> Component.Signature.t -> t -> t *)

val add_functor_parameter : Odoc_model.Lang.FunctorParameter.t -> t -> t

val open_class_signature : Odoc_model.Lang.ClassSignature.t -> t -> t

val open_signature : Odoc_model.Lang.Signature.t -> t -> t

val open_unit : Odoc_model.Lang.Compilation_unit.t -> t -> t

val initial_env :
  Odoc_model.Lang.Compilation_unit.t ->
  resolver ->
  Odoc_model.Lang.Compilation_unit.Import.t list * t

val modules_of :
  t ->
  (Odoc_model.Paths.Identifier.Path.Module.t
  * Component.Module.t Component.Delayed.t)
  list

val len : int ref

val n : int ref

val verify_lookups : t -> lookup_type list -> bool

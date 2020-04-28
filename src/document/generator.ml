(*
 * Copyright (c) 2016 Thomas Refis <trefis@janestreet.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)


open Odoc_model.Names
module Location = Odoc_model.Location_
module Paths = Odoc_model.Paths
open Types

module O = Codefmt
open O.Infix

(* TODO: Title formatting should be a renderer decision *)
let format_title kind name =
  let mk title =
    let level = 1 and label = None in
    [Item.Heading { level ; label ; title}]
  in
  let prefix s = mk (inline (Text (s ^" ")) :: O.code (O.txt name)) in
  match kind with
  | `Mod -> prefix "Module"
  | `Arg -> prefix "Parameter"
  | `Mty -> prefix "Module type"
  | `Cty -> prefix "Class type"
  | `Class -> prefix "Class"
  | `Page -> mk [inline @@ Text name]

let make_name_from_path {Url.Path. name ; parent ; _ } =
  match parent with
  | None -> name
  | Some p ->
    Printf.sprintf "%s.%s" p.name name

let label t ppf = match t with
  | Odoc_model.Lang.TypeExpr.Label s -> O.pf ppf "%s" s
  | Optional s -> O.pf ppf "?%t%s" (O.entity "#8288") s

let tag tag t ppf =
  O.pf ppf "@{<%s>%t@}" tag t

let type_var tv =
  tag "type-var" (O.txt tv)

let enclose ~l ~r x =
  O.span (fun ppf -> O.pf ppf "%s%t%s" l x r)

let path p txt =
  !O.elt [inline @@ InternalLink (InternalLink.Resolved (Url.from_path p, txt))]
let resolved p txt =
  !O.elt [inline @@ InternalLink (InternalLink.Resolved (p, txt))]
let unresolved txt =
  !O.elt [inline @@ InternalLink (InternalLink.Unresolved txt)]

let path_to_id path =
  match Url.Anchor.from_identifier (path :> Paths.Identifier.t) with
  | Error _ -> None
  | Ok url -> Some url

let attach_expansion ?(status=`Default) page text = match page with
  | None -> O.documentedSrc text
  | Some content ->
    let summary = O.render text in
    [DocumentedSrc.Subpage { summary ; content ; status }]

include Generator_signatures

module Make (Syntax : SYNTAX) = struct

module Link :
sig
  val from_path : stop_before:bool -> Paths.Path.t -> text
  val from_fragment :
    base:Paths.Identifier.Signature.t -> Paths.Fragment.t -> text
  val render_fragment : Paths.Fragment.t -> string
end =
struct
  open Paths

  let rec from_path : stop_before:bool -> Path.t -> text =
    fun ~stop_before path ->
      match path with
      | `Root root -> unresolved [inline @@ Text root]
      | `Forward root -> unresolved [inline @@ Text root] (* FIXME *)
      | `Dot (prefix, suffix) ->
        let link = from_path ~stop_before:true (prefix :> Path.t) in
        link ++ O.txt ("." ^ suffix)
      | `Apply (p1, p2) ->
        let link1 = from_path ~stop_before (p1 :> Path.t) in
        let link2 = from_path ~stop_before (p2 :> Path.t) in
        link1 ++ O.txt "(" ++ link2 ++ O.txt ")"
      | `Resolved rp ->
        let id = Paths.Path.Resolved.identifier rp in
        let txt = Url.render_path path in
        begin match Url.from_identifier ~stop_before id with
        | Ok href ->
          resolved href [inline @@ Text txt]
        | Error Url.Error.Not_linkable _ -> O.txt txt
        | Error exn ->
          Printf.eprintf "Id.href failed: %S\n%!" (Url.Error.to_string exn);
          O.txt txt
        end

  let dot prefix suffix =
    match prefix with
    | "" -> suffix
    | _  -> prefix ^ "." ^ suffix

  let rec render_fragment : Fragment.t -> string =
    fun fragment ->
    match fragment with
    | `Resolved rr -> render_resolved_fragment rr
    | `Dot (prefix, suffix) ->
      dot (render_fragment (prefix :> Fragment.t)) suffix

  and render_resolved_fragment : Fragment.Resolved.t -> string =
    let open Fragment.Resolved in
    fun fragment ->
      match fragment with
      | `Root -> ""
      | `Subst (_, rr) -> render_resolved_fragment (rr :> t)
      | `SubstAlias (_, rr) -> render_resolved_fragment (rr :> t)
      | `Module (rr, s) ->
        dot (render_resolved_fragment (rr :> t)) (ModuleName.to_string s)
      | `Type (rr, s) ->
        dot (render_resolved_fragment (rr :> t)) (TypeName.to_string s)
      | `Class (rr, s) ->
        dot (render_resolved_fragment ( rr :> t)) (ClassName.to_string s)
      | `ClassType (rr, s) ->
        dot (render_resolved_fragment (rr :> t)) (ClassTypeName.to_string s)

  let rec fragment_to_ir : stop_before:bool ->
    base:Identifier.Signature.t -> Fragment.t -> text =
    fun ~stop_before ~base fragment ->
    let open Fragment in
    match fragment with
    | `Resolved `Root ->
      let id = (base :> Identifier.t) in
      begin match Url.from_identifier ~stop_before:true id with
      | Ok href ->
        resolved href [inline @@ Text (Identifier.name id)]
      | Error (Not_linkable _) ->
        unresolved [inline @@ Text (Identifier.name id)]
      | Error exn ->
        Printf.eprintf "[FRAG] Id.href failed: %S\n%!"
          (Url.Error.to_string exn);
        unresolved [inline @@ Text (Identifier.name id)]
      end
    | `Resolved rr ->
      let id = Resolved.identifier base (rr :> Resolved.t) in
      let txt = render_resolved_fragment rr in
      begin match Url.from_identifier ~stop_before id with
      | Ok href ->
        resolved href [inline @@ Text txt]
      | Error (Not_linkable _) ->
        unresolved [inline @@ Text txt]
      | Error exn ->
        Printf.eprintf "[FRAG] Id.href failed: %S\n%!"
          (Url.Error.to_string exn);
        unresolved [inline @@ Text txt]
      end
    | `Dot (prefix, suffix) ->
      let link = fragment_to_ir ~stop_before:true ~base (prefix :> Fragment.t) in
      link ++ O.txt ("." ^ suffix)

  let from_fragment = fragment_to_ir ~stop_before:false

end


module Type_expression :
sig
  val type_expr : ?needs_parentheses:bool -> Lang.TypeExpr.t -> text

  val format_type_path :
    delim:[ `parens | `brackets ] -> Lang.TypeExpr.t list -> text -> text
end =
struct
  let rec te_variant (t : Odoc_model.Lang.TypeExpr.Polymorphic_variant.t) =
    let style_arguments ~constant arguments =
      (* Multiple arguments in a polymorphic variant constructor correspond
         to a conjunction of types, not a product: [`Lbl int&float].
         If constant is [true], the conjunction starts with an empty type,
         for instance [`Lbl &int].
      *)
      let wrapped_type_expr =
        (* type conjunction in Reason is printed as `Lbl (t1)&(t2)` *)
        if Syntax.Type.Variant.parenthesize_params then
          fun x ->
            O.span (O.txt "(" ++ type_expr x ++ O.txt ")")
        else
          fun x -> type_expr x
      in
      let arguments =
        O.list
          arguments
          ~sep:(O.txt " & ")
          ~f:wrapped_type_expr
      in
      if constant
      then O.txt "& " ++ arguments
      else arguments
    in
    let rec style_elements ~add_pipe = function
      | [] -> O.noop
      | first :: rest ->
        let first =
          match first with
          | Odoc_model.Lang.TypeExpr.Polymorphic_variant.Type te ->
            let res = type_expr te in
            if add_pipe
            then O.txt " " ++ O.span (O.txt "| " ++ res)
            else res
          | Constructor {constant; name; arguments; _} ->
            let constr =
              let name = "`" ^ name in
              if add_pipe
              then O.span (O.txt ("| " ^ name))
              else O.txt name
            in
            let res =
              match arguments with
              | [] -> constr
              | _ ->
                let arguments = style_arguments ~constant arguments in
                O.span (
                  if Syntax.Type.Variant.parenthesize_params
                  then constr ++ arguments
                  else constr ++ O.txt  " of " ++ arguments
                )
            in
            if add_pipe
            then O.txt " " ++ res
            else res
        in
        first ++ style_elements ~add_pipe:true rest
    in
    let elements = style_elements ~add_pipe:false t.elements in
    O.span (
       match t.kind with
       | Fixed -> O.txt "[ " ++ elements ++ O.txt " ]"
       | Open -> O.txt "[> " ++ elements ++ O.txt " ]"
       | Closed [] -> O.txt "[< " ++ elements ++ O.txt " ]"
       | Closed lst ->
         let constrs = String.concat " " lst in
         O.txt "[< " ++ elements ++ (O.txt (" " ^ constrs ^ " ]"))
     )

  and te_object (t : Odoc_model.Lang.TypeExpr.Object.t) =
    let fields =
      O.list t.fields ~f:(function
        | Odoc_model.Lang.TypeExpr.Object.Method {name; type_} ->
          O.txt (name ^ Syntax.Type.annotation_separator)
          ++ type_expr type_
          ++ O.txt Syntax.Obj.field_separator
        | Inherit type_ ->
            type_expr type_ ++ O.txt Syntax.Obj.field_separator)
    in
    let open_tag =
        if t.open_ then O.txt Syntax.Obj.open_tag_extendable
        else O.txt Syntax.Obj.open_tag_closed
    in
    let close_tag =
        if t.open_ then O.txt Syntax.Obj.close_tag_extendable
        else O.txt Syntax.Obj.close_tag_closed
    in
    open_tag ++ fields ++ close_tag

  and format_type_path
        ~delim (params : Odoc_model.Lang.TypeExpr.t list) (path : text) : text =
    match params with
    | [] -> path
    | [param] ->
        let param = (type_expr ~needs_parentheses:true param) in
        let args =
          if Syntax.Type.parenthesize_constructor
          then O.txt "(" ++ param ++ O.txt ")"
          else param
        in
      Syntax.Type.handle_constructor_params path args
    | params  ->
      let params =
        O.list params ~sep:(O.txt ",\194\160")
          ~f:type_expr
      in
      let params = match delim with
        | `parens   -> enclose ~l:"(" params ~r:")"
        | `brackets -> enclose ~l:"[" params ~r:"]"
      in
      Syntax.Type.handle_constructor_params path params

  and type_expr
        ?(needs_parentheses=false) (t : Odoc_model.Lang.TypeExpr.t) =
    match t with
    | Var s -> type_var (Syntax.Type.var_prefix ^ s)
    | Any  -> type_var Syntax.Type.any
    | Alias (te, alias) ->
      type_expr ~needs_parentheses:true te ++
      O.txt " " ++ O.keyword "as" ++ O.txt " '" ++ O.txt alias
    | Arrow (None, src, dst) ->
      let res =
        type_expr ~needs_parentheses:true src ++
        O.txt " " ++ Syntax.Type.arrow ++ O.txt " " ++ type_expr dst
      in
      if not needs_parentheses then
        res
      else
        enclose ~l:"(" res ~r:")"
    | Arrow (Some lbl, src, dst) ->
      let res =
        O.span (
          label lbl ++ O.txt ":" ++ type_expr ~needs_parentheses:true src
        ) ++ O.txt " " ++ Syntax.Type.arrow ++ O.txt " " ++ type_expr dst
      in
      if not needs_parentheses then
        res
      else
        enclose ~l:"(" res ~r:")"
    | Tuple lst ->
      let res =
        O.list
          lst
          ~sep:(O.txt Syntax.Type.Tuple.element_separator)
          ~f:(type_expr ~needs_parentheses:true)
      in
      if Syntax.Type.Tuple.always_parenthesize || needs_parentheses then
        enclose ~l:"(" res ~r:")"
      else
        res
    | Constr (path, args) ->
      let link = Link.from_path ~stop_before:false (path :> Paths.Path.t) in
      format_type_path ~delim:(`parens) args link
    | Polymorphic_variant v -> te_variant v
    | Object o -> te_object o
    | Class (path, args) ->
      format_type_path ~delim:(`brackets) args
        (Link.from_path ~stop_before:false (path :> Paths.Path.t))
    | Poly (polyvars, t) ->
      O.txt (String.concat " " polyvars ^ ". ") ++ type_expr t
    | Package pkg ->
      enclose ~l:"(" ~r:")" (
         O.keyword "module" ++ O.txt " " ++
           Link.from_path ~stop_before:false (pkg.path :> Paths.Path.t) ++
         match pkg.substitutions with
         | [] -> O.noop
         | lst ->
           O.txt " " ++ O.keyword "with" ++ O.txt " " ++
           O.list
             ~sep:(O.txt " " ++ O.keyword "and" ++ O.txt " ")
             lst
             ~f:(package_subst pkg.path)
       )

  and package_subst (pkg_path : Paths.Path.ModuleType.t)
        (frag_typ, te : Paths.Fragment.Type.t * Odoc_model.Lang.TypeExpr.t)
    : text =
    let typath = match pkg_path with
    | `Resolved rp ->
      let base =
        (Paths.Path.Resolved.ModuleType.identifier rp :> Paths.Identifier.Signature.t)
      in
      Link.from_fragment ~base (frag_typ :> Paths.Fragment.t)
    | _ ->
      O.txt (Link.render_fragment (frag_typ :> Paths.Fragment.t))
    in
    O.keyword "type" ++
    O.txt " " ++
    typath ++
      O.txt " = " ++
      type_expr te
end
open Type_expression



(* Also handles constructor declarations for exceptions and extensible
   variants, and exposes a few helpers used in formatting classes and signature
   constraints. *)
module Type_declaration :
sig
  val type_decl :
    ?is_substitution:bool -> Lang.Signature.recursive * Lang.TypeDecl.t ->
      Item.t
  val extension : Lang.Extension.t -> Item.t
  val exn : Lang.Exception.t -> Item.t

  val format_params :
    ?delim:[ `parens | `brackets ] ->
    Lang.TypeDecl.param list -> text

  val format_manifest : ?is_substitution:bool -> ?compact_variants:bool -> Lang.TypeDecl.Equation.t -> text * bool

  val format_constraints : (Lang.TypeExpr.t * Lang.TypeExpr.t) list -> text
end =
struct
  let record fields =
    let field mutable_ id typ =
      match Url.from_identifier ~stop_before:true id with
      | Error e -> failwith (Url.Error.to_string e)
      | Ok url ->
        let name = Paths.Identifier.name id in
        let attrs = ["def"; "record"; url.kind] in
        let cell =
          (* O.td ~a:[ O.a_class ["def"; kind ] ]
           *   [O.a ~a:[O.a_href ("#" ^ anchor); O.a_class ["anchor"]] []
           *   ; *)
              O.code (
                (if mutable_ then O.keyword "mutable" ++ O.txt " " else O.noop)
                ++ O.txt name
                ++ O.txt Syntax.Type.annotation_separator
                ++ type_expr typ
                ++ O.txt Syntax.Type.Record.field_separator
              )
            (* ] *)
        in
        url, attrs, cell
    in
    let rows =
      fields |> List.map (fun fld ->
        let open Odoc_model.Lang.TypeDecl.Field in
        let url, attrs, code =
          field fld.mutable_ (fld.id :> Paths.Identifier.t) fld.type_
        in
        let anchor = Some url in
        let rhs = Comment.to_ir fld.doc in
        let doc = if not (Comment.has_doc fld.doc) then [] else rhs in
        DocumentedSrc.Documented { anchor; attrs; code; doc }
      )
    in
    let content =
      O.documentedSrc (O.txt "{")
      @ rows
      @ O.documentedSrc (O.txt "}")
    in
    content


  let constructor
    : Paths.Identifier.t -> Odoc_model.Lang.TypeDecl.Constructor.argument
      -> Odoc_model.Lang.TypeExpr.t option
      -> DocumentedSrc.t
    = fun id args ret_type ->
      let name = Paths.Identifier.name id in
      let kind = Url.kind id in
      let cstr = tag kind (O.txt name) in
      let is_gadt, ret_type =
        match ret_type with
        | None -> false, O.noop
        | Some te ->
          let constant =
            match args with
            | Tuple [] -> true
            | _ -> false
          in
          let ret_type =
            O.txt " " ++
              (if constant then O.txt ":" else Syntax.Type.GADT.arrow) ++
              O.txt " " ++
              type_expr te
          in
          true, ret_type
      in
      match args with
      | Tuple [] -> O.documentedSrc (cstr ++ ret_type)
      | Tuple lst ->
        let params = O.list lst
            ~sep:(O.txt Syntax.Type.Tuple.element_separator)
            ~f:(type_expr ~needs_parentheses:is_gadt)
        in
        O.documentedSrc (cstr ++
            (if Syntax.Type.Variant.parenthesize_params
              then O.txt "(" ++ params ++ O.txt ")"
              else
                (if is_gadt then
                    O.txt Syntax.Type.annotation_separator
                  else
                    O.txt " " ++ O.keyword "of" ++ O.txt " ") ++
                  params
            )
          ++ ret_type)
      | Record fields ->
        if is_gadt then
          O.documentedSrc (cstr ++ O.txt Syntax.Type.annotation_separator)
          @ record fields
          @ O.documentedSrc ret_type
        else
          O.documentedSrc (cstr ++ O.txt " " ++ O.keyword "of" ++ O.txt " ")
          @ record fields



  let variant cstrs : DocumentedSrc.t =
    let constructor id args res =
      match Url.from_identifier ~stop_before:true id with
      | Error e -> failwith (Url.Error.to_string e)
      | Ok url ->
        let attrs = ["def" ; "variant" ; url.kind] in
        let content =
          let doc = constructor id args res in
          O.documentedSrc (O.txt "| ") @ doc
        in
        url, attrs, content
    in
    match cstrs with
    | [] -> O.documentedSrc (O.txt "|")
    | _ :: _ ->
      let rows =
        cstrs |> List.map (fun cstr ->
          let open Odoc_model.Lang.TypeDecl.Constructor in
          let url, attrs, code =
            constructor (cstr.id :> Paths.Identifier.t) cstr.args cstr.res
          in
          let anchor = Some url in
          let rhs = Comment.to_ir cstr.doc in
          let doc = if not (Comment.has_doc cstr.doc) then [] else rhs in
          DocumentedSrc.Nested { anchor; attrs; code; doc }
        )
      in
      rows



  let extension_constructor (t : Odoc_model.Lang.Extension.Constructor.t) =
    (* TODO doc *)
    let doc = constructor (t.id :> Paths.Identifier.t) t.args t.res in
    O.documentedSrc (O.txt "| ") @ doc

  let extension (t : Odoc_model.Lang.Extension.t) =
    let content =
      O.documentedSrc (O.keyword "type" ++
          O.txt " " ++
          Link.from_path ~stop_before:false (t.type_path :> Paths.Path.t) ++
          O.txt " += ")
      @
        Utils.flatmap t.constructors
          ~f:extension_constructor
      @
        O.documentedSrc
          (if Syntax.Type.type_def_semicolon then O.txt ";" else O.noop)
    in
    let kind = Some "extension" in
    let anchor = None in
    let doc = Comment.to_ir t.doc in
    Item.Declaration {kind ; anchor ; doc ; content}



  let exn (t : Odoc_model.Lang.Exception.t) =
    let cstr = constructor (t.id :> Paths.Identifier.t) t.args t.res in
    let content =
      O.documentedSrc (O.keyword "exception" ++ O.txt " ")
      @ cstr
      @ O.documentedSrc
          (if Syntax.Type.Exception.semicolon then O.txt ";" else O.noop)
    in
    let kind = Some "exception" in
    let anchor = path_to_id t.id in
    let doc = Comment.to_ir t.doc in
    Item.Declaration {kind ; anchor ; doc ; content}



  let polymorphic_variant
      ~type_ident (t : Odoc_model.Lang.TypeExpr.Polymorphic_variant.t) =
    let row item =
      let kind_approx, cstr, doc =
        match item with
        | Odoc_model.Lang.TypeExpr.Polymorphic_variant.Type te ->
          "unknown", O.code (type_expr te), None
        | Constructor {constant; name; arguments; doc; _} ->
          let cstr = "`" ^ name in
          "constructor",
          begin match arguments with
          | [] -> O.code (O.txt cstr)
          | _ ->
            (* Multiple arguments in a polymorphic variant constructor correspond
               to a conjunction of types, not a product: [`Lbl int&float].
               If constant is [true], the conjunction starts with an empty type,
               for instance [`Lbl &int].
            *)
            let wrapped_type_expr =
              (* type conjunction in Reason is printed as `Lbl (t1)&(t2)` *)
              if Syntax.Type.Variant.parenthesize_params then
                fun x -> O.txt "(" ++ type_expr x ++ O.txt ")"
              else
                fun x -> type_expr x
            in
            let params = O.list arguments
                ~sep:(O.txt " & ")
                ~f:wrapped_type_expr
            in
            let params =
              if constant then O.txt "& " ++ params else params in
            O.code (
              O.txt cstr ++
                (
                  if Syntax.Type.Variant.parenthesize_params
                  then params
                  else O.txt " " ++ O.keyword "of" ++ O.txt " " ++ params
                )
            )
          end,
          match doc with
          | [] ->
            None
          | _ ->
            Some (Comment.to_ir doc)
      in
      try
        let url = Url.Anchor.polymorphic_variant ~type_ident item in
        let attrs = ["def"; url.kind] in
        let anchor = Some url in
        let code =
          O.code (O.txt "| ") @ cstr
        in
        let doc = match doc with
          | None -> []
          | Some doc -> doc
        in
        DocumentedSrc.Documented { attrs ; anchor ; code ; doc }
      with Failure s ->
        Printf.eprintf "ERROR: %s\n%!" s;
        let code = O.code (O.txt "| " ) @ cstr in
        let attrs = ["def"; kind_approx] in
        let doc = [] in
        let anchor = None in
        DocumentedSrc.Documented { attrs ; anchor ; code ; doc }
    in
    let variants = List.map row t.elements in
    let intro, ending = match t.kind with
    | Fixed ->
      O.documentedSrc (O.txt "[ "),
      O.documentedSrc (O.txt " ]")
    | Open ->
      O.documentedSrc (O.txt "[> "),
      O.documentedSrc (O.txt " ]")
    | Closed [] ->
      O.documentedSrc (O.txt "[< "),
      O.documentedSrc (O.txt " ]")
    | Closed lst ->
      let constrs = String.concat " " lst in
      O.documentedSrc (O.txt "[< "),
      O.documentedSrc (O.txt (" " ^ constrs ^ " ]"))
    in
    intro @ variants @ ending

  let format_params
    : 'row. ?delim:[`parens | `brackets] -> Odoc_model.Lang.TypeDecl.param list
    -> text
  = fun ?(delim=`parens) params ->
    let format_param (desc, variance_opt) =
      let param_desc =
        match desc with
        | Odoc_model.Lang.TypeDecl.Any -> "_"
        | Var s -> "'" ^ s
      in
      match variance_opt with
      | None -> param_desc
      | Some Odoc_model.Lang.TypeDecl.Pos -> "+" ^ param_desc
      | Some Odoc_model.Lang.TypeDecl.Neg -> "-" ^ param_desc
    in
    O.txt (
      match params with
      | [] -> ""
      | [x] -> format_param x |> Syntax.Type.handle_format_params
      | lst ->
        let params = String.concat ", " (List.map format_param lst) in
        (match delim with `parens -> "(" | `brackets -> "[")
        ^ params ^
        (match delim with `parens -> ")" | `brackets -> "]")
    )

  let format_constraints constraints =
    O.list constraints ~f:begin fun (t1, t2) ->
      O.txt " " ++
      O.keyword "constraint" ++
      O.txt " " ++
      type_expr t1 ++
      O.txt " = " ++
      type_expr t2
    end

  let format_manifest
    : 'inner_row 'outer_row. ?is_substitution:bool -> ?compact_variants:bool
    -> Odoc_model.Lang.TypeDecl.Equation.t
    -> text * bool
  = fun ?(is_substitution=false) ?(compact_variants=true) equation ->
    let _ = compact_variants in (* TODO *)
    let private_ = equation.private_ in
    match equation.manifest with
    | None -> O.noop, private_
    | Some t ->
      let manifest =
        O.txt (if is_substitution then " := " else " = ") ++
        (if private_ then
            O.keyword Syntax.Type.private_keyword ++ O.txt " "
        else O.noop) ++
        type_expr t
      in
      manifest, false



  let type_decl ?(is_substitution=false) ((recursive, t) : Lang.Signature.recursive * Lang.TypeDecl.t) =
    let tyname = Paths.Identifier.name t.id in
    let constraints = format_constraints t.equation.constraints in
    let manifest, need_private =
      match t.equation.manifest with
      | Some (Odoc_model.Lang.TypeExpr.Polymorphic_variant variant) ->
        let code =
          polymorphic_variant ~type_ident:(t.id :> Paths.Identifier.t) variant
        in
        let manifest =
          O.documentedSrc
            (O.txt (if is_substitution then " := " else " = ") ++
                if t.equation.private_ then
                  O.keyword Syntax.Type.private_keyword ++ O.txt " "
                else
                  O.noop)
          @ code
        in
        manifest, false
      | _ ->
        let manifest, need_private =
          format_manifest ~is_substitution t.equation
        in
        O.documentedSrc manifest, need_private
    in
    let representation =
      match t.representation with
      | None -> []
      | Some repr ->
        let content = match repr with
          | Extensible -> O.documentedSrc (O.txt "..")
          | Variant cstrs -> variant cstrs
          | Record fields -> record fields
        in
        O.documentedSrc (
          O.txt " = " ++
          if need_private then
            O.keyword Syntax.Type.private_keyword ++ O.txt " "
          else
            O.noop
        ) @ content
    in
    let tconstr =
      match t.equation.params with
      | [] -> O.txt tyname
      | l ->
        let params = format_params l in
        Syntax.Type.handle_constructor_params (O.txt tyname) params
    in
    let content =
      let keyword' =
        match recursive with
        | Ordinary | Rec -> O.keyword "type"
        | And -> O.keyword "and"
        | Nonrec -> O.keyword "type" ++ O.txt " " ++ O.keyword "nonrec"
      in
      O.documentedSrc (keyword' ++ O.txt " " ++ tconstr)
      @ manifest
      @ representation
      @ O.documentedSrc constraints
      @ O.documentedSrc
          (if Syntax.Type.type_def_semicolon then O.txt ";" else O.noop)
    in
    let kind = Some (if is_substitution then "type-subst" else "type") in
    let anchor = path_to_id t.id in
    let doc = Comment.to_ir t.doc in
    Item.Declaration {kind ; anchor ; doc ; content}
end
open Type_declaration



module Value :
sig
  val value : Lang.Value.t -> Item.t
  val external_ : Lang.External.t -> Item.t
end =
struct
  let value (t : Odoc_model.Lang.Value.t) =
    let name = Paths.Identifier.name t.id in
    let content =
      O.documentedSrc (
        O.keyword Syntax.Value.variable_keyword ++
          O.txt " " ++
          O.txt name ++
          O.txt Syntax.Type.annotation_separator ++
          type_expr t.type_
        ++ (if Syntax.Value.semicolon then O.txt ";" else O.noop)
      )
    in
    let kind = Some "value" in
    let anchor = path_to_id t.id in
    let doc = Comment.to_ir t.doc in
    Item.Declaration { kind ; anchor ; doc ; content}

  let external_ (t : Odoc_model.Lang.External.t) =
    let name = Paths.Identifier.name t.id in
    let content =
      O.documentedSrc (
        O.keyword Syntax.Value.variable_keyword ++
          O.txt " " ++
          O.txt name ++
          O.txt Syntax.Type.annotation_separator ++
          type_expr t.type_
        ++ (if Syntax.Type.External.semicolon then O.txt ";" else O.noop)
      )
    in
    let kind = Some "external" in
    let anchor = path_to_id t.id in
    let doc = Comment.to_ir t.doc in
    Item.Declaration {kind ; anchor ; doc ; content}
end
open Value

(* This chunk of code is responsible for sectioning list of items
   according to headings and comments.

   In particular, it extracts headings as Items and shift heading levels
   of included items.

   TODO: This sectioning would be better done as a pass on the model directly.
*)
module Sectioning :
sig
  open Odoc_model

  type heading_level_shift

  val comment_items :
    ?level_shift:heading_level_shift ->
    Comment.docs ->
    heading_level_shift option * Item.t list

  val docs :
    Comment.docs -> Item.t list * Item.t list
end =
struct

  type heading_level_shift = int

  let level_to_int = function
    | `Title -> 0
    | `Section -> 1
    | `Subsection -> 2
    | `Subsubsection -> 3
    | `Paragraph -> 4
    | `Subparagraph -> 5

  let shift shift_by level =
    match shift_by with
    | Some i when i > 0 -> begin
        match level_to_int level + i with
        | 0 -> assert false
        | 1 -> `Section
        | 2 -> `Subsection
        | 3 -> `Subsubsection
        | 4 -> `Paragraph
        | n ->
          assert (n >= 5);
          `Subparagraph
      end
    | None | Some _ -> level

  let take_until_heading_or_end (docs : Odoc_model.Comment.docs) =
    let content, _, rest =
      Doctree.Take.until docs ~classify:(fun b ->
        match b.Location.value with
        | `Heading _ -> Stop_and_keep
        | #Odoc_model.Comment.attached_block_element as doc ->
          let content = Comment.attached_block_element doc in
          Accum content
      )
    in
    content, rest

  let comment_items ?level_shift (input0 : Odoc_model.Comment.docs) =
    let rec loop next_level_shift input_comment acc =
      match input_comment with
      | [] -> next_level_shift, List.rev acc
      | element::input_comment ->
        match element.Location.value with
        | `Heading (level, label, content) ->
          let level = shift level_shift level in
          let h = `Heading (level, label, content) in
          let item = Comment.heading h in
          loop (Some (level_to_int level)) input_comment (item :: acc)
        | _ ->
          let content, input_comment =
            take_until_heading_or_end (element::input_comment)
          in
          let item = Item.Text content in
          loop next_level_shift input_comment (item :: acc)
    in
    loop level_shift input0 []



  (* For doc pages, we want the header to contain everything until
     the first heading, then everything before the next heading which
     is either lower, or a section.
  *)
  let docs input_comment =
    let _, items = comment_items input_comment in
    let until_first_heading, o, items =
      Doctree.Take.until items ~classify:(function
        | Item.Heading h as i ->
          Stop_and_accum ([i], Some h.level)
        | i -> Accum [i]
      )
    in
    match o with
    | None -> until_first_heading, items
    | Some level ->
      let max_level = if level = 1 then 2 else level in
      let before_second_heading, _, items =
      Doctree.Take.until items ~classify:(function
        | Item.Heading h when h.level >= max_level -> Stop_and_keep
        | i -> Accum [i]
      )
      in
      let header = until_first_heading @ before_second_heading in
      header, items

end

module Class :
sig
  val class_ :
    Lang.Signature.recursive -> Lang.Class.t -> Item.t

  val class_type :
    Lang.Signature.recursive -> Lang.ClassType.t -> Item.t
end =
struct

  let class_type_expr (cte : Odoc_model.Lang.ClassType.expr) =
    match cte with
    | Constr (path, args) ->
      let link = Link.from_path ~stop_before:false (path :> Paths.Path.t) in
      format_type_path ~delim:(`brackets) args link
    | Signature _ ->
      Syntax.Class.open_tag
      ++ O.txt " ... "
      ++ Syntax.Class.close_tag

  let method_ (t : Odoc_model.Lang.Method.t) =
    let name = Paths.Identifier.name t.id in
    let virtual_ =
      if t.virtual_ then O.keyword "virtual" ++ O.txt " " else O.noop in
    let private_ =
      if t.private_ then O.keyword "private" ++ O.txt " " else O.noop in
    let content =
      O.documentedSrc (
        O.keyword "method" ++
          O.txt " " ++
          private_ ++
          virtual_ ++
          O.txt name ++
          O.txt Syntax.Type.annotation_separator ++
          type_expr t.type_
      )
    in
    let kind = Some "method" in
    let anchor = path_to_id t.id in
    let doc = Comment.to_ir t.doc in
    Item.Declaration {kind ; anchor ; doc ; content}

  let instance_variable (t : Odoc_model.Lang.InstanceVariable.t) =
    let name = Paths.Identifier.name t.id in
    let virtual_ =
      if t.virtual_ then O.keyword "virtual" ++ O.txt " " else O.noop in
    let mutable_ =
      if t.mutable_ then O.keyword "mutable" ++ O.txt " " else O.noop in
    let content =
      O.documentedSrc (
        O.keyword "val" ++
          O.txt " " ++
          mutable_ ++
          virtual_ ++
          O.txt name ++
          O.txt Syntax.Type.annotation_separator ++
          type_expr t.type_
      )
    in
    let kind = Some "instance-variable" in
    let anchor = path_to_id t.id in
    let doc = Comment.to_ir t.doc in
    Item.Declaration {kind ; anchor ; doc ; content}

  let inherit_ cte =
    let content =
      O.documentedSrc (
        O.keyword "inherit" ++
          O.txt " " ++
          class_type_expr cte
      )
    in
    let kind = Some "inherit" in
    let anchor = None in
    let doc = [] in
    Item.Declaration {kind ; anchor ; doc ; content}

  let constraint_ t1 t2 =
    let content =
      O.documentedSrc (format_constraints [(t1, t2)])
    in
    let kind = None in
    let anchor = None in
    let doc = [] in
    Item.Declaration {kind ; anchor ; doc ; content}

  let class_signature (c : Lang.ClassSignature.t) =
    let rec loop l acc_items =
      match l with
      | [] -> List.rev acc_items
      | item :: rest ->
        let continue item = loop rest (item :: acc_items) in
        match (item : Lang.ClassSignature.item) with
        | Inherit (Signature _) ->
          assert false (* Bold. *)
        | Inherit cty         -> continue @@ inherit_ cty
        | Method m            -> continue @@ method_ m
        | InstanceVariable v  -> continue @@ instance_variable v
        | Constraint (t1, t2) -> continue @@ constraint_ t1 t2
        | Comment `Stop ->
          let rest = Utils.skip_until rest ~p:(function
            | Lang.ClassSignature.Comment `Stop -> true
            | _ -> false)
          in
          loop rest acc_items
        | Comment (`Docs c) ->
          let _, items = Sectioning.comment_items c in
          loop rest (List.rev_append items acc_items)
    in
    (* FIXME: use [t.self] *)
    loop c.items []

  let rec class_decl (cd : Odoc_model.Lang.Class.decl) =
    match cd with
    | ClassType expr -> class_type_expr expr
    (* TODO: factorize the following with [type_expr] *)
    | Arrow (None, src, dst) ->
      type_expr ~needs_parentheses:true src ++
        O.txt " " ++ Syntax.Type.arrow ++ O.txt " " ++ class_decl dst
    | Arrow (Some lbl, src, dst) ->
      label lbl ++ O.txt ":" ++
        type_expr ~needs_parentheses:true src ++
        O.txt " " ++ Syntax.Type.arrow ++ O.txt " " ++ class_decl dst

  let class_ recursive (t : Odoc_model.Lang.Class.t) =
    let name = Paths.Identifier.name t.id in
    let params = format_params ~delim:(`brackets) t.params in
    let virtual_ =
      if t.virtual_ then O.keyword "virtual" ++ O.txt " " else O.noop in

    let cname, expansion =
      match t.expansion with
      | None ->
        O.documentedSrc @@ O.txt name,
        None
      | Some csig ->
        let doc = Comment.standalone t.doc in
        let items = class_signature csig in
        let url = Url.Path.from_identifier t.id in
        let header = format_title `Class (make_name_from_path url) @ doc in
        let page = Subpage.Page { title = name ; header ; items ; url } in
        O.documentedSrc @@ path url [inline @@ Text name],
        Some page
    in
    let cd = attach_expansion expansion
        (O.txt Syntax.Type.annotation_separator ++ class_decl t.type_)
    in
    let content =
      let open Lang.Signature in
      let keyword' =
        match recursive with
        | Ordinary | Nonrec | Rec -> "class"
        | And -> "and"
      in
      O.documentedSrc (
        O.keyword keyword' ++
        O.txt " " ++
        virtual_ ++
        params ++
        O.txt " ")
      @ cname
      @ cd
    in
    let kind = Some "class" in
    let anchor = path_to_id t.id in
    let doc = Comment.first_to_ir t.doc in
    Item.Declaration {kind ; anchor ; doc ; content}

  let class_type recursive (t : Odoc_model.Lang.ClassType.t) =
    let name = Paths.Identifier.name t.id in
    let params = format_params ~delim:(`brackets) t.params in
    let virtual_ =
      if t.virtual_ then O.keyword "virtual" ++ O.txt " " else O.noop in
    let cname, expansion =
      match t.expansion with
      | None ->
        O.documentedSrc @@ O.txt name,
        None
      | Some csig ->
        let url = Url.Path.from_identifier t.id in
        let doc = Comment.standalone t.doc in
        let items = class_signature csig in
        let header = format_title `Cty (make_name_from_path url) @ doc in
        let page = Subpage.Page { title = name ; header ; items ; url } in
        O.documentedSrc @@ path url [inline @@ Text name],
        Some page
    in
    let expr =
      attach_expansion expansion (O.txt " = " ++ class_type_expr t.expr)
    in
    let content =
      let open Lang.Signature in
      let keyword' =
        match recursive with
        | Ordinary | Nonrec | Rec ->
          O.keyword "class" ++ O.txt " " ++ O.keyword "type"
        | And -> O.keyword "and"
      in
      O.documentedSrc (
        keyword' ++
          O.txt " " ++
          virtual_ ++
          params ++
          O.txt " ")
      @ cname
      @ expr
    in
    let kind = Some "class-type" in
    let anchor = path_to_id t.id in
    let doc = Comment.first_to_ir t.doc in
    Item.Declaration {kind ; anchor ; doc ; content}
end
open Class



module Module :
sig
  val signature
    : ?level_shift:Sectioning.heading_level_shift
    -> Lang.Signature.t
    -> Item.t list
end =
struct

  let rec signature ?level_shift:level_shift0 s : Item.t list =
    let rec loop ?level_shift l acc_items =
      match l with
      | [] -> List.rev acc_items
      | item :: rest ->
        let continue (item : Item.t) =
          loop ?level_shift rest (item :: acc_items)
        in
        match (item : Lang.Signature.item) with
        | Module (recursive, m)    -> continue @@ module_ recursive m
        | ModuleType m             -> continue @@ module_type m
        | Class (recursive, c)     -> continue @@ class_ recursive c
        | ClassType (recursive, c) -> continue @@ class_type recursive c
        | Include m                -> continue @@ include_ level_shift m
        | ModuleSubstitution m     -> continue @@ module_substitution m

        | TypeSubstitution t ->
          continue @@ type_decl ~is_substitution:true (Ordinary, t)
        | Type (r, t) -> continue @@ type_decl (r, t)
        | TypExt e    -> continue @@ extension e
        | Exception e -> continue @@ exn e
        | Value v     -> continue @@ value v
        | External e  -> continue @@ external_ e

        | Comment `Stop ->
          let rest = Utils.skip_until rest ~p:(function
            | Lang.Signature.Comment `Stop -> true
            | _ -> false)
          in
          loop ?level_shift rest acc_items
        | Comment (`Docs c) ->
          let level_shift, items =
            Sectioning.comment_items ?level_shift:level_shift0 c
          in
          loop ?level_shift rest (List.rev_append items acc_items)
    in
    loop ?level_shift:level_shift0 s []

  and functor_argument
    : Odoc_model.Lang.FunctorParameter.parameter -> DocumentedSrc.t
    = fun arg ->
      let open Odoc_model.Lang.FunctorParameter in
      let name = Paths.Identifier.name arg.id in
      let content =
        match arg.expansion with
        | None ->
          O.documentedSrc (
            O.keyword "module" ++ O.txt " " ++
            O.txt (Paths.Identifier.name arg.id) ++
              O.txt Syntax.Type.annotation_separator ++
              mty (arg.id :> Paths.Identifier.Signature.t) arg.expr
          )
        | Some expansion ->
          let expansion =
            match expansion with
            | AlreadyASig ->
              begin match arg.expr with
              | Signature sg -> Odoc_model.Lang.Module.Signature sg
              | _ -> assert false
              end
            | e -> e
          in
          let url = Url.Path.from_identifier arg.id in
          let modname = path url [inline @@ Text name] in
          let modtyp =
            let prelude, items = module_expansion expansion in
            let header =
              format_title `Arg (make_name_from_path url) @ prelude
            in
            let title = name in
            let content = Subpage.Page { items ; title ; header ; url } in
            let summary =
              O.render (
                O.txt Syntax.Type.annotation_separator
                ++ mty (arg.id :> Paths.Identifier.Signature.t) arg.expr)
            in
            let status = `Default in
            [DocumentedSrc.Subpage { content ; summary ; status }]
          in
          O.documentedSrc (O.keyword "module" ++ O.txt " " ++ modname)
          @ modtyp
      in
      content

  and module_substitution (t : Odoc_model.Lang.ModuleSubstitution.t) =
    let name = Paths.Identifier.name t.id in
    let path = Link.from_path ~stop_before:true (t.manifest :> Paths.Path.t) in
    let content =
      O.documentedSrc (
        O.keyword "module" ++
          O.txt " " ++
          O.txt name ++
          O.txt " := " ++
          path
      )
    in
    let kind = Some "module-substitution" in
    let anchor = path_to_id t.id in
    let doc = Comment.to_ir t.doc in
    Item.Declaration {kind ; anchor ; doc ; content}

  and module_expansion
    : Odoc_model.Lang.Module.expansion
      -> Item.t list * Item.t list
    = fun t ->
      match t with
      | AlreadyASig -> assert false
      | Signature sg ->
        let expansion = signature sg in
        [], expansion
      | Functor (args, sg) ->
        let content = signature sg in
        let params =
          Utils.flatmap args ~f:(fun arg ->
            match arg with
            | Odoc_model.Lang.FunctorParameter.Unit -> []
            | Named arg ->
              let content = functor_argument arg in
              let kind = Some "parameter" in
              let anchor =
                Utils.option_of_result @@
                Url.Anchor.from_identifier (arg.id :> Paths.Identifier.t)
              in
              let doc = [] in
              [Item.Declaration { content ; anchor ; kind ; doc }]
          )
        in
        let prelude =
          [Item.Heading {
            label = Some "heading" ; level = 3 ; title = [inline @@ Text "Parameters"];
          }]
          @ params
          @ [Item.Heading {
            label = Some "heading" ; level = 3 ; title = [inline @@ Text "Signature"];
          }]
        in
        prelude, content

  and module_
    : Odoc_model.Lang.Signature.recursive ->
      Odoc_model.Lang.Module.t ->
      Item.t
    = fun recursive t ->
      let modname = Paths.Identifier.name t.id in
      let modname, expansion =
        match t.expansion with
        | None ->
          O.documentedSrc (O.txt modname),
          None
        | Some expansion ->
          let expansion =
            match expansion with
            | AlreadyASig ->
              begin match t.type_ with
              | ModuleType (Odoc_model.Lang.ModuleType.Signature sg) ->
                Odoc_model.Lang.Module.Signature sg
              | _ -> assert false
              end
            | e -> e
          in
          let doc = Comment.standalone t.doc in
          let prelude, items = module_expansion expansion in
          let url = Url.Path.from_identifier t.id in
          let link = path url [inline @@ Text modname] in
          let title = modname in
          let header =
            format_title `Mod (make_name_from_path url) @ doc @ prelude
          in
          let page = Subpage.Page {items ; title ; header ; url } in
          O.documentedSrc link, Some page
      in
      let modexpr =
        attach_expansion expansion @@
        module_decl (t.id :> Paths.Identifier.Signature.t)
          (match t.display_type with
            | None -> t.type_
            | Some t -> t)
      in
      let content =
        let keyword' =
          match recursive with
          | Ordinary | Nonrec -> O.keyword "module"
          | Rec -> O.keyword "module" ++ O.txt " " ++ O.keyword "rec"
          | And -> O.keyword "and"
        in
        O.documentedSrc (keyword' ++ O.txt " ")
        @ modname
        @ modexpr
        @ O.documentedSrc
            (if Syntax.Mod.close_tag_semicolon then O.txt ";" else O.noop)
      in
      let kind = Some "module" in
      let anchor = path_to_id t.id in
      let doc = Comment.first_to_ir t.doc in
      Item.Declaration {kind ; anchor ; doc ; content}

  and module_decl (base : Paths.Identifier.Signature.t) md =
    begin match md with
    | Alias _ -> O.txt " = "
    | ModuleType _ -> O.txt Syntax.Type.annotation_separator
    end ++
      module_decl' base md

  and extract_path_from_mt ~(default: Paths.Identifier.Signature.t) =
    let open Odoc_model.Lang.ModuleType in
    function
    | Path (`Resolved r) ->
      (Paths.Path.Resolved.ModuleType.identifier r :> Paths.Identifier.Signature.t)
    | With (mt, _) -> extract_path_from_mt ~default mt
    | TypeOf (Odoc_model.Lang.Module.Alias (`Resolved r)) ->
      (Paths.Path.Resolved.Module.identifier r :> Paths.Identifier.Signature.t)
    | TypeOf (Odoc_model.Lang.Module.ModuleType mt) ->
      extract_path_from_mt ~default mt
    | _ -> default

  and module_decl'
    : Paths.Identifier.Signature.t -> Odoc_model.Lang.Module.decl -> text =
    fun base -> function
      | Alias mod_path ->
        Link.from_path ~stop_before:true (mod_path :> Paths.Path.t)
      | ModuleType mt -> mty (extract_path_from_mt ~default:base mt) mt

  and module_type (t : Odoc_model.Lang.ModuleType.t) =
    let modname = Paths.Identifier.name t.id in
    let modname, expansion =
      match t.expansion with
      | None ->
        O.documentedSrc @@ O.txt modname, None
      | Some expansion ->
        let expansion =
          match expansion with
          | AlreadyASig ->
            begin match t.expr with
            | Some (Signature sg) -> Odoc_model.Lang.Module.Signature sg
            | _ -> assert false
            end
          | e -> e
        in
        let doc = Comment.standalone t.doc in
        let prelude, items = module_expansion expansion in
        let url = Url.Path.from_identifier t.id in
        let link = path url [inline @@ Text modname] in
        let title = modname in
        let header =
          format_title `Mty (make_name_from_path url) @ doc @ prelude
        in
        let page = Subpage.Page {items ; title ; header ; url } in
        O.documentedSrc link, Some page
    in
    let mty =
      attach_expansion expansion @@
      match t.expr with
      | None -> O.noop
      | Some expr ->
        O.txt " = " ++ mty (t.id :> Paths.Identifier.Signature.t) expr
    in
    let content =
      O.documentedSrc (
        O.keyword "module" ++
          O.txt " " ++
          O.keyword "type" ++
          O.txt " ")
      @ modname
      @ mty
      @ O.documentedSrc
          (if Syntax.Mod.close_tag_semicolon then O.txt ";" else O.noop)
    in
    let kind = Some "module-type" in
    let anchor = path_to_id t.id in
    let doc = Comment.first_to_ir t.doc in
    Item.Declaration {kind ; anchor ; doc ; content}

  and mty
    : Paths.Identifier.Signature.t -> Odoc_model.Lang.ModuleType.expr -> text
    = fun base -> function
      | Path mty_path ->
        Link.from_path ~stop_before:true (mty_path :> Paths.Path.t)
      | Signature _ ->
        Syntax.Mod.open_tag ++ O.txt " ... " ++ Syntax.Mod.close_tag
      | Functor (Unit, expr) ->
        (if Syntax.Mod.functor_keyword then O.keyword "functor" else O.noop) ++
          O.txt " () " ++
          mty base expr
      | Functor (Named arg, expr) ->
        let name =
          let open Odoc_model.Lang.FunctorParameter in
          let name = Paths.Identifier.name arg.id in
          match
            Url.from_identifier
              ~stop_before:(arg.expansion = None) (arg.id :> Paths.Identifier.t)
          with
          | Error _ -> O.txt name
          | Ok href -> resolved href [inline @@ Text name]
        in
        (if Syntax.Mod.functor_keyword then O.keyword "functor" else O.noop) ++
          O.txt " (" ++ name ++ O.txt Syntax.Type.annotation_separator ++
          mty base arg.expr ++
          O.txt ")" ++ O.txt " " ++ Syntax.Type.arrow ++ O.txt " " ++
          mty base expr
      | With (expr, substitutions) ->
        mty base expr ++
          O.txt " " ++
          O.keyword "with" ++
          O.txt " " ++
          O.list
            ~sep:(O.txt " " ++ O.keyword "and" ++ O.txt " ")
            ~f:(substitution base)
            substitutions
      | TypeOf md ->
        O.keyword "module" ++
          O.txt " " ++
          O.keyword "type" ++
          O.txt " " ++
          O.keyword "of" ++
          O.txt " " ++
          module_decl' base md

  (* TODO : Centralize the list juggling for type parameters *)
  and type_expr_in_subst ~base td typath =
    let typath = Link.from_fragment ~base typath in
    match td.Lang.TypeDecl.Equation.params with
    | [] -> typath
    | l -> Syntax.Type.handle_substitution_params typath (format_params l)

  and substitution
    : Paths.Identifier.Signature.t -> Odoc_model.Lang.ModuleType.substitution
      -> text
    = fun base -> function
      | ModuleEq (frag_mod, md) ->
        O.keyword "module" ++
          O.txt " " ++
          Link.from_fragment ~base (frag_mod :> Paths.Fragment.t)
        ++ O.txt " = " ++
          module_decl' base md
      | TypeEq (frag_typ, td) ->
        O.keyword "type" ++
          O.txt " " ++
          type_expr_in_subst ~base td (frag_typ :> Paths.Fragment.t) ++
          fst (format_manifest td) ++
          format_constraints td.Odoc_model.Lang.TypeDecl.Equation.constraints
      | ModuleSubst (frag_mod, mod_path) ->
        O.keyword "module" ++
          O.txt " " ++
          Link.from_fragment
            ~base (frag_mod :> Paths.Fragment.t) ++
          O.txt " := " ++
          Link.from_path ~stop_before:true (mod_path :> Paths.Path.t)
      | TypeSubst (frag_typ, td) ->
        O.keyword "type" ++
          O.txt " " ++
          type_expr_in_subst ~base td (frag_typ :> Paths.Fragment.t) ++
          O.txt " := " ++
          match td.Lang.TypeDecl.Equation.manifest with
          | None -> assert false (* cf loader/cmti *)
          | Some te ->
            type_expr te

  and include_ level_shift (t : Odoc_model.Lang.Include.t) =
    let status =
      let is_inline_tag element = element.Odoc_model.Location_.value = `Tag `Inline in
      let is_open_tag element = element.Odoc_model.Location_.value = `Tag `Open in
      let is_closed_tag element = element.Odoc_model.Location_.value = `Tag `Closed in
      if List.exists is_inline_tag t.doc then `Inline
      else if List.exists is_open_tag t.doc then `Open
      else if List.exists is_closed_tag t.doc then `Closed
      else `Default
    in
    let content =
      let level_shift =
        if status = `Inline then
          level_shift
        else
          None
      in
      Subpage.Items (signature ?level_shift t.expansion.content)
    in
    let summary =
      O.render (
        O.keyword "include" ++
          O.txt " " ++
          module_decl' t.parent t.decl ++
          (if Syntax.Mod.include_semicolon then O.keyword ";" else O.noop)
      )
    in
    let content = {Subpage. content; status; summary} in
    let kind = Some "include" in
    let anchor = None in
    let doc = Comment.first_to_ir t.doc in
    Item.Subpage {kind ; anchor ; doc ; content}

end
open Module



module Page :
sig
  val compilation_unit : Lang.Compilation_unit.t -> Page.t
  val page : Lang.Page.t -> Page.t
end =
struct
  let pack
    : Odoc_model.Lang.Compilation_unit.Packed.t -> Item.t list
  = fun t ->
    let open Odoc_model.Lang in
    let f x =
      let id = x.Compilation_unit.Packed.id in
      let modname = Paths.Identifier.name id in
      let md_def =
        O.keyword "module" ++
          O.txt " " ++
          O.txt modname ++
          O.txt " = " ++
          Link.from_path ~stop_before:false (x.path :> Paths.Path.t)
      in
      let content = O.documentedSrc md_def in
      let anchor =
        Utils.option_of_result @@
        Url.Anchor.from_identifier (id :> Paths.Identifier.t)
      in
      let kind = Some "modules" in
      let doc = [] in
      let decl = {Item. anchor ; content ; kind ; doc } in
      Item.Declaration decl
    in
    List.map f t

  let compilation_unit (t : Odoc_model.Lang.Compilation_unit.t) : Page.t =
    let title = Paths.Identifier.name t.id in
    let header =
      format_title `Mod title @ Comment.standalone t.doc
    in
    let url = Url.Path.from_identifier t.id in
    let items =
      match t.content with
      | Module sign -> signature sign
      | Pack packed -> pack packed
    in
    {Page. title ; header ; items ; url }

  let page (t : Odoc_model.Lang.Page.t) : Page.t =
    let name =
      match t.name with
      | `Page (_, name) -> name
    in
    let title = Odoc_model.Names.PageName.to_string name in
    let url = Url.Path.from_identifier t.name in
    let header, items = Sectioning.docs t.content in
    {Page. title ; header ; items ; url }
end
include Page
end

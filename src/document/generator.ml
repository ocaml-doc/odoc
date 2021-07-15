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
    let level = 0 and label = None in
    [ Item.Heading { level; label; title } ]
  in
  let prefix s = mk (inline (Text (s ^ " ")) :: O.code (O.txt name)) in
  match kind with
  | `Mod -> prefix "Module"
  | `Arg -> prefix "Parameter"
  | `Mty -> prefix "Module type"
  | `Cty -> prefix "Class type"
  | `Class -> prefix "Class"
  | `Page -> mk [ inline @@ Text name ]

let make_name_from_path { Url.Path.name; parent; _ } =
  match parent with
  | None -> name
  | Some p -> Printf.sprintf "%s.%s" p.name name

let label t ppf =
  match t with
  | Odoc_model.Lang.TypeExpr.Label s -> O.pf ppf "%s" s
  | Optional s -> O.pf ppf "?%s" s

let tag tag t ppf = O.pf ppf "@{<%s>%t@}" tag t

let type_var tv = tag "type-var" (O.txt tv)

let enclose ~l ~r x = O.span (fun ppf -> O.pf ppf "%s%t%s" l x r)

let path p txt =
  !O.elt
    [ inline @@ InternalLink (InternalLink.Resolved (Url.from_path p, txt)) ]

let resolved p txt =
  !O.elt [ inline @@ InternalLink (InternalLink.Resolved (p, txt)) ]

let unresolved txt =
  !O.elt [ inline @@ InternalLink (InternalLink.Unresolved txt) ]

let path_to_id path =
  match Url.Anchor.from_identifier (path :> Paths.Identifier.t) with
  | Error _ -> None
  | Ok url -> Some url

let attach_expansion ?(status = `Default) (eq, o, e) page text =
  match page with
  | None -> O.documentedSrc text
  | Some (page : Page.t) ->
      let url = page.url in
      let summary = O.render text in
      let expansion =
        O.documentedSrc (O.txt eq ++ O.keyword o)
        @ DocumentedSrc.[ Subpage { status; content = page } ]
        @ O.documentedSrc (O.keyword e)
      in
      DocumentedSrc.
        [ Alternative (Expansion { summary; url; status; expansion }) ]

(** Returns the preamble as an item. Stop the preamble at the first heading. The
    rest is inserted into [items]. *)
let prepare_preamble comment items =
  let preamble, first_comment =
    Utils.split_at
      ~f:(function
        | { Odoc_model.Location_.value = `Heading _; _ } -> true | _ -> false)
      comment
  in
  (Comment.standalone preamble, Comment.standalone first_comment @ items)

let make_expansion_page title kind url ?(header_title = make_name_from_path url)
    comments items =
  let comment = List.concat comments in
  let preamble, items = prepare_preamble comment items in
  let header = format_title kind header_title @ preamble in
  { Page.title; header; items; url }

include Generator_signatures

module Make (Syntax : SYNTAX) = struct
  module Link : sig
    val from_path : Paths.Path.t -> text

    val from_fragment : Paths.Fragment.leaf -> text
  end = struct
    open Paths

    let rec from_path : Path.t -> text =
     fun path ->
      match path with
      | `Identifier (id, _) ->
          unresolved [ inline @@ Text (Identifier.name id) ]
      | `Root root -> unresolved [ inline @@ Text root ]
      | `Forward root -> unresolved [ inline @@ Text root ] (* FIXME *)
      | `Dot (prefix, suffix) ->
          let link = from_path (prefix :> Path.t) in
          link ++ O.txt ("." ^ suffix)
      | `Apply (p1, p2) ->
          let link1 = from_path (p1 :> Path.t) in
          let link2 = from_path (p2 :> Path.t) in
          link1 ++ O.txt "(" ++ link2 ++ O.txt ")"
      | `Resolved _ when Paths.Path.is_hidden path ->
          let txt = Url.render_path path in
          Format.eprintf "Warning, resolved hidden path: %s\n%!" txt;
          unresolved [ inline @@ Text txt ]
      | `Resolved rp -> (
          (* If the path is pointing to an opaque module or module type
             there won't be a page generated - so we stop before; at
             the parent page, and link instead to the anchor representing
             the declaration of the opaque module(_type) *)
          let stop_before =
            match rp with
            | `OpaqueModule _ | `OpaqueModuleType _ -> true
            | _ -> false
          in
          let id = Paths.Path.Resolved.identifier rp in
          let txt = Url.render_path path in
          match Url.from_identifier ~stop_before id with
          | Ok href -> resolved href [ inline @@ Text txt ]
          | Error (Url.Error.Not_linkable _) -> O.txt txt
          | Error exn ->
              Printf.eprintf "Id.href failed: %S\n%!" (Url.Error.to_string exn);
              O.txt txt)

    let dot prefix suffix = prefix ^ "." ^ suffix

    let rec render_fragment_any : Fragment.t -> string =
     fun fragment ->
      match fragment with
      | `Resolved rr -> render_resolved_fragment rr
      | `Dot (`Root, suffix) -> suffix
      | `Dot (prefix, suffix) ->
          dot (render_fragment_any (prefix :> Fragment.t)) suffix
      | `Root -> assert false

    and render_resolved_fragment : Fragment.Resolved.t -> string =
      let open Fragment.Resolved in
      fun fragment ->
        match fragment with
        | `Root _ -> assert false
        | `Subst (_, rr) -> render_resolved_fragment (rr :> t)
        | `Alias (_, rr) -> render_resolved_fragment (rr :> t)
        | `Module (`Root _, s) -> ModuleName.to_string s
        | `Module_type (`Root _, s) -> ModuleTypeName.to_string s
        | `Type (`Root _, s) -> TypeName.to_string s
        | `Class (`Root _, s) -> ClassName.to_string s
        | `ClassType (`Root _, s) -> ClassTypeName.to_string s
        | `Module (rr, s) ->
            dot (render_resolved_fragment (rr :> t)) (ModuleName.to_string s)
        | `Module_type (rr, s) ->
            dot
              (render_resolved_fragment (rr :> t))
              (ModuleTypeName.to_string s)
        | `Type (rr, s) ->
            dot (render_resolved_fragment (rr :> t)) (TypeName.to_string s)
        | `Class (rr, s) ->
            dot (render_resolved_fragment (rr :> t)) (ClassName.to_string s)
        | `ClassType (rr, s) ->
            dot (render_resolved_fragment (rr :> t)) (ClassTypeName.to_string s)
        | `OpaqueModule r -> render_resolved_fragment (r :> t)

    let resolved_fragment_to_ir : Fragment.Resolved.leaf -> text =
     fun fragment ->
      let open Fragment in
      let id = Resolved.identifier (fragment :> Resolved.t) in
      let txt = render_resolved_fragment (fragment :> Resolved.t) in
      match Url.from_identifier ~stop_before:false id with
      | Ok href -> resolved href [ inline @@ Text txt ]
      | Error (Not_linkable _) -> unresolved [ inline @@ Text txt ]
      | Error exn ->
          Printf.eprintf "[FRAG] Id.href failed: %S\n%!"
            (Url.Error.to_string exn);
          unresolved [ inline @@ Text txt ]

    let from_fragment : Fragment.leaf -> text = function
      | `Resolved r
        when not (Fragment.Resolved.is_hidden (r :> Fragment.Resolved.t)) ->
          resolved_fragment_to_ir r
      | f ->
          let txt = render_fragment_any (f :> Fragment.t) in
          unresolved [ inline @@ Text txt ]
  end

  module Type_expression : sig
    val type_expr : ?needs_parentheses:bool -> Lang.TypeExpr.t -> text

    val format_type_path :
      delim:[ `parens | `brackets ] -> Lang.TypeExpr.t list -> text -> text
  end = struct
    let rec te_variant (t : Odoc_model.Lang.TypeExpr.Polymorphic_variant.t) =
      let style_arguments ~constant arguments =
        (* Multiple arguments in a polymorphic variant constructor correspond
           to a conjunction of types, not a product: [`Lbl int&float].
           If constant is [true], the conjunction starts with an empty type,
           for instance [`Lbl &int].
        *)
        let wrapped_type_expr =
          (* type conjunction in Reason is printed as `Lbl (t1)&(t2)` *)
          if Syntax.Type.Variant.parenthesize_params then fun x ->
            enclose ~l:"(" ~r:")" (type_expr x)
          else fun x -> type_expr x
        in
        let arguments =
          O.list arguments ~sep:(O.txt " & ") ~f:wrapped_type_expr
        in
        if constant then O.txt "& " ++ arguments else arguments
      in
      let rec style_elements ~add_pipe = function
        | [] -> O.noop
        | first :: rest ->
            let first =
              match first with
              | Odoc_model.Lang.TypeExpr.Polymorphic_variant.Type te ->
                  let res = type_expr te in
                  if add_pipe then O.txt " " ++ O.span (O.txt "| " ++ res)
                  else res
              | Constructor { constant; name; arguments; _ } ->
                  let constr =
                    let name = "`" ^ name in
                    if add_pipe then O.span (O.txt ("| " ^ name))
                    else O.txt name
                  in
                  let res =
                    match arguments with
                    | [] -> constr
                    | _ ->
                        let arguments = style_arguments ~constant arguments in
                        O.span
                          (if Syntax.Type.Variant.parenthesize_params then
                           constr ++ arguments
                          else constr ++ O.txt " of " ++ arguments)
                  in
                  if add_pipe then O.txt " " ++ res else res
            in
            first ++ style_elements ~add_pipe:true rest
      in
      let elements = style_elements ~add_pipe:false t.elements in
      O.span
        (match t.kind with
        | Fixed -> O.txt "[ " ++ elements ++ O.txt " ]"
        | Open -> O.txt "[> " ++ elements ++ O.txt " ]"
        | Closed [] -> O.txt "[< " ++ elements ++ O.txt " ]"
        | Closed lst ->
            let constrs = String.concat " " lst in
            O.txt "[< " ++ elements ++ O.txt (" " ^ constrs ^ " ]"))

    and te_object (t : Odoc_model.Lang.TypeExpr.Object.t) =
      let fields =
        O.list t.fields ~f:(function
          | Odoc_model.Lang.TypeExpr.Object.Method { name; type_ } ->
              O.txt (name ^ Syntax.Type.annotation_separator)
              ++ type_expr type_
              ++ O.txt Syntax.Obj.field_separator
          | Inherit type_ -> type_expr type_ ++ O.txt Syntax.Obj.field_separator)
      in
      let open_tag =
        if t.open_ then O.txt Syntax.Obj.open_tag_extendable
        else O.txt Syntax.Obj.open_tag_closed
      in
      let close_tag =
        if t.open_ then O.txt Syntax.Obj.close_tag_extendable
        else O.txt Syntax.Obj.close_tag_closed
      in
      O.span (open_tag ++ fields ++ close_tag)

    and format_type_path ~delim (params : Odoc_model.Lang.TypeExpr.t list)
        (path : text) : text =
      match params with
      | [] -> path
      | [ param ] ->
          let param = type_expr ~needs_parentheses:true param in
          let args =
            if Syntax.Type.parenthesize_constructor then
              O.txt "(" ++ param ++ O.txt ")"
            else param
          in
          Syntax.Type.handle_constructor_params path args
      | params ->
          let params = O.list params ~sep:(O.txt ",\194\160") ~f:type_expr in
          let params =
            match delim with
            | `parens -> enclose ~l:"(" params ~r:")"
            | `brackets -> enclose ~l:"[" params ~r:"]"
          in
          Syntax.Type.handle_constructor_params path params

    and type_expr ?(needs_parentheses = false) (t : Odoc_model.Lang.TypeExpr.t)
        =
      match t with
      | Var s -> type_var (Syntax.Type.var_prefix ^ s)
      | Any -> type_var Syntax.Type.any
      | Alias (te, alias) ->
          type_expr ~needs_parentheses:true te
          ++ O.txt " " ++ O.keyword "as" ++ O.txt " '" ++ O.txt alias
      | Arrow (None, src, dst) ->
          let res =
            O.span
              (type_expr ~needs_parentheses:true src
              ++ O.txt " " ++ Syntax.Type.arrow)
            ++ O.txt " " ++ type_expr dst
          in
          if not needs_parentheses then res else enclose ~l:"(" res ~r:")"
      | Arrow (Some lbl, src, dst) ->
          let res =
            O.span
              (label lbl ++ O.txt ":"
              ++ type_expr ~needs_parentheses:true src
              ++ O.sp ++ Syntax.Type.arrow)
            ++ O.sp ++ type_expr dst
          in
          if not needs_parentheses then res else enclose ~l:"(" res ~r:")"
      | Tuple lst ->
          let res =
            O.list lst
              ~sep:(O.txt Syntax.Type.Tuple.element_separator)
              ~f:(type_expr ~needs_parentheses:true)
          in
          if Syntax.Type.Tuple.always_parenthesize || needs_parentheses then
            enclose ~l:"(" res ~r:")"
          else res
      | Constr (path, args) ->
          let link = Link.from_path (path :> Paths.Path.t) in
          format_type_path ~delim:`parens args link
      | Polymorphic_variant v -> te_variant v
      | Object o -> te_object o
      | Class (path, args) ->
          format_type_path ~delim:`brackets args
            (Link.from_path (path :> Paths.Path.t))
      | Poly (polyvars, t) ->
          O.txt (String.concat " " polyvars ^ ". ") ++ type_expr t
      | Package pkg ->
          enclose ~l:"(" ~r:")"
            (O.keyword "module" ++ O.txt " "
            ++ Link.from_path (pkg.path :> Paths.Path.t)
            ++
            match pkg.substitutions with
            | [] -> O.noop
            | lst ->
                O.txt " " ++ O.keyword "with" ++ O.txt " "
                ++ O.list
                     ~sep:(O.txt " " ++ O.keyword "and" ++ O.txt " ")
                     lst ~f:package_subst)

    and package_subst
        ((frag_typ, te) : Paths.Fragment.Type.t * Odoc_model.Lang.TypeExpr.t) :
        text =
      let typath = Link.from_fragment (frag_typ :> Paths.Fragment.leaf) in
      O.keyword "type" ++ O.txt " " ++ typath ++ O.txt " = " ++ type_expr te
  end

  open Type_expression

  (* Also handles constructor declarations for exceptions and extensible
     variants, and exposes a few helpers used in formatting classes and signature
     constraints. *)
  module Type_declaration : sig
    val type_decl :
      ?is_substitution:bool ->
      Lang.Signature.recursive * Lang.TypeDecl.t ->
      Item.t

    val extension : Lang.Extension.t -> Item.t

    val exn : Lang.Exception.t -> Item.t

    val format_params :
      ?delim:[ `parens | `brackets ] -> Lang.TypeDecl.param list -> text

    val format_manifest :
      ?is_substitution:bool ->
      ?compact_variants:bool ->
      Lang.TypeDecl.Equation.t ->
      text * bool

    val format_constraints : (Lang.TypeExpr.t * Lang.TypeExpr.t) list -> text
  end = struct
    let record fields =
      let field mutable_ id typ =
        match Url.from_identifier ~stop_before:true id with
        | Error e -> failwith (Url.Error.to_string e)
        | Ok url ->
            let name = Paths.Identifier.name id in
            let attrs =
              [ "def"; "record"; Url.Anchor.string_of_kind url.kind ]
            in
            let cell =
              (* O.td ~a:[ O.a_class ["def"; kind ] ]
               *   [O.a ~a:[O.a_href ("#" ^ anchor); O.a_class ["anchor"]] []
               *   ; *)
              O.code
                ((if mutable_ then O.keyword "mutable" ++ O.txt " " else O.noop)
                ++ O.txt name
                ++ O.txt Syntax.Type.annotation_separator
                ++ type_expr typ
                ++ O.txt Syntax.Type.Record.field_separator)
              (* ] *)
            in
            (url, attrs, cell)
      in
      let rows =
        fields
        |> List.map (fun fld ->
               let open Odoc_model.Lang.TypeDecl.Field in
               let url, attrs, code =
                 field fld.mutable_ (fld.id :> Paths.Identifier.t) fld.type_
               in
               let anchor = Some url in
               let rhs = Comment.to_ir fld.doc in
               let doc = if not (Comment.has_doc fld.doc) then [] else rhs in
               let markers = Syntax.Comment.markers in
               DocumentedSrc.Documented { anchor; attrs; code; doc; markers })
      in
      let content =
        O.documentedSrc (O.txt "{") @ rows @ O.documentedSrc (O.txt "}")
      in
      content

    let constructor :
        Paths.Identifier.t ->
        Odoc_model.Lang.TypeDecl.Constructor.argument ->
        Odoc_model.Lang.TypeExpr.t option ->
        DocumentedSrc.t =
     fun id args ret_type ->
      let name = Paths.Identifier.name id in
      let kind = Url.(kind id |> Anchor.string_of_kind) in
      let cstr = tag kind (O.txt name) in
      let is_gadt, ret_type =
        match ret_type with
        | None -> (false, O.noop)
        | Some te ->
            let constant = match args with Tuple [] -> true | _ -> false in
            let ret_type =
              O.txt " "
              ++ (if constant then O.txt ":" else Syntax.Type.GADT.arrow)
              ++ O.txt " " ++ type_expr te
            in
            (true, ret_type)
      in
      match args with
      | Tuple [] -> O.documentedSrc (cstr ++ ret_type)
      | Tuple lst ->
          let params =
            O.list lst
              ~sep:(O.txt Syntax.Type.Tuple.element_separator)
              ~f:(type_expr ~needs_parentheses:is_gadt)
          in
          O.documentedSrc
            (cstr
            ++ (if Syntax.Type.Variant.parenthesize_params then
                O.txt "(" ++ params ++ O.txt ")"
               else
                 (if is_gadt then O.txt Syntax.Type.annotation_separator
                 else O.txt " " ++ O.keyword "of" ++ O.txt " ")
                 ++ params)
            ++ ret_type)
      | Record fields ->
          if is_gadt then
            O.documentedSrc (cstr ++ O.txt Syntax.Type.annotation_separator)
            @ record fields @ O.documentedSrc ret_type
          else
            O.documentedSrc (cstr ++ O.txt " " ++ O.keyword "of" ++ O.txt " ")
            @ record fields

    let rec read_typ_exp typ_expr =
      let open Lang.TypeExpr in
      let open Paths.Path in
      match typ_expr with
      | Constr (p, ts) ->
          is_hidden (p :> Paths.Path.t)
          || List.exists (fun t -> read_typ_exp t) ts
      | Poly (_, t) | Alias (t, _) -> read_typ_exp t
      | Arrow (_, t, t2) -> read_typ_exp t || read_typ_exp t2
      | Tuple ts | Class (_, ts) -> List.exists (fun t -> read_typ_exp t) ts
      | _ -> false

    let internal_cstr_arg t =
      let open Lang.TypeDecl.Constructor in
      let open Lang.TypeDecl.Field in
      match t.args with
      | Tuple type_exprs ->
          List.exists (fun type_expr -> read_typ_exp type_expr) type_exprs
      | Record fields ->
          List.exists (fun field -> read_typ_exp field.type_) fields

    let variant cstrs : DocumentedSrc.t =
      let constructor id args res =
        match Url.from_identifier ~stop_before:true id with
        | Error e -> failwith (Url.Error.to_string e)
        | Ok url ->
            let attrs =
              [ "def"; "variant"; Url.Anchor.string_of_kind url.kind ]
            in
            let content =
              let doc = constructor id args res in
              O.documentedSrc (O.txt "| ") @ doc
            in
            (url, attrs, content)
      in
      match cstrs with
      | [] -> O.documentedSrc (O.txt "|")
      | _ :: _ ->
          let rows =
            cstrs
            |> List.filter (fun cstr -> not (internal_cstr_arg cstr))
            |> List.map (fun cstr ->
                   let open Odoc_model.Lang.TypeDecl.Constructor in
                   let url, attrs, code =
                     constructor
                       (cstr.id :> Paths.Identifier.t)
                       cstr.args cstr.res
                   in
                   let anchor = Some url in
                   let rhs = Comment.to_ir cstr.doc in
                   let doc =
                     if not (Comment.has_doc cstr.doc) then [] else rhs
                   in
                   let markers = Syntax.Comment.markers in
                   DocumentedSrc.Nested { anchor; attrs; code; doc; markers })
          in
          rows

    let extension_constructor (t : Odoc_model.Lang.Extension.Constructor.t) =
      let id = (t.id :> Paths.Identifier.t) in
      match Url.from_identifier ~stop_before:true id with
      | Error e -> failwith (Url.Error.to_string e)
      | Ok url ->
          let anchor = Some url in
          let attrs = [ "def"; Url.Anchor.string_of_kind url.kind ] in
          let code =
            O.documentedSrc (O.txt "| ") @ constructor id t.args t.res
          in
          let doc = Comment.to_ir t.doc in
          let markers = Syntax.Comment.markers in
          DocumentedSrc.Nested { anchor; attrs; code; doc; markers }

    let extension (t : Odoc_model.Lang.Extension.t) =
      let content =
        O.documentedSrc
          (O.keyword "type" ++ O.txt " "
          ++ Link.from_path (t.type_path :> Paths.Path.t)
          ++ O.txt " += ")
        @ List.map extension_constructor t.constructors
        @ O.documentedSrc
            (if Syntax.Type.type_def_semicolon then O.txt ";" else O.noop)
      in
      let attr = [ "type"; "extension" ] in
      let anchor = Some (Url.Anchor.extension_decl t) in
      let doc = Comment.to_ir t.doc in
      Item.Declaration { attr; anchor; doc; content }

    let exn (t : Odoc_model.Lang.Exception.t) =
      let cstr = constructor (t.id :> Paths.Identifier.t) t.args t.res in
      let content =
        O.documentedSrc (O.keyword "exception" ++ O.txt " ")
        @ cstr
        @ O.documentedSrc
            (if Syntax.Type.Exception.semicolon then O.txt ";" else O.noop)
      in
      let attr = [ "exception" ] in
      let anchor = path_to_id t.id in
      let doc = Comment.to_ir t.doc in
      Item.Declaration { attr; anchor; doc; content }

    let polymorphic_variant ~type_ident
        (t : Odoc_model.Lang.TypeExpr.Polymorphic_variant.t) =
      let row item =
        let kind_approx, cstr, doc =
          match item with
          | Odoc_model.Lang.TypeExpr.Polymorphic_variant.Type te ->
              ("unknown", O.code (type_expr te), None)
          | Constructor { constant; name; arguments; doc; _ } -> (
              let cstr = "`" ^ name in
              ( "constructor",
                (match arguments with
                | [] -> O.code (O.txt cstr)
                | _ ->
                    (* Multiple arguments in a polymorphic variant constructor correspond
                       to a conjunction of types, not a product: [`Lbl int&float].
                       If constant is [true], the conjunction starts with an empty type,
                       for instance [`Lbl &int].
                    *)
                    let wrapped_type_expr =
                      (* type conjunction in Reason is printed as `Lbl (t1)&(t2)` *)
                      if Syntax.Type.Variant.parenthesize_params then fun x ->
                        O.txt "(" ++ type_expr x ++ O.txt ")"
                      else fun x -> type_expr x
                    in
                    let params =
                      O.list arguments ~sep:(O.txt " & ") ~f:wrapped_type_expr
                    in
                    let params =
                      if constant then O.txt "& " ++ params else params
                    in
                    O.code
                      (O.txt cstr
                      ++
                      if Syntax.Type.Variant.parenthesize_params then params
                      else O.txt " " ++ O.keyword "of" ++ O.txt " " ++ params)),
                match doc with [] -> None | _ -> Some (Comment.to_ir doc) ))
        in
        let markers = Syntax.Comment.markers in
        try
          let url = Url.Anchor.polymorphic_variant ~type_ident item in
          let attrs = [ "def"; Url.Anchor.string_of_kind url.kind ] in
          let anchor = Some url in
          let code = O.code (O.txt "| ") @ cstr in
          let doc = match doc with None -> [] | Some doc -> doc in
          DocumentedSrc.Documented { attrs; anchor; code; doc; markers }
        with Failure s ->
          Printf.eprintf "ERROR: %s\n%!" s;
          let code = O.code (O.txt "| ") @ cstr in
          let attrs = [ "def"; kind_approx ] in
          let doc = [] in
          let anchor = None in
          DocumentedSrc.Documented { attrs; anchor; code; doc; markers }
      in
      let variants = List.map row t.elements in
      let intro, ending =
        match t.kind with
        | Fixed -> (O.documentedSrc (O.txt "[ "), O.documentedSrc (O.txt " ]"))
        | Open -> (O.documentedSrc (O.txt "[> "), O.documentedSrc (O.txt " ]"))
        | Closed [] ->
            (O.documentedSrc (O.txt "[< "), O.documentedSrc (O.txt " ]"))
        | Closed lst ->
            let constrs = String.concat " " lst in
            ( O.documentedSrc (O.txt "[< "),
              O.documentedSrc (O.txt (" " ^ constrs ^ " ]")) )
      in
      intro @ variants @ ending

    let format_params :
          'row.
          ?delim:[ `parens | `brackets ] ->
          Odoc_model.Lang.TypeDecl.param list ->
          text =
     fun ?(delim = `parens) params ->
      let format_param { Odoc_model.Lang.TypeDecl.desc; variance; injectivity }
          =
        let desc =
          match desc with
          | Odoc_model.Lang.TypeDecl.Any -> [ "_" ]
          | Var s -> [ "'"; s ]
        in
        let var_desc =
          match variance with
          | None -> desc
          | Some Odoc_model.Lang.TypeDecl.Pos -> "+" :: desc
          | Some Odoc_model.Lang.TypeDecl.Neg -> "-" :: desc
        in
        let final = if injectivity then "!" :: var_desc else var_desc in
        String.concat "" final
      in
      O.txt
        (match params with
        | [] -> ""
        | [ x ] -> format_param x |> Syntax.Type.handle_format_params
        | lst -> (
            let params = String.concat ", " (List.map format_param lst) in
            (match delim with `parens -> "(" | `brackets -> "[")
            ^ params
            ^ match delim with `parens -> ")" | `brackets -> "]"))

    let format_constraints constraints =
      O.list constraints ~f:(fun (t1, t2) ->
          O.txt " " ++ O.keyword "constraint" ++ O.txt " " ++ type_expr t1
          ++ O.txt " = " ++ type_expr t2)

    let format_manifest :
          'inner_row 'outer_row.
          ?is_substitution:bool ->
          ?compact_variants:bool ->
          Odoc_model.Lang.TypeDecl.Equation.t ->
          text * bool =
     fun ?(is_substitution = false) ?(compact_variants = true) equation ->
      let _ = compact_variants in
      (* TODO *)
      let private_ = equation.private_ in
      match equation.manifest with
      | None -> (O.noop, private_)
      | Some t ->
          let manifest =
            O.txt (if is_substitution then " := " else " = ")
            ++ (if private_ then
                O.keyword Syntax.Type.private_keyword ++ O.txt " "
               else O.noop)
            ++ type_expr t
          in
          (manifest, false)

    let type_decl ?(is_substitution = false)
        ((recursive, t) : Lang.Signature.recursive * Lang.TypeDecl.t) =
      let tyname = Paths.Identifier.name t.id in
      let constraints = format_constraints t.equation.constraints in
      let manifest, need_private =
        match t.equation.manifest with
        | Some (Odoc_model.Lang.TypeExpr.Polymorphic_variant variant) ->
            let code =
              polymorphic_variant
                ~type_ident:(t.id :> Paths.Identifier.t)
                variant
            in
            let manifest =
              O.documentedSrc
                (O.txt (if is_substitution then " := " else " = ")
                ++
                if t.equation.private_ then
                  O.keyword Syntax.Type.private_keyword ++ O.txt " "
                else O.noop)
              @ code
            in
            (manifest, false)
        | _ ->
            let manifest, need_private =
              format_manifest ~is_substitution t.equation
            in
            (O.documentedSrc manifest, need_private)
      in
      let representation =
        match t.representation with
        | None -> []
        | Some repr ->
            let content =
              match repr with
              | Extensible -> O.documentedSrc (O.txt "..")
              | Variant cstrs -> variant cstrs
              | Record fields -> record fields
            in
            if List.length content > 0 then
              O.documentedSrc
                (O.txt " = "
                ++
                if need_private then
                  O.keyword Syntax.Type.private_keyword ++ O.txt " "
                else O.noop)
              @ content
            else []
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
        @ manifest @ representation
        @ O.documentedSrc constraints
        @ O.documentedSrc
            (if Syntax.Type.type_def_semicolon then O.txt ";" else O.noop)
      in
      let attr = "type" :: (if is_substitution then [ "subst" ] else []) in
      let anchor = path_to_id t.id in
      let doc = Comment.to_ir t.doc in
      Item.Declaration { attr; anchor; doc; content }
  end

  open Type_declaration

  module Value : sig
    val value : Lang.Value.t -> Item.t
  end = struct
    let value (t : Odoc_model.Lang.Value.t) =
      let extra_attr, semicolon =
        match t.value with
        | Abstract -> ([], Syntax.Value.semicolon)
        | External _ -> ([ "external" ], Syntax.Type.External.semicolon)
      in
      let name = Paths.Identifier.name t.id in
      let content =
        O.documentedSrc
          (O.keyword Syntax.Value.variable_keyword
          ++ O.txt " " ++ O.txt name
          ++ O.txt Syntax.Type.annotation_separator
          ++ type_expr t.type_
          ++ if semicolon then O.txt ";" else O.noop)
      in
      let attr = [ "value" ] @ extra_attr in
      let anchor = path_to_id t.id in
      let doc = Comment.to_ir t.doc in
      Item.Declaration { attr; anchor; doc; content }
  end

  open Value

  (* This chunk of code is responsible for sectioning list of items
     according to headings by extracting headings as Items.

     TODO: This sectioning would be better done as a pass on the model directly.
  *)
  module Sectioning : sig
    open Odoc_model

    val comment_items : Comment.docs -> Item.t list

    val docs : Comment.docs -> Item.t list * Item.t list
  end = struct
    let take_until_heading_or_end (docs : Odoc_model.Comment.docs) =
      let content, _, rest =
        Doctree.Take.until docs ~classify:(fun b ->
            match b.Location.value with
            | `Heading _ -> Stop_and_keep
            | #Odoc_model.Comment.attached_block_element as doc ->
                let content = Comment.attached_block_element doc in
                Accum content)
      in
      (content, rest)

    let comment_items (input0 : Odoc_model.Comment.docs) =
      let rec loop input_comment acc =
        match input_comment with
        | [] -> List.rev acc
        | element :: input_comment -> (
            match element.Location.value with
            | `Heading (level, label, content) ->
                let h = `Heading (level, label, content) in
                let item = Comment.heading h in
                loop input_comment (item :: acc)
            | _ ->
                let content, input_comment =
                  take_until_heading_or_end (element :: input_comment)
                in
                let item = Item.Text content in
                loop input_comment (item :: acc))
      in
      loop input0 []

    (* For doc pages, we want the header to contain everything until
       the first heading, then everything before the next heading which
       is either lower, or a section.
    *)
    let docs input_comment =
      let items = comment_items input_comment in
      let until_first_heading, o, items =
        Doctree.Take.until items ~classify:(function
          | Item.Heading h as i -> Stop_and_accum ([ i ], Some h.level)
          | i -> Accum [ i ])
      in
      match o with
      | None -> (until_first_heading, items)
      | Some level ->
          let max_level = if level = 1 then 2 else level in
          let before_second_heading, _, items =
            Doctree.Take.until items ~classify:(function
              | Item.Heading h when h.level >= max_level -> Stop_and_keep
              | i -> Accum [ i ])
          in
          let header = until_first_heading @ before_second_heading in
          (header, items)
  end

  module Class : sig
    val class_ : Lang.Class.t -> Item.t

    val class_type : Lang.ClassType.t -> Item.t
  end = struct
    let class_type_expr (cte : Odoc_model.Lang.ClassType.expr) =
      match cte with
      | Constr (path, args) ->
          let link = Link.from_path (path :> Paths.Path.t) in
          format_type_path ~delim:`brackets args link
      | Signature _ ->
          Syntax.Class.open_tag ++ O.txt " ... " ++ Syntax.Class.close_tag

    let method_ (t : Odoc_model.Lang.Method.t) =
      let name = Paths.Identifier.name t.id in
      let virtual_ =
        if t.virtual_ then O.keyword "virtual" ++ O.txt " " else O.noop
      in
      let private_ =
        if t.private_ then O.keyword "private" ++ O.txt " " else O.noop
      in
      let content =
        O.documentedSrc
          (O.keyword "method" ++ O.txt " " ++ private_ ++ virtual_ ++ O.txt name
          ++ O.txt Syntax.Type.annotation_separator
          ++ type_expr t.type_)
      in
      let attr = [ "method" ] in
      let anchor = path_to_id t.id in
      let doc = Comment.to_ir t.doc in
      Item.Declaration { attr; anchor; doc; content }

    let instance_variable (t : Odoc_model.Lang.InstanceVariable.t) =
      let name = Paths.Identifier.name t.id in
      let virtual_ =
        if t.virtual_ then O.keyword "virtual" ++ O.txt " " else O.noop
      in
      let mutable_ =
        if t.mutable_ then O.keyword "mutable" ++ O.txt " " else O.noop
      in
      let content =
        O.documentedSrc
          (O.keyword "val" ++ O.txt " " ++ mutable_ ++ virtual_ ++ O.txt name
          ++ O.txt Syntax.Type.annotation_separator
          ++ type_expr t.type_)
      in
      let attr = [ "value"; "instance-variable" ] in
      let anchor = path_to_id t.id in
      let doc = Comment.to_ir t.doc in
      Item.Declaration { attr; anchor; doc; content }

    let inherit_ cte =
      let content =
        O.documentedSrc (O.keyword "inherit" ++ O.txt " " ++ class_type_expr cte)
      in
      let attr = [ "inherit" ] in
      let anchor = None in
      let doc = [] in
      Item.Declaration { attr; anchor; doc; content }

    let constraint_ t1 t2 =
      let content = O.documentedSrc (format_constraints [ (t1, t2) ]) in
      let attr = [] in
      let anchor = None in
      let doc = [] in
      Item.Declaration { attr; anchor; doc; content }

    let class_signature (c : Lang.ClassSignature.t) =
      let rec loop l acc_items =
        match l with
        | [] -> List.rev acc_items
        | item :: rest -> (
            let continue item = loop rest (item :: acc_items) in
            match (item : Lang.ClassSignature.item) with
            | Inherit (Signature _) -> assert false (* Bold. *)
            | Inherit cty -> continue @@ inherit_ cty
            | Method m -> continue @@ method_ m
            | InstanceVariable v -> continue @@ instance_variable v
            | Constraint (t1, t2) -> continue @@ constraint_ t1 t2
            | Comment `Stop ->
                let rest =
                  Utils.skip_until rest ~p:(function
                    | Lang.ClassSignature.Comment `Stop -> true
                    | _ -> false)
                in
                loop rest acc_items
            | Comment (`Docs c) ->
                let items = Sectioning.comment_items c in
                loop rest (List.rev_append items acc_items))
      in
      (* FIXME: use [t.self] *)
      (c.doc, loop c.items [])

    let rec class_decl (cd : Odoc_model.Lang.Class.decl) =
      match cd with
      | ClassType expr -> class_type_expr expr
      (* TODO: factorize the following with [type_expr] *)
      | Arrow (None, src, dst) ->
          O.span
            (type_expr ~needs_parentheses:true src
            ++ O.txt " " ++ Syntax.Type.arrow)
          ++ O.txt " " ++ class_decl dst
      | Arrow (Some lbl, src, dst) ->
          O.span
            (label lbl ++ O.txt ":"
            ++ type_expr ~needs_parentheses:true src
            ++ O.txt " " ++ Syntax.Type.arrow)
          ++ O.txt " " ++ class_decl dst

    let class_ (t : Odoc_model.Lang.Class.t) =
      let name = Paths.Identifier.name t.id in
      let params = format_params ~delim:`brackets t.params in
      let virtual_ =
        if t.virtual_ then O.keyword "virtual" ++ O.txt " " else O.noop
      in

      let cname, expansion, expansion_doc =
        match t.expansion with
        | None -> (O.documentedSrc @@ O.txt name, None, None)
        | Some csig ->
            let expansion_doc, items = class_signature csig in
            let url = Url.Path.from_identifier t.id in
            let page =
              make_expansion_page name `Class url [ t.doc; expansion_doc ] items
            in
            ( O.documentedSrc @@ path url [ inline @@ Text name ],
              Some page,
              Some expansion_doc )
      in
      let summary =
        O.txt Syntax.Type.annotation_separator ++ class_decl t.type_
      in
      let cd =
        attach_expansion
          (Syntax.Type.annotation_separator, "object", "end")
          expansion summary
      in
      let content =
        O.documentedSrc
          (O.keyword "class" ++ O.txt " " ++ virtual_ ++ params ++ O.txt " ")
        @ cname @ cd
      in
      let attr = [ "class" ] in
      let anchor = path_to_id t.id in
      let doc = Comment.synopsis ~decl_doc:t.doc ~expansion_doc in
      Item.Declaration { attr; anchor; doc; content }

    let class_type (t : Odoc_model.Lang.ClassType.t) =
      let name = Paths.Identifier.name t.id in
      let params = format_params ~delim:`brackets t.params in
      let virtual_ =
        if t.virtual_ then O.keyword "virtual" ++ O.txt " " else O.noop
      in
      let cname, expansion, expansion_doc =
        match t.expansion with
        | None -> (O.documentedSrc @@ O.txt name, None, None)
        | Some csig ->
            let url = Url.Path.from_identifier t.id in
            let expansion_doc, items = class_signature csig in
            let page =
              make_expansion_page name `Cty url [ t.doc; expansion_doc ] items
            in
            ( O.documentedSrc @@ path url [ inline @@ Text name ],
              Some page,
              Some expansion_doc )
      in
      let summary = O.txt " = " ++ class_type_expr t.expr in
      let expr = attach_expansion (" = ", "object", "end") expansion summary in
      let content =
        O.documentedSrc
          (O.keyword "class" ++ O.txt " " ++ O.keyword "type" ++ O.txt " "
         ++ virtual_ ++ params ++ O.txt " ")
        @ cname @ expr
      in
      let attr = [ "class-type" ] in
      let anchor = path_to_id t.id in
      let doc = Comment.synopsis ~decl_doc:t.doc ~expansion_doc in
      Item.Declaration { attr; anchor; doc; content }
  end

  open Class

  module Module : sig
    val signature : Lang.Signature.t -> Comment.Comment.docs * Item.t list
    (** Returns [header_doc, content]. *)
  end = struct
    let internal_module m =
      let open Lang.Module in
      match m.id with
      | `Module (_, name) when ModuleName.is_internal name -> true
      | _ -> false

    let internal_type t =
      let open Lang.TypeDecl in
      match t.id with
      | `Type (_, name) when TypeName.is_internal name -> true
      | _ -> false

    let internal_value v =
      let open Lang.Value in
      match v.id with
      | `Value (_, name) when ValueName.is_internal name -> true
      | _ -> false

    let internal_module_type t =
      let open Lang.ModuleType in
      match t.id with
      | `ModuleType (_, name) when ModuleTypeName.is_internal name -> true
      | _ -> false

    let internal_module_substitution t =
      let open Lang.ModuleSubstitution in
      match t.id with
      | `Module (_, name) when ModuleName.is_internal name -> true
      | _ -> false

    let internal_module_type_substitution t =
      let open Lang.ModuleTypeSubstitution in
      match t.id with
      | `ModuleType (_, name) when ModuleTypeName.is_internal name -> true
      | _ -> false

    let rec signature (s : Lang.Signature.t) =
      let rec loop l acc_items =
        match l with
        | [] -> List.rev acc_items
        | item :: rest -> (
            let continue (item : Item.t) = loop rest (item :: acc_items) in
            match (item : Lang.Signature.item) with
            | Module (_, m) when internal_module m -> loop rest acc_items
            | Type (_, t) when internal_type t -> loop rest acc_items
            | Value v when internal_value v -> loop rest acc_items
            | ModuleType m when internal_module_type m -> loop rest acc_items
            | ModuleSubstitution m when internal_module_substitution m ->
                loop rest acc_items
            | ModuleTypeSubstitution m when internal_module_type_substitution m
              ->
                loop rest acc_items
            | ModuleTypeSubstitution m -> continue @@ module_type_substitution m
            | Module (_, m) -> continue @@ module_ m
            | ModuleType m -> continue @@ module_type m
            | Class (_, c) -> continue @@ class_ c
            | ClassType (_, c) -> continue @@ class_type c
            | Include m -> continue @@ include_ m
            | ModuleSubstitution m -> continue @@ module_substitution m
            | TypeSubstitution t ->
                continue @@ type_decl ~is_substitution:true (Ordinary, t)
            | Type (r, t) -> continue @@ type_decl (r, t)
            | TypExt e -> continue @@ extension e
            | Exception e -> continue @@ exn e
            | Value v -> continue @@ value v
            | Open _ -> loop rest acc_items
            | Comment `Stop ->
                let rest =
                  Utils.skip_until rest ~p:(function
                    | Lang.Signature.Comment `Stop -> true
                    | _ -> false)
                in
                loop rest acc_items
            | Comment (`Docs c) ->
                let items = Sectioning.comment_items c in
                loop rest (List.rev_append items acc_items))
      in
      (Lang.extract_signature_doc s, loop s.items [])

    and functor_parameter :
        Odoc_model.Lang.FunctorParameter.parameter -> DocumentedSrc.t =
     fun arg ->
      let open Odoc_model.Lang.FunctorParameter in
      let name = Paths.Identifier.name arg.id in
      let render_ty = arg.expr in
      let modtyp =
        mty_in_decl (arg.id :> Paths.Identifier.Signature.t) render_ty
      in
      let modname, mod_decl =
        match expansion_of_module_type_expr arg.expr with
        | None ->
            let modname = O.txt (Paths.Identifier.name arg.id) in
            (modname, O.documentedSrc modtyp)
        | Some (expansion_doc, items) ->
            let url = Url.Path.from_identifier arg.id in
            let modname = path url [ inline @@ Text name ] in
            let type_with_expansion =
              let content =
                make_expansion_page name `Arg url [ expansion_doc ] items
              in
              let summary = O.render modtyp in
              let status = `Default in
              let expansion =
                O.documentedSrc
                  (O.txt Syntax.Type.annotation_separator ++ O.keyword "sig")
                @ DocumentedSrc.[ Subpage { content; status } ]
                @ O.documentedSrc (O.keyword "end")
              in
              DocumentedSrc.
                [
                  Alternative
                    (Expansion { status = `Default; summary; url; expansion });
                ]
            in
            (modname, type_with_expansion)
      in
      O.documentedSrc (O.keyword "module" ++ O.txt " ")
      @ O.documentedSrc modname @ mod_decl

    and module_substitution (t : Odoc_model.Lang.ModuleSubstitution.t) =
      let name = Paths.Identifier.name t.id in
      let path = Link.from_path (t.manifest :> Paths.Path.t) in
      let content =
        O.documentedSrc
          (O.keyword "module" ++ O.txt " " ++ O.txt name ++ O.txt " := " ++ path)
      in
      let attr = [ "module-substitution" ] in
      let anchor = path_to_id t.id in
      let doc = Comment.to_ir t.doc in
      Item.Declaration { attr; anchor; doc; content }

    and module_type_substitution (t : Odoc_model.Lang.ModuleTypeSubstitution.t)
        =
      let modname = Paths.Identifier.name t.id in
      let modname, expansion_doc, mty =
        module_type_manifest ~subst:true modname t.id t.doc (Some t.manifest)
      in
      let content =
        O.documentedSrc
          (O.keyword "module" ++ O.txt " " ++ O.keyword "type" ++ O.txt " ")
        @ modname @ mty
        @ O.documentedSrc
            (if Syntax.Mod.close_tag_semicolon then O.txt ";" else O.noop)
      in
      let attr = [ "module-type" ] in
      let anchor = path_to_id t.id in
      let doc = Comment.synopsis ~decl_doc:t.doc ~expansion_doc in
      Item.Declaration { attr; anchor; doc; content }

    and simple_expansion :
        Odoc_model.Lang.ModuleType.simple_expansion ->
        Comment.Comment.docs * Item.t list =
     fun t ->
      let rec extract_functor_params
          (f : Odoc_model.Lang.ModuleType.simple_expansion) =
        match f with
        | Signature sg -> (None, sg)
        | Functor (p, expansion) ->
            let add_to params =
              match p with Unit -> params | Named p -> p :: params
            in
            let params, sg = extract_functor_params expansion in
            let params = match params with None -> [] | Some p -> p in
            (Some (add_to params), sg)
      in
      match extract_functor_params t with
      | None, sg -> signature sg
      | Some params, sg ->
          let sg_doc, content = signature sg in
          let params =
            Utils.flatmap params ~f:(fun arg ->
                let content = functor_parameter arg in
                let attr = [ "parameter" ] in
                let anchor =
                  Utils.option_of_result
                  @@ Url.Anchor.from_identifier (arg.id :> Paths.Identifier.t)
                in
                let doc = [] in
                [ Item.Declaration { content; anchor; attr; doc } ])
          in
          let prelude =
            Item.Heading
              {
                label = Some "parameters";
                level = 1;
                title = [ inline @@ Text "Parameters" ];
              }
            :: params
          and content =
            Item.Heading
              {
                label = Some "signature";
                level = 1;
                title = [ inline @@ Text "Signature" ];
              }
            :: content
          in
          (sg_doc, prelude @ content)

    and expansion_of_module_type_expr :
        Odoc_model.Lang.ModuleType.expr ->
        (Comment.Comment.docs * Item.t list) option =
     fun t ->
      let rec simple_expansion_of (t : Odoc_model.Lang.ModuleType.expr) =
        match t with
        | Path { p_expansion = None; _ }
        | TypeOf { t_expansion = None; _ }
        | With { w_expansion = None; _ } ->
            None
        | Path { p_expansion = Some e; _ }
        | TypeOf { t_expansion = Some e; _ }
        | With { w_expansion = Some e; _ } ->
            Some e
        | Signature sg -> Some (Signature sg)
        | Functor (f_parameter, e) -> (
            match simple_expansion_of e with
            | Some e -> Some (Functor (f_parameter, e))
            | None -> None)
      in
      match simple_expansion_of t with
      | None -> None
      | Some e -> Some (simple_expansion e)

    and module_ : Odoc_model.Lang.Module.t -> Item.t =
     fun t ->
      let modname = Paths.Identifier.name t.id in
      let expansion =
        match t.type_ with
        | Alias (_, Some e) -> Some (simple_expansion e)
        | Alias (_, None) -> None
        | ModuleType e -> expansion_of_module_type_expr e
      in
      let modname, status, expansion, expansion_doc =
        match expansion with
        | None -> (O.documentedSrc (O.txt modname), `Default, None, None)
        | Some (expansion_doc, items) ->
            let status =
              match t.type_ with
              | ModuleType (Signature _) -> `Inline
              | _ -> `Default
            in
            let url = Url.Path.from_identifier t.id in
            let link = path url [ inline @@ Text modname ] in
            let page =
              make_expansion_page modname `Mod url [ t.doc; expansion_doc ]
                items
            in
            (O.documentedSrc link, status, Some page, Some expansion_doc)
      in
      let summary = mdexpr_in_decl t.id t.type_ in
      let modexpr =
        attach_expansion ~status
          (Syntax.Type.annotation_separator, "sig", "end")
          expansion summary
      in
      let content =
        O.documentedSrc (O.keyword "module" ++ O.txt " ")
        @ modname @ modexpr
        @ O.documentedSrc
            (if Syntax.Mod.close_tag_semicolon then O.txt ";" else O.noop)
      in
      let attr = [ "module" ] in
      let anchor = path_to_id t.id in
      let doc = Comment.synopsis ~decl_doc:t.doc ~expansion_doc in
      Item.Declaration { attr; anchor; doc; content }

    and simple_expansion_in_decl (base : Paths.Identifier.Module.t) se =
      let rec ty_of_se :
          Lang.ModuleType.simple_expansion -> Lang.ModuleType.expr = function
        | Signature sg -> Signature sg
        | Functor (arg, sg) -> Functor (arg, ty_of_se sg)
      in
      mty_in_decl (base :> Paths.Identifier.Signature.t) (ty_of_se se)

    and mdexpr_in_decl (base : Paths.Identifier.Module.t) md =
      let sig_dotdotdot =
        O.txt Syntax.Type.annotation_separator
        ++ Syntax.Mod.open_tag ++ O.txt " ... " ++ Syntax.Mod.close_tag
      in
      match md with
      | Alias (_, Some se) -> simple_expansion_in_decl base se
      | Alias (p, _) when not Paths.Path.(is_hidden (p :> t)) ->
          O.txt " = " ++ mdexpr md
      | Alias _ -> sig_dotdotdot
      | ModuleType mt -> mty_in_decl (base :> Paths.Identifier.Signature.t) mt

    and mdexpr : Odoc_model.Lang.Module.decl -> text = function
      | Alias (mod_path, _) -> Link.from_path (mod_path :> Paths.Path.t)
      | ModuleType mt -> mty mt

    and module_type_manifest ~subst modname id doc manifest =
      let expansion =
        match manifest with
        | None -> None
        | Some e -> expansion_of_module_type_expr e
      in
      let modname, expansion, expansion_doc =
        match expansion with
        | None -> (O.documentedSrc @@ O.txt modname, None, None)
        | Some (expansion_doc, items) ->
            let url = Url.Path.from_identifier id in
            let link = path url [ inline @@ Text modname ] in
            let page =
              make_expansion_page modname `Mty url [ doc; expansion_doc ] items
            in
            (O.documentedSrc link, Some page, Some expansion_doc)
      in
      let summary =
        match manifest with
        | None -> O.noop
        | Some expr -> (if subst then O.txt " := " else O.txt " = ") ++ mty expr
      in
      ( modname,
        expansion_doc,
        attach_expansion (" = ", "sig", "end") expansion summary )

    and module_type (t : Odoc_model.Lang.ModuleType.t) =
      let modname = Paths.Identifier.name t.id in
      let modname, expansion_doc, mty =
        module_type_manifest ~subst:false modname t.id t.doc t.expr
      in
      let content =
        O.documentedSrc
          (O.keyword "module" ++ O.txt " " ++ O.keyword "type" ++ O.txt " ")
        @ modname @ mty
        @ O.documentedSrc
            (if Syntax.Mod.close_tag_semicolon then O.txt ";" else O.noop)
      in
      let attr = [ "module-type" ] in
      let anchor = path_to_id t.id in
      let doc = Comment.synopsis ~decl_doc:t.doc ~expansion_doc in
      Item.Declaration { attr; anchor; doc; content }

    and umty_hidden : Odoc_model.Lang.ModuleType.U.expr -> bool = function
      | Path p -> Paths.Path.(is_hidden (p :> t))
      | With (_, expr) -> umty_hidden expr
      | TypeOf { t_desc = ModPath m; _ }
      | TypeOf { t_desc = StructInclude m; _ } ->
          Paths.Path.(is_hidden (m :> t))
      | Signature _ -> false

    and mty_hidden : Odoc_model.Lang.ModuleType.expr -> bool = function
      | Path { p_path = mty_path; _ } -> Paths.Path.(is_hidden (mty_path :> t))
      | With { w_expr; _ } -> umty_hidden w_expr
      | TypeOf { t_desc = ModPath m; _ }
      | TypeOf { t_desc = StructInclude m; _ } ->
          Paths.Path.(is_hidden (m :> t))
      | _ -> false

    and mty_with subs expr =
      umty expr ++ O.txt " " ++ O.keyword "with" ++ O.txt " "
      ++ O.list
           ~sep:(O.txt " " ++ O.keyword "and" ++ O.txt " ")
           ~f:(fun x -> O.span (substitution x))
           subs

    and mty_typeof t_desc =
      match t_desc with
      | Odoc_model.Lang.ModuleType.ModPath m ->
          O.keyword "module" ++ O.txt " " ++ O.keyword "type" ++ O.txt " "
          ++ O.keyword "of" ++ O.txt " "
          ++ Link.from_path (m :> Paths.Path.t)
      | StructInclude m ->
          O.keyword "module" ++ O.txt " " ++ O.keyword "type" ++ O.txt " "
          ++ O.keyword "of" ++ O.txt " " ++ O.keyword "struct" ++ O.txt " "
          ++ O.keyword "include" ++ O.txt " "
          ++ Link.from_path (m :> Paths.Path.t)
          ++ O.txt " " ++ O.keyword "end"

    and umty : Odoc_model.Lang.ModuleType.U.expr -> text =
     fun m ->
      match m with
      | Path p -> Link.from_path (p :> Paths.Path.t)
      | With (subs, expr) -> mty_with subs expr
      | TypeOf { t_desc; _ } -> mty_typeof t_desc
      | Signature _ ->
          Syntax.Mod.open_tag ++ O.txt " ... " ++ Syntax.Mod.close_tag

    and mty : Odoc_model.Lang.ModuleType.expr -> text =
     fun m ->
      if mty_hidden m then
        Syntax.Mod.open_tag ++ O.txt " ... " ++ Syntax.Mod.close_tag
      else
        match m with
        | Path { p_path = mty_path; _ } ->
            Link.from_path (mty_path :> Paths.Path.t)
        | Functor (Unit, expr) ->
            (if Syntax.Mod.functor_keyword then O.keyword "functor" else O.noop)
            ++ O.span (O.txt " () " ++ Syntax.Type.arrow)
            ++ O.txt " " ++ mty expr
        | Functor (Named arg, expr) ->
            let arg_expr = arg.expr in
            let stop_before = expansion_of_module_type_expr arg_expr = None in
            let name =
              let open Odoc_model.Lang.FunctorParameter in
              let name = Paths.Identifier.name arg.id in
              match
                Url.from_identifier ~stop_before (arg.id :> Paths.Identifier.t)
              with
              | Error _ -> O.txt name
              | Ok href -> resolved href [ inline @@ Text name ]
            in
            (if Syntax.Mod.functor_keyword then O.keyword "functor" else O.noop)
            ++ O.span
                 (O.txt " (" ++ name
                 ++ O.txt Syntax.Type.annotation_separator
                 ++ mty arg_expr ++ O.txt ")" ++ O.txt " " ++ Syntax.Type.arrow
                 )
            ++ O.txt " " ++ mty expr
        | With { w_substitutions; w_expr; _ } -> mty_with w_substitutions w_expr
        | TypeOf { t_desc; _ } -> mty_typeof t_desc
        | Signature _ ->
            Syntax.Mod.open_tag ++ O.txt " ... " ++ Syntax.Mod.close_tag

    and mty_in_decl :
        Paths.Identifier.Signature.t -> Odoc_model.Lang.ModuleType.expr -> text
        =
     fun base -> function
      | (Path _ | Signature _ | With _ | TypeOf _) as m ->
          O.txt Syntax.Type.annotation_separator ++ mty m
      | Functor _ as m when not Syntax.Mod.functor_contraction ->
          O.txt Syntax.Type.annotation_separator ++ mty m
      | Functor (arg, expr) ->
          let text_arg =
            match arg with
            | Unit -> O.txt "()"
            | Named arg ->
                let arg_expr = arg.expr in
                let stop_before =
                  expansion_of_module_type_expr arg_expr = None
                in
                let name =
                  let open Odoc_model.Lang.FunctorParameter in
                  let name = Paths.Identifier.name arg.id in
                  match
                    Url.from_identifier ~stop_before
                      (arg.id :> Paths.Identifier.t)
                  with
                  | Error _ -> O.txt name
                  | Ok href -> resolved href [ inline @@ Text name ]
                in
                O.txt "(" ++ name
                ++ O.txt Syntax.Type.annotation_separator
                ++ mty arg.expr ++ O.txt ")"
          in
          O.txt " " ++ text_arg ++ mty_in_decl base expr

    (* TODO : Centralize the list juggling for type parameters *)
    and type_expr_in_subst td typath =
      let typath = Link.from_fragment typath in
      match td.Lang.TypeDecl.Equation.params with
      | [] -> typath
      | l -> Syntax.Type.handle_substitution_params typath (format_params l)

    and substitution : Odoc_model.Lang.ModuleType.substitution -> text =
      function
      | ModuleEq (frag_mod, md) ->
          O.keyword "module" ++ O.sp
          ++ Link.from_fragment (frag_mod :> Paths.Fragment.leaf)
          ++ O.sp ++ O.txt "= " ++ mdexpr md
      | ModuleTypeEq (frag_mty, md) ->
          O.keyword "module" ++ O.txt " " ++ O.keyword "type" ++ O.txt " "
          ++ Link.from_fragment (frag_mty :> Paths.Fragment.leaf)
          ++ O.txt " = " ++ mty md
      | TypeEq (frag_typ, td) ->
          O.keyword "type" ++ O.sp
          ++ type_expr_in_subst td (frag_typ :> Paths.Fragment.leaf)
          ++ fst (format_manifest td)
          ++ format_constraints td.Odoc_model.Lang.TypeDecl.Equation.constraints
      | ModuleSubst (frag_mod, mod_path) ->
          O.keyword "module" ++ O.sp
          ++ Link.from_fragment (frag_mod :> Paths.Fragment.leaf)
          ++ O.sp ++ O.txt ":= "
          ++ Link.from_path (mod_path :> Paths.Path.t)
      | ModuleTypeSubst (frag_mty, md) ->
          O.keyword "module" ++ O.txt " " ++ O.keyword "type" ++ O.txt " "
          ++ Link.from_fragment (frag_mty :> Paths.Fragment.leaf)
          ++ O.txt " := " ++ mty md
      | TypeSubst (frag_typ, td) -> (
          O.keyword "type" ++ O.sp
          ++ type_expr_in_subst td (frag_typ :> Paths.Fragment.leaf)
          ++ O.sp ++ O.txt ":= "
          ++
          match td.Lang.TypeDecl.Equation.manifest with
          | None -> assert false (* cf loader/cmti *)
          | Some te -> type_expr te)

    and include_ (t : Odoc_model.Lang.Include.t) =
      let decl_hidden =
        match t.decl with
        | Alias p -> Paths.Path.(is_hidden (p :> t))
        | ModuleType mty -> umty_hidden mty
      in
      let status = if decl_hidden then `Inline else t.status in

      let include_decl =
        match t.decl with
        | Odoc_model.Lang.Include.Alias mod_path ->
            Link.from_path (mod_path :> Paths.Path.t)
        | ModuleType mt -> umty mt
      in

      let _, content = signature t.expansion.content in
      let summary =
        O.render
          (O.keyword "include" ++ O.txt " " ++ include_decl
          ++ if Syntax.Mod.include_semicolon then O.keyword ";" else O.noop)
      in
      let content = { Include.content; status; summary } in
      let attr = [ "include" ] in
      let anchor = None in
      let doc =
        (* Documentation attached to includes behave differently than other
           declarations, which show only the synopsis. We can't only show the
           synopsis because no page is generated to render it and we'd loose
           the full documentation.
           The documentation from the expansion is not used. *)
        Comment.to_ir t.doc
      in
      Item.Include { attr; anchor; doc; content }
  end

  open Module

  module Page : sig
    val compilation_unit : Lang.Compilation_unit.t -> Page.t

    val page : Lang.Page.t -> Page.t
  end = struct
    let pack : Odoc_model.Lang.Compilation_unit.Packed.t -> Item.t list =
     fun t ->
      let open Odoc_model.Lang in
      let f x =
        let id = x.Compilation_unit.Packed.id in
        let modname = Paths.Identifier.name id in
        let md_def =
          O.keyword "module" ++ O.txt " " ++ O.txt modname ++ O.txt " = "
          ++ Link.from_path (x.path :> Paths.Path.t)
        in
        let content = O.documentedSrc md_def in
        let anchor =
          Utils.option_of_result
          @@ Url.Anchor.from_identifier (id :> Paths.Identifier.t)
        in
        let attr = [ "modules" ] in
        let doc = [] in
        let decl = { Item.anchor; content; attr; doc } in
        Item.Declaration decl
      in
      List.map f t

    let compilation_unit (t : Odoc_model.Lang.Compilation_unit.t) : Page.t =
      let title = Paths.Identifier.name t.id in
      let url = Url.Path.from_identifier t.id in
      let unit_doc, items =
        match t.content with
        | Module sign -> signature sign
        | Pack packed -> ([], pack packed)
      in
      make_expansion_page title ~header_title:title `Mod url [ unit_doc ] items

    let page (t : Odoc_model.Lang.Page.t) : Page.t =
      let name =
        match t.name with `Page (_, name) | `LeafPage (_, name) -> name
      in
      let title = Odoc_model.Names.PageName.to_string name in
      let url = Url.Path.from_identifier t.name in
      let header, items = Sectioning.docs t.content in
      { Page.title; header; items; url }
  end

  include Page
end

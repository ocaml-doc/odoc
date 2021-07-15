open Odoc_document.Types
open Types
module Doctree = Odoc_document.Doctree

let rec list_concat_map ?sep ~f = function
  | [] -> []
  | [ x ] -> f x
  | x :: xs -> (
      let hd = f x in
      let tl = list_concat_map ?sep ~f xs in
      match sep with None -> hd @ tl | Some sep -> hd @ (sep :: tl))

module Link = struct
  let rec flatten_path ppf (x : Odoc_document.Url.Path.t) =
    match x.parent with
    | Some p ->
        Fmt.pf ppf "%a-%a-%s" flatten_path p Odoc_document.Url.Path.pp_kind
          x.kind x.name
    | None -> Fmt.pf ppf "%a-%s" Odoc_document.Url.Path.pp_kind x.kind x.name

  let page p = Format.asprintf "%a" flatten_path p

  let label (x : Odoc_document.Url.t) =
    match x.anchor with
    | "" -> page x.page
    | anchor -> Format.asprintf "%a-%s" flatten_path x.page anchor

  let rec is_class_or_module_path (url : Odoc_document.Url.Path.t) =
    match url.kind with
    | `Module | `LeafPage | `Class | `Page -> (
        match url.parent with
        | None -> true
        | Some url -> is_class_or_module_path url)
    | _ -> false

  let should_inline status url =
    match status with
    | `Inline | `Open -> true
    | `Closed -> false
    | `Default -> not @@ is_class_or_module_path url

  let get_dir_and_file url =
    let open Odoc_document in
    let l = Url.Path.to_list url in
    let is_dir = function `Page -> true | _ -> false in
    let dir, file = Url.Path.split ~is_dir l in
    let segment_to_string (_kind, name) = name in
    let dir = List.map segment_to_string dir in
    match (dir, file) with
    | [], [] -> assert false
    | dir, [] ->
        let rev_dir = List.rev dir in
        let file' = List.hd rev_dir in
        let dir' = List.tl rev_dir |> List.rev in
        (dir', file')
    | _, xs -> (dir, String.concat "." (List.map segment_to_string xs))

  let filename url =
    let dir, file = get_dir_and_file url in
    let file = Fpath.(v (String.concat dir_sep (dir @ [ file ]))) in
    Fpath.(add_ext "tex" file)
end

let style = function
  | `Emphasis | `Italic -> Raw.emph
  | `Bold -> Raw.bold
  | `Subscript -> Raw.subscript
  | `Superscript -> Raw.superscript

let gen_hyperref pp r ppf =
  match (r.target, r.text) with
  | "", None -> ()
  | "", Some content -> Raw.inline_code pp ppf content
  | s, None -> Raw.ref ppf s
  | s, Some content ->
      let pp =
        if r.short then Raw.inline_code pp
        else fun ppf x ->
          Fmt.pf ppf "%a[p%a]" (Raw.inline_code pp) x Raw.pageref_star s
      in
      Raw.hyperref s pp ppf content

let label = function
  | None -> []
  | Some x (* {Odoc_document.Url.Anchor.anchor ; page;  _ }*) ->
      [ Label (Link.label x) ]

let level_macro = function
  | 0 -> Raw.section
  | 1 -> Raw.subsection
  | 2 -> Raw.subsubsection
  | 3 | _ -> Raw.subsubsection

let none _ppf () = ()

let list kind pp ppf x =
  let list =
    match kind with Block.Ordered -> Raw.enumerate | Unordered -> Raw.itemize
  in
  let elt ppf = Raw.item pp ppf in
  match x with
  | [] -> (* empty list are not supported *) ()
  | _ -> list (Fmt.list ~sep:(fun ppf () -> Raw.break ppf Aesthetic) elt) ppf x

let escape_entity = function "#45" -> "-" | "gt" -> ">" | s -> s

let filter_map f x =
  List.rev
  @@ List.fold_left
       (fun acc x -> match f x with Some x -> x :: acc | None -> acc)
       [] x

let elt_size (x : elt) =
  match x with
  | Txt _ | Internal_ref _ | External_ref _ | Label _ | Style _ | Inlined_code _
  | Code_fragment _ | Tag _ | Break _ | Ligaturable _ ->
      Small
  | List _ | Section _ | Verbatim _ | Raw _ | Code_block _ | Indented _
  | Description _ ->
      Large
  | Table _ -> Huge

let table = function
  | [] -> []
  | a :: _ as m ->
      let start = List.map (fun _ -> Empty) a in
      let content_size l =
        List.fold_left (fun s x -> max s (elt_size x)) Empty l
      in
      let row mask l = List.map2 (fun x y -> max x @@ content_size y) mask l in
      let mask = List.fold_left row start m in
      let filter_empty = function
        | Empty, _ -> None
        | (Small | Large | Huge), x -> Some x
      in
      let filter_row row = filter_map filter_empty @@ List.combine mask row in
      let row_size = List.fold_left max Empty mask in
      [ Table { row_size; tbl = List.map filter_row m } ]

let txt ~verbatim ~in_source ws =
  if verbatim then [ Txt ws ]
  else
    let escaped = List.map (Raw.Escape.text ~code_hyphenation:in_source) ws in
    match List.filter (( <> ) "") escaped with [] -> [] | l -> [ Txt l ]

let entity ~in_source ~verbatim x =
  if in_source && not verbatim then Ligaturable (escape_entity x)
  else Txt [ escape_entity x ]

(** Tables with too many rows are hard to typeset correctly on
    the same page.
    Splitting tables on multiple pages is unreliable with longtable + hyperref.
    Thus we limit the height of the tables that we render as latex tables.
    This variable is kept separated because we may want to make it tunable
    by the user.
*)
let small_table_height_limit = 10

let rec pp_elt ppf = function
  | Txt words -> Fmt.list Fmt.string ~sep:none ppf words
  | Section { level; label; content } ->
      let with_label ppf (label, content) =
        pp ppf content;
        match label with None -> () | Some label -> Raw.label ppf label
      in
      level_macro level with_label ppf (label, content)
  | Break lvl -> Raw.break ppf lvl
  | Raw s -> Fmt.string ppf s
  | Verbatim s -> Raw.verbatim ppf s
  | Internal_ref r -> hyperref ppf r
  | External_ref (l, x) -> href ppf (l, x)
  | Style (s, x) -> style s pp ppf x
  | Code_block [] -> ()
  | Code_block x -> Raw.code_block pp ppf x
  | Inlined_code x -> Raw.inline_code pp ppf x
  | Code_fragment x -> Raw.code_fragment pp ppf x
  | List { typ; items } -> list typ pp ppf items
  | Description items -> Raw.description pp ppf items
  | Table { row_size = Large | Huge; tbl } -> large_table ppf tbl
  | Table { row_size = Small | Empty; tbl } ->
      if List.length tbl <= small_table_height_limit then
        Raw.small_table pp ppf tbl
      else large_table ppf tbl
  | Label x -> Raw.label ppf x
  | Indented x -> Raw.indent pp ppf x
  | Ligaturable s -> Fmt.string ppf s
  | Tag (s, t) -> tag s ppf t

and pp ppf = function
  | [] -> ()
  | Break _ :: (Table _ :: _ as q) -> pp ppf q
  | (Table _ as t) :: Break _ :: q -> pp ppf (t :: q)
  | Break a :: Break b :: q -> pp ppf (Break (max a b) :: q)
  | Ligaturable "-" :: Ligaturable ">" :: q ->
      Raw.rightarrow ppf;
      pp ppf q
  | a :: q ->
      pp_elt ppf a;
      pp ppf q

and hyperref ppf r = gen_hyperref pp r ppf

and href ppf (l, txt) =
  match txt with
  | Some txt ->
      Raw.href l pp ppf txt;
      Raw.footnote ppf l
  | None -> Raw.url ppf l

and large_table ppf tbl =
  let rec row ppf = function
    | [] -> Raw.break ppf Line
    | [ a ] ->
        pp ppf a;
        Raw.break ppf Line
    | [ a; b ] -> Fmt.pf ppf "%a%a%a" pp a Raw.break Aesthetic (Raw.indent pp) b
    | a :: (_ :: _ as q) ->
        Fmt.pf ppf "%a%a%a" pp a Raw.break Aesthetic (Raw.indent row) q
  in
  let matrix ppf m = List.iter (row ppf) m in
  Raw.indent matrix ppf tbl

and tag s ppf x = Raw.ocamltag s pp ppf x

let raw_markup (t : Raw_markup.t) =
  let target, content = t in
  match Astring.String.Ascii.lowercase target with
  | "latex" | "tex" -> [ Raw content ]
  | _ -> []

let source k (t : Source.t) =
  let rec token (x : Source.token) =
    match x with
    | Elt i -> k i
    | Tag (None, l) -> tokens l
    | Tag (Some s, l) -> [ Tag (s, tokens l) ]
  and tokens t = list_concat_map t ~f:token in
  tokens t

let rec internalref ~verbatim ~in_source (t : InternalLink.t) =
  match t with
  | Resolved (uri, content) ->
      let target = Link.label uri in
      let text = Some (inline ~verbatim ~in_source content) in
      let short = in_source in
      Internal_ref { short; text; target }
  | Unresolved content ->
      let target = "xref-unresolved" in
      let text = Some (inline ~verbatim ~in_source content) in
      let short = in_source in
      Internal_ref { short; target; text }

and inline ~in_source ~verbatim (l : Inline.t) =
  let one (t : Inline.one) =
    match t.desc with
    | Text _s -> assert false
    | Linebreak -> [ Break Line ]
    | Styled (style, c) -> [ Style (style, inline ~verbatim ~in_source c) ]
    | Link (ext, c) ->
        let content = inline ~verbatim:false ~in_source:false c in
        [ External_ref (ext, Some content) ]
    | InternalLink c -> [ internalref ~in_source ~verbatim c ]
    | Source c ->
        [ Inlined_code (source (inline ~verbatim:false ~in_source:true) c) ]
    | Raw_markup r -> raw_markup r
    | Entity s -> [ entity ~in_source ~verbatim s ]
  in

  let take_text (l : Inline.t) =
    Doctree.Take.until l ~classify:(function
      | { Inline.desc = Text code; _ } -> Accum [ code ]
      | { desc = Entity e; _ } -> Accum [ escape_entity e ]
      | _ -> Stop_and_keep)
  in
  (* if in_source then block_code_txt s else if_not_empty (fun x -> Txt x) s *)
  let rec prettify = function
    | { Inline.desc = Inline.Text _; _ } :: _ as l ->
        let words, _, rest = take_text l in
        txt ~in_source ~verbatim words @ prettify rest
    | o :: q -> one o @ prettify q
    | [] -> []
  in
  prettify l

let heading (h : Heading.t) =
  let content = inline ~in_source:false ~verbatim:false h.title in
  [ Section { label = h.label; level = h.level; content }; Break Aesthetic ]

let non_empty_block_code c =
  let s = source (inline ~verbatim:true ~in_source:true) c in
  match s with
  | [] -> []
  | _ :: _ as l -> [ Break Separation; Code_block l; Break Separation ]

let non_empty_code_fragment c =
  let s = source (inline ~verbatim:false ~in_source:true) c in
  match s with [] -> [] | _ :: _ as l -> [ Code_fragment l ]

let rec block ~in_source (l : Block.t) =
  let one (t : Block.one) =
    match t.desc with
    | Inline i -> inline ~verbatim:false ~in_source:false i
    | Paragraph i ->
        inline ~in_source:false ~verbatim:false i
        @ if in_source then [] else [ Break Paragraph ]
    | List (typ, l) ->
        [ List { typ; items = List.map (block ~in_source:false) l } ]
    | Description l ->
        [
          (let item i =
             ( inline ~in_source ~verbatim:false i.Description.key,
               block ~in_source i.Description.definition )
           in
           Description (List.map item l));
        ]
    | Raw_markup r -> raw_markup r
    | Verbatim s -> [ Verbatim s ]
    | Source c -> non_empty_block_code c
  in
  list_concat_map l ~f:one

let rec is_only_text l =
  let is_text : Item.t -> _ = function
    | Heading _ | Text _ -> true
    | Declaration _ -> false
    | Include { content = items; _ } -> is_only_text items.content
  in
  List.for_all is_text l

let rec documentedSrc (t : DocumentedSrc.t) =
  let open DocumentedSrc in
  let rec to_latex t =
    match t with
    | [] -> []
    | Code _ :: _ ->
        let take_code l =
          Doctree.Take.until l ~classify:(function
            | Code code -> Accum code
            | _ -> Stop_and_keep)
        in
        let code, _, rest = take_code t in
        non_empty_code_fragment code @ to_latex rest
    | Alternative (Expansion e) :: rest ->
        (if Link.should_inline e.status e.url then to_latex e.expansion
        else non_empty_code_fragment e.summary)
        @ to_latex rest
    | Subpage subp :: rest ->
        Indented (items subp.content.items) :: to_latex rest
    | (Documented _ | Nested _) :: _ ->
        let take_descr l =
          Doctree.Take.until l ~classify:(function
            | Documented { attrs; anchor; code; doc; markers } ->
                Accum
                  [
                    {
                      DocumentedSrc.attrs;
                      anchor;
                      code = `D code;
                      doc;
                      markers;
                    };
                  ]
            | Nested { attrs; anchor; code; doc; markers } ->
                Accum
                  [
                    {
                      DocumentedSrc.attrs;
                      anchor;
                      code = `N code;
                      doc;
                      markers;
                    };
                  ]
            | _ -> Stop_and_keep)
        in
        let l, _, rest = take_descr t in
        let one dsrc =
          let content =
            match dsrc.code with
            | `D code -> inline ~verbatim:false ~in_source:true code
            | `N n -> to_latex n
          in
          let doc = [ block ~in_source:true dsrc.doc ] in
          (content @ label dsrc.anchor) :: doc
        in
        table (List.map one l) @ to_latex rest
  in
  to_latex t

and items l =
  let rec walk_items ~only_text acc (t : Item.t list) =
    let continue_with rest elts =
      walk_items ~only_text (List.rev_append elts acc) rest
    in
    match t with
    | [] -> List.rev acc
    | Text _ :: _ as t ->
        let text, _, rest =
          Doctree.Take.until t ~classify:(function
            | Item.Text text -> Accum text
            | _ -> Stop_and_keep)
        in
        let content = block ~in_source:false text in
        let elts = content in
        elts |> continue_with rest
    | Heading h :: rest -> heading h |> continue_with rest
    | Include
        { attr = _; anchor; doc; content = { summary; status = _; content } }
      :: rest ->
        let included = items content in
        let docs = block ~in_source:true doc in
        let summary = source (inline ~verbatim:false ~in_source:true) summary in
        let content = included in
        label anchor @ docs @ summary @ content |> continue_with rest
    | Declaration { Item.attr = _; anchor; content; doc } :: rest ->
        let content = label anchor @ documentedSrc content in
        let elts =
          match doc with
          | [] -> content @ [ Break Line ]
          | docs ->
              content
              @ [ Indented (block ~in_source:true docs); Break Separation ]
        in
        continue_with rest elts
  and items l = walk_items ~only_text:(is_only_text l) [] l in
  items l

module Doc = struct
  let link_children ppf children =
    let input_child ppf child =
      Raw.input ppf child.Odoc_document.Renderer.filename
    in
    Fmt.list input_child ppf children

  let make ~with_children url content children =
    let filename = Link.filename url in
    let label = Label (Link.page url) in
    let content =
      match content with
      | [] -> [ label ]
      | (Section _ as s) :: q -> s :: label :: q
      | q -> label :: q
    in
    let children_input ppf =
      if with_children then link_children ppf children else ()
    in
    let content ppf = Fmt.pf ppf "@[<v>%a@,%t@]@." pp content children_input in
    { Odoc_document.Renderer.filename; content; children }
end

module Page = struct
  let on_sub = function `Page _ -> Some 1 | `Include _ -> None

  let rec subpage ~with_children (p : Subpage.t) =
    if Link.should_inline p.status p.content.url then []
    else [ page ~with_children p.content ]

  and subpages ~with_children i =
    List.flatten
    @@ List.map (subpage ~with_children)
    @@ Doctree.Subpages.compute i

  and page ~with_children ({ Page.title = _; header; items = i; url } as p) =
    let i = Doctree.Shift.compute ~on_sub i in
    let subpages = subpages ~with_children p in
    let header = items header in
    let content = items i in
    let page = Doc.make ~with_children url (header @ content) subpages in
    page
end

let render ~with_children page = Page.page ~with_children page

let files_of_url url =
  if Link.is_class_or_module_path url then [ Link.filename url ] else []

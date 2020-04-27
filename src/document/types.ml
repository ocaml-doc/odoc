type style = [
  | `Bold
  | `Italic
  | `Emphasis
  | `Superscript
  | `Subscript
]

module rec Class : sig

  type t = string list

end = Class

and InternalLink : sig

  type resolved = Url.t * Inline.t
  type unresolved = Inline.t
  type t =
    | Resolved of resolved
    | Unresolved of Inline.t

end = InternalLink

and Raw_markup : sig

  type target = Odoc_model.Comment.raw_markup_target
  and t = target * string

end = Raw_markup

and Source : sig

  type t = token list
  and tag = string option
  and token =
    | Elt of Inline.t
    | Tag of tag * t

end = Source

and Inline : sig

  type entity = string
  type href = string

  type t = one list

  and one = {
    attr : Class.t ;
    desc : desc ;
  }

  and desc =
    | Text of string
    | Entity of entity
    | Linebreak
    | Styled of style * t
    | Link of href * t
    | InternalLink of InternalLink.t
    | Source of Source.t
    | Raw_markup of Raw_markup.t

end = Inline

and Heading : sig

  type t = {
    label : string option ;
    level : int ;
    title : Inline.t ;
  }

end = Heading

and Block : sig

  type t = one list

  and one = {
    attr : Class.t ;
    desc : desc ;
  }

  and desc =
    | Inline of Inline.t
    | Paragraph of Inline.t
    | List of list_type * t list
    | Description of (Inline.t * t) list
    | Source of Source.t
    | Verbatim of string
    | Raw_markup of Raw_markup.t

  and list_type =
    | Ordered
    | Unordered

end = Block

and DocumentedSrc : sig

  type 'a documented = {
    attrs : Class.t ;
    anchor : Url.Anchor.t option ;
    code : 'a ;
    doc : Block.t ;
  }

  type t = one list
  and one =
    | Code of Source.t
    | Documented of Inline.t documented
    | Nested of t documented
    | Subpage of Subpage.t

end = DocumentedSrc

and Subpage : sig

  type status = [ `Inline | `Open | `Closed | `Default ]

  type t = {
    summary : Source.t ;
    status : status ;
    content : content ;
  }

  and content =
    | Items of Item.t list
    | Page of Page.t

end = Subpage


and Item : sig

  type 'a item = {
    kind : string option ;
    anchor : Url.Anchor.t option ;
    content : 'a ;
    doc : Block.t ;
  }

  type declaration = DocumentedSrc.t item
  type text = Block.t

  type t =
    | Text of text
    | Heading of Heading.t
    | Declaration of DocumentedSrc.t item
    | Subpage of Subpage.t item

end = Item

and Page : sig

  type t = {
    title : string ;
    header : Item.t list ;
    items : Item.t list ;
    url : Url.Path.t ;
  }

end = Page

let inline ?(attr=[]) desc = Inline.{attr ; desc}
let block ?(attr=[]) desc = Block.{attr ; desc}

module Path = Odoc_model.Paths.Path
module Reference = Odoc_model.Paths.Reference
module Identifier = Odoc_model.Paths.Identifier
module Comment = Odoc_model.Comment

type 'a with_location = 'a Odoc_model.Location_.with_location



type reference_kind = [ `Simple | `With_text ]

type inline_element = [
  | `Space
  | `Word of string
  | `Code_span of string
  | `Raw_markup of Comment.raw_markup_target * string
  | `Styled of Comment.style * (inline_element with_location) list
  | `Reference of
      reference_kind * Reference.t * (inline_element with_location) list
  | `Link of string * (inline_element with_location) list
]

type nestable_block_element = [
  | `Paragraph of (inline_element with_location) list
  | `Code_block of string option * string
  | `Verbatim of string
  | `Modules of Reference.Module.t list
  | `List of
    [ `Unordered | `Ordered ] *
    ((nestable_block_element with_location) list) list
]

type tag = [
  | `Author of string
  | `Deprecated of (nestable_block_element with_location) list
  | `Param of string * (nestable_block_element with_location) list
  | `Raise of string * (nestable_block_element with_location) list
  | `Return of (nestable_block_element with_location) list
  | `See of
      [ `Url | `File | `Document ] *
      string *
      (nestable_block_element with_location) list
  | `Since of string
  | `Before of string * (nestable_block_element with_location) list
  | `Version of string
  | `Canonical of Path.Module.t * Reference.Module.t
  | `Inline
  | `Open
  | `Closed
]

type block_element = [
  | nestable_block_element
  | `Heading of int * string option * (inline_element with_location) list
  | `Tag of tag
]

type docs = (block_element with_location) list



type sections_allowed = [ `All | `No_titles | `None ]

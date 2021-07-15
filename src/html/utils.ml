(* Shared utility functions *)

(* = Option.fold *)
let fold_option ~none ~some = function Some x -> some x | None -> none

let rec list_concat_map ?sep ~f = function
  | [] -> []
  | [ x ] -> f x
  | x :: xs -> (
      let hd = f x in
      let tl = list_concat_map ?sep ~f xs in
      match sep with None -> hd @ tl | Some sep -> hd @ (sep :: tl))

let optional_elt f ?a = function [] -> [] | l -> [ f ?a l ]

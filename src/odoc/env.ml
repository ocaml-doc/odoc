(*
 * Copyright (c) 2014 Leo White <leo@lpw25.net>
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



type t = {
  expander : Xref.expander ;
  resolver : Xref.resolver ;
}

module Accessible_paths = struct
  type t = {
    root_map : Fs.File.t Model.Root.Hash_table.t;
    file_map : (string, Model.Root.t) Hashtbl.t;
    directories : Fs.Directory.t list;
  }

  let create ~directories =
    { root_map = Model.Root.Hash_table.create 42
    ; file_map = Hashtbl.create 42
    ; directories }

  let find_root ?digest t ~filename =
    let root = Hashtbl.find t.file_map filename in
    match digest with
    | Some d when Digest.compare d root.digest <> 0 ->
      Printf.eprintf "WARNING: digest of %s doesn't match the one expected for file %s\n%!"
          (Model.Root.Hash_table.find t.root_map root |> Fs.File.to_string) filename;
      root
    | _ ->
      root

  let file_of_root t root =
    Model.Root.Hash_table.find t.root_map root

  let scan_directories t =
    let scan_directory d =
      let odoc_files =
        Fs.Directory.ls d
        |> List.filter (Fs.File.has_ext ".odoc")
      in
      List.iter
        (fun path ->
          let root = Root.read path in
          let filename =
            match root.file with
            | Page page_name -> "page-" ^ page_name
            | Compilation_unit { name; _ } -> name
          in
          if Hashtbl.mem t.file_map filename then begin
            let err = Model.Error.filename_only
              "Duplicate root name found in include path"
              (Fs.File.to_string path) in
            Model.Error.raise_exception err
          end;
          Hashtbl.add t.file_map filename root;
          Model.Root.Hash_table.add t.root_map root path)
        odoc_files
    in
    List.iter scan_directory t.directories
end

let rec lookup_unit ~important_digests ap target_name =
  let find_root ~digest =
    match Accessible_paths.find_root ap ~filename:target_name ?digest with
    | exception Not_found -> Not_found
    | root ->
      match root.file with
      | Compilation_unit {hidden; _} -> Xref.Found {root; hidden}
      | Page _ -> assert false
  in
  function
  | [] when important_digests -> Xref.Not_found
  | [] -> find_root ~digest:None
  | import :: imports ->
    match import with
    | Model.Lang.Compilation_unit.Import.Unresolved (name, digest)
      when name = target_name ->
      begin match digest with
      | None when important_digests -> Forward_reference
      | _ -> find_root ~digest
      end
    | Model.Lang.Compilation_unit.Import.Resolved root
      when Model.Root.Odoc_file.name root.file =
          target_name -> begin
        match root.file with
        | Compilation_unit {hidden; _} -> Found {root; hidden}
        | Page _ -> assert false
      end
    | _ -> lookup_unit ~important_digests ap target_name imports

let lookup_page ap target_name =
  match Accessible_paths.find_root ap ~filename:("page-" ^ target_name) with
  | root -> Some root
  | exception Not_found -> None

let fetch_page ap root =
  match Accessible_paths.file_of_root ap root with
  | path -> Page.load path
  | exception Not_found ->
    Printf.eprintf "No unit for root: %s\n%!" (Model.Root.to_string root);
    exit 2

let fetch_unit ap root =
  match Accessible_paths.file_of_root ap root with
  | path -> Compilation_unit.load path
  | exception Not_found ->
    Printf.eprintf "No unit for root: %s\n%!" (Model.Root.to_string root);
    exit 2

type builder = [ `Unit of Compilation_unit.t | `Page of Page.t ] -> t

let create ?(important_digests=true) ~directories : builder =
  let ap = Accessible_paths.create ~directories in
  Accessible_paths.scan_directories ap;
  fun unit_or_page ->
    let lookup_unit target_name : Xref.lookup_result =
      match unit_or_page with
      | `Page _ -> lookup_unit ~important_digests:false ap target_name []
      | `Unit unit ->
        let lookup_result =
          lookup_unit
            ~important_digests
            ap
            target_name
            unit.Model.Lang.Compilation_unit.imports
        in
        match lookup_result with
        | Not_found -> begin
            let root = Compilation_unit.root unit in
            match root.file with
            | Page _ -> assert false
            | Compilation_unit {name;hidden} when target_name = name ->
              Found { root; hidden }
            | Compilation_unit _ -> Not_found
          end
        | x -> x
    in
    let fetch_unit root : Model.Lang.Compilation_unit.t =
      match unit_or_page with
      | `Page _ -> fetch_unit ap root
      | `Unit unit ->
        let current_root = Compilation_unit.root unit in
        if Model.Root.equal root current_root then
          unit
        else
          fetch_unit ap root
    in
    let lookup_page target_name = lookup_page ap target_name in
    let fetch_page root : Model.Lang.Page.t =
      match unit_or_page with
      | `Unit _ -> fetch_page ap root
      | `Page page ->
        let current_root = Page.root page in
        if Model.Root.equal root current_root then
          page
        else
          fetch_page ap root
    in
    let resolver =
      Xref.build_resolver lookup_unit fetch_unit lookup_page fetch_page
    in
    let expander =
      (* CR trefis: what is the ~root param good for? *)
      let fetch ~root:_ root = fetch_unit root in
      let lookup _ s = lookup_unit s in
      Xref.build_expander (lookup ()) fetch
    in
    { expander; resolver }

let build builder unit =
  builder unit

let resolver t = t.resolver
let expander t = t.expander

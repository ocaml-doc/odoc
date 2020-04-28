
open StdLabels
open Or_error
open Odoc_document

let document_of_page ~syntax v =
  match syntax with
  | Renderer.Reason -> Odoc_document.Reason.page v
  | Renderer.OCaml -> Odoc_document.ML.page v

let document_of_compilation_unit ~syntax v =
  match syntax with
  | Renderer.Reason -> Odoc_document.Reason.compilation_unit v
  | Renderer.OCaml -> Odoc_document.ML.compilation_unit v

let mk_page ~syntax v =
  Odoc_manpage.Generator.render @@
  document_of_page ~syntax v

let mk_compilation_unit ~syntax v =
  Odoc_manpage.Generator.render @@
  document_of_compilation_unit ~syntax v

let from_odoc ~env ?(syntax=Renderer.OCaml) ~output:root_dir input =
  Root.read input >>= fun root ->
  match root.file with
  | Page page_name ->
    Page.load input >>= fun page ->
    let resolve_env = Env.build env (`Page page) in
    Odoc_xref.resolve_page (Env.resolver resolve_env) page >>= fun odoctree ->
    let pkg_name = root.package in
    let pages = mk_page ~syntax odoctree in
    let pkg_dir = Fs.Directory.reach_from ~dir:root_dir pkg_name in
    Fs.Directory.mkdir_p pkg_dir;
    Renderer.traverse pages ~f:(fun ~parents _pkg_name content ->
      assert (parents = []);
      let oc =
        let f = Fs.File.create ~directory:pkg_dir ~name:(page_name ^ ".3o") in
        open_out (Fs.File.to_string f)
      in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%t@?" content;
      close_out oc
    );
    Ok ()
  | Compilation_unit {hidden = _; _} ->
    (* If hidden, we should not generate HTML. See
         https://github.com/ocaml/odoc/issues/99. *)
    Compilation_unit.load input >>= fun unit ->
    let unit = Odoc_xref.Lookup.lookup unit in
    begin
      (* See comment in compile for explanation regarding the env duplication. *)
      let resolve_env = Env.build env (`Unit unit) in
      Odoc_xref.resolve (Env.resolver resolve_env) unit >>= fun resolved ->
      let expand_env = Env.build env (`Unit resolved) in
      Odoc_xref.expand (Env.expander expand_env) resolved >>= fun expanded ->
      Odoc_xref.Lookup.lookup expanded
      |> Odoc_xref.resolve (Env.resolver expand_env) (* Yes, again. *)
    end >>= fun odoctree ->
    let pkg_dir = Fs.Directory.reach_from ~dir:root_dir root.package in
    Fs.Directory.mkdir_p pkg_dir;
    let pages = mk_compilation_unit ~syntax odoctree in
    Renderer.traverse pages ~f:(fun ~parents name content ->
      let page_name = String.concat ~sep:"." (parents @ [name]) in
      let oc =
        let f = Fs.File.create ~directory:pkg_dir ~name:(page_name ^ ".3o") in
        open_out (Fs.File.to_string f)
      in
      let fmt = Format.formatter_of_out_channel oc in
      Format.fprintf fmt "%t@?" content;
      close_out oc
    );
    Ok ()

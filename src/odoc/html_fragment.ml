
let from_odoc ~env ~output:root_dir input =
  let root = Root.read input in
  match root.file with
  | Page page_name ->

    let page = Page.load input in
    let odoctree =
      let resolve_env = Env.build env (`Page page) in
      Xref.resolve_page (Env.resolver resolve_env) page
    in
    let pkg_name = root.package in
    let pkg_dir = Fs.Directory.reach_from ~dir:root_dir pkg_name in
    Fs.Directory.mkdir_p pkg_dir;

    (* NOTE: URI resolution for fragments.
     *
     * Unfortunately URI resolution uses a global stack value located at
     * [Html.Html_tree.path]. Not [enter]ing with an appropriate packade and
     * page results in broken references. *)
    Html.Html_tree.enter pkg_name;
    Html.Html_tree.enter ~kind:`Page page_name;

    let content = Html.Documentation.to_html odoctree.content in
    let oc =
      let f = Fs.File.create ~directory:pkg_dir ~name:(page_name ^ ".html") in
      open_out (Fs.File.to_string f)
    in
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a@." (Format.pp_print_list (Tyxml.Html.pp_elt ())) content;
    close_out oc
  | Compilation_unit _ ->
    Printf.eprintf "ERROR: HTML fragments for compilation units are not \
                    currently supported.\n%!";
    exit 1


let from_mld ~root_uri ~env ~output input =
  (* Internal names, they don't have effect on the output. *)
  let page_name = "__fragment_page__" in
  let package = "__fragment_package__" in
  let digest = Digest.file (Fs.File.to_string input) in
  let root =
    let file = Model.Root.Odoc_file.create_page page_name in
    {Model.Root.package; file; digest}
  in
  let name = Model.Paths.Identifier.Page (root, page_name) in
  let location =
    let pos =
      Lexing.{
        pos_fname = Fs.File.to_string input;
        pos_lnum = 0;
        pos_cnum = 0;
        pos_bol = 0
      }
    in
    Location.{ loc_start = pos; loc_end = pos; loc_ghost = true }
  in
  match Fs.File.read input with
  | Error (`Msg s) ->
    Printf.eprintf "ERROR: %s\n%!" s;
    exit 1
  | Ok str ->
    let content =
      match Loader.read_string name location str with
      | Error e -> failwith (Model.Error.to_string e)
      | Ok (`Docs content) -> content
      | Ok `Stop -> [] (* TODO: Error? *)
    in
    (* This is a mess. *)
    let page = Model.Lang.Page.{ name; content; digest } in
    let page = Xref.Lookup.lookup_page page in
    let env = Env.build env (`Page page) in
    let resolved = Xref.resolve_page (Env.resolver env) page in

    let content = Html.Documentation.to_html ~root_uri resolved.content in
    let oc = open_out (Fs.File.to_string output) in
    let fmt = Format.formatter_of_out_channel oc in
    Format.fprintf fmt "%a@." (Format.pp_print_list (Tyxml.Html.pp_elt ())) content;
    close_out oc


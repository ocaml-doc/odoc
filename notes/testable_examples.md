# [DRAFT] Testable examples

Library authors are encouraged to include examples and short snippets of code
in documentation to demonstrate how to effectively use their library. Such code
snippets are included in docstrings as code blocks and therefore cannot be
executed and tested in the same way regular source files are. This leads to
code duplication for library authors who want to make sure their examples can
be correctly executed, and to out-of-date examples when they forget to update
them, as the library’s API changes.

To address this problem odoc implements the ability to extract code blocks from
documented interfaces and documentation pages (`mli` and `mld` files
respectively) into source code files. With this, build systems can implement
user-friendly workflows for execution, testing and even promotion of corrected
examples. In addition, the extracted examples can be installed as documentation
assets and thus avoid the need to duplicate them as separate files for
distribution.

## Named code blocks

In the new version of odoc, code blocks can be annotated with a file name. This
file name is used by odoc to group related code blocks for extraction, and also
to correctly annotate the markup for syntax highlighting.

The following table demonstrates the two variants of code blocks: the
traditionally supported *anonymous* code blocks and the new *named* code
blocks.

| **Anonymous code block** | **Named code block**                |
| ------------------------ | ----------------------------------- |
| `"{[" <content> "]}"`    | `"{" <filename> "[" <content> "]}"` |


### Code extraction

Both named and anonymous code blocks can be extracted by odoc via the
command-line interface. Code blocks with the same file name in a given
documentation file will be concatenated and written into a file with that name.
Optionally, a different output file name for a given group can be provided.
Users are always required to provide an output file name for extraction of
anonymous code blocks.

#### Hidden code blocks

In some cases it might be useful to execute code blocks that contain
configuration or setup logic. If those code blocks are not essential for
documentation, they can be hidden by placing them between the special stop
comment (see [The Stop special
comment](https://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#sec355) in
the OCaml manual). Note that hidden code blocks will still be executed, to
prevent the execution of a code block `execute=false` can be added to the
annotation. ([TODO] Not currently covered by the proposal.)

To facilitate debugging and allow the tooling to implement expect-style
promotions, popularized by cram and dune, the extracted examples can be
optionally annotated with line numbers and the source file name (see [Line
number directives](https://caml.inria.fr/pub/docs/manual-ocaml/lex.html#sec86)
in the OCaml manual).

**Note**: unrelated code blocks do not need to have a unique file name, it is
recommended to group them by using a file name like `examples.ml` or similar.

The described functionality will also be exposed as a library to facilitate
integration with build systems and test promotion tooling.

### Syntax highlighting

The file names used to annotated code blocks are also used by odoc to decide
what language should be used for syntax highlighting in the generated HTML. The
language is decided based on the file name’s extension.

**Note:** code blocks without a file name will be assumed to be in OCaml for
syntax highlighting purposes. To completely disable syntax highlighting,
verbatim blocks should be used (`{v ... v}`).


## Command-line interface

The following simplified manual page defines the command-line interface for
code extraction:


    odoc-extract-code(1)              Odoc Manual             odoc-extract-code(1)


    NAME
           odoc-extract-code - Extract code blocks included in documentation files.

    SYNOPSIS
           odoc extract-code [OPTION]... FILE

    OPTIONS
           --name=NAME
               The name of the code block to extract.

           --anonymous
               Extract code blocks without name. Cannot be used with the `--name'
               option.

           --all
               Extract all code blocks from the input file (including anonymous
               code blocks). The output PATH will be treated as a dirctory when
               invoking this option.

           -o PATH, --output=PATH
               Output path. If omitted and the `--name=NAME' option is
               provided, NAME will be used as an outupt file path. Required for
               extraction of anonymous code blocks. Must be a directory path
               when `--all' code blocks are extracted.

           --with-line-numbers
               Include line number and file name of the extracted code blocks.

           FILE (required)
               Input cmti, cmt, cmi, mli or mld file.


    Odoc 11VERSION11                                          odoc-extract-code(1)

Similar to other odoc commands that produce output, a complementary `odoc
extract-code-targets` command will be added to list all code block names
present in the input file.


## Dune integration

Here is an excerpt from a documented interface file that demonstrates named code blocks.

**Io_utils.mli**

```ocaml
val read_file : string -> string
(** [read_file path] is the content of the file located at [path] read into a string.

    {4 Examples}
    Given a text file with the content:

    {letters.txt[abcdef]}

    The following example will print the number of letters in the file:

    {count_letters.ml[
    # let letters = read_file "assets/letters.txt";;
    val letters : string = "abcdef"
    # String.length letters;;
    - : int = 6
    ]} *)
```

The user wants to test the two code blocks in this example and all the
anonymous code blocks. To achieve this, the library stanza can be instructed to
extract and execute the code blocks from the documentation:


```dune
(library
  (public_name io-utils)
  (name Io_utils)
  (libraries base bos)
  (documentation
    (extract_code
      (letters.txt as assets/letters.txt)
      count_letters.ml
      (:anonymous as examples.ml))
    (execute_code examples.ml count_letters.ml)))
```

Here is a detailed description of these options:

- `(extract_code <filenames>)` where `<filenames>` field follows the [Ordered
  set language](http://#). This is a set of code block names found in `mli`
  files of the library that should be extracted into files. Where `:standard`
  refers to all annotated code blocks found in the library. Optionally the name
  of the extracted file can be changed by using the following form:
  `(<code_block> as <filename>)`, for example, `(letters.txt as
  assets/letters.txt)`. Untitled code blocks can be extracted by providing a
  file name to a special `:anonymous` name: `(:anonymous as <filename>)`.
- `(execute_code <filenames>)` where `<filenames>` field follows the [Ordered
  set language](http://#). This is a set of extracted code files that will be
  compiled and executed during documentation generation. Currently only the
  files with the `ml` and `re` extensions are supported.

With these two options it is possible to precisely control what gets extracted
and what gets executed. Furthermore the extracted files can also be installed
by dune.

The top-level `documentation` stanza for `mld` files can also be extended to
support these options.

----------

## Requirements

- In the common case users should be able to execute all the code blocks in a
  single environment. This behaviour should be the default.
- It should be possible to assign different environments to code blocks. Code
  blocks with the same environment will be executed in the same toplevel
  session.
- Allow the errors to be highlighted in examples in the original file. Might require
  https://github.com/ocaml/odoc/issues/147
- Produce `.corrected` files to allow dune (or other build systems) to support
  promotion of corrected files.
- The code block name should contain the language information for syntax
  highlighting.


## Questions

- Should odoc require code block annotations to be filenames with extension?
  The extension could be used to identify the language and correctly do code
  highlighting. On the other hand the code blocks could be annotated only with
  the language name (*i.e.* `{ocaml[...]}`), but this would limit the scope of
  the feature. In particular this would make it impossible to:
  1. Write examples in code blocks that read input from files extracted from
     other code blocks;
  2. Explicitly select the examples that should be compiled (ignoring others);
  3. Install multiple extracted examples without compiling them.
- Should “execution” of `mli` files be supported too? Might be useful for basic
  type-checking of the signature items.
- Should code blocks with the same name from different `mli` and `mld` files
  (in the same library) be extracted into the same file? This might be
  problematic with anonymous code blocks. On the other hand the
  `--with-line-numbers` can be used to keep track of the name of the original
  file.


## Alternatives

- Examples could be loaded from existing files into odoc's output. This is more
  limited than the current proposal because it does not allow to interleve
  comments and code. But, on the other hand, would not need any additional
  build tooling as the examples can be directly compiled/tested.
- Introduce something like `mlt` files where code is mixed with comments. These
  files could be converted into `mld` files for HTML rendering. See
  https://github.com/janestreet/toplevel_expect_test


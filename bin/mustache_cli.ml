module Mustache = struct
  include Mustache
  include With_locations
end

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (Bytes.to_string s)

let locate_template search_path relative_filename =
  search_path
  |> List.map (fun path -> Filename.concat path relative_filename)
  |> List.find_opt Sys.file_exists

let load_template template_filename =
  let template_data = load_file template_filename in
  let lexbuf = Lexing.from_string template_data in
  let () =
    let open Lexing in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = template_filename };
  in
  try Mustache.parse_lx lexbuf
  with Mustache.Template_parse_error err ->
    Format.eprintf "Template parse error:@\n%a@."
      Mustache.pp_template_parse_error err;
    exit 3

let load_json json_filename =
  Ezjsonm.from_string (load_file json_filename)

let run search_path json_filename template_filename =
  let env = load_json json_filename in
  let tmpl = load_template template_filename in
  let partials name =
    let file = Printf.sprintf "%s.mustache" name in
    let path = locate_template search_path file in
    Option.map load_template path
  in
  try
    let output = Mustache.render ~partials tmpl env in
    print_string output;
    flush stdout
  with Mustache.Render_error err ->
    Format.eprintf "Template render error:@\n%a@."
      Mustache.pp_render_error err;
    exit 2

let run_command =
  let open Cmdliner in
  let doc = "renders Mustache template from JSON data files" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) is a command-line tool coming with the $(i,ocaml-mustache) library,
        an OCaml implementation of the Mustache template format.
        $(tname) takes a data file and a template file as command-line parameters;
        it renders the populated template on standard output.";

    `P "Mustache is a simple and popular template format,
        with library implementations in many programming languages.
        It is named from its {{..}} delimiters.";

    `I ("Mustache website",
        "https://mustache.github.io/");
    `I ("Mustache templates documentation",
        "https://mustache.github.io/mustache.5.html");
    `I ("ocaml-mustache website:",
        "https://github.com/rgrinberg/ocaml-mustache");

    `P "The $(i,ocaml-mustache) implementation is tested against
        the Mustache specification testsuite.
        All features are supported, except for lambdas and setting delimiter tags.";
    `S Manpage.s_options;
    `S "PARTIALS";
    `P "The $(i,ocaml-mustache) library gives programmatic control over the meaning of partials {{>foo}}.
        For the $(tname) tool, partials are interpreted as template file inclusion: '{{>foo}}' includes
        the template file 'foo.mustache'.";
    `P "Included files are resolved in a search path, which contains the current working directory
        (unless the $(b,--no-working-dir) option is used)
        and include directories passed through $(b,-I DIR) options.";
    `P "If a file exists in several directories of the search path, the directory included first
        (leftmost $(b,-I) option) has precedence, and the current working directory has precedence
        over include directories.";
    `S Manpage.s_examples;
    `Pre
      {|
\$ cat data.json
{ "name": "OCaml",
  "qualities": [{"name": "simple"}, {"name": "fun"}] }

\$ cat hello.mustache
Hello {{name}}!
Mustache is:
{{#qualities}}
- {{name}}
{{/qualities}}

\$ $(tname) data.json hello.mustache
Hello OCaml!
Mustache is:
- simple
- fun


\$ cat page.mustache
<html>
  <body>
    {{>hello}}
  </body>
</html>

\$ $(tname) data.json page.mustache
<html>
  <body>
    Hello OCaml!
    Mustache is:
    - simple
    - fun
  </body>
</html>

|};
    `S Manpage.s_bugs;
    `P "Report bugs on https://github.com/rgrinberg/ocaml-mustache/issues";
  ]
  in
  let json_file =
    let doc = "data file in JSON format" in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"DATA.json" ~doc)
  in
  let template_file =
    let doc = "mustache template" in
    Arg.(required & pos 1 (some file) None & info [] ~docv:"TEMPLATE.mustache" ~doc)
  in
  let search_path =
    let includes =
      let doc = "Adds the directory $(docv) to the search path for partials." in
      Arg.(value & opt_all dir [] & info ["I"] ~docv:"DIR" ~doc)
    in
    let no_working_dir =
      let doc = "Disable the implicit inclusion of the working directory
                 in the search path for partials." in
      Arg.(value & flag & info ["no-working-dir"] ~doc)
    in
    let search_path includes no_working_dir =
      if no_working_dir then includes
      else Filename.current_dir_name :: includes
    in
    Term.(const search_path $ includes $ no_working_dir)
  in
  Term.(const run $ search_path $ json_file $ template_file),
  Term.info "mustache" ~doc ~man

let () =
  let open Cmdliner in
  Term.exit @@ Term.eval run_command

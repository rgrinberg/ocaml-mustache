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

let run json_filename template_filename =
  let env = load_json json_filename in
  let tmpl = load_template template_filename in
  let partials name =
    let path = Printf.sprintf "%s.mustache" name in
    if not (Sys.file_exists path) then None
    else Some (load_template path) in
  try Mustache.render ~partials tmpl env |> print_endline
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
  Term.(const run $ json_file $ template_file),
  Term.info "mustache" ~doc ~man


let () =
  let open Cmdliner in
  Term.exit @@ Term.eval run_command

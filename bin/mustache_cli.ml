let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (Bytes.to_string s)

let locate_template search_path filename =
  if Filename.is_relative filename then
    search_path
    |> List.map (fun path -> Filename.concat path filename)
    |> List.find_opt Sys.file_exists
  else if Sys.file_exists filename then
    Some filename
  else None

let load_template template_filename =
  let template_data = load_file template_filename in
  let lexbuf = Lexing.from_string template_data in
  let () =
    let open Lexing in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = template_filename };
  in
  try Mustache.parse_lx lexbuf
  with Mustache.Parse_error err ->
    Format.eprintf "%a@."
      Mustache.pp_template_parse_error err;
    exit 3

module Json = struct
  type error =
    | Jsonm of Jsonm.error
    | Incomplete_input
    | Parse_error of string

  type jsonm_loc = (int * int) * (int * int)

  exception Error of jsonm_loc * error

  let json_of_decoder (d : Jsonm.decoder) =
    (* inspired from the Jsonm documentation example *)
    let raise_err err = raise (Error (Jsonm.decoded_range d, err)) in
    let dec d = match Jsonm.decode d with
    | `Lexeme l -> l
    | `Error e -> raise_err (Jsonm e)
    | `End | `Await -> raise_err Incomplete_input
    in
    let wrap k = (k : Mustache.Json.value -> 'r :> Mustache.Json.t -> 'r) in
    let rec value v (k : Mustache.Json.value -> _) d = match v with
    | `Os -> obj [] (wrap k) d  | `As -> arr [] (wrap k) d
    | `Null | `Bool _ | `String _ | `Float _ as v -> k v d
    | _ -> raise_err (Parse_error "value fields expected")
    and arr vs (k : Mustache.Json.t -> _) d = match dec d with
    | `Ae -> k (`A (List.rev vs)) d
    | v -> value v (fun v -> arr (v :: vs) k) d
    and obj ms (k : Mustache.Json.t -> _) d = match dec d with
    | `Oe -> k (`O (List.rev ms)) d
    | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
    | _ -> raise_err (Parse_error "object fields expected")
    in
    let t v (k : Mustache.Json.t -> _) d = match v with
    | `Os -> obj [] k d  | `As -> arr [] k d
    | _ -> raise_err (Parse_error "Json.t expected")
    in
    t (dec d) (fun v _ -> v) d

  let pp_error ppf (fname, jsonm_loc, error) =
    let ((start_line, start_col), (end_line, end_col)) = jsonm_loc in
    let lexpos line col : Lexing.position = {
      pos_fname = fname;
      pos_lnum = line;
      pos_bol = 0;
      pos_cnum = col;
    } in
    let loc : Mustache.loc = {
      loc_start = lexpos start_line start_col;
      loc_end = lexpos end_line end_col;
    } in
    Format.fprintf ppf "%a:@ %t"
      Mustache.pp_loc loc
      (fun ppf -> match error with
         | Jsonm e -> Jsonm.pp_error ppf e
         | Incomplete_input -> Format.fprintf ppf "Incomplete input."
         | Parse_error s -> Format.fprintf ppf "Parse error: %s." s
      )
end

let load_json json_filename =
  let input = load_file json_filename in
  let decoder = Jsonm.decoder (`String input) in
  try Json.json_of_decoder decoder with
  | Json.Error (jsonm_loc, error) ->
    Format.eprintf "%a@."
      Json.pp_error (json_filename, jsonm_loc, error);
    exit 4

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
    Format.eprintf "%a@."
      Mustache.pp_render_error err;
    exit 2

let manpage = Cmdliner.[
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
  `I ("$(i,ocaml-mustache) website:",
      "https://github.com/rgrinberg/ocaml-mustache");

  `S Manpage.s_options;
  (* The content of this section is filled by Cmdliner; it is used here
     to enforce the placement of the non-standard sections below. *)

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

  `S "TEMPLATE INHERITANCE / PARTIALS WITH PARAMETERS";

  `P "$(i,ocaml-mustache) supports a common extension to the original Mustache specification,
      called 'template inheritance' or 'parent partials', or here 'partials with parameters'.
      In addition to usual partials '{{>foo}}', which include a partial template, one can use
      the syntax '{{<bar}} {{\\$param1}}...{{/param1}} {{\\$param2}}...{{/param2}} {{/bar}}' to
      pass parameters to the included partial template. Inside this included template, parameters
      are used as '{{\\$param}}...{{/param}}', with the inner content being used by default
      if this parameter was not specified by the caller.";

  `P "This is typically used to define page layouts that are wrapped 'around' the current template.
      See our EXAMPLES.";

  `S Manpage.s_examples;
  `Pre {|
## Simple usage.

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


## Including a subpage; see $(b,PARTIALS).

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


## Including a layount around a page; see $(b,PARTIALS WITH PARAMETERS).

\$ cat new-post.json
{
  "title": "New Post",
  "authors": "Foo and Bar",
  "date": "today",
  "content": "Shiny new content."
}

\$ cat post.mustache
{{<post-layout}}
  {{\$page-title}}Post: {{title}}{{/page-title}}
  {{\$content}}
    <h1>{{title}}</h1>
    <p>{{content}}</p>
  {{/content}}
{{/post-layout}}

\$ cat post-layout.mustache
<html>
  <head>
    <title>{{\$page-title}}Default Title{{/page-title}}</title>
  </head>
  <body>
    {{\$content}}{{/content}}
  </body>
</html>

\$ $(tname) new-post.json post.mustache
<html>
  <head>
    <title>Post: New Post</title>
  </head>
  <body>
    <h1>New Post</h1>
    <p>Shiny new content.</p>
  </body>
</html>|};

  `S "CONFORMING TO";

  `P "The $(i,ocaml-mustache) implementation is tested against
      the Mustache specification testsuite.
      All features are supported, except for lambdas and setting delimiter tags.";

  `I ("Mustache specification testsuite",
      "https://github.com/mustache/spec");

  `I ("Semi-official specification of PARTIALS WITH PARAMETERS",
      "https://github.com/mustache/spec/pull/75");

  `S "REPORTING BUGS";
  `P "Report bugs on https://github.com/rgrinberg/ocaml-mustache/issues";
]

let run_command =
  let open Cmdliner in
  let doc = "renders Mustache template from JSON data files" in
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
  Term.info "mustache" ~doc ~man:manpage

let () =
  let open Cmdliner in
  Term.exit @@ Term.eval run_command

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

let usage () =
  print_endline "Usage: mustache-cli json_filename template_filename"

let () =
  match Sys.argv with
  | [| _ ; json_filename ; template_filename |]
    -> run json_filename template_filename
  | _ -> usage ()

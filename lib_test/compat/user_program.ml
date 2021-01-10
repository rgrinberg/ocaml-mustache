module Mustache = Mustache_v200

(* A simple user program that could have been written against ocaml-mustache
   2.0.0; we check that it keeps working as expected. *)

let json =
  `O
    [ ("name", `String "OCaml")
    ; ( "qualities"
      , `A
          [ `O [ ("name", `String "awesome") ]
          ; `O [ ("name", `String "simple") ]
          ; `O [ ("name", `String "fun") ]
          ] )
    ]

let section heading =
  print_newline ();
  print_string "# ";
  print_endline heading

let subsection heading =
  print_newline ();
  print_string "## ";
  print_endline heading

let test tmpl =
  subsection "parsed";
  print_endline "---";
  print_endline (Mustache.to_string tmpl);
  print_endline "---";

  subsection "rendered";
  print_endline "---";
  print_endline (Mustache.render tmpl json);
  print_endline "---";
  ()

let () = section "Parsed template"

let parsed_template =
  Mustache.of_string
    "Hello {{name}}\nMustache is:\n{{#qualities}}\n* {{name}}\n{{/qualities}}\n"

let () = test parsed_template

let () = section "Programmed template"

let programmed_template =
  let open Mustache in
  concat
    [ raw "Hello "
    ; escaped "name"
    ; raw "\n"
    ; raw "Mustache is:"
    ; raw "\n"
    ; section "qualities" @@ concat [ raw "* "; escaped "name"; raw "\n" ]
    ]

let () = test programmed_template

let () = section "Output comparison"

let () =
  let parsed_output = Mustache.render parsed_template json in
  let programmed_output = Mustache.render programmed_template json in
  if String.equal parsed_output programmed_output then
    print_endline "Outputs match as expected."
  else
    print_endline "Outputs DO NOT match, this is suspcious."

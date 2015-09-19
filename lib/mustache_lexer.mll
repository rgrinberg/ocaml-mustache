{
  open Lexing
  open Mustache_parser
  open Mustache_types
}

let space = [' ' '\t' '\n']*
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']+

rule ident = parse
  | space '.' space { "." }
  | space (id as x) space { x }
  | _ { raise (Invalid_template "Invalid section") }

and mustache = parse
  | "{{{"        { UNESCAPE_START (ident lexbuf) }
  | "{{&"        { UNESCAPE_START_AMPERSAND (ident lexbuf) }
  | "{{#"        { SECTION_START (ident lexbuf) }
  | "{{^"        { SECTION_INVERT_START (ident lexbuf) }
  | "{{/"        { SECTION_END (ident lexbuf) }
  | "{{>"        { PARTIAL_START (ident lexbuf) }
  | "{{"         { ESCAPE_START (ident lexbuf) }
  | space "}}"   { END }
  | [^ '{' '}']* { RAW (lexeme lexbuf) }
  | ['{' '}']    { RAW (lexeme lexbuf) }
  | eof          { EOF }

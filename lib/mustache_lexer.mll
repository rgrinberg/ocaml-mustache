(*{{{ The MIT License (MIT)

   Copyright (c) 2015 Rudi Grinberg

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to
   deal in the Software without restriction, including without limitation the
   rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
   sell copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
   IN THE SOFTWARE.  }}}*)
{
  open Lexing
  open Mustache_parser
  open Mustache_types
}

let space = [' ' '\t' '\n']*
let id = ['a'-'z' 'A'-'Z' '_' '/'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '/']+

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
  | "{{!"        { COMMENT_START }
  | "{{"         { ESCAPE_START (ident lexbuf) }
  | "}}}"        { UNESCAPE_END }
  | "}}"         { END }
  | [^ '{' '}']* { RAW (lexeme lexbuf) }
  | ['{' '}']    { RAW (lexeme lexbuf) }
  | eof          { EOF }

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

  let with_space space f lexbuf =
    let start_p = lexbuf.Lexing.lex_start_p in
    let () = space lexbuf in
    let x = f lexbuf in
    space lexbuf;
    lexbuf.Lexing.lex_start_p <- start_p;
    x

  let split_on_char sep s =
    let open String in
    let r = ref [] in
    let j = ref (length s) in
    for i = length s - 1 downto 0 do
      if unsafe_get s i = sep then begin
        r := sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      end
    done;
    sub s 0 !j :: !r

  let split_ident ident =
    if ident = "." then []
    else split_on_char '.' ident
}

let blank = [' ' '\t']*
let newline = ('\n' | "\r\n")
let raw = [^ '{' '}' '\n']*
let id = ['a'-'z' 'A'-'Z' '_' '/'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '/']*
let ident = ('.' | id ('.' id)*)

rule space = parse
  | blank newline { new_line lexbuf; space lexbuf }
  | blank { () }

and id = parse
  | id { lexeme lexbuf }

and ident = parse
  | ident { lexeme lexbuf }

and mustache = parse
  | "{{{"        { UNESCAPE_START (with_space space ident lexbuf |> split_ident) }
  | "{{&"        { UNESCAPE_START_AMPERSAND (with_space space ident lexbuf |> split_ident) }
  | "{{#"        { SECTION_START (with_space space ident lexbuf |> split_ident) }
  | "{{^"        { SECTION_INVERT_START (with_space space ident lexbuf |> split_ident) }
  | "{{/"        { SECTION_END (with_space space ident lexbuf |> split_ident) }
  | "{{>"        { PARTIAL_START (with_space space id lexbuf) }
  | "{{!"        { COMMENT_START }
  | "{{"         { ESCAPE_START (with_space space ident lexbuf |> split_ident) }
  | "}}}"        { UNESCAPE_END }
  | "}}"         { END }
  | raw newline  { new_line lexbuf; RAW (lexeme lexbuf) }
  | raw          { RAW (lexeme lexbuf) }
  | ['{' '}']    { RAW (lexeme lexbuf) }
  | eof          { EOF }

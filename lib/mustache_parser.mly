/*{{{ The MIT License (MIT)

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
   IN THE SOFTWARE. *)
}}}*/
%{
  open Mustache_types
  open Mustache_types.Locs
  let parse_section start_s end_s contents =
    if start_s = end_s
    then { contents; name=start_s }
    else
      let msg =
        Printf.sprintf "Mismatched section %s with %s" start_s end_s in
      raise (Invalid_template msg)

  let loc () =
    { loc_start = Parsing.symbol_start_pos ();
      loc_end = Parsing.symbol_end_pos () }

  let with_loc desc =
    let loc =
      { loc_start = Parsing.symbol_start_pos ();
        loc_end = Parsing.symbol_end_pos () } in
    { loc; desc }
%}

%token EOF
%token END
%token <string> ESCAPE_START
%token <string> UNESCAPE_START_AMPERSAND
%token <string> SECTION_INVERT_START
%token <string> SECTION_START
%token <string> SECTION_END
%token <string> PARTIAL_START
%token <string> UNESCAPE_START
%token COMMENT_START
%token UNESCAPE_END

%token <string> RAW

%start mustache
%type <Mustache_types.Locs.t> mustache

%%

section:
  | SECTION_INVERT_START END mustache SECTION_END END {
    with_loc (Inverted_section (parse_section $1 $4 $3)) }
  | SECTION_START END mustache SECTION_END END {
    with_loc (Section (parse_section $1 $4 $3)) }

mustache_element:
  | UNESCAPE_START UNESCAPE_END { with_loc (Unescaped $1) }
  | UNESCAPE_START_AMPERSAND END { with_loc (Unescaped $1) }
  | ESCAPE_START END { with_loc (Escaped $1) }
  | PARTIAL_START END { with_loc (Partial $1) }
  | COMMENT_START RAW END { with_loc (Comment $2) }
  | section { $1 }

string:
  | RAW { with_loc (String $1) }

mustache_l:
  | mustache_element mustache_l { ($1 :: $2) }
  | string mustache_l { ($1 :: $2) }
  | mustache_element { [$1] }
  | string { [$1] }

mustache:
  | mustache_l {
    match $1 with
    | [x] -> x
    | x -> with_loc (Concat x)
  }
  | EOF { with_loc (String "") }

%%

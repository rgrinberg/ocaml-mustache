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

  let parse_section start_name end_name contents =
    if start_name <> end_name then
      raise (Mismatched_section { start_name; end_name });
    { contents; name = start_name }

  let with_loc (startpos, endpos) desc =
    let loc =
      { loc_start = startpos;
        loc_end = endpos } in
    { loc; desc }
%}

%token EOF
%token <string list> ESCAPE
%token <string list> UNESCAPE
%token <string list> OPEN_INVERTED_SECTION
%token <string list> OPEN_SECTION
%token <string list> CLOSE_SECTION
%token <int * string> PARTIAL
%token <string> COMMENT

%token <string> RAW

%start mustache
%type <Mustache_types.Locs.t> mustache

%%

section:
  | ss = OPEN_INVERTED_SECTION
    e = mustache_expr
    se = CLOSE_SECTION {
    with_loc $sloc
      (Inverted_section (parse_section ss se e))
  }
  | ss = OPEN_SECTION
    e = mustache_expr
    se = CLOSE_SECTION {
    with_loc $sloc
      (Section (parse_section ss se e))
  }

mustache_element:
  | elt = UNESCAPE { with_loc $sloc (Unescaped elt) }
  | elt = ESCAPE { with_loc $sloc (Escaped elt) }
  | elt = PARTIAL {
      with_loc $sloc
        (Partial { indent = fst elt;
                   name = snd elt;
                   contents = lazy None })
    }
  | s = COMMENT { with_loc $sloc (Comment s) }
  | sec = section { sec }
  | s = RAW { with_loc $sloc (String s) }

mustache_expr:
  | elts = list(mustache_element) {
    match elts with
    | [] -> with_loc $sloc (String "")
    | [x] -> x
    | xs -> with_loc $sloc (Concat xs)
  }

mustache:
  | mexpr = mustache_expr EOF { mexpr }

%%

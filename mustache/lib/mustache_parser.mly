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
  open Mustache_types.Ast

  let mkloc (start_pos, end_pos) =
    { loc_start = start_pos;
      loc_end = end_pos }

  let check_matching loc name_kind start_name end_name =
    if start_name <> end_name then
      raise (Mismatched_names (mkloc loc, { name_kind; start_name; end_name }))

  let dotted name =
    string_of_dotted_name name

  let with_loc loc desc =
    { loc = mkloc loc; desc }
%}

%token EOF
%token <string list> ESCAPE
%token <string list> UNESCAPE
%token <string list> OPEN_INVERTED_SECTION
%token <string list> OPEN_SECTION
%token <int * string> PARTIAL
%token <int * string> OPEN_PARTIAL_WITH_PARAMS
%token <int * string> OPEN_PARAM
%token <string> CLOSE
%token <string> COMMENT

%token <string> RAW

%start mustache
%type <Mustache_types.Ast.t> mustache

%%

mustache_element:
  | elt = ESCAPE { with_loc $sloc (Escaped elt) }
  | elt = UNESCAPE { with_loc $sloc (Unescaped elt) }
  | start_name = OPEN_SECTION
    contents = mustache_expr
    end_name = CLOSE {
      check_matching $sloc Section_name (dotted start_name) end_name;
      with_loc $sloc
        (Section { name = start_name; contents })
  }
  | start_name = OPEN_INVERTED_SECTION
    contents = mustache_expr
    end_name = CLOSE {
      check_matching $sloc Inverted_section_name (dotted start_name) end_name;
      with_loc $sloc
        (Inverted_section { name = start_name; contents })
  }
  | partial = PARTIAL {
      let (indent, name) = partial in
      with_loc $sloc
        (Partial { indent; name; params = None;
                   contents = lazy None })
  }
  | partial = OPEN_PARTIAL_WITH_PARAMS
    params = params
    end_name = CLOSE {
      let (indent, start_name) = partial in
      check_matching $sloc Partial_with_params_name start_name end_name;
      with_loc $sloc
        (Partial { indent; name = start_name; params = Some params;
                   contents = lazy None })
  }
  | param = OPEN_PARAM
    contents = mustache_expr
    end_name = CLOSE {
      let (indent, start_name) = param in
      check_matching $sloc Param_name start_name end_name;
      with_loc $sloc
        (Param { indent; name = start_name; contents })
  }
  | s = COMMENT { with_loc $sloc (Comment s) }
  | s = RAW { with_loc $sloc (String s) }

mustache_expr:
  | elts = list(mustache_element) {
    match elts with
    | [] -> with_loc $sloc (String "")
    | [x] -> x
    | xs -> with_loc $sloc (Concat xs)
  }

(* The template-inheritance specification describes partial-with-params
   application of the form:

   {{<foo}}
     text here is ignored
     {{$param1}}default value{{/param1}}
     ignored again
     {{$param2}}default value{{/param2}}
     still ignored
   {{/foo}}

  It is weird that content that is not a template parameter is simply
  ignored inside the partial invocation, but this is explicitly
  mandated (for raw content) by the specification.

  We could at least fail when non-raw content is ignored; not
  implemented yet.
*)
params:
  | elts = list(mustache_element) {
    elts |> List.filter_map (function
      | { loc = _; desc = Param param } -> Some param
      | _ -> None
    )
  }

mustache:
  | mexpr = mustache_expr EOF { mexpr }

%%

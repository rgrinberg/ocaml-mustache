(*{{{ The MIT License (MIT)

  Copyright (c) 2015 Rudi Grinberg

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE. }}}*)

type name = string

type dotted_name = string list

type loc =
  { loc_start : Lexing.position
  ; loc_end : Lexing.position
  }

let pp_dotted_name fmt = function
  | [] -> Format.fprintf fmt "."
  | n :: ns ->
    Format.fprintf fmt "%s" n;
    List.iter (fun n -> Format.fprintf fmt ".%s" n) ns

let string_of_dotted_name n = Format.asprintf "%a" pp_dotted_name n

module Ast = struct
  [@@@warning "-30"]

  type t =
    { loc : loc
    ; desc : desc
    }

  and desc =
    | String of string
    | Escaped of dotted_name
    | Unescaped of dotted_name
    | Section of section
    | Inverted_section of section
    | Partial of partial
    | Param of param
    | Concat of t list
    | Comment of string

  and section =
    { name : dotted_name
    ; contents : t
    }

  and partial =
    { indent : int
    ; name : name
    ; params : param list option
    ; contents : t option Lazy.t
    }

  and param =
    { indent : int
    ; name : name
    ; contents : t
    }
end

type name_kind =
  | Section_name
  | Inverted_section_name
  | Partial_with_params_name
  | Param_name

type name_mismatch_error =
  { name_kind : name_kind
  ; start_name : name
  ; end_name : name
  }

(* these exceptions are used internally in the parser, never exposed to users *)
exception Mismatched_names of loc * name_mismatch_error
exception Invalid_as_partial_parameter of name * Ast.t

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

[@@@warning "-6"]

include Mustache_types
module List = ListLabels
module String = StringLabels

module Json = struct
  type value =
    [ `Null
    | `Bool of bool
    | `Float of float
    | `String of string
    | `A of value list
    | `O of (string * value) list
    ]

  type t =
    [ `A of value list
    | `O of (string * value) list
    ]

  let value : t -> value = fun t -> (t :> value)
end

let escape_html s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string b "&amp;"
      | '"' -> Buffer.add_string b "&quot;"
      | '\'' -> Buffer.add_string b "&apos;"
      | '>' -> Buffer.add_string b "&gt;"
      | '<' -> Buffer.add_string b "&lt;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* Utility functions that allow converting between the ast with locations and
   without locations. *)

let dummy_loc = { loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos }

(* [erase_locs] and [dummy_locs] are not so useful now that we only have one AST
   (with locations).

   They are kept for backwards compatibility; they can also be useful to compare
   templates while ignoring location information. *)
let erase_locs t =
  let open Ast in
  let rec erase_locs (t : t) : t =
    { loc = dummy_loc; desc = erase_locs_desc t.desc }
  and erase_locs_desc = function
    | String s -> String s
    | Escaped s -> Escaped s
    | Section s -> Section (erase_locs_section s)
    | Unescaped s -> Unescaped s
    | Partial p -> Partial (erase_locs_partial p)
    | Param pa -> Param (erase_locs_param pa)
    | Inverted_section s -> Inverted_section (erase_locs_section s)
    | Concat l -> Concat (List.map erase_locs l)
    | Comment s -> Comment s
  and erase_locs_section (s : section) : section =
    { name = s.name; contents = erase_locs s.contents }
  and erase_locs_partial (p : partial) : partial =
    { indent = p.indent
    ; name = p.name
    ; params = Option.map (List.map ~f:erase_locs_param) p.params
    ; contents = lazy (Option.map erase_locs (Lazy.force p.contents))
    }
  and erase_locs_param (pa : param) : param =
    { indent = pa.indent; name = pa.name; contents = erase_locs pa.contents }
  in
  erase_locs t

let add_dummy_locs t = erase_locs t

(* Printing: defined on the ast without locations. *)

let rec pp fmt m =
  let open Ast in
  match m.desc with
  | String s -> Format.pp_print_string fmt s
  | Escaped s -> Format.fprintf fmt "{{ %a }}" pp_dotted_name s
  | Unescaped s -> Format.fprintf fmt "{{& %a }}" pp_dotted_name s
  | Inverted_section s ->
    Format.fprintf fmt "{{^%a}}%a{{/%a}}" pp_dotted_name s.name pp s.contents
      pp_dotted_name s.name
  | Section s ->
    Format.fprintf fmt "{{#%a}}%a{{/%a}}" pp_dotted_name s.name pp s.contents
      pp_dotted_name s.name
  | Partial p -> (
    match p.params with
    | None -> Format.fprintf fmt "{{> %s }}" p.name
    | Some params ->
      Format.fprintf fmt "{{< %s }}%a{{/ %s }}" p.name
        (Format.pp_print_list pp_param)
        params p.name )
  | Param pa -> Format.fprintf fmt "%a" pp_param pa
  | Comment s -> Format.fprintf fmt "{{! %s }}" s
  | Concat s -> List.iter (pp fmt) s

and pp_param fmt pa =
  Format.fprintf fmt "{{$%s}}%a{{/%s}}" pa.name pp pa.contents pa.name

let to_string x =
  let b = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer b in
  pp fmt x;
  Format.pp_print_flush fmt ();
  Buffer.contents b

(* Parsing: produces an ast with locations. *)
type template_parse_error =
  { loc : loc
  ; kind : template_parse_error_kind
  }

and template_parse_error_kind =
  | Lexing of string
  | Parsing
  | Mismatched_names of name_mismatch_error

exception Parse_error of template_parse_error

let parse_lx (lexbuf : Lexing.lexbuf) : Ast.t =
  let loc_of lexbuf =
    let open Lexing in
    { loc_start = lexbuf.lex_start_p; loc_end = lexbuf.lex_curr_p }
  in
  let raise_err loc kind = raise (Parse_error { loc; kind }) in
  try
    MenhirLib.Convert.Simplified.traditional2revised Mustache_parser.mustache
      Mustache_lexer.(handle_standalone mustache lexbuf)
  with
  | Mustache_lexer.Error msg -> raise_err (loc_of lexbuf) (Lexing msg)
  | Mustache_parser.Error -> raise_err (loc_of lexbuf) Parsing
  | Mismatched_names (loc, { name_kind; start_name; end_name }) ->
    raise_err loc (Mismatched_names { name_kind; start_name; end_name })

let of_string s = parse_lx (Lexing.from_string s)

let pp_loc ppf loc =
  let open Lexing in
  let fname = loc.loc_start.pos_fname in
  let is_dummy_pos pos = pos.pos_lnum < 0 || pos.pos_cnum < 0 in
  let extract pos = (pos.pos_lnum, pos.pos_cnum - pos.pos_bol) in
  let loc_start, loc_end =
    let loc_start = loc.loc_start in
    let loc_end = loc.loc_end in
    let orelse p1 p2 =
      if not (is_dummy_pos p1) then
        p1
      else
        p2
    in
    (orelse loc_start loc_end, orelse loc_end loc_start)
  in
  let p ppf = Format.fprintf ppf in
  if is_dummy_pos loc_start && is_dummy_pos loc_end then
    p ppf "(At unknown location)"
  else
    let start_line, start_col = extract loc.loc_start in
    let end_line, end_col = extract loc.loc_end in
    let pp_range ppf (start, end_) =
      if start = end_ then
        p ppf " %d" start
      else
        p ppf "s %d-%d" start end_
    in
    p ppf "@[";
    if fname <> "" then
      p ppf "File %S,@ l" fname
    else
      p ppf "L";
    p ppf "ine%a,@ character%a" pp_range (start_line, end_line) pp_range
      (start_col, end_col)

let pp_template_parse_error ppf ({ loc; kind } : template_parse_error) =
  let p ppf = Format.fprintf ppf in
  p ppf "@[%a:@ " pp_loc loc;
  ( match kind with
  | Lexing msg -> p ppf "%s" msg
  | Parsing -> p ppf "syntax error"
  | Mismatched_names { name_kind; start_name; end_name } ->
    p ppf "Open/close tag mismatch: {{%c %s }} is closed by {{/ %s }}"
      ( match name_kind with
      | Section_name -> '#'
      | Inverted_section_name -> '^'
      | Partial_with_params_name -> '<'
      | Param_name -> '$' )
      start_name end_name );
  p ppf ".@]"

type render_error_kind =
  | Invalid_param of
      { name : dotted_name
      ; expected_form : string
      }
  | Missing_variable of { name : dotted_name }
  | Missing_section of { name : dotted_name }
  | Missing_partial of { name : name }

type render_error =
  { loc : loc
  ; kind : render_error_kind
  }

exception Render_error of render_error

let pp_render_error ppf ({ loc; kind } : render_error) =
  let p ppf = Format.fprintf ppf in
  p ppf "@[%a:@ " pp_loc loc;
  ( match kind with
  | Invalid_param { name; expected_form } ->
    p ppf "the value of '%a' is not a valid %s" pp_dotted_name name
      expected_form
  | Missing_variable { name } ->
    p ppf "the variable '%a' is missing" pp_dotted_name name
  | Missing_section { name } ->
    p ppf "the section '%a' is missing" pp_dotted_name name
  | Missing_partial { name } -> p ppf "the partial '%s' is missing" name );
  p ppf ".@]"

let () =
  let pretty_print exn_name pp_error err =
    let buf = Buffer.create 42 in
    Format.fprintf
      (Format.formatter_of_buffer buf)
      "Mustache.%s:@\n%a@?" exn_name pp_error err;
    Buffer.contents buf
  in
  Printexc.register_printer (function
    | Parse_error err ->
      Some (pretty_print "Parse_error" pp_template_parse_error err)
    | Render_error err -> Some (pretty_print "Render_error" pp_render_error err)
    | _ -> None)

(* Utility modules, that help looking up values in the json data during the
   rendering phase. *)

module Contexts : sig
  type t

  val start : Json.value -> t

  val top : t -> Json.value

  val add : t -> Json.value -> t

  val find_name : t -> string -> Json.value option

  val add_param : t -> Ast.param -> t

  val find_param : t -> string -> Ast.param option
end = struct
  type t =
    { (* nonempty stack of contexts, most recent first *)
      stack : Json.value * Json.value list
    ; (* an associative list of partial parameters that have been defined *)
      params : Ast.param list
    }

  let start js = { stack = (js, []); params = [] }

  let top { stack = js, _rest; _ } = js

  let add ctxs ctx =
    let top, rest = ctxs.stack in
    { ctxs with stack = (ctx, top :: rest) }

  let rec find_name ctxs name =
    let top, _ = ctxs.stack in
    match top with
    | `Null
    | `Bool _
    | `Float _
    | `String _
    | `A _ ->
      find_in_rest ctxs name
    | `O dict -> (
      match List.assoc name dict with
      | exception Not_found -> find_in_rest ctxs name
      | v -> Some v )

  and find_in_rest ctxs name =
    let _, rest = ctxs.stack in
    match rest with
    | [] -> None
    | top :: rest -> find_name { ctxs with stack = (top, rest) } name

  let param_has_name name (p : Ast.param) = String.equal p.name name

  (* Note: the template-inheritance specification for Mustache
     (https://github.com/mustache/spec/pull/75) mandates that in case of
     multi-level inclusion, the "topmost" definition of the parameter wins. In
     other terms, when traversing the template during rendering, the value
     defined first for this parameter has precedence over later definitions.

     This is not a natural choice for our partial-with-arguments view, where we
     would expect the parameter binding closest to the use-site to win. This
     corresponds to an object-oriented view where applying a
     partial-with-parameters is seen as "inheriting" the parent/partial
     template, overriding a method for each parameter. Multi-level inclusions
     correspond to inheritance hierarchies (the parent template itself inherits
     from a grandparent), and then late-binding mandates that the definition
     "last" in the inheritance chain (so closest to the start of the rendering)
     wins.*)
  let add_param ctxs (param : Ast.param) =
    if List.exists (param_has_name param.name) ctxs.params then
      (* if the parameter is already bound, the existing binding has precedence *)
      ctxs
    else
      { ctxs with params = param :: ctxs.params }

  let find_param ctxs name = List.find_opt (param_has_name name) ctxs.params
end

let raise_err loc kind = raise (Render_error { loc; kind })

module Lookup = struct
  let scalar ?(strict = true) ~loc ~name = function
    | `Null ->
      if strict then
        "null"
      else
        ""
    | `Bool true -> "true"
    | `Bool false -> "false"
    | `Float f -> Printf.sprintf "%.12g" f
    | `String s -> s
    | `A _
    | `O _ ->
      raise_err loc (Invalid_param { name; expected_form = "scalar" })

  let simple_name ?(strict = true) ctxs ~loc n =
    match Contexts.find_name ctxs n with
    | None ->
      if strict then raise_err loc (Missing_variable { name = [ n ] });
      None
    | Some _ as result -> result

  let dotted_name ?(strict = true) ctxs ~loc ~key =
    let rec lookup acc (js : Json.value) ~key =
      match key with
      | [] -> Some js
      | n :: ns -> (
        match js with
        | `Null
        | `Float _
        | `Bool _
        | `String _
        | `A _ ->
          raise_err loc
            (Invalid_param { name = List.rev acc; expected_form = "object" })
        | `O dict -> (
          match List.assoc n dict with
          | exception Not_found ->
            if strict then
              raise_err loc (Missing_variable { name = List.rev (n :: acc) });
            None
          | js -> lookup (n :: acc) js ns ) )
    in
    match key with
    | [] -> Some (Contexts.top ctxs)
    | n :: ns -> (
      match simple_name ~strict ctxs ~loc n with
      | None -> None
      | Some js -> lookup [ n ] js ns )

  let str ?(strict = true) ctxs ~loc ~key =
    match dotted_name ~strict ctxs ~loc ~key with
    | None -> ""
    | Some js -> scalar ~strict ~loc ~name:key js

  let section ?(strict = true) ctxs ~loc ~key =
    match dotted_name ~strict:false ctxs ~loc ~key with
    | None ->
      if strict then raise_err loc (Missing_section { name = key });
      `Bool false
    | Some js -> (
      match js with
      (* php casting *)
      | `Null
      | `Float _
      | `Bool false
      | `String "" ->
        `Bool false
      | (`A _ | `O _) as js -> js
      | _ -> js )

  let inverted ctxs ~loc ~key =
    match dotted_name ~strict:false ctxs ~loc ~key with
    | None -> true
    | Some (`A [] | `Bool false | `Null) -> true
    | _ -> false

  let param ctxs ~loc:_ ~key = Contexts.find_param ctxs key
end

module Render = struct
  (* Rendering is defined on the ast without locations. *)

  open Ast

  (* Render a template whose partials have already been expanded.

     Note: the reason we expand partials once before rendering, instead of
     expanding on the fly during rendering, is to avoid expanding many times the
     partials that are inside a list. However, this as the consequence that some
     partials that may not be used in a given rendering may be expanded, and
     that partial expansion cannot have access to the specific context of each
     partial usage -- some other Mustache APIs pass this context information to
     the partial-resolution function. *)
  let render_expanded ?(strict = true) (buf : Buffer.t) (m : Ast.t)
      (js : Json.t) =
    let beginning_of_line = ref true in

    let print_indented buf indent line =
      assert (indent >= 0);
      if String.equal line "" then
        ()
      else (
        for _i = 1 to indent do
          Buffer.add_char buf ' '
        done;
        Buffer.add_string buf line;
        beginning_of_line := false
      )
    in

    let print_dedented buf dedent line =
      assert (dedent >= 0);
      let rec print_from i =
        if i = String.length line then
          ()
        else if
          i < dedent
          &&
          match line.[i] with
          | ' '
          | '\t' ->
            true
          | _ -> false
        then
          print_from (i + 1)
        else (
          Buffer.add_substring buf line i (String.length line - i);
          beginning_of_line := false
        )
      in
      print_from 0
    in

    let print_line indent line =
      if not !beginning_of_line then
        Buffer.add_string buf line
      else if indent >= 0 then
        print_indented buf indent line
      else
        print_dedented buf (-indent) line
    in

    let print_newline buf =
      Buffer.add_char buf '\n';
      beginning_of_line := true
    in

    let print_indented_string indent s =
      let lines = String.split_on_char '\n' s in
      print_line indent (List.hd lines);
      List.iter
        (fun line ->
          print_newline buf;
          print_line indent line)
        (List.tl lines)
    in

    let print_interpolated indent data =
      (* per the specification, interpolated data should be spliced into the
         document, with further lines *not* indented specifically; this effect
         is obtained by calling print_line on the (possibly multiline) data. *)
      print_line indent data
    in

    let rec render indent m (ctxs : Contexts.t) =
      let loc = m.loc in
      match m.desc with
      | String s -> print_indented_string indent s
      | Escaped name ->
        print_interpolated indent
          (escape_html (Lookup.str ~strict ~loc ~key:name ctxs))
      | Unescaped name ->
        print_interpolated indent (Lookup.str ~strict ~loc ~key:name ctxs)
      | Inverted_section s ->
        if Lookup.inverted ctxs ~loc ~key:s.name then
          render indent s.contents ctxs
      | Section s -> (
        let enter ctx = render indent s.contents (Contexts.add ctxs ctx) in
        match Lookup.section ~strict ctxs ~loc ~key:s.name with
        | `Bool false -> ()
        | `A elems -> List.iter enter elems
        | elem -> enter elem )
      | Partial { indent = partial_indent; name; params; contents } -> (
        let partial = Lazy.force contents in
        let ctxs =
          match params with
          | None -> ctxs
          | Some params ->
            List.fold_left ~f:Contexts.add_param ~init:ctxs params
        in
        match partial with
        | None -> if strict then raise_err loc (Missing_partial { name })
        | Some partial -> render (indent + partial_indent) partial ctxs )
      | Param default_param ->
        let param =
          match Lookup.param ctxs ~loc ~key:default_param.name with
          | Some passed_param -> passed_param
          | None -> default_param
        in
        render
          (indent + default_param.indent - param.indent)
          param.contents ctxs
      | Comment _c -> ()
      | Concat templates -> List.iter (fun x -> render indent x ctxs) templates
    in

    render 0 m (Contexts.start (Json.value js))
end

(* Packing up everything in two modules of similar signature:
   [Without_locations] and [With_locations]. In the toplevel signature, only
   [With_locations] appears, and [Without_locations] contents are directly
   included at the toplevel. *)

module With_locations = struct
  include Ast

  (* re-exported here for backward-compatibility *)
  type nonrec loc = loc =
    { loc_start : Lexing.position
    ; loc_end : Lexing.position
    }

  let dummy_loc = dummy_loc

  let parse_lx = parse_lx

  let of_string = of_string

  let pp fmt x = pp fmt x

  let to_formatter = pp

  let to_string x = to_string x

  let rec fold ~string ~section ~escaped ~unescaped ~partial ~param ~comment
      ~concat t =
    let go =
      fold ~string ~section ~escaped ~unescaped ~partial ~param ~comment ~concat
    in
    let { desc; loc } = t in
    match desc with
    | String s -> string ~loc s
    | Escaped s -> escaped ~loc s
    | Unescaped s -> unescaped ~loc s
    | Comment s -> comment ~loc s
    | Section { name; contents } ->
      section ~loc ~inverted:false name (go contents)
    | Inverted_section { name; contents } ->
      section ~loc ~inverted:true name (go contents)
    | Concat ms -> concat ~loc (List.map ms ~f:go)
    | Partial p ->
      let params =
        Option.map
          (List.map ~f:(fun { indent; name; contents } ->
               (indent, name, go contents)))
          p.params
      in
      partial ~loc ?indent:(Some p.indent) p.name ?params p.contents
    | Param { indent; name; contents } ->
      param ~loc ?indent:(Some indent) name (go contents)

  module Infix = struct
    let ( ^ ) t1 t2 = { desc = Concat [ t1; t2 ]; loc = dummy_loc }
  end

  let raw ~loc s = { desc = String s; loc }

  let escaped ~loc s = { desc = Escaped s; loc }

  let unescaped ~loc s = { desc = Unescaped s; loc }

  let section ~loc n c = { desc = Section { name = n; contents = c }; loc }

  let inverted_section ~loc n c =
    { desc = Inverted_section { name = n; contents = c }; loc }

  let partial ~loc ?(indent = 0) n ?params c =
    let params =
      Option.map
        (List.map ~f:(fun (indent, name, contents) ->
             { indent; name; contents }))
        params
    in
    { desc = Partial { indent; name = n; params; contents = c }; loc }

  let concat ~loc t = { desc = Concat t; loc }

  let comment ~loc s = { desc = Comment s; loc }

  let param ~loc ?(indent = 0) n c =
    { desc = Param { indent; name = n; contents = c }; loc }

  let rec expand_partials (partials : name -> t option) : t -> t =
    let section ~loc ~inverted =
      if inverted then
        inverted_section ~loc
      else
        section ~loc
    in
    let partial ~loc ?indent name ?params contents =
      let contents' =
        lazy
          ( match Lazy.force contents with
          | None -> Option.map (expand_partials partials) (partials name)
          | Some t_opt -> Some t_opt )
      in
      partial ~loc ?indent name ?params contents'
    in
    fold ~string:raw ~section ~escaped ~unescaped ~partial ~param ~comment
      ~concat

  let render_buf ?strict ?(partials = fun _ -> None) buf (m : t) (js : Json.t) =
    let m = expand_partials partials m in
    Render.render_expanded buf ?strict m js

  let render ?strict ?partials (m : t) (js : Json.t) =
    let buf = Buffer.create 0 in
    render_buf ?strict ?partials buf m js;
    Buffer.contents buf

  let render_fmt ?strict ?partials fmt m js =
    let str = render ?strict ?partials m js in
    Format.pp_print_string fmt str;
    Format.pp_print_flush fmt ()
end

module Without_locations = struct
  include Ast
  include With_locations

  let noloc f x = f ~loc:dummy_loc x

  let raw = noloc raw

  let escaped = noloc escaped

  let unescaped = noloc unescaped

  let section = noloc section

  let inverted_section = noloc inverted_section

  let partial ?indent = noloc (partial ?indent)

  let param ?indent = noloc (param ?indent)

  let concat = noloc concat

  let comment = noloc comment

  let fold ~string ~section ~escaped ~unescaped ~partial ~param ~comment ~concat
      =
    let igloc f ~loc:_ = f in
    let string = igloc string
    and section = igloc section
    and escaped = igloc escaped
    and unescaped = igloc unescaped
    and partial = igloc partial
    and param = igloc param
    and comment = igloc comment
    and concat = igloc concat in
    fold ~string ~section ~escaped ~unescaped ~partial ~param ~comment ~concat
end

(* Include [Without_locations] at the toplevel, to preserve backwards
   compatibility of the API. *)

include Without_locations

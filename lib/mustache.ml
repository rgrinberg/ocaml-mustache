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
   IN THE SOFTWARE. }}}*)

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
    | `O of (string * value) list ]

  type t =
    [ `A of value list
    | `O of (string * value) list ]

  let value: t -> value = fun t -> (t :> value)
end

let option_map o f = match o with
  | None -> None
  | Some x -> Some (f x)

let escape_html s =
  let b = Buffer.create (String.length s) in
  String.iter ( function
    | '&'  -> Buffer.add_string b "&amp;"
    | '"'  -> Buffer.add_string b "&quot;"
    | '\'' -> Buffer.add_string b "&apos;"
    | '>'  -> Buffer.add_string b "&gt;"
    | '<'  -> Buffer.add_string b "&lt;"
    | c    -> Buffer.add_char b c
  ) s ;
  Buffer.contents b

(* Utility functions that allow converting between the ast with locations and
   without locations. *)

let dummy_loc =
  { loc_start = Lexing.dummy_pos;
    loc_end = Lexing.dummy_pos }

let rec erase_locs { Locs.desc; _ } =
  erase_locs_desc desc
and erase_locs_desc = function
  | Locs.String s -> No_locs.String s
  | Locs.Escaped s -> No_locs.Escaped s
  | Locs.Section s -> No_locs.Section (erase_locs_section s)
  | Locs.Unescaped s -> No_locs.Unescaped s
  | Locs.Partial p -> No_locs.Partial (erase_locs_partial p)
  | Locs.Inverted_section s -> No_locs.Inverted_section (erase_locs_section s)
  | Locs.Concat l -> No_locs.Concat (List.map erase_locs l)
  | Locs.Comment s -> No_locs.Comment s
and erase_locs_section { Locs.name; Locs.contents } =
  { No_locs.name; No_locs.contents = erase_locs contents }
and erase_locs_partial { Locs.indent; Locs.name; Locs.contents } =
  { No_locs.indent;
    No_locs.name;
    No_locs.contents = lazy (option_map (Lazy.force contents) erase_locs) }

let rec add_dummy_locs t =
  { Locs.loc = dummy_loc;
    Locs.desc = add_dummy_locs_desc t }
and add_dummy_locs_desc = function
  | No_locs.String s -> Locs.String s
  | No_locs.Escaped s -> Locs.Escaped s
  | No_locs.Section s -> Locs.Section (add_dummy_locs_section s)
  | No_locs.Unescaped s -> Locs.Unescaped s
  | No_locs.Partial p -> Locs.Partial (add_dummy_locs_partial p)
  | No_locs.Inverted_section s ->
    Locs.Inverted_section (add_dummy_locs_section s)
  | No_locs.Concat l -> Locs.Concat (List.map add_dummy_locs l)
  | No_locs.Comment s -> Locs.Comment s
and add_dummy_locs_section { No_locs.name; No_locs.contents } =
  { Locs.name; Locs.contents = add_dummy_locs contents }
and add_dummy_locs_partial { No_locs.indent; No_locs.name; No_locs.contents } =
  { Locs.indent;
    Locs.name;
    Locs.contents = lazy (option_map (Lazy.force contents) add_dummy_locs) }

(* Printing: defined on the ast without locations. *)

let rec pp fmt =
  let open No_locs in
  function
  | String s ->
    Format.pp_print_string fmt s

  | Escaped s ->
    Format.fprintf fmt "{{ %a }}" pp_dotted_name s

  | Unescaped s ->
    Format.fprintf fmt "{{& %a }}" pp_dotted_name s

  | Inverted_section s ->
    Format.fprintf fmt "{{^%a}}%a{{/%a}}"
      pp_dotted_name s.name pp s.contents pp_dotted_name s.name

  | Section s ->
    Format.fprintf fmt "{{#%a}}%a{{/%a}}"
      pp_dotted_name s.name pp s.contents pp_dotted_name s.name

  | Partial p ->
    Format.fprintf fmt "{{> %s }}" p.name

  | Comment s ->
    Format.fprintf fmt "{{! %s }}" s

  | Concat s ->
    List.iter (pp fmt) s

let to_string x =
  let b = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer b in
  pp fmt x ;
  Format.pp_print_flush fmt () ;
  Buffer.contents b

(* Parsing: produces an ast with locations. *)
type template_parse_error = {
  loc: loc;
  kind: template_parse_error_kind;
}
and template_parse_error_kind =
  | Lexing of string
  | Parsing
  | Mismatched_section of {
      start_name: dotted_name;
      end_name: dotted_name;
    }

exception Parse_error of template_parse_error

let parse_lx (lexbuf: Lexing.lexbuf) : Locs.t =
  let loc_of lexbuf =
    let open Lexing in
    { loc_start = lexbuf.lex_start_p; loc_end = lexbuf.lex_curr_p } in
  let raise_err loc kind =
    raise (Parse_error { loc; kind })
  in
  try
    MenhirLib.Convert.Simplified.traditional2revised
      Mustache_parser.mustache
      Mustache_lexer.(handle_standalone mustache lexbuf)
  with
  | Mustache_lexer.Error msg ->
    raise_err (loc_of lexbuf) (Lexing msg)
  | Mustache_parser.Error ->
    raise_err (loc_of lexbuf) Parsing
  | Mismatched_section { loc; start_name; end_name } ->
    raise_err loc (Mismatched_section { start_name; end_name })

let of_string s = parse_lx (Lexing.from_string s)

let pp_loc ppf loc =
  let open Lexing in
  let fname = loc.loc_start.pos_fname in
  let is_dummy_pos pos = pos.pos_lnum < 0 || pos.pos_cnum < 0 in
  let extract pos = (pos.pos_lnum, pos.pos_cnum - pos.pos_bol) in
  let loc_start, loc_end =
    let loc_start = loc.loc_start in
    let loc_end = loc.loc_end in
    let orelse p1 p2 = if not (is_dummy_pos p1) then p1 else p2 in
    orelse loc_start loc_end, orelse loc_end loc_start in
  let p ppf = Format.fprintf ppf in
  if is_dummy_pos loc_start && is_dummy_pos loc_end then
    p ppf "(At unknown location)"
  else begin
    let (start_line, start_col) = extract loc.loc_start in
    let (end_line, end_col) = extract loc.loc_end in
    let pp_range ppf (start, end_) =
      if start = end_ then
        p ppf " %d" start
      else
        p ppf "s %d-%d" start end_
    in
    p ppf "@[";
    begin if fname <> "" then
      p ppf "File %S,@ l" fname
    else
      p ppf "L"
    end;
    p ppf "ine%a,@ character%a"
      pp_range (start_line, end_line)
      pp_range (start_col, end_col)
  end

let pp_template_parse_error ppf ({ loc; kind; } : template_parse_error) =
  let p ppf = Format.fprintf ppf in
  p ppf "@[%a:@ " pp_loc loc;
  begin match kind with
  | Lexing msg ->
    p ppf "%s" msg
  | Parsing ->
    p ppf "syntax error"
  | Mismatched_section { start_name; end_name } ->
    p ppf "Section mismatch: {{#%a}} is closed by {{/%a}}"
      pp_dotted_name start_name
      pp_dotted_name end_name
  end;
  p ppf ".@]"

type render_error_kind =
  | Invalid_param of { name: dotted_name; expected_form: string; }
  | Missing_variable of { name: dotted_name; }
  | Missing_section of { name: dotted_name; }
  | Missing_partial of { name: name; }

type render_error = { loc: loc; kind : render_error_kind }

exception Render_error of render_error

let pp_render_error ppf ({ loc; kind; } : render_error) =
  let p ppf = Format.fprintf ppf in
  p ppf "@[%a:@ " pp_loc loc;
  begin match kind with
  | Invalid_param { name; expected_form; } ->
    p ppf "the value of '%a' is not a valid %s"
      pp_dotted_name name
      expected_form
  | Missing_variable { name; } ->
    p ppf "the variable '%a' is missing"
      pp_dotted_name name
  | Missing_section { name; } ->
    p ppf "the section '%a' is missing"
      pp_dotted_name name
  | Missing_partial { name } ->
    p ppf "the partial '%s' is missing"
      name
  end;
  p ppf ".@]"


let () =
  let pretty_print exn_name pp_error err =
    let buf = Buffer.create 42 in
    Format.fprintf (Format.formatter_of_buffer buf)
      "Mustache.%s:@\n%a@?"
      exn_name
      pp_error err;
    Buffer.contents buf in
  Printexc.register_printer (function
    | Parse_error err ->
      Some (pretty_print "Parse_error" pp_template_parse_error err)
    | Render_error err ->
      Some (pretty_print "Render_error" pp_render_error err)
    | _ -> None
  )

(* Utility modules, that help looking up values in the json data during the
   rendering phase. *)

module Contexts : sig
  type t
  val start : Json.value -> t
  val top : t -> Json.value
  val add : t -> Json.value -> t
  val find_name : t -> string -> Json.value option
end = struct
  (* a nonempty stack of contexts, most recent first *)
  type t = Json.value * Json.value list

  let start js = (js, [])

  let top (js, _rest) = js

  let add (top, rest) ctx = (ctx, top::rest)

  let rec find_name ((top, rest) : t) name =
    match top with
    | `Null
    | `Bool _
    | `Float _
    | `String _
    | `A _
      -> find_in_rest rest name
    | `O dict ->
      match List.assoc name dict with
      | exception Not_found -> find_in_rest rest name
      | v -> Some v

  and find_in_rest rest name =
    match rest with
    | [] -> None
    | top :: rest -> find_name (top, rest) name
end

let raise_err loc kind =
  raise (Render_error { loc; kind })

module Lookup = struct
  let scalar ?(strict=true) ~loc ~name = function
    | `Null -> if strict then "null" else ""
    | `Bool true -> "true"
    | `Bool false -> "false"
    | `Float f -> Printf.sprintf "%.12g" f
    | `String s -> s
    | `A _ | `O _ ->
      raise_err loc (Invalid_param { name; expected_form = "scalar" })

  let simple_name ?(strict=true) ctxs ~loc n =
    match Contexts.find_name ctxs n with
    | None ->
      if strict then raise_err loc (Missing_variable { name = [n] });
      None
    | Some _ as result -> result

  let dotted_name ?(strict=true) ctxs ~loc ~key =
    let rec lookup acc (js : Json.value) ~key =
      match key with
      | [] -> Some js
      | n :: ns ->
        match js with
        | `Null | `Float _ | `Bool _
        | `String _ | `A _ ->
          raise_err loc (Invalid_param { name = List.rev acc; expected_form = "object" })
        | `O dict ->
          match List.assoc n dict with
          | exception Not_found ->
            if strict then raise_err loc (Missing_variable { name = List.rev (n :: acc) });
            None
          | js -> lookup (n :: acc) js ns
    in
    match key with
    | [] -> Some (Contexts.top ctxs)
    | n :: ns ->
      match simple_name ~strict ctxs ~loc n with
      | None -> None
      | Some js -> lookup [n] js ns

  let str ?(strict=true) ctxs ~loc ~key =
    match dotted_name ~strict ctxs ~loc ~key with
    | None -> ""
    | Some js -> scalar ~strict ~loc ~name:key js

  let section ?(strict=true) ctxs ~loc ~key =
    match dotted_name ~strict:false ctxs ~loc ~key with
    | None ->
      if strict then raise_err loc (Missing_section { name = key });
      `Bool false
    | Some js ->
      match js with
      (* php casting *)
      | `Null | `Float _ | `Bool false | `String "" -> `Bool false
      | (`A _ | `O _) as js -> js
      | _ -> js

  let inverted ctxs ~loc ~key =
    match dotted_name ~strict:false ctxs ~loc ~key with
    | None -> true
    | Some (`A [] | `Bool false | `Null) -> true
    | _ -> false

end

module Render = struct
  (* Rendering is defined on the ast without locations. *)

  open Locs

  (* Render a template whose partials have already been expanded.

     Note: the reason we expand partials once before rendering,
     instead of expanding on the fly during rendering, is to avoid
     expanding many times the partials that are inside a list. However,
     this as the consequence that some partials that may not be used
     in a given rendering may be expanded, and that partial expansion
     cannot have access to the specific context of each partial usage
     -- some other Mustache APIs pass this context information to the
     partial-resolution function. *)
  let render_expanded
        ?(strict = true)
        (buf : Buffer.t) (m : Locs.t) (js : Json.t)
    =
    let print_indent indent =
      for _ = 0 to indent - 1 do
        Buffer.add_char buf ' '
      done
    in

    let beginning_of_line = ref true in

    let align indent =
      if !beginning_of_line then (
        print_indent indent;
        beginning_of_line := false
      )
    in

    let print_indented_string indent s =
      let lines = String.split_on_char '\n' s in
      align indent; Buffer.add_string buf (List.hd lines);
      List.iter (fun line ->
        Buffer.add_char buf '\n';
        beginning_of_line := true;
        if line <> "" then (
          align indent;
          Buffer.add_string buf line;
        )
      ) (List.tl lines)
    in

    let rec render indent m (ctxs : Contexts.t) =
      let loc = m.loc in
      match m.desc with

      | String s ->
        print_indented_string indent s

      | Escaped name ->
        align indent;
        Buffer.add_string buf (escape_html (Lookup.str ~strict ~loc ~key:name ctxs))

      | Unescaped name ->
        align indent;
        Buffer.add_string buf (Lookup.str ~strict ~loc ~key:name ctxs)

      | Inverted_section s ->
        if Lookup.inverted ctxs ~loc ~key:s.name
        then render indent s.contents ctxs

      | Section s ->
        let enter ctx = render indent s.contents (Contexts.add ctxs ctx) in
        begin match Lookup.section ~strict ctxs ~loc ~key:s.name with
        | `Bool false -> ()
        | `A elems    -> List.iter enter elems
        | elem        -> enter elem
        end

      | Partial { indent = partial_indent; name; contents } ->
        begin match (Lazy.force contents, strict) with
        | Some p, _ -> render (indent + partial_indent) p ctxs
        | None, false -> ()
        | None, true ->
          raise_err loc (Missing_partial { name })
        end

      | Comment _c -> ()

      | Concat templates ->
        List.iter (fun x -> render indent x ctxs) templates

    in render 0 m (Contexts.start (Json.value js))
end

(* Packing up everything in two modules of similar signature:
   [Without_locations] and [With_locations]. In the toplevel signature, only
   [With_locations] appears, and [Without_locations] contents are directly
   included at the toplevel.
*)

module Without_locations = struct
  include No_locs

  let parse_lx lexbuf = erase_locs (parse_lx lexbuf)
  let of_string s = erase_locs (of_string s)

  let pp = pp
  let to_formatter = pp

  let to_string = to_string

  let rec fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat t =
    let go = fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat in
    match t with
    | String s -> string s
    | Escaped s -> escaped s
    | Unescaped s -> unescaped s
    | Comment s -> comment s
    | Section { name; contents } ->
      section ~inverted:false name (go contents)
    | Inverted_section { name; contents } ->
      section ~inverted:true name (go contents)
    | Concat ms ->
      concat (List.map ms ~f:go)
    | Partial p -> partial p.indent p.name p.contents

  module Infix = struct
    let (^) y x = Concat [x; y]
  end

  let raw s = String s
  let escaped s = Escaped s
  let unescaped s = Unescaped s
  let section n c = Section { name = n ; contents = c }
  let inverted_section n c = Inverted_section { name = n ; contents = c }
  let partial ?(indent = 0) n c = Partial { indent ; name = n ; contents = c }
  let concat t = Concat t
  let comment s = Comment s

  let rec expand_partials (partials : name -> t option) : t -> t =
    let section ~inverted =
      if inverted then inverted_section else section
    in
    let partial indent name contents =
      let contents' = lazy (
        match Lazy.force contents with
        | None -> option_map (partials name) (expand_partials partials)
        | Some t_opt -> Some t_opt
      )
      in
      partial ~indent name contents'
    in
    fold ~string:raw ~section ~escaped ~unescaped ~partial ~comment ~concat


  let render_buf ?strict ?(partials = fun _ -> None) buf (m : t) (js : Json.t) =
    let m = add_dummy_locs (expand_partials partials m) in
    Render.render_expanded buf ?strict m js

  let render ?strict ?partials (m : t) (js : Json.t) =
    let buf = Buffer.create 0 in
    render_buf ?strict ?partials buf m js ;
    Buffer.contents buf

  let render_fmt ?strict ?partials fmt m js =
    let str = render ?strict ?partials m js in
    Format.pp_print_string fmt str;
    Format.pp_print_flush fmt ()
end

module With_locations = struct
  include Locs

  (* re-exported here for backward-compatibility *)
  type nonrec loc = loc = { loc_start: Lexing.position; loc_end: Lexing.position }

  let dummy_loc = dummy_loc
  let parse_lx = parse_lx
  let of_string = of_string

  let pp fmt x = pp fmt (erase_locs x)
  let to_formatter = pp

  let to_string x = to_string (erase_locs x)

  let rec fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat t =
    let go = fold ~string ~section ~escaped ~unescaped ~partial ~comment ~concat in
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
    | Concat ms ->
      concat ~loc (List.map ms ~f:go)
    | Partial p -> partial ~loc p.indent p.name p.contents

  module Infix = struct
    let (^) t1 t2 = { desc = Concat [t1; t2]; loc = dummy_loc }
  end

  let raw ~loc s = { desc = String s; loc }
  let escaped ~loc s = { desc = Escaped s; loc }
  let unescaped ~loc s = { desc = Unescaped s; loc }
  let section ~loc n c =
    { desc = Section { name = n; contents = c };
      loc }
  let inverted_section ~loc n c =
    { desc = Inverted_section { name = n; contents = c };
      loc }
  let partial ~loc ?(indent = 0) n c =
    { desc = Partial { indent; name = n; contents = c };
      loc }
  let concat ~loc t = { desc = Concat t; loc }
  let comment ~loc s = { desc = Comment s; loc }

  let rec expand_partials (partials : name -> t option) : t -> t =
    let section ~loc ~inverted =
      if inverted then inverted_section ~loc else section ~loc
    in
    let partial ~loc indent name contents =
      let contents' = lazy (
        match Lazy.force contents with
        | None -> option_map (partials name) (expand_partials partials)
        | Some t_opt -> Some t_opt
      )
      in
      partial ~loc ~indent name contents'
    in
    fold ~string:raw ~section ~escaped ~unescaped ~partial ~comment ~concat

  let render_buf ?strict ?(partials = fun _ -> None) buf (m : t) (js : Json.t) =
    let m = expand_partials partials m in
    Render.render_expanded buf ?strict m js

  let render ?strict ?partials (m : t) (js : Json.t) =
    let buf = Buffer.create 0 in
    render_buf ?strict ?partials buf m js ;
    Buffer.contents buf

  let render_fmt ?strict ?partials fmt m js =
    let str = render ?strict ?partials m js in
    Format.pp_print_string fmt str;
    Format.pp_print_flush fmt ()
end


(* Include [Without_locations] at the toplevel, to preserve backwards
   compatibility of the API. *)

include Without_locations

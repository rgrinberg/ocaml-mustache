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
open MoreLabels
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
  { Locs.loc_start = Lexing.dummy_pos;
    Locs.loc_end = Lexing.dummy_pos }

let rec erase_locs { Locs.desc; _ } =
  erase_locs_desc desc
and erase_locs_desc = function
  | Locs.String s -> No_locs.String s
  | Locs.Escaped s -> No_locs.Escaped s
  | Locs.Section s -> No_locs.Section (erase_locs_section s)
  | Locs.Unescaped s -> No_locs.Unescaped s
  | Locs.Partial s -> No_locs.Partial s
  | Locs.Inverted_section s -> No_locs.Inverted_section (erase_locs_section s)
  | Locs.Concat l -> No_locs.Concat (List.map erase_locs l)
  | Locs.Comment s -> No_locs.Comment s
and erase_locs_section { Locs.name; Locs.contents } =
  { No_locs.name; No_locs.contents = erase_locs contents }

let rec add_dummy_locs t =
  { Locs.loc = dummy_loc;
    Locs.desc = add_dummy_locs_desc t }
and add_dummy_locs_desc = function
  | No_locs.String s -> Locs.String s
  | No_locs.Escaped s -> Locs.Escaped s
  | No_locs.Section s -> Locs.Section (add_dummy_locs_section s)
  | No_locs.Unescaped s -> Locs.Unescaped s
  | No_locs.Partial s -> Locs.Partial s
  | No_locs.Inverted_section s ->
    Locs.Inverted_section (add_dummy_locs_section s)
  | No_locs.Concat l -> Locs.Concat (List.map add_dummy_locs l)
  | No_locs.Comment s -> Locs.Comment s
and add_dummy_locs_section { No_locs.name; No_locs.contents } =
  { Locs.name; Locs.contents = add_dummy_locs contents }

(* Printing: defined on the ast without locations. *)

let rec pp fmt =
  let open No_locs in
  function
  | String s ->
    Format.pp_print_string fmt s

  | Escaped s ->
    Format.fprintf fmt "{{ %s }}" s

  | Unescaped s ->
    Format.fprintf fmt "{{& %s }}" s

  | Inverted_section s ->
    Format.fprintf fmt "{{^%s}}%a{{/%s}}"
      s.name pp s.contents s.name

  | Section s ->
    Format.fprintf fmt "{{#%s}}%a{{/%s}}"
      s.name pp s.contents s.name

  | Partial s ->
    Format.fprintf fmt "{{> %s }}" s

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

(* Rendering: defined on the ast without locations. *)

module Lookup = struct
  let scalar ?(strict=true) = function
    | `Null -> if strict then "null" else ""
    | `Bool true -> "true"
    | `Bool false -> "false"
    | `Float f -> string_of_float f
    | `String s -> s
    | `A _ | `O _ -> raise (Invalid_param "Lookup.scalar: not a scalar")

  let str ?(strict=true) (js : Json.value) ~key =
    match js with
    | `Null | `Float _ | `Bool _
    | `String _ | `A _ -> raise (Invalid_param ("str. not an object"))
    | `O assoc ->
      try
        scalar (List.assoc key assoc)
      with Not_found ->
        if strict then raise (Missing_variable key) else ""

  let section ?(strict=true) (js : Json.value) ~key =
    match js with
    | `Null | `Float _ | `A _
    | `Bool _ | `String _ ->
      if strict then raise (Invalid_param ("section: " ^ key)) else `Bool false
    | `O elems ->
      try
        match List.assoc key elems with
        (* php casting *)
        | `Null | `Float _ | `Bool false | `String "" -> `Bool false
        | (`A _ | `O _) as js -> js
        | _ -> js
      with Not_found ->
        if strict then raise (Missing_section key) else `Bool false

  let inverted (js : Json.value) ~key =
    match js with
    | `Null
    | `Bool false
    | `A [] -> true
    | `O map -> not (List.mem_assoc ~map key)
    | _ -> false

end

let render_fmt ?(strict=true) (fmt : Format.formatter) (m : No_locs.t) (js : Json.t) =
  let open No_locs in
  let rec render' m (js : Json.value) = match m with

    | String s ->
      Format.pp_print_string fmt s

    | Escaped "." ->
      Format.pp_print_string fmt (escape_html (Lookup.scalar js))
    | Escaped key ->
      Format.pp_print_string fmt (escape_html (Lookup.str ~strict ~key js))

    | Unescaped "." ->
      Format.pp_print_string fmt (Lookup.scalar js)
    | Unescaped key ->
      Format.pp_print_string fmt (Lookup.str ~strict ~key js)

    | Inverted_section s ->
      if Lookup.inverted js s.name
      then render' (Section s) js

    | Section s ->
      begin match Lookup.section ~strict js ~key:s.name with
      | `Bool false -> ()
      | `Bool true  -> render' s.contents js
      | `A contexts -> List.iter (render' s.contents) contexts
      | context     -> render' s.contents context
      end

    | Partial _ ->
      pp fmt m

    | Comment c -> ()

    | Concat templates ->
      List.iter (fun x -> render' x js) templates

  in render' m (Json.value js)

let render ?(strict=true) (m : No_locs.t) (js : Json.t) =
  let b = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer b in
  render_fmt ~strict fmt m js ;
  Format.pp_print_flush fmt () ;
  Buffer.contents b

(* Parsing: produces an ast with locations. *)

let parse_lx : Lexing.lexbuf -> Locs.t =
  Mustache_parser.mustache Mustache_lexer.mustache

let of_string s = parse_lx (Lexing.from_string s)

(* Packing up everything in two modules of similar signature:
   [With_locations] and [Without_locations].
*)

module With_locations = struct
  include Locs

  let dummy_loc = dummy_loc
  let parse_lx = parse_lx
  let of_string = of_string

  let pp fmt x = pp fmt (erase_locs x)
  let to_formatter = pp

  let to_string x = to_string (erase_locs x)

  let render_fmt ?strict fmt m js =
    render_fmt ?strict fmt (erase_locs m) js

  let render ?strict m js =
    render ?strict (erase_locs m) js

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
    | Partial p -> partial ~loc p

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
  let partial ~loc s = { desc = Partial s; loc }
  let concat ~loc t = { desc = Concat t; loc }
  let comment ~loc s = { desc = Comment s; loc }

  let rec expand_partials =
    let section ~loc ~inverted =
      if inverted then inverted_section ~loc else section ~loc
    in
    fun partial ->
      fold ~string:raw ~section ~escaped ~unescaped ~partial ~comment ~concat
end

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
    | Partial p -> partial p

  module Infix = struct
    let (^) y x = Concat [x; y]
  end

  let raw s = String s
  let escaped s = Escaped s
  let unescaped s = Unescaped s
  let section n c = Section { name = n ; contents = c }
  let inverted_section n c = Inverted_section { name = n ; contents = c }
  let partial s = Partial s
  let concat t = Concat t
  let comment s = Comment s

  let rec expand_partials =
    let section ~inverted =
      if inverted then inverted_section else section
    in
    fun partial ->
      fold ~string:raw ~section ~escaped ~unescaped ~partial ~comment ~concat
end

(* Include [Without_locations] at the toplevel, to preserve backwards
   compatibility of the API. *)

include Without_locations

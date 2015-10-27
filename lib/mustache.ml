open MoreLabels
include Mustache_types

module List = ListLabels
module String = StringLabels

module Infix = struct
  let (^) y x = Concat [x; y]
end

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

let parse_lx = Mustache_parser.mustache Mustache_lexer.mustache
let of_string s = parse_lx (Lexing.from_string s)

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

let rec to_formatter fmt = function

  | String s ->
    Format.pp_print_string fmt s

  | Escaped s ->
    Format.fprintf fmt "{{ %s }}" s

  | Unescaped s ->
    Format.fprintf fmt "{{& %s }}" s

  | Inverted_section s ->
    Format.fprintf fmt "{{^%s}}%a{{/%s}}"
      s.name to_formatter s.contents s.name

  | Section s ->
    Format.fprintf fmt "{{#%s}}%a{{/%s}}"
      s.name to_formatter s.contents s.name

  | Partial s ->
    Format.fprintf fmt "{{> %s }}" s

  | Comment s ->
    Format.fprintf fmt "{{! %s }}" s

  | Concat s ->
    List.iter (to_formatter fmt) s

let to_string x =
  let b = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer b in
  to_formatter fmt x ;
  Format.pp_print_flush fmt () ;
  Buffer.contents b

let rec fold
	     ?(string = fun _ acc -> acc)
	     ?(escaped = fun _ acc -> acc)
	     ?(unescaped = fun _ acc -> acc)
	     ?(partial = fun _ acc -> acc)
	     ?(comment = fun _ acc -> acc) =
  function
  | String s -> string s
  | Escaped s -> escaped s
  | Unescaped s -> unescaped s
  | Comment s -> comment s
  | Section {contents} | Inverted_section {contents} ->
    fold ~string ~escaped ~unescaped ~partial ~comment contents
  | Concat ms ->
    fun init ->
      List.fold_left
	~f:(fun acc ms ->
	  fold ~string ~escaped ~unescaped ~partial ~comment ms acc)
	~init ms
  | Partial p -> partial p

let rec expand_partials f = function
  | String _ | Escaped _ | Unescaped _ | Comment _ as m -> m
  | Section s ->
    Section {s with contents = expand_partials f s.contents}
  | Inverted_section s ->
    Inverted_section {s with contents = expand_partials f s.contents}
  | Concat ms -> Concat (List.map (expand_partials f) ms)
  | Partial p -> f p

module Lookup = struct
  let scalar = function
    | `Null -> "null"
    | `Bool true -> "true"
    | `Bool false -> "false"
    | `Float f -> string_of_float f
    | `String s -> s
    | `A _ | `O _ -> raise (Invalid_param "Lookup.scalar: not a scalar")

  let str (js : Json.value) ~key =
    match js with
    | `Null | `Float _ | `Bool _
    | `String _ | `A _ -> raise (Invalid_param ("str. not an object"))
    | `O assoc ->
      scalar (try List.assoc key assoc
              with Not_found -> raise (Missing_variable key))

  let section (js : Json.value) ~key =
    match js with
    | `Null | `Float _ | `A _
    | `Bool _ | `String _ -> raise (Invalid_param ("section: " ^ key))
    | `O elems ->
      match List.assoc key elems with
      (* php casting *)
      | `Null | `Float _ | `Bool false | `String "" -> `Bool false
      | (`A _ | `O _) as js -> js
      | _ -> js

  let inverted (js : Json.value) ~key =
    match js with
    | `Null
    | `Bool false
    | `A [] -> true
    | `O map -> not (List.mem_assoc ~map key)
    | _ -> false

end

let render_fmt (fmt : Format.formatter) (m : t) (js : Json.t) =

  let rec render' m (js : Json.value) = match m with

    | String s ->
      Format.pp_print_string fmt s

    | Escaped "." ->
      Format.pp_print_string fmt (escape_html (Lookup.scalar js))
    | Escaped key ->
      Format.pp_print_string fmt (escape_html (Lookup.str ~key js))

    | Unescaped "." ->
      Format.pp_print_string fmt (Lookup.scalar js)
    | Unescaped key ->
      Format.pp_print_string fmt (Lookup.str ~key js)

    | Inverted_section s ->
      if Lookup.inverted js s.name
      then render' (Section s) js

    | Section s ->
      begin match Lookup.section js s.name with
      | `Bool false -> ()
      | `Bool true  -> render' s.contents js
      | `A contexts -> List.iter (render' s.contents) contexts
      | context     -> render' s.contents context
      end

    | Partial _ ->
      to_formatter fmt m

    | Comment c -> ()

    | Concat templates ->
      List.iter (fun x -> render' x js) templates

  in render' m (Json.value js)

let render (m : t) (js : Json.t) =
  let b = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer b in
  render_fmt fmt m js ;
  Format.pp_print_flush fmt () ;
  Buffer.contents b

let raw s = String s
let escaped s = Escaped s
let unescaped s = Unescaped s
let section n c = Section { name = n ; contents = c }
let inverted_section n c = Inverted_section { name = n ; contents = c }
let partial s = Partial s
let concat t = Concat t
let comment s = Comment s

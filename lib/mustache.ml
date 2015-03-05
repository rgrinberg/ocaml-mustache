open Sexplib.Std
open Printf
open MoreLabels
include Mustache_types
open Mustache_parser

module Str = Re_str
module List = ListLabels
module String = StringLabels

let (|>) x f = f x

let concat templates = Concat templates

module Infix = struct
  let (^) y x = Concat [x; y]
end

(* TODO: rename *)
let return = function
  | [] -> String ""
  | [x] -> x
  | xs -> Concat xs

let parse_lx = Mustache_parser.mustache Mustache_lexer.mustache
let of_string s = s |> Lexing.from_string |> parse_lx

let escape_table = [
  ("&", "&amp;");
  ("\"", "&quot;");
  ("'", "&apos;");
  (">", "&gt;");
  ("<", "&lt;");
] |> List.map ~f:(fun (re, v) -> (Str.regexp_string re, v))

let escape_html init =
  List.fold_left ~f:(fun s (search, replace) ->
    Str.global_replace search replace s
  ) ~init escape_table


let rec to_string = function
  | Iter_var -> "{{.}}"
  | String s -> s
  | Escaped s -> sprintf "{{ %s }}" s
  | Unescaped s -> sprintf "{{& %s }}" s
  | Inverted_section { name; contents } ->
    sprintf "{{^ %s }}%s{{/%s}}" name (to_string contents) name
  | Section { name; contents } ->
    sprintf "{{# %s }}%s{{/%s}}" name (to_string contents) name
  | Partial s -> sprintf "{{> %s }}" s
  | Concat s -> s
                |> List.map ~f:to_string
                |> String.concat ~sep:""

module Lookup = struct
  let scalar x =
    match x with
    | `Null -> "null"
    | `Bool v -> if v then "true" else "false"
    | `Float f -> string_of_float f
    | `String s -> s
    | `A _ | `O _ -> raise (Invalid_param "Lookup.scalar: not a scalar")

  let str (js : Ezjsonm.value) ~key =
    match js with
    | `Null | `Float _ | `Bool _
    | `String _ | `A _ -> raise (Invalid_param ("str. not an object"))
    | `O assoc ->
      assoc |> List.assoc key |> scalar

  let section (js : Ezjsonm.value) ~key =
    match js with
    | `Null | `Float _ | `A _
    | `Bool _ | `String _ -> raise (Invalid_param ("section: " ^ key))
    | `O elems ->
      match List.assoc key elems with
      (* php casting *)
      | `Null | `Float _ | `Bool false | `String "" -> `Bool false
      | `Bool true -> `Bool true
      | `A e -> `List e
      | `O o -> `Scope (`O o)
      | _ -> raise (Invalid_param ("section: invalid key: " ^ key))

  let inverted (js : Ezjsonm.value) ~key =
    match js with
    | `Null
    | `Bool false
    | `A [] -> true
    | `O map -> not (List.mem_assoc ~map key)
    | _ -> false

end

let render m (js : Ezjsonm.t) =
  let rec render' m (js : Ezjsonm.value) =
    match m with
    | Iter_var -> js |> Lookup.scalar
    | String s -> s
    | Escaped key -> js |> Lookup.str ~key
    | Unescaped key -> js |> Lookup.str ~key
    | Inverted_section ({ name=key; _ } as sec) when Lookup.inverted js ~key ->
      render' (Section sec) js
    | Inverted_section _ -> ""
    | Section { name=key; contents } ->
      begin match js |> Lookup.section ~key with
      | `Bool false -> ""
      | `Bool true -> render' contents js
      | `List elems ->
        elems
        |> List.map ~f:(render' contents)
        |> String.concat ~sep:""
      | `Scope obj -> render' contents obj
      end
    | Partial s -> to_string m
    | Concat templates ->
      templates
      |> List.map ~f:(fun tmpl -> render' tmpl js)
      |> String.concat ~sep:""
  in js |> Ezjsonm.value |> render' m

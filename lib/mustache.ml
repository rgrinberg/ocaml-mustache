open Sexplib.Std
open Printf
open MoreLabels
module Str = Re_str
module List = ListLabels
module String = StringLabels

exception Invalid_param of string with sexp
exception Invalid_template of string with sexp

type t =
  | Iter_var
  | String of string
  | Escaped of string                
  | Section of section
  | Unescaped of string
  | Partial of string
  | Concat of t list
and section = {
  name: string;
  contents: t;
} with sexp

let concat templates = Concat templates

module Infix = struct
  let (^) y x = Concat [x; y]
end

(* TODO: very inefficient *)
let tokenize s tokens =
  let res = tokens |> List.map ~f:(fun (t, re) -> (t, Str.regexp re)) in
  let re = tokens
           |> List.map ~f:snd
           |> String.concat ~sep:"\\|"
           |> Str.regexp in
  let open Str in
  s
  |> Str.full_split re
  |> List.map ~f:(function
    | Text s -> `Text s
    | Delim s ->
      let group = ref s in
      let tag =
        res
        |> List.find ~f:(fun (tag, re) ->
          if Str.string_match re s 0
          then begin
            group := Str.matched_group 1 s;
            true
          end
          else false)
        |> fst
      in
      `Token (tag, !group))

(* TODO: rename *)
let return = function
  | [] -> String ""
  | [x] -> x
  | xs -> Concat xs

let of_string s =
  let token_table = [
    (`Iter          , "{{ *\\(\\.\\) *}}"      );
    (`Escape        , "{{{ *\\([^} ]+\\) *}}}" );
    (`Escape        , "{{& *\\([^} ]+\\) *}}"  );
    (`Section_start , "{{# *\\([^} ]+\\) *}}"  );
    (`Section_end   , "{{/ *\\([^} ]+\\) *}}"  );
    (`Partial       , "{{> *\\([^} ]+\\) *}}"  );
    (`Unescape      , "{{ *\\([^} ]+\\) *}}"   );
  ] in
  (* TODO: clean the hell up *)
  let rec parse section_count name acc = function
    | [] when section_count > 0 ->
      raise @@ Invalid_template ("Section: " ^ name ^ " is not terminated")
    | [] -> (List.rev acc, [])
    | (`Text s)::rest ->
      parse section_count name ((String s)::acc) rest
    | (`Token (`Iter, _))::rest ->
      parse section_count name (Iter_var::acc) rest
    | (`Token (`Escape, s))::rest ->
      parse section_count name ((Escaped s)::acc) rest
    | (`Token (`Section_start, name_))::rest ->
      let (contents, rest) = parse (succ section_count) name_ [] rest in
      let section = Section { name=name_; contents=(return contents) } in
      parse section_count name (section::acc) rest
    | (`Token (`Section_end, section))::rest when section=name ->
      (List.rev acc, rest)
    | (`Token (`Section_end, section))::rest ->
      raise @@ Invalid_template ("Mismatched section: " ^ section)
    | (`Token (`Unescape, s))::rest ->
      parse section_count name ((Unescaped s)::acc) rest
    | (`Token (`Partial, s))::rest ->
      parse section_count name ((Partial s)::acc) rest
  in
  let tokens = tokenize s token_table in
  match parse 0 "" [] tokens with
  | templates, [] -> return templates
  | _, _::_ -> assert false

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
  | Section { name; contents } ->
    sprintf "{{# %s }}%s{{/%s}}" name (to_string contents) name
  | Partial s -> sprintf "{{> %s }}" s
  | Concat s -> s
                |> List.map ~f:to_string
                |> String.concat ~sep:""

module Lookup = struct
  let scalar x =
    match x with
    | `Null | `Float _ | `Bool _ -> Ezjsonm.to_string x
    | `String s -> s
    | `A _ | `O _ -> raise @@ Invalid_param "Lookup.scalar: not a scalar"

  let str (js : Ezjsonm.t) ~key =
    match js with
    | `Null | `Float _ | `Bool _
    | `String _ | `A _ -> raise @@ Invalid_param ("str. not an object")
    | `O assoc ->
      assoc |> List.assoc key |> scalar

  let section (js : Ezjsonm.t) ~key =
    match js with
    | `Null | `Float _ | `A _
    | `Bool _ | `String _ -> raise @@ Invalid_param ("section: " ^ key)
    | `O elems ->
      match List.assoc key elems with
      (* php casting *)
      | `Null | `Float _ | `Bool false | `String "" -> `Bool false
      | `Bool true -> `Bool true
      | `A e -> `List e
      | _ -> raise @@ Invalid_param ("section: invalid key: " ^ key)
end

let rec render m js =
  match m with
  | Iter_var -> js |> Lookup.scalar
  | String s -> s
  | Escaped key -> js |> Lookup.str ~key
  | Unescaped key -> js |> Lookup.str ~key
  | Section { name=key; contents } ->
    begin match js |> Lookup.section ~key with
    | `Bool false -> ""
    | `Bool true -> render contents js
    | `List elems ->
      elems
      |> List.map ~f:(render contents)
      |> String.concat ~sep:""
    end
  | Partial s -> to_string m
  | Concat templates ->
    templates
    |> List.map ~f:(fun tmpl -> render tmpl js)
    |> String.concat ~sep:""

open Sexplib.Std
open Printf
open MoreLabels
open Cow
module Str = Re_str
module List = ListLabels
module String = StringLabels

exception Missing_param of string with sexp
exception Bad_template of string with sexp

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
    (`Escape        , "{{{ *\\([^}]+\\) *}}}" );
    (`Escape        , "{{& *\\([^}]+\\) *}}"  );
    (`Section_start , "{{# *\\([^}]+\\) *}}"  );
    (`Section_end   , "{{/ *\\([^}]+\\) *}}"  );
    (`Partial       , "{{> *\\([^}]+\\) *}}"  );
    (`Unescape      , "{{ *\\([^}]+\\) *}}"   );
  ] in
  (* TODO: clean the hell up *)
  let rec parse ?(terminated=true) name acc = function
    | [] when terminated ->
      failwith @@ "Section: " ^ name ^ " is not terminated"
    | [] -> (List.rev acc, [])
    | (`Text s)::rest ->
      parse ~terminated name ((String s)::acc) rest
    | (`Token (`Iter, _))::rest ->
      parse ~terminated name (Iter_var::acc) rest
    | (`Token (`Escape, s))::rest ->
      parse ~terminated name ((Escaped s)::acc) rest
    | (`Token (`Section_start, name_))::rest ->
      let (contents, rest) = parse name_ [] rest in
      let section = Section { name=name_; contents=(return contents) } in
      parse ~terminated:true name (section::acc) rest
    | (`Token (`Section_end, section))::rest when section=name ->
      (List.rev acc, rest)
    | (`Token (`Section_end, section))::rest ->
      failwith @@ "Mismatched section: " ^ section
    | (`Token (`Unescape, s))::rest ->
      parse ~terminated name ((Unescaped s)::acc) rest
    | (`Token (`Partial, s))::rest ->
      parse ~terminated name ((Partial s)::acc) rest
  in
  match parse ~terminated:false "" [] (tokenize s token_table) with
  | templates, [] -> return templates
  | _, _::_ -> assert false

let escape_html s = s

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
  open Json
  exception Invalid_lookup of string
  (* look up a string, normal substitution *)
  let str js ~key =
    match js with
    | Null | Int _ | Float _ | Bool _ -> Json.to_string js
    | String s -> s
    | Array _
    | Object _ -> raise @@ Invalid_lookup ("Lookup.str: invalid key: " ^ key)

  let scalar x =
    match x with
    | Null | Int _ | Float _ | Bool _ | String _ -> Json.to_string x
    | Array _ | Object _ -> failwith "Lookup.scalar: not a scalar"

  let section js ~key =
    match js with
    | Null | Int _ | Float _ | Array _
    | Bool _ | String _ -> invalid_arg @@ "section: " ^ key
    | Object elems ->
      match List.assoc key elems with
      (* php casting *)
      | Null | Int _ | Float _ | Bool false | String "" -> `Bool false
      | Bool true -> `Bool true
      | Array e -> `List e
      | _ -> invalid_arg @@ "section: invalid key: " ^ key
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

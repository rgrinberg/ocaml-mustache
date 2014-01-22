open Printf
open MoreLabels
open Cow
module Str = Re_str
module List = ListLabels
module String = StringLabels

exception Missing_param of string
exception Bad_template of string

type mustache =
  | Iter_var
  | String of string
  | Escaped of string                
  | Section of section
  | Unescaped of string
  | Partial of string
  | Concat of mustache list

and section = {
  name: string;
  contents: mustache;
}

module Tokenizer = struct
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
        let tag = res
                  |> List.find ~f:(fun (tag, re) ->
                    if Str.string_match re s 0
                    then begin
                      group := Str.matched_group 0 s;
                      true
                    end
                    else false)
                  |> fst
        in
        `Token (tag, !group))
end

let of_string s =
  let token_table = [
    (`Iter          , "{{ \\(*\\.\\) *}}"      );
    (`Escape        , "{{{ * \\([^}]+\\) *}}}" );
    (`Escape        , "{{& * \\([^}]+\\) *}}"  );
    (`Section_start , "{{# * \\([^}]+\\) *}}"  );
    (`Section_end   , "{{/ * \\([^}]+\\) *}}"  );
    (`Partial       , "{{> * \\([^}]+\\) *}}"  );
    (`Unescape      , "{{ * \\([^}]+\\) *}}"   );
  ] in
  let rec parse sections = function
    | [] -> String ""
    | (`Text s)::rest -> Concat [String s; parse sections rest]
    | (`Token (`Iter, _))::rest -> Iter_var
    | (`Token (`Escape, _))::rest -> Iter_var
    | (`Token (`Section_start, _))::rest -> Iter_var
    | (`Token (`Section_end, _))::rest -> Iter_var
    | (`Token (`Unescape, _))::rest -> Iter_var
    | (`Token (`Partial, _))::rest -> Iter_var
  in parse [] (Tokenizer.tokenize s token_table)

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

open Sexplib.Std

type t =
  | Iter_var
  | String of string
  | Escaped of string
  | Section of section
  | Unescaped of string
  | Partial of string
  | Inverted_section of string
  | Concat of t list
and section = {
  name: string;
  contents: t;
} with sexp

exception Invalid_param of string with sexp
exception Invalid_template of string with sexp


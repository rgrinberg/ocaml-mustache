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

val of_string : string -> t
val to_string : t -> string
val render : t -> Ezjsonm.t -> string

val escape_html : string -> string

val tokenize : string -> ('a * string) list
  -> [> `Text of string | `Token of 'a * string ] list
(** Exported only for testing. Do not use *)

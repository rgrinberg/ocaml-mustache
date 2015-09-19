(** A module for creating and rendering mustache templates in OCaml *)
exception Invalid_param of string with sexp
exception Invalid_template of string with sexp

type t with sexp

val parse_lx : Lexing.lexbuf -> t
val of_string : string -> t

val to_formatter : Format.formatter -> t -> unit
val to_string : t -> string

val render_fmt : Format.formatter -> t -> Ezjsonm.t -> unit
val render : t -> Ezjsonm.t -> string

(** Concatenate a list of mustache templates. Faster than concatenating
    them as strings. *)
val concat : t list -> t

module Infix : sig
  val (^) : t -> t -> t
end

val escape_html : string -> string

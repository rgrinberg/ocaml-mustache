(** A module for creating and rendering mustache templates in OCaml. *)
exception Invalid_param of string
exception Invalid_template of string

(** Raised when a missing variable in a template is not substituted *)
exception Missing_variable of string

module Json : sig (** Compatible with Ezjsonm *)
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
end

type t

(** Read *)
val parse_lx : Lexing.lexbuf -> t
val of_string : string -> t

(** [to_formatter fmt template] print a template as raw mustache to
    the formatter [fmt].  *)
val to_formatter : Format.formatter -> t -> unit

(** [to_string template] uses [to_formatter] in order to return
    a string representing the template as raw mustache.  *)
val to_string : t -> string

(** [render_fmt fmt template json] render [template], filling it
    with data from [json], printing it to formatter [fmt]. *)
val render_fmt : Format.formatter -> t -> Json.t -> unit

(** [render template json] use [render_fmt] to render [template]
    with data from [json] and returns the resulting string. *)
val render : t -> Json.t -> string

(** [fold template] is the composition of [f] over parts of [template], called
    in order of occurrence, where each [f] is one of the labelled arguments
    applied to the corresponding part.  The default for [f] is the identity
    function.

    @param iter_var Called on each [{{.}}].
    @param string Applied to each literal part of the template.
    @param escaped Applied to ["name"] for occurrences of [{{name}}].
    @param unescaped Applied to ["name"] for occurrences of [{{{name}}}].
    @param partial Applied to ["box"] for occurrences of [{{> box}}].
    @param comment Applied to ["comment"] for occurrences of [{{! comment}}]. *)
val fold : ?iter_var: ('a -> 'a) ->
	   ?string: (string -> 'a -> 'a) ->
	   ?escaped: (string -> 'a -> 'a) ->
	   ?unescaped: (string -> 'a -> 'a) ->
	   ?partial: (string -> 'a -> 'a) ->
	   ?comment: (string -> 'a -> 'a) ->
	   t -> 'a -> 'a

val expand_partials : (string -> t) -> t -> t
(** [expand_partials f template] is [template] with [f p] substituted for each
    partial [p]. *)

(** Shortcut for concatening two templates pieces. *)
module Infix : sig
  val (^) : t -> t -> t
end

(** Escape [&], ["\""], ['], [<] and [>]
    character for html rendering. *)
val escape_html : string -> string

(** [{{.}}] *)
val iter_var : t

(** [<p>This is raw text.</p>] *)
val raw : string -> t

(** [{{name}}] *)
val escaped : string -> t

(** [{{{name}}}] *)
val unescaped : string -> t

(** [{{^person}} {{/person}}] *)
val inverted_section : string -> t -> t

(** [{{#person}} {{/person}}] *)
val section : string -> t -> t

(** [{{> box}}] *)
val partial : string -> t

(** [{{! this is a comment}}] *)
val comment : string -> t

(** Group a [t list] as a single [t]. *)
val concat : t list -> t

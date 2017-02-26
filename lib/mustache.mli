(** A module for creating and rendering mustache templates in OCaml. *)
exception Invalid_param of string
exception Invalid_template of string

(** Raised when a missing variable in a template is not substituted *)
exception Missing_variable of string
exception Missing_section of string

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

type t =
  | String of string
  | Escaped of string
  | Section of section
  | Unescaped of string
  | Partial of string
  | Inverted_section of section
  | Concat of t list
  | Comment of string
and section =
  { name: string;
    contents: t }

(** Read *)
val parse_lx : Lexing.lexbuf -> t
val of_string : string -> t

(** [pp fmt template] print a template as raw mustache to
    the formatter [fmt].  *)
val pp : Format.formatter -> t -> unit

val to_formatter : Format.formatter -> t -> unit
(** Alias for compatibility *)

(** [to_string template] uses [to_formatter] in order to return
    a string representing the template as raw mustache.  *)
val to_string : t -> string

(** [render_fmt fmt template json] render [template], filling it
    with data from [json], printing it to formatter [fmt]. *)
val render_fmt : ?strict:bool -> Format.formatter -> t -> Json.t -> unit

(** [render template json] use [render_fmt] to render [template]
    with data from [json] and returns the resulting string. *)
val render : ?strict:bool -> t -> Json.t -> string

(** [fold template] is the composition of [f] over parts of [template], called
    in order of occurrence, where each [f] is one of the labelled arguments
    applied to the corresponding part.  The default for [f] is the identity
    function.

    @param string Applied to each literal part of the template.
    @param escaped Applied to ["name"] for occurrences of [{{name}}].
    @param unescaped Applied to ["name"] for occurrences of [{{{name}}}].
    @param partial Applied to ["box"] for occurrences of [{{> box}}].
    @param comment Applied to ["comment"] for occurrences of [{{! comment}}]. *)
val fold : string: (string -> 'a) ->
  section: (inverted:bool -> string -> 'a -> 'a) ->
  escaped: (string -> 'a) ->
  unescaped: (string -> 'a) ->
  partial: (string -> 'a) ->
  comment: (string -> 'a) ->
  concat:('a list -> 'a) ->
  t -> 'a

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

(** Variant of the [t] mustache datatype which includes source-file locations,
    and associated functions. *)
module With_locations : sig
  type loc =
    { loc_start: Lexing.position;
      loc_end: Lexing.position }

  type desc =
    | String of string
    | Escaped of string
    | Section of section
    | Unescaped of string
    | Partial of string
    | Inverted_section of section
    | Concat of t list
    | Comment of string
  and section =
    { name: string;
      contents: t }
  and t =
    { loc : loc;
      desc : desc }

  (** A value of type [loc], guaranteed to be different from any valid
      location.  *)
  val dummy_loc : loc

  (** Read *)
  val parse_lx : Lexing.lexbuf -> t
  val of_string : string -> t

  (** [pp fmt template] print a template as raw mustache to
      the formatter [fmt].  *)
  val pp : Format.formatter -> t -> unit

  val to_formatter : Format.formatter -> t -> unit
  (** Alias for compatibility *)

  (** [to_string template] uses [to_formatter] in order to return
      a string representing the template as raw mustache.  *)
  val to_string : t -> string

  (** [render_fmt fmt template json] render [template], filling it
      with data from [json], printing it to formatter [fmt]. *)
  val render_fmt : ?strict:bool -> Format.formatter -> t -> Json.t -> unit

  (** [render template json] use [render_fmt] to render [template]
      with data from [json] and returns the resulting string. *)
  val render : ?strict:bool -> t -> Json.t -> string

  (** [fold template] is the composition of [f] over parts of [template], called
      in order of occurrence, where each [f] is one of the labelled arguments
      applied to the corresponding part.  The default for [f] is the identity
      function.

      @param string Applied to each literal part of the template.
      @param escaped Applied to ["name"] for occurrences of [{{name}}].
      @param unescaped Applied to ["name"] for occurrences of [{{{name}}}].
      @param partial Applied to ["box"] for occurrences of [{{> box}}].
      @param comment Applied to ["comment"] for occurrences of [{{! comment}}]. *)
  val fold : string: (loc:loc -> string -> 'a) ->
    section: (loc:loc -> inverted:bool -> string -> 'a -> 'a) ->
    escaped: (loc:loc -> string -> 'a) ->
    unescaped: (loc:loc -> string -> 'a) ->
    partial: (loc:loc -> string -> 'a) ->
    comment: (loc:loc -> string -> 'a) ->
    concat:(loc:loc -> 'a list -> 'a) ->
    t -> 'a

  val expand_partials : (loc:loc -> string -> t) -> t -> t
  (** [expand_partials f template] is [template] with [f p] substituted for each
      partial [p]. *)

  (** Shortcut for concatening two templates pieces. *)
  module Infix : sig
    (** The location of the created [Concat] node has location [dummy_loc].
        Use [concat] to provide a location. *)
    val (^) : t -> t -> t
  end

  (** [<p>This is raw text.</p>] *)
  val raw : loc:loc -> string -> t

  (** [{{name}}] *)
  val escaped : loc:loc -> string -> t

  (** [{{{name}}}] *)
  val unescaped : loc:loc -> string -> t

  (** [{{^person}} {{/person}}] *)
  val inverted_section : loc:loc -> string -> t -> t

  (** [{{#person}} {{/person}}] *)
  val section : loc:loc -> string -> t -> t

  (** [{{> box}}] *)
  val partial : loc:loc -> string -> t

  (** [{{! this is a comment}}] *)
  val comment : loc:loc -> string -> t

  (** Group a [t list] as a single [t]. *)
  val concat : loc:loc -> t list -> t
end

(** Erase locations from a mustache value of type [With_locations.t]. *)
val erase_locs : With_locations.t -> t

(** Add the [dummy_loc] location to each node of a mustache value of type
    [t]. *)
val add_dummy_locs : t -> With_locations.t

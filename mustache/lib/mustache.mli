(** A module for creating and rendering mustache templates in OCaml. *)

[@@@warning "-30"]

module Json : sig
  (** Compatible with Ezjsonm *)
  type value =
    [ `Null
    | `Bool of bool
    | `Float of float
    | `String of string
    | `A of value list
    | `O of (string * value) list
    ]

  type t =
    [ `A of value list
    | `O of (string * value) list
    ]
end

type loc =
  { loc_start : Lexing.position
  ; loc_end : Lexing.position
  }

type name = string

type dotted_name = string list

type t =
  { loc : loc
  ; desc : desc
  }

and desc =
  | String of string
  | Escaped of dotted_name
  | Unescaped of dotted_name
  | Section of section
  | Inverted_section of section
  | Partial of partial
  | Param of param
  | Concat of t list
  | Comment of string

and section =
  { name : dotted_name
  ; contents : t
  }

and partial =
  { indent : int
  ; name : name
  ; params : param list option
  ; contents : t option Lazy.t
  }

and param =
  { indent : int
  ; name : name
  ; contents : t
  }

val pp_loc : Format.formatter -> loc -> unit

(** Read template files; those function may raise [Parse_error]. *)
type template_parse_error

exception Parse_error of template_parse_error

val parse_lx : Lexing.lexbuf -> t

val of_string : string -> t

(** [pp_template_parse_error fmt err] prints a human-readable description of a
    template parse error on the given formatter. The function does not flush the
    formatter (in case you want to use it within boxes), so you should remember
    to do it yourself.

    {[ try ignore (Mustache.of_string "{{!") with Mustache.Parse_error err ->
    Format.eprintf "%a@." Mustache.pp_template_parse_error err ]} *)
val pp_template_parse_error : Format.formatter -> template_parse_error -> unit

(** [pp fmt template] print a template as raw mustache to the formatter [fmt]. *)
val pp : Format.formatter -> t -> unit

(** Alias for compatibility *)
val to_formatter : Format.formatter -> t -> unit

(** [to_string template] uses [to_formatter] in order to return a string
    representing the template as raw mustache. *)
val to_string : t -> string

(** Render templates; those functions may raise [Render_error]. *)
type render_error_kind =
  | Invalid_param of
      { name : dotted_name
      ; expected_form : string
      }
  | Missing_variable of { name : dotted_name }
  | Missing_section of { name : dotted_name }
  | Missing_partial of { name : name }

type render_error =
  { loc : loc
  ; kind : render_error_kind
  }

exception Render_error of render_error

val pp_render_error : Format.formatter -> render_error -> unit

(** [render_fmt fmt template json] renders [template], filling it with data from
    [json], printing it to formatter [fmt].

    For each partial [p], if [partials p] is [Some t] then the partial is
    substituted by [t]. Otherwise, the partial is substituted by the empty
    string is [strict] is [false]. If [strict] is [true], a {!Missing_partial}
    error is raised.

    @raise Render_error when there is a mismatch between the template and the
    data. The [Missing_*] cases are only raised in strict mode, when [strict] is
    true. *)
val render_fmt :
     ?strict:bool
  -> ?partials:(name -> t option)
  -> Format.formatter
  -> t
  -> Json.t
  -> unit

(** [render_buf buf template json] renders [template], filling it with data from
    [json], printing it to the buffer [buf]. See {!render_fmt}. *)
val render_buf :
     ?strict:bool
  -> ?partials:(name -> t option)
  -> Buffer.t
  -> t
  -> Json.t
  -> unit

(** [render template json] renders [template], filling it with data from [json],
    and returns the resulting string. See {!render_fmt}. *)
val render :
  ?strict:bool -> ?partials:(name -> t option) -> t -> Json.t -> string

(** [fold template] is the composition of [f] over parts of [template], called
    in order of occurrence, where each [f] is one of the labelled arguments
    applied to the corresponding part. The default for [f] is the identity
    function.

    @param string Applied to each literal part of the template.
    @param escaped Applied to ["name"] for occurrences of [{{name}}].
    @param unescaped Applied to ["name"] for occurrences of [{{{name}}}].
    @param partial Applied to ["box"] for occurrences of [{{> box}}] or
    [{{< box}}].
    @param params Applied to ["param"] for occurrences of [{{$ param}}].
    @param comment Applied to ["comment"] for occurrences of [{{! comment}}]. *)
val fold :
     string:(string -> 'a)
  -> section:(inverted:bool -> dotted_name -> 'a -> 'a)
  -> escaped:(dotted_name -> 'a)
  -> unescaped:(dotted_name -> 'a)
  -> partial:
       (   ?indent:int
        -> name
        -> ?params:(int * name * 'a) list
        -> t option Lazy.t
        -> 'a)
  -> param:(?indent:int -> name -> 'a -> 'a)
  -> comment:(string -> 'a)
  -> concat:('a list -> 'a)
  -> t
  -> 'a

(** [expand_partials f template] is [template] where for each [Partial p] node,
    [p.contents] now evaluates to [f p.name] if they were evaluating to [None].
    Note that no lazy is forced at this point, and calls to [f] are delayed
    until [p.contents] is forced. *)
val expand_partials : (name -> t option) -> t -> t

(** Shortcut for concatening two templates pieces. *)
module Infix : sig
  val ( ^ ) : t -> t -> t
end

(** Escape [&], ["\""], ['], [<] and [>] character for html rendering. *)
val escape_html : string -> string

(** [<p>This is raw text.</p>] *)
val raw : string -> t

(** [{{name}}] *)
val escaped : dotted_name -> t

(** [{{{name}}}] *)
val unescaped : dotted_name -> t

(** [{{^person}} {{/person}}] *)
val inverted_section : dotted_name -> t -> t

(** [{{#person}} {{/person}}] *)
val section : dotted_name -> t -> t

(** [{{> box}}] or

    {[
      {{< box}}
        {{$param1}} default value for param1 {{/param1}}
        {{$param2}} default value for param1 {{/param2}}
      {{/box}}
    ]} *)
val partial :
  ?indent:int -> name -> ?params:(int * name * t) list -> t option Lazy.t -> t

(** [{{$foo}} {{/foo}}] *)
val param : ?indent:int -> name -> t -> t

(** [{{! this is a comment}}] *)
val comment : string -> t

(** Group a [t list] as a single [t]. *)
val concat : t list -> t

(** Variant of the [t] mustache datatype which includes source-file locations,
    and associated functions. *)
module With_locations : sig
  (* these types have been moved out, keep an alias for backward-compatibility *)
  type nonrec loc = loc =
    { loc_start : Lexing.position
    ; loc_end : Lexing.position
    }

  and desc = desc =
    | String of string
    | Escaped of dotted_name
    | Unescaped of dotted_name
    | Section of section
    | Inverted_section of section
    | Partial of partial
    | Param of param
    | Concat of t list
    | Comment of string

  and section = section =
    { name : dotted_name
    ; contents : t
    }

  and partial = partial =
    { indent : int
    ; name : name
    ; params : param list option
    ; contents : t option Lazy.t
    }

  and param = param =
    { indent : int
    ; name : name
    ; contents : t
    }

  and t = t =
    { loc : loc
    ; desc : desc
    }

  (** A value of type [loc], guaranteed to be different from any valid location. *)
  val dummy_loc : loc

  (** Read *)
  val parse_lx : Lexing.lexbuf -> t

  val of_string : string -> t

  (** [pp fmt template] print a template as raw mustache to the formatter [fmt]. *)
  val pp : Format.formatter -> t -> unit

  (** Alias for compatibility *)
  val to_formatter : Format.formatter -> t -> unit

  (** [to_string template] uses [to_formatter] in order to return a string
      representing the template as raw mustache. *)
  val to_string : t -> string

  (** [render_fmt fmt template json] renders [template], filling it with data
      from [json], printing it to formatter [fmt].

      For each partial [p], if [partials p] is [Some t] then the partial is
      substituted by [t]. Otherwise, the partial is substituted by the empty
      string is [strict] is [false]. If [strict] is [true], a {!Missing_partial}
      error is raised.

      @raise Render_error when there is a mismatch between the template and the
      data. The [Missing_*] cases are only raised in strict mode, when [strict]
      is true. *)
  val render_fmt :
       ?strict:bool
    -> ?partials:(name -> t option)
    -> Format.formatter
    -> t
    -> Json.t
    -> unit

  (** [render_buf buf template json] renders [template], filling it with data
      from [json], printing it to the buffer [buf]. See {!render_fmt}. *)
  val render_buf :
       ?strict:bool
    -> ?partials:(name -> t option)
    -> Buffer.t
    -> t
    -> Json.t
    -> unit

  (** [render template json] renders [template], filling it with data from
      [json], and returns the resulting string. See {!render_fmt}. *)
  val render :
    ?strict:bool -> ?partials:(name -> t option) -> t -> Json.t -> string

  (** [fold template] is the composition of [f] over parts of [template], called
      in order of occurrence, where each [f] is one of the labelled arguments
      applied to the corresponding part. The default for [f] is the identity
      function.

      @param string Applied to each literal part of the template.
      @param escaped Applied to ["name"] for occurrences of [{{name}}].
      @param unescaped Applied to ["name"] for occurrences of [{{{name}}}].
      @param partial Applied to ["box"] for occurrences of [{{> box}}] or
      [{{< box}}].
      @param params Applied to ["param"] for occurrences of [{{$ param}}].
      @param comment Applied to ["comment"] for occurrences of [{{! comment}}]. *)
  val fold :
       string:(loc:loc -> string -> 'a)
    -> section:(loc:loc -> inverted:bool -> dotted_name -> 'a -> 'a)
    -> escaped:(loc:loc -> dotted_name -> 'a)
    -> unescaped:(loc:loc -> dotted_name -> 'a)
    -> partial:
         (   loc:loc
          -> ?indent:int
          -> name
          -> ?params:(int * name * 'a) list
          -> t option Lazy.t
          -> 'a)
    -> param:(loc:loc -> ?indent:int -> name -> 'a -> 'a)
    -> comment:(loc:loc -> string -> 'a)
    -> concat:(loc:loc -> 'a list -> 'a)
    -> t
    -> 'a

  (** [expand_partials f template] is [template] where for each [Partial p]
      node, [p.contents] now evaluates to [f p.name] if they were evaluating to
      [None]. Note that no lazy is forced at this point, and calls to [f] are
      delayed until [p.contents] is forced. *)
  val expand_partials : (name -> t option) -> t -> t

  (** Shortcut for concatening two templates pieces. *)
  module Infix : sig
    (** The location of the created [Concat] node has location [dummy_loc]. Use
        [concat] to provide a location. *)
    val ( ^ ) : t -> t -> t
  end

  (** [<p>This is raw text.</p>] *)
  val raw : loc:loc -> string -> t

  (** [{{name}}] *)
  val escaped : loc:loc -> dotted_name -> t

  (** [{{{name}}}] *)
  val unescaped : loc:loc -> dotted_name -> t

  (** [{{^person}} {{/person}}] *)
  val inverted_section : loc:loc -> dotted_name -> t -> t

  (** [{{#person}} {{/person}}] *)
  val section : loc:loc -> dotted_name -> t -> t

  (** [{{> box}}] or

      {[
        {{< box}}
          {{$param1}} default value for param1 {{/param1}}
          {{$param2}} default value for param1 {{/param2}}
        {{/box}}
      ]} *)
  val partial :
       loc:loc
    -> ?indent:int
    -> name
    -> ?params:(int * name * t) list
    -> t option Lazy.t
    -> t

  (** [{{$foo}} {{/foo}}] *)
  val param : loc:loc -> ?indent:int -> name -> t -> t

  (** [{{! this is a comment}}] *)
  val comment : loc:loc -> string -> t

  (** Group a [t list] as a single [t]. *)
  val concat : loc:loc -> t list -> t
end

(** Erase locations from a mustache value of type [With_locations.t]. *)
val erase_locs : With_locations.t -> t

(** Add the [dummy_loc] location to each node of a mustache value of type [t]. *)
val add_dummy_locs : t -> With_locations.t

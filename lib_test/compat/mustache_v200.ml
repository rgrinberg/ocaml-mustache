(** This test is designed to get a sense of the backward-compatibility
   impact of changes to the ocaml-mustache library.

   mustache_v200.mli is exactly a copy of the Mustache interface as it
   existed in the version v2.0.0, and it should not change.

   mustache_v200.ml is a reimplementation of this interface using the
   current ocaml-mustache library. If the library changes, this
   reimplementation will have to be fixed as well; the invasiveness of
   this fix can be used to estimate the invasiveness of the change for
   end-users.

   v2.0.0 was chosen because it does not contain an explicit AST
   definition or "fold" function -- whose compatibility breaks for
   basically any new language feature or representation change
   (for example when adding locations in the AST).
*)

include Mustache

(** The exceptions below are not used anymore. *)
exception Invalid_param of string
exception Invalid_template of string

let iter_var = Mustache.escaped []

let render_fmt = Mustache.render_fmt ?strict:None ?partials:None
let render = Mustache.render ?strict:None ?partials:None

open struct
  let dotted name =
    String.split_on_char '.' name
    |> List.filter (fun n -> n <> "")
end

let escaped name = escaped (dotted name)
let unescaped name = unescaped (dotted name)
let inverted_section name = inverted_section (dotted name)
let section name = section (dotted name)

let partial name = partial ?indent:None ?params:None name (lazy None)

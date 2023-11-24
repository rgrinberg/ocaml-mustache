### 3.3.0
* Rename the CLI tool to `mustache-ocaml`. It's now part of the new opam
  package `mustache-cli` (@psafont, #71)

### 3.2.0

* Remove the AST without locations: now all functions build an AST with locations;
  in particular, parsing always provide located error messages.
  To ease backward-compatibility, the smart constructors still use the
  same interface, using dummy locations by default, with
  a With_locations module for users who wish to explicitly provide
  locations.
  (@gasche, #65)
* Support for "template inheritance" (partials with parameters)
  `{{<foo}} {{$param1}}...{{/param1}} {{$param2}}...{{/param2}} {{/foo}`
  following the widely-implemented semi-official specification
    https://github.com/mustache/spec/pull/75
  (@gasche, 58)
* Partials are now supported in the `mustache` command-line tool (@gasche, #57)
  They are interpreted as template inclusion: "{{>foo/bar}}" will include
  "foo/bar.mustache", relative to the current working directory.
* Improve error messages (@gasche, #47, #51, #56)
  Note: the exceptions raised by Mustache have changed, this breaks
  compatibility for users that would catch and deconstruct existing
  exceptions.
* Add `render_buf` to render templates directly to buffers (@gasche, #48)
* When a lookup fails in the current context, lookup in parents contexts.
  This should fix errors when using "{{#foo}}" for a scalar variable
  'foo' to check that the variable exists.
  (@gasche, #49)

### 3.1.0

* Install `mustache` command line utility (@avsm, @anton-trunov)
* Update opam metadata to 2.0 format (@avsm)
* Port build to Dune (@avsm)
* Fix ocamldoc syntax to be compatible with odoc (@avsm)
* Test OCaml 4.06 and 4.07 as well (@avsm)

### 3.0.2 (08-05-2017)

* Add .descr file to repository

### 3.0.1 (06-09-2017)

* Switch to jbuilder (#30)

### 3.0.0

* Proper handling of partials (#28)
* Handle standalone elements (#27)
* Support for dotted names (#26)
* Switch parser to menhir (#24)
* keep track of locations in AST (#21)

  $ touch empty.json
  $ echo '{ "foo": "Foo"; "bar": "Bar" }' > invalid.json
  $ echo '{{foo}}' > foo.mustache

Empty json file:
  $ mustache empty.json foo.mustache
  Fatal error: exception Ezjsonm.Parse_error(870828711, "JSON.of_buffer expected JSON text (JSON value)")
  [2]

Invalid json file:
  $ mustache invalid.json foo.mustache
  Fatal error: exception Ezjsonm.Parse_error(870828711, "JSON.of_buffer expected value separator or object end (',' or '}')")
  [2]

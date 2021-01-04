  $ touch empty.json
  $ echo '{ "foo": "Foo"; "bar": "Bar" }' > invalid.json
  $ echo '{{foo}}' > foo.mustache

Empty json file:
  $ mustache empty.json foo.mustache
  File "empty.json", line 1, character 0: expected JSON text (JSON value)
  [4]

Invalid json file:
  $ mustache invalid.json foo.mustache
  File "invalid.json", line 1, characters 15-29:
  expected value separator or object end (',' or '}')
  [4]

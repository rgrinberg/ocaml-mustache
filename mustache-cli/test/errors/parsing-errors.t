  $ echo '{ "foo": "Foo"}' > foo.json

Delimiter problems:
  $ PROBLEM=no-closing-mustache.mustache
  $ echo "{{foo" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "no-closing-mustache.mustache", line 2, character 0: '}}' expected.
  [3]

  $ PROBLEM=one-closing-mustache.mustache
  $ echo "{{foo}" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "one-closing-mustache.mustache", line 1, character 5: '}}' expected.
  [3]

  $ PROBLEM=eof-before-variable.mustache
  $ echo "{{" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "eof-before-variable.mustache", line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-section.mustache
  $ echo "{{#" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "eof-before-section.mustache", line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-section-end.mustache
  $ echo "{{#foo}} {{.}} {{/" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "eof-before-section-end.mustache", line 2, character 0: '}}' expected.
  [3]

  $ PROBLEM=eof-before-inverted-section.mustache
  $ echo "{{^" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "eof-before-inverted-section.mustache", line 2, character 0:
  ident expected.
  [3]

  $ PROBLEM=eof-before-unescape.mustache
  $ echo "{{{" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "eof-before-unescape.mustache", line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-unescape.mustache
  $ echo "{{&" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "eof-before-unescape.mustache", line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-partial.mustache
  $ echo "{{>" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "eof-before-partial.mustache", line 2, character 0: '}}' expected.
  [3]

  $ PROBLEM=eof-in-comment.mustache
  $ echo "{{! non-terminated comment" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "eof-in-comment.mustache", line 2, character 0: non-terminated comment.
  [3]


Mismatches between opening and closing mustaches:

  $ PROBLEM=two-three.mustache
  $ echo "{{ foo }}}" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "two-three.mustache", line 1, characters 7-10: '}}' expected.
  [3]

  $ PROBLEM=three-two.mustache
  $ echo "{{{ foo }}" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "three-two.mustache", line 1, characters 8-10: '}}}' expected.
  [3]


Mismatch between section-start and section-end:

  $ PROBLEM=foo-bar.mustache
  $ echo "{{#foo}} {{.}} {{/bar}}" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "foo-bar.mustache", line 1, characters 0-23:
  Open/close tag mismatch: {{# foo }} is closed by {{/ bar }}.
  [3]

  $ PROBLEM=foo-not-closed.mustache
  $ echo "{{#foo}} {{.}} {{foo}}" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "foo-not-closed.mustache", line 2, character 0: syntax error.
  [3]

  $ PROBLEM=wrong-nesting.mustache
  $ echo "{{#bar}} {{#foo}} {{.}} {{/bar}} {{/foo}}" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "wrong-nesting.mustache", line 1, characters 9-32:
  Open/close tag mismatch: {{# foo }} is closed by {{/ bar }}.
  [3]

  $ PROBLEM=wrong-nesting-variable.mustache
  $ echo '{{#bar}} {{$foo}} {{.}} {{/bar}} {{/foo}}' > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "wrong-nesting-variable.mustache", line 1, characters 9-32:
  Open/close tag mismatch: {{$ foo }} is closed by {{/ bar }}.
  [3]

  $ PROBLEM=wrong-nesting-partial.mustache
  $ echo "{{#foo}} {{<foo-bar}} {{/foo}} {{/foo-bar}}" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "wrong-nesting-partial.mustache", line 1, characters 9-30:
  Open/close tag mismatch: {{< foo-bar }} is closed by {{/ foo }}.
  [3]



Weird cases that may confuse our lexer or parser:

  $ PROBLEM=weird-tag-name.mustache
  $ echo "{{.weird}} foo bar" > $PROBLEM
  $ mustache-ocaml foo.json $PROBLEM
  File "weird-tag-name.mustache", line 1, character 3: '}}' expected.
  [3]

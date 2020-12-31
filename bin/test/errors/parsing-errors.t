  $ echo '{ "foo": "Foo"}' > foo.json

Delimiter problems:
  $ PROBLEM=no-closing-mustache.mustache
  $ echo "{{foo" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "no-closing-mustache.mustache", line 2, character 0: '}}' expected.
  [3]

  $ PROBLEM=one-closing-mustache.mustache
  $ echo "{{foo}" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "one-closing-mustache.mustache", line 1, character 5: '}}' expected.
  [3]

  $ PROBLEM=eof-before-variable.mustache
  $ echo "{{" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "eof-before-variable.mustache", line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-section.mustache
  $ echo "{{#" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "eof-before-section.mustache", line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-section-end.mustache
  $ echo "{{#foo}} {{.}} {{/" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "eof-before-section-end.mustache", line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-inverted-section.mustache
  $ echo "{{^" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "eof-before-inverted-section.mustache", line 2, character 0:
  ident expected.
  [3]

  $ PROBLEM=eof-before-unescape.mustache
  $ echo "{{{" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "eof-before-unescape.mustache", line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-unescape.mustache
  $ echo "{{&" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "eof-before-unescape.mustache", line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-partial.mustache
  $ echo "{{>" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "eof-before-partial.mustache", line 2, character 0: '}}' expected.
  [3]

  $ PROBLEM=eof-in-comment.mustache
  $ echo "{{! non-terminated comment" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "eof-in-comment.mustache", line 2, character 0: non-terminated comment.
  [3]


Mismatches between opening and closing mustaches:

  $ PROBLEM=two-three.mustache
  $ echo "{{ foo }}}" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "two-three.mustache", line 1, characters 7-10: '}}' expected.
  [3]

  $ PROBLEM=three-two.mustache
  $ echo "{{{ foo }}" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "three-two.mustache", line 1, characters 8-10: '}}}' expected.
  [3]


Mismatch between section-start and section-end:

  $ PROBLEM=foo-bar.mustache
  $ echo "{{#foo}} {{.}} {{/bar}}" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "foo-bar.mustache", lines 1-2, characters 23-0:
  Section mismatch: {{#foo}} is closed by {{/bar}}.
  [3]

  $ PROBLEM=foo-not-closed.mustache
  $ echo "{{#foo}} {{.}} {{foo}}" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "foo-not-closed.mustache", line 2, character 0: syntax error.
  [3]

  $ PROBLEM=wrong-nesting.mustache
  $ echo "{{#bar}} {{#foo}} {{.}} {{/bar}} {{/foo}}" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "wrong-nesting.mustache", lines 1-2, characters 41-0:
  Section mismatch: {{#foo}} is closed by {{/bar}}.
  [3]


Weird cases that may confuse our lexer or parser:

  $ PROBLEM=weird-tag-name.mustache
  $ echo "{{.weird}} foo bar" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Template parse error:
  File "weird-tag-name.mustache", line 1, character 3: '}}' expected.
  [3]

  $ echo '{ "foo": "Foo"}' > foo.json

Delimiter problems:
  $ PROBLEM=no-closing-mustache.mustache
  $ echo "{{foo" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Line 2, character 0: syntax error.
  [3]

  $ PROBLEM=one-closing-mustache.mustache
  $ echo "{{foo}" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Lines 1-2, characters 6-0: syntax error.
  [3]

  $ PROBLEM=eof-before-variable.mustache
  $ echo "{{" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-section.mustache
  $ echo "{{#" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-section-end.mustache
  $ echo "{{#foo}} {{.}} {{/" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-inverted-section.mustache
  $ echo "{{^" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-unescape.mustache
  $ echo "{{{" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-unescape.mustache
  $ echo "{{&" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-before-partial.mustache
  $ echo "{{>" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Line 2, character 0: ident expected.
  [3]

  $ PROBLEM=eof-in-comment.mustache
  $ echo "{{! non-terminated comment" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Line 2, character 0: non-terminated comment.
  [3]


Mismatches between opening and closing mustaches:

  $ PROBLEM=two-three.mustache
  $ echo "{{ foo }}}" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Lines 1-2, characters 10-0: syntax error.
  [3]

  $ PROBLEM=three-two.mustache
  $ echo "{{{ foo }}" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Lines 1-2, characters 10-0: syntax error.
  [3]


Mismatch between section-start and section-end:

  $ PROBLEM=foo-bar.mustache
  $ echo "{{#foo}} {{.}} {{/bar}}" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Lines 1-2, characters 23-0: Mismatched section 'foo' with 'bar'.
  [3]

  $ PROBLEM=foo-not-closed.mustache
  $ echo "{{#foo}} {{.}} {{foo}}" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Line 2, character 0: syntax error.
  [3]

  $ PROBLEM=wrong-nesting.mustache
  $ echo "{{#bar}} {{#foo}} {{.}} {{/bar}} {{/foo}}" > $PROBLEM
  $ mustache foo.json $PROBLEM
  Lines 1-2, characters 41-0: Mismatched section 'foo' with 'bar'.
  [3]

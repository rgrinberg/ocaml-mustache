  $ echo "{}" > data.json

# Reference example

This test is the reference example from the template-inheritance
specification: https://github.com/mustache/spec/pull/75

  $ cat data.json         # the json data
  {}

  $ cat mypage.mustache   # the page the user would write
  {{<base}}
    {{$header}}
      {{<header}}
        {{$title}}My page title{{/title}}
      {{/header}}
    {{/header}}
    {{$content}}
      <h1>Hello world</h1>
    {{/content}}
  {{/base}}

  $ cat base.mustache     # the base layout
  <html>
    {{$header}}{{/header}}
    <body>
      {{$content}}{{/content}}
    </body>
  </html>

  $ cat header.mustache   # this is slightly overkill...
  <head>
    <title>{{$title}}Default title{{/title}}</title>
  </head>

  $ mustache-ocaml data.json mypage.mustache
  <html>
    <head>
      <title>My page title</title>
    </head>
    <body>
      <h1>Hello world</h1>
    </body>
  </html>


# Indentation of parameter blocks

  $ cat test-indentation.mustache
  <p>
    The test below should be indented in the same way as this line.
    {{$indented-block}}{{/indented-block}}
  </p>

  $ cat test-indent-more.mustache
  {{<test-indentation}}
  {{$indented-block}}
  This text is not indented in the source,
  it should be indented naturally in the output.
  {{/indented-block}}
  {{/test-indentation}}

  $ mustache-ocaml data.json test-indent-more.mustache
  <p>
    The test below should be indented in the same way as this line.
    This text is not indented in the source,
    it should be indented naturally in the output.
  </p>

  $ cat test-indent-less.mustache
  {{<test-indentation}}
      {{$indented-block}}
      This text is very indented in the source,
      it should be indented naturally in the output.
      {{/indented-block}}
  {{/test-indentation}}

  $ mustache-ocaml data.json test-indent-less.mustache
  <p>
    The test below should be indented in the same way as this line.
    This text is very indented in the source,
    it should be indented naturally in the output.
  </p>


# Errors on invalid partial block content

Inside a partial block, we expect parameter blocks. The specification
mandates that text be accepted and ignored, but we error on other tags.

  $ cat invalid-partial-usage.mustache
  {{<base}}
    This text is ignored.
    {{$header}}
      This is a parameter as expected.
    {{/header}}
    {{! commments are also fine }}
    {{#content}}
      This is a typo, it should be $content.
    {{/content}}
  {{/base}}

  $ mustache-ocaml data.json invalid-partial-usage.mustache
  File "invalid-partial-usage.mustache", lines 7-9, characters 2-14:
  Inside the partial block {{< base }}...{{/ base }},
  we expect parameter blocks {{$foo}...{{/foo}} but no other sorts of tags.
  [3]

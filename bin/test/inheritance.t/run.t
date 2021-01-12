  $ echo "{}" > data.json

This test is the reference example from the template-inheritance specification:
https://github.com/mustache/spec/pull/75

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

  $ mustache data.json mypage.mustache
  <html>
    <head>
      <title>My page title</title>
    </head>
    <body>
      <h1>Hello world</h1>
    </body>
  </html>


We also test the indentation of parameter blocks.

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

  $ mustache data.json test-indent-more.mustache
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

  $ mustache data.json test-indent-less.mustache
  <p>
    The test below should be indented in the same way as this line.
    This text is very indented in the source,
    it should be indented naturally in the output.
  </p>

Simple usage:

  $ cat data.json
  { "name": "OCaml",
    "qualities": [{"name": "simple"}, {"name": "fun"}] }

  $ cat hello.mustache
  Hello {{name}}!
  Mustache is:
  {{#qualities}}
  - {{name}}
  {{/qualities}}

  $ mustache data.json hello.mustache
  Hello OCaml!
  Mustache is:
  - simple
  - fun


Using a partial to include a subpage:

  $ cat page.mustache
  <html>
    <body>
      {{>hello}}
    </body>
  </html>

  $ mustache data.json page.mustache
  <html>
    <body>
      Hello OCaml!
      Mustache is:
      - simple
      - fun
    </body>
  </html>


Using a partial with parameters to include a layout around a page:

  $ cat new-post.json
  {
    "title": "New Post",
    "content": "Shiny new content."
  }

  $ cat post.mustache
  {{<page-layout}}
    {{$page-title}}Post: {{title}}{{/page-title}}
    {{$content}}
      <h1>{{title}}</h1>
      <p>{{content}}</p>
    {{/content}}
  {{/page-layout}}

  $ cat page-layout.mustache
  <html>
    <head>
      <title>{{$page-title}}Default Title{{/page-title}}</title>
    </head>
    <body>
      {{$content}}{{/content}}
    </body>
  </html>

  $ mustache new-post.json post.mustache
  <html>
    <head>
      <title>Post: New Post</title>
    </head>
    <body>
      <h1>New Post</h1>
      <p>Shiny new content.</p>
    </body>
  </html>

  $ echo "{}" > data.json

This test is the reference example from the template-inheritance specification:
https://github.com/mustache/spec/pull/75

  $ mustache data.json mypage.mustache
  <html>
      <head>
        <title>My page title</title>
      </head>
    <body>
      <h1>Hello world</h1>
    </body>
  </html>

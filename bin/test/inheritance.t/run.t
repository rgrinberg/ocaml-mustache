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


We also test the indentation of parameter blocks.

  $ mustache data.json test-indent-more.mustache
  <p>
    The test below should be indented in the same way as this line.
  This text is not indented in the source,
  it should be indented naturally in the output.
  </p>

  $ mustache data.json test-indent-less.mustache
  <p>
    The test below should be indented in the same way as this line.
      This text is very indented in the source,
      it should be indented naturally in the output.
  </p>

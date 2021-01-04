ocaml-mustache
==============

mustache.js logic-less templates in OCaml


Example usage
-------------

```ocaml
let tmpl =
  try
    Mustache.of_string "Hello {{name}}\n\
                        Mustache is:\n\
                        {{#qualities}}\
                        * {{name}}\n\
                        {{/qualities}}"
  with Mustache.Parse_error err ->
    Format.eprintf "%a@."
      Mustache.pp_template_parse_error err;
    exit 3

let json =
  `O [ "name", `String "OCaml"
     ; "qualities", `A [ `O ["name", `String "awesome"]
                       ; `O ["name", `String "simple"]
                       ; `O ["name", `String "fun"]
                       ]
     ]

let rendered =
  try Mustache.render tmpl json
  with Mustache.Render_error err ->
    Format.eprintf "%a@."
      Mustache.pp_render_error err;
    exit 2
```

Spec compliance
-----------

ocaml-mustache complies¹ to the latest [mustache specification](https://github.com/mustache/spec/tree/v1.1.3), and is automatically tested against it.

¹: except for lambdas and set delimiters tags.

Todo/Wish List
-----------
* Support for ropes


http://mustache.github.io/

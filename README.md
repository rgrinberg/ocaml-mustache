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

Supported template language
---------------------------

ocaml-mustache accepts the whole Mustache template language, except:
- it does not support setting delimiter tags to something else than '{{' and '}}'.
- it does not support lambdas inside the provided data

It is automatically tested against the latest
[mustache specification testsuite](https://github.com/mustache/spec/tree/v1.1.3).

ocaml-mustache also supports template inheritance / partials with parameters,
tested against the [semi-official specification](https://github.com/mustache/spec/pull/75).

Todo/Wish List
-----------
* Support for ropes


http://mustache.github.io/

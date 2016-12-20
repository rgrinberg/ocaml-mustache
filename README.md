ocaml-mustache
==============

mustache.js logic-less templates in OCaml


Example usage
-------------

```ocaml
let tmpl =
  Mustache.of_string "Hello {{name}}\n\
                      Mustache is:\n\
                      {{#qualities}}\
                      * {{name}}\n\
                      {{/qualities}}"

let json =
  `O [ "name", `String "OCaml"
     ; "qualities", `A [ `O ["name", `String "awesome"]
                       ; `O ["name", `String "simple"]
                       ; `O ["name", `String "fun"]
                       ]
     ]

let rendered =
  Mustache.render tmpl json
```

Todo/Wish List
-----------
* Support for ropes


http://mustache.github.io/

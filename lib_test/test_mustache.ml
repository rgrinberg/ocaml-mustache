open OUnit2
open Mustache

module List = ListLabels
module String = StringLabels

(* [ ( Parser input, expected parser output,
 *     [ (json data, expected rendering) ] ) ] *)

let tests = [
    ( "Hello {{ name }}!"
    , concat [ raw "Hello " ; escaped "name" ; raw "!" ]
    , [ ( `O ["name" , `String "testing"] , "Hello testing!" ) ] ) ;

    ( "{{#bool}}there{{/bool}}"
    , section "bool" (raw "there")
    , [ ( `O ["bool", `Bool true], "there") ;
        ( `O ["bool", `Bool false], "") ] ) ;

    ( "{{#implicit}}{{.}}.{{/implicit}}"
    , section "implicit" (concat [ escaped "." ; raw "." ])
    ,  [ ( `O ["implicit", `A [`String "1" ;
                               `String "2" ;
                               `String "3"] ], "1.2.3.") ;
         ( `O ["implicit", `A [] ], "") ] ) ;

    ( "{{#things}}{{v1}}-{{v2}}{{/things}}"
    , section "things" (concat [ escaped "v1"
                               ; raw "-"
                               ; escaped "v2" ])
    , [ ( `O [ "things" ,
               `A [ `O [ ("v1", `String "1" ) ;
                         ("v2", `String "one" ) ] ;
                    `O [ ("v1", `String "2" ) ;
                         ("v2", `String "two" ) ] ;
                  ] ],
          "1-one2-two" ) ] ) ;

    ( "<style>div { border: {{border}}; }</style>"
    , concat [ raw "<style>div " ;
               raw "{" ;
               raw " border: " ;
               escaped "border" ;
               raw "; " ;
               raw "}" ;
               raw "</style>" ]
    , [ ( `O [ "border", `String "solid"],
          "<style>div { border: solid; }</style>" ) ] ) ;

    ( "<h1>Today{{! ignore me }}.</h1>"
    , concat [ raw "<h1>Today" ;
               comment " ignore me " ;
               raw ".</h1>" ]
    , [ (`O [], "<h1>Today.</h1>") ] );

    ("{{ foo }}\
      {{{ foo }}}\
      {{& foo }}"
    , concat [ escaped "foo" ;
               unescaped "foo" ;
               unescaped "foo" ]
    , [ (`O [ "foo", `String "<b>bar</b>"],
         "&lt;b&gt;bar&lt;/b&gt;\
          <b>bar</b>\
          <b>bar</b>" ) ] ) ;

    ("Hello {{ /foo }}!"
    , concat [ raw "Hello " ;
               escaped "/foo" ;
               raw "!" ]
    , [ (`O [ "/foo", `String "World" ], "Hello World!") ] ) ;

    ("{{& deep/path/string }}"
    , unescaped "deep/path/string"
    , [ (`O [ "deep/path/string", `String "<p>Test content</p>" ],
         "<p>Test content</p>") ] ) ;

    ( "{{#things/with/slashes}}{{v/1/}}-{{v/2/}}{{/things/with/slashes}}"
    , section "things/with/slashes" (concat [ escaped "v/1/"
                                            ; raw "-"
                                            ; escaped "v/2/" ])
    , [ ( `O [ "things/with/slashes" ,
               `A [ `O [ ("v/1/", `String "1" ) ;
                         ("v/2/", `String "one" ) ] ;
                    `O [ ("v/1/", `String "2" ) ;
                         ("v/2/", `String "two" ) ] ;
                  ] ],
          "1-one2-two" ) ] ) ;

    ("{{#a}}{{x}}{{/a}}"
    , section "a" (escaped "x")
    , [ ( `O [ "a" , `A [ `O [ ("x", `Float 1.) ]; `O [ ("x", `Float 2.) ] ] ],
          "12" ) ] ) ;

  ]

let roundtrip : t -> t =
  fun t -> erase_locs (add_dummy_locs t)

let () =

  let assert_equal ?(printer=fun _ -> "") a b =
    fun _ -> assert_equal ~printer a b in

  "Mustache test suite" >:::

    (List.mapi
       (fun i (input, expected_parsing, rendering_tests) ->
        let template =
          try Mustache.of_string input
          with exn ->
            failwith (
              Printf.sprintf "Parsing of test %d failed: %s"
                i (Printexc.to_string exn)
            )
        in
        (Printf.sprintf "%d - erase_locs/add_dummy_locs roundtrip" i
         >:: assert_equal (roundtrip template) template)
        :: (Printf.sprintf "%d - parsing" i
         >:: assert_equal expected_parsing template)
        :: List.mapi (fun j (data, expected) ->
          let rendered =
            try Mustache.render template data
            with exn ->
              failwith (
                Printf.sprintf "Rendering %d of test %d failed: %s"
                  j i (Printexc.to_string exn)
              )
          in
          (Printf.sprintf "%d - rendering (%d)" i j)
          >:: (assert_equal ~printer:(fun x -> x) expected rendered)
        ) rendering_tests
       ) tests
     |> List.flatten)

  |> run_test_tt_main

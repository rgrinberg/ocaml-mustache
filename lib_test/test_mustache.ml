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
    , section "implicit" (concat [ iter_var ; raw "." ])
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

  ]

let () =

  let assert_equal ?(printer=fun _ -> "") a b =
    fun _ -> assert_equal ~printer a b in

  "Mustache test suite" >:::

    (List.mapi
       (fun i (input, expected_parsing, rendering_tests) ->
        let template = Mustache.of_string input in
        (Printf.sprintf "%d - parsing" i
         >:: assert_equal expected_parsing template)
        :: List.mapi (fun j (data, expected) ->
                      (Printf.sprintf "%d - rendering (%d)" i j)
                      >:: (Mustache.render template data
                           |> assert_equal ~printer:(fun x -> x)
                                           expected ) )
                     rendering_tests )
       tests
     |> List.flatten)

  |> run_test_tt_main

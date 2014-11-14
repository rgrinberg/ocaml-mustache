open OUnit2
open Printf
open Sexplib.Std

module List = ListLabels
module String = StringLabels

let list_string_printer s =
  "(" ^ (String.concat ~sep:";" s) ^ ")"

let mustache_printer t =
  t |> Mustache.sexp_of_t |> Sexplib.Sexp.to_string_hum

let string_printer x = x

let mustache1 _ =
  let s = "Hello {{ name }}!" in
  let tmpl = Mustache.of_string s in
  printf "Hello tmplate: %s\n" (mustache_printer tmpl);
  (* printf "tmpl parsed: %s\n" (Mustache.to_string tmpl); *)
  let js = `O ["name", `String "testing"] in
  (* printf "Rendered: %s\n" (Mustache.render tmpl js); *)
  assert_equal ~printer:string_printer "Hello testing!" (Mustache.render tmpl js)

let mustache2 _ =
  let tmpl = Mustache.of_string "{{#bool}}there{{/bool}}" in
  let js' b = `O ["bool", `Bool b] in
  assert_equal "there" (Mustache.render tmpl @@ js' true);
  assert_equal "" (Mustache.render tmpl @@ js' false)

let mustache_section_list1 _ =
  let tmpl = Mustache.of_string "{{#implicit}}{{.}}{{/implicit}}" in
  let js' b =
    let strs = b |> List.map ~f:(fun s -> `String s) in
    `O ["implicit", `A strs]
  in
  assert_equal "" (Mustache.render tmpl @@ js' []);
  assert_equal "onetwothree" (Mustache.render tmpl @@ js' ["one";"two";"three"])

let mustache_section_list2 _ =
  let tmpl = Mustache.of_string "{{#things}}{{v1}}-{{v2}}{{/things}}" in
  let js' pairs =
    let dict (v1,v2) = `O [
      ("v1", (`String v1));
      ("v2", (`String v2))
    ] in
    let pairs = pairs |> List.map ~f:dict in
    `O ["things", `A pairs]
  in
  assert_equal "" (Mustache.render tmpl @@ js' []);
  let vs = [("one","1");("two","2")] in
  assert_equal "one-1two-2" (Mustache.render tmpl @@ js' vs)

let test_scoped _ =
  let tmpl = Mustache.of_string "{{#item}}{{content}}{{/item}}" in
  let js = `O [ "item", `O [ "content", `String "internal"] ] in
  assert_equal "internal" (Mustache.render tmpl js)

let test_html_escape1 _ =
  let html = "<b>foo bar</b>" in
  let escaped = Mustache.escape_html html in
  assert_equal ~printer:(fun s -> s) "&lt;b&gt;foo bar&lt;/b&gt;" escaped

let test_sexp_conversion _ =
  let s = Mustache.of_string "Hello {{ name }}" in
  let s = s |> Mustache.sexp_of_t |> Sexplib.Sexp.to_string in
  Printf.printf "Sexp: %s\n" s;
  assert_bool "created sexp" true

let suite =
  "test mustache" >:::
  [
    "mustache1" >:: mustache1;
    "bool sections" >:: mustache2;
    "mustache section list 1" >:: mustache_section_list1;
    "mustache section list 2" >:: mustache_section_list2;
    "object scoped section" >:: test_scoped;
    "test html escaping" >:: test_html_escape1;
    "test sexp conversion" >:: test_sexp_conversion;
  ]

let () = run_test_tt_main suite

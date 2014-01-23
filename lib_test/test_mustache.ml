open OUnit2
open Printf
module List = ListLabels
module String = StringLabels

let list_string_printer s =
  "(" ^ (String.concat ~sep:";" s) ^ ")"

let token1 _ =
  let s = "  rudi  __ testing _ 123 __ kthulu _ borg_453" in
  let (words, numbers, text) = (ref [], ref [], ref 0) in
  let (e_words, e_numbers, e_text) = (
    ["rudi"; "testing"; "kthulu"; "borg"],
    ["123"; "453"],
    5
  ) in
  let tokens = Mustache.tokenize s [
    (`Word, " *\\([a-z]+\\) *");
    (`Number, " *\\([0-9]+\\) *");
  ] in
  tokens |> List.iter ~f:(function
    | `Text s -> incr text
    | `Token (`Word, s) -> words := s :: (!words)
    | `Token (`Number, s) -> numbers := s :: (!numbers)
  );
  let printer = list_string_printer in
  assert_equal ~printer ~msg:"words" !words (List.rev e_words);
  assert_equal ~printer ~msg:"numbers" !numbers (List.rev e_numbers);
  assert_equal ~msg:"text" ~printer:string_of_int !text e_text


let mustache1 _ =
  let s = "Hello {{ name }}!" in
  let tmpl = Mustache.of_string s in
  printf "tmpl parsed: %s\n" (Mustache.to_string tmpl);
  let open Cow in
  let js = Json.Object ["name", Json.String "testing"] in
  assert_equal "Hello testing!" (Mustache.render tmpl js)

let suite =
  "test mustache" >:::
  [
    "test simple token parsing" >:: token1;
    "mustache1" >:: mustache1;
  ]

let () = run_test_tt_main suite

(* Build with `ocamlbuild -pkg alcotest simple.byte` *)

open Js_parser
open Tokenizer

let pattern = Regex.re_pattern "^hello ([a-z]+)$"
let str = "hello world"

(* The tests *)

let test_regex () =
  Alcotest.(check (option string))
    "regex simple test" (Some "world")
    (Regex.match_and_capture pattern str)

open Parser

let test_parser () =
  let result = Parser.make () |> Parser.parse "" in
  let expected = Program [] in
  assert (result = expected)

(* Run it *)
let () =
  let open Alcotest in
  run "Utils"
    [
      ( "string-case",
        [
          test_case "Simple regex test" `Quick test_regex;
          test_case "Test parser on empty program" `Quick test_parser;
        ] );
    ]

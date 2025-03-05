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

let test_parser () = assert true

(* Run it *)
let () =
  let open Alcotest in
  run "Utils"
    [
      ( "string-case",
        [
          test_case "Simple regex test" `Quick test_regex;
          test_case "Test parser" `Quick test_parser;
        ] );
    ]

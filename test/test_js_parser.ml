(* Build with `ocamlbuild -pkg alcotest simple.byte` *)

(* A module with functions to test *)
module To_test = struct
  (* let lowercase = String.lowercase_ascii *)
  let capitalize = String.capitalize_ascii
  let str_concat = String.concat ""
  let list_concat = List.append
end

open Js_parser
open Tokenizer

let pattern = re_pattern "hello ([a-z]+)"
let str = "hello world"

(* The tests *)

let test_regex () =
  Alcotest.(check (option string))
    "regex simple test" (Some "world")
    (match_and_capture pattern str)

let test_capitalize () =
  Alcotest.(check string) "same string" "World." (To_test.capitalize "world.")

let test_str_concat () =
  Alcotest.(check string)
    "same string" "foobar"
    (To_test.str_concat [ "foo"; "bar" ])

let test_list_concat () =
  Alcotest.(check (list int))
    "same lists" [ 1; 2; 3 ]
    (To_test.list_concat [ 1 ] [ 2; 3 ])

(* Run it *)
let () =
  let open Alcotest in
  run "Utils"
    [
      ( "string-case",
        [
          test_case "Simple regex test" `Quick test_regex;
          test_case "Capitalization" `Quick test_capitalize;
        ] );
      ("string-concat", [ test_case "String mashing" `Quick test_str_concat ]);
      ("list-concat", [ test_case "List mashing" `Slow test_list_concat ]);
    ]

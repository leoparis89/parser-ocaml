let re_pattern str = Re.compile (Re.Perl.re str)

let match_regex pattern str =
  try Some (Re.exec pattern str) with Not_found -> None

let get_capture group match_result =
  try Some (Re.Group.get match_result group) with Not_found -> None

(* Example usage *)
let pattern = re_pattern "hello ([a-z]+)"
let str = "hello world"

let _ =
  match match_regex pattern str with
  | Some result -> (
      (* Get the first capture group *)
      match get_capture 1 result with
      | Some captured -> Printf.printf "Captured: %s\n" captured
      | None -> Printf.printf "No capture found\n")
  | None -> Printf.printf "No match\n"

type token = Semi_colon | String
type t = { _string : string; cursor : int }

let is_EOF t = t.cursor >= String.length t._string
let has_more_tokens t = t.cursor < String.length t._string

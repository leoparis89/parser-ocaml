let re_pattern str = Re.compile (Re.Perl.re str)

let match_regex pattern str =
  try Some (Re.exec pattern str) with Not_found -> None

let get_capture group match_result =
  try Some (Re.Group.get match_result group) with Not_found -> None

let match_and_capture pattern str =
  match match_regex pattern str with
  | Some result -> (
      (* Get the first capture group *)
      match get_capture 1 result with
      | Some captured -> Some captured
      | None -> None)
  | None -> None

(* Example usage *)

type token = Semi_colon | String
type matcher = { pattern : Re.re; token : token option }

let matchers =
  [
    { pattern = re_pattern "^([a-z]+)$"; token = Some String };
    { pattern = re_pattern "^;$"; token = Some Semi_colon };
  ]

type t = { _string : string; cursor : int }

let is_EOF t = t.cursor >= String.length t._string
let has_more_tokens t = t.cursor < String.length t._string

let match_token pattern str (t : t) =
  match match_and_capture pattern str with
  | Some result ->
      (Some result, { t with cursor = t.cursor + String.length result })
  | None -> (None, t)

let rec get_next_token t (matchers : matcher list) =
  match matchers with
  | [] -> (None, t)
  | matcher :: matchers -> (
      let token_value, t = match_token matcher.pattern t._string t in
      match token_value with
      | Some result -> (Some (matcher.token, result), t)
      | None -> get_next_token t matchers)

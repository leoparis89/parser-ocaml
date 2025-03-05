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
type token_value = { token : token; value : string }

let matchers =
  [
    { pattern = re_pattern "^([a-z]+)$"; token = Some String };
    { pattern = re_pattern "^;$"; token = Some Semi_colon };
  ]

type t = { _string : string; mutable cursor : int }

let is_EOF t = t.cursor >= String.length t._string
let has_more_tokens t = t.cursor < String.length t._string

let match_token_pattern pattern str (t : t) =
  match match_and_capture pattern str with
  | Some result ->
      t.cursor <- t.cursor + String.length result;
      Some result
  | None -> None

let get_remaining_string t =
  String.sub t._string t.cursor (String.length t._string - t.cursor)

let rec get_next_token tokenizer (token_matchers : matcher list) =
  if not (has_more_tokens tokenizer) then None
  else
    let remaining_string = get_remaining_string tokenizer in
    let rec loop_matchers t (token_matchers : matcher list) str =
      match token_matchers with
      | [] -> None
      | { pattern; token = None } :: _matchers ->
          (* increments cursor *)
          let _token_value = match_token_pattern pattern str t in
          get_next_token t token_matchers
      | { pattern; token = Some token } :: matchers -> (
          let token_value = match_token_pattern pattern str t in
          match token_value with
          | None -> loop_matchers t matchers str
          | Some result -> Some { token; value = result })
    in
    loop_matchers tokenizer token_matchers remaining_string

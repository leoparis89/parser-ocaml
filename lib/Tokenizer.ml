type token = Semi_colon | String
type t = { _string : string; cursor : int }



let is_EOF t = t.cursor >= String.length t._string
let has_more_tokens t = t.cursor < String.length t._string



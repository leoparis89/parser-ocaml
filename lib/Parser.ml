type statement = string
type ast = Program of statement list

type t = {
  _string : string;
  mutable lookahead : Tokenizer.token_value option;
  tokenizer : Tokenizer.t;
}

let make string =
  { _string = string; lookahead = None; tokenizer = Tokenizer.make string }

let program _t = Program []

let parse parser =
  parser.lookahead <- Tokenizer.get_next_token parser.tokenizer;
  program parser

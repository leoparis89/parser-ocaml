type statement = string
type ast = Program of statement list

type t = {
  mutable _string : string;
  mutable lookahead : Tokenizer.token_value option;
  tokenizer : Tokenizer.t;
}

let make () = { _string = ""; lookahead = None; tokenizer = Tokenizer.make "" }
let program _t = Program []
let expression_statement parser = None

let parse program parser =
  parser.lookahead <- Tokenizer.get_next_token parser.tokenizer;
  parser._string <- program;
  Tokenizer.init parser._string parser.tokenizer;
  Program []

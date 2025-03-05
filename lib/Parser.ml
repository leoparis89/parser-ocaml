type t = {
  mutable _string : string;
  mutable lookahead : Tokenizer.token_value option;
  tokenizer : Tokenizer.t;
}

let make _string =
  { _string; lookahead = None; tokenizer = Tokenizer.make _string }

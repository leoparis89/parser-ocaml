type t = {
  mutable _string : string;
  mutable lookahead : Tokenizer.token_value option;
  tokenizer : Tokenizer.t;
}

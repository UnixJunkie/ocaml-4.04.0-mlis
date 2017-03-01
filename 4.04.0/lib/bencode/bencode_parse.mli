type token =
  | INT of (int)
  | STRING of (string)
  | LIST_START
  | DICT_START
  | END
  | EOF

val bencode :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Bencode_types.t
val bencodes :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Bencode_types.t list

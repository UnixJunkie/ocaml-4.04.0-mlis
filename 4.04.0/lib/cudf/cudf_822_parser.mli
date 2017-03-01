type token =
  | FIELD of (string * (Cudf_types.loc * string))
  | CONT of (Cudf_types.loc * string)
  | EOL
  | EOF

val doc_822 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (string * (Cudf_types.loc * string)) list list
val stanza_822 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (string * (Cudf_types.loc * string)) list option

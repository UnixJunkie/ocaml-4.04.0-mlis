type token =
  | IDENT of (string)
  | PKGNAME of (string)
  | QSTRING of (string)
  | RELOP of (string)
  | POSINT of (string)
  | NEGINT of (string)
  | LBRACKET
  | RBRACKET
  | LPAREN
  | RPAREN
  | COMMA
  | PIPE
  | COLON
  | EQ
  | VPKGTRUE
  | VPKGFALSE
  | EOL

val int_top :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
val ident_top :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
val qstring_top :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
val pkgname_top :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cudf_types.pkgname
val vpkg_top :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cudf_types.vpkg
val vpkglist_top :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cudf_types.vpkglist
val vpkgformula_top :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cudf_types.vpkgformula
val typedecl_top :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cudf_types.typedecl
val type_top :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Cudf_types.typ

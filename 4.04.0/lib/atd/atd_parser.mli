
(* The type of tokens. *)

type token = 
  | UIDENT of ( string )
  | TYPE
  | TILDE
  | TIDENT of ( string )
  | STRING of ( string )
  | STAR
  | SEMICOLON
  | QUESTION
  | OP_PAREN
  | OP_CURL
  | OP_BRACK
  | OF
  | LT
  | LIDENT of ( string )
  | INHERIT
  | GT
  | EQ
  | EOF
  | COMMA
  | COLON
  | CL_PAREN
  | CL_CURL
  | CL_BRACK
  | BAR

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val full_module: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ( Atd_ast.full_module )

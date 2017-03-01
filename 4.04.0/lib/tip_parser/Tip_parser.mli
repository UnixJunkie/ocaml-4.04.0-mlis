
(* The type of tokens. *)

type token = 
  | TRUE
  | RIGHT_PAREN
  | QUOTED of (string)
  | PAR
  | OR
  | NOT
  | MATCH
  | LET
  | LEMMA
  | LEFT_PAREN
  | IF
  | IDENT of (string)
  | FUN
  | FORALL
  | FALSE
  | EXISTS
  | EQ
  | EOI
  | DISTINCT
  | DEFINE_FUN_REC
  | DEFINE_FUNS_REC
  | DEFINE_FUN
  | DEFAULT
  | DECLARE_SORT
  | DECLARE_FUN
  | DECLARE_CONST
  | DATA
  | CHECK_SAT
  | CASE
  | BOOL
  | AT
  | ASSERT_NOT
  | ASSERT
  | AS
  | ARROW
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val parse_ty: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Tip_ast.ty)

val parse_term: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Tip_ast.term)

val parse_list: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Tip_ast.statement list)

val parse: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Tip_ast.statement)

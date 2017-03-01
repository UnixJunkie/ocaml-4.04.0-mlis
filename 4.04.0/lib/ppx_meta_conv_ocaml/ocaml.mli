open Meta_conv

type t =
  | Bool of bool
  | Int31 of int
  | Int63 of int64
  | Int32 of int32
  | Int64 of int64
  | Nativeint32 of int32
  | Nativeint64 of int64
  | Float of float
  | Char of char
  | String of string
  | List of t list
  | Array of t list
  | Variant of string * t list
  | Poly_variant of string * t list
  | Record of (string * t) list
  | Object of (string * t) list
  | Tuple of t list
  | Unit
  | Escaped of string (* Something outside of OCaml values *)
(*
  | Let_rec of string * t * t
  | Var of string
*)

type ocaml = t

val format : ?no_poly: bool -> ?raw_string: bool -> Format.formatter -> t -> unit
val format_with : ?no_poly: bool -> ?raw_string: bool -> ('a -> t) -> Format.formatter -> 'a -> unit
(** [no_poly=true] prints polymorphic variants and objects as non-poly variants and records *)


module Parser : sig
  
  type error = [ `Invalid_construct of Location.t
               | `Lexer of Location.t * Lexer.error
               | `Parser of Syntaxerr.error
	       | `Syntax_error of Location.t
               | `Exn of exn ]

  exception Error of error

  val loc_of_error : error -> Location.t
  val format_error : Format.formatter -> error -> unit

  (** They are not re-entrant, since OCaml's lexer is not. *)
  val from_lexbuf   : Lexing.lexbuf -> t list
  val from_channel  : in_channel -> t list
  val from_string   : string -> t list
  val from_function : (string -> int -> int) -> t list
end

type load_error = [ `Conv of t Error.t | Parser.error ]

val format_load_error : Format.formatter -> load_error -> unit

exception Load_error of load_error

val load_with 
  : (t -> ('a, t Error.t) Result.t) 
  -> string 
  -> [> ('a list, load_error) Result.t ]

val load_with_exn
  : (t -> ('a, t Error.t) Result.t) 
  -> string 
  -> 'a list
(** May raise Load_error *)

val save_with : ('a -> t) -> perm:int -> ?no_poly:bool -> string -> 'a list -> unit
(** perm is unix mode. Use Octal, ex: 0o600 for owner read only *)

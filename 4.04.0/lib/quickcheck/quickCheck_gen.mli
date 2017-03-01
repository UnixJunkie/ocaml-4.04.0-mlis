(*
 * QuickCheck_gen - different generators
 *)

(**

   Generator combinator and types.
**)

type 'a gen = Gen of (int -> 'a)
(** Generator is just a function of random integer seed to value
*)

val sized : (int -> 'a gen) -> 'a gen
(** Used to construct generators that depend on the size parameter.
*)

val resize : int -> 'a gen -> 'a gen
(** Overrides the size parameter. Returns a generator which uses
    the given size instead of the runtime-size parameter.
*)

val promote : ('a -> 'b gen) -> ('a -> 'b) gen
(** Promote generator to a generator of generators
*)

val variant : int -> 'a gen -> 'a gen
(** nodoc  *)

val generate : int -> 'a gen -> 'a
(** Run generator to compute a value
*)

val map_gen : ('a -> 'b) -> 'a gen -> 'b gen
(** Applies function in context of generator
*)

val ret_gen : 'a -> 'a gen
(** Creates generator from function
*)

val (>>=) : 'a gen -> ('a -> 'b gen) -> 'b gen
(** Generators bind
*)

val lift_gen : ('a -> 'b) -> 'a -> 'b gen
(** Lifts function to generator
*)

val choose_int : int * int -> int gen
(** Chooses integer value from given range
*)

val choose_int32 : int32 * int32 -> int32 gen
(** Chooses integer 32bit value from given range
*)

val choose_int0 : int -> int gen
(** Chooses integer value from 0 to given high mark
*)

val choose_char : char * char -> char gen
(** Choose char from given given range
*)

val choose_float : float * float -> float gen
(** Choose float value from given range
*)

val  elements : 'a list -> 'a gen
(** Generates one of the given values
*)

val vector : 'a gen -> int -> 'a list gen
(** Generates a list of given length
*)

val oneof : 'a gen list -> 'a gen
(** Randomly uses one of the given generators
*)

val such_that : ('a -> bool) -> 'a gen -> 'a gen
(** Generates a value, that satisfies a predicate
*)

val frequency : (int * 'a gen) list -> 'a gen
(** Chooses one of the given generators, with a weighted random
    distribution. The input list must be non-empty.
*)

val list : 'a gen -> 'a list gen
(** Generates a list of random length.
    The maximum length depends on the size parameter.
*)

val list1 : 'a gen -> 'a list gen
(** Generates a non-empty list of random length.
    The maximum length depends on the size parameter.
*)

val listN : int -> 'a gen -> 'a list gen
(** Generate list of arbitrary elements with given length
*)

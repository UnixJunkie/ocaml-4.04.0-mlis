(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

val quadruple_of_int32 :
      int32 -> int32 -> (int * int * int * int)

val int32_of_quadruple :
      (int * int * int * int) -> int32 ref -> int32 ref -> unit

(* ======================================================================
 * History:
 * 
 * $Log$
 * 
 *)

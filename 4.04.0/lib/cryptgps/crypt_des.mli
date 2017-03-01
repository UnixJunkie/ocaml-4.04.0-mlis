(* $Id: crypt_des.mli,v 1.1 1999/06/17 14:55:04 gerd Exp $
 * ----------------------------------------------------------------------
 * This module is part of the cryptgps package by Gerd Stolpmann.
 *)

(* DES, created by the NSA, is a 64 bit block cipher,
 * so the following applies:
 *)

module Cryptsystem : Cryptsystem_64.T;;

module Cryptmodes : Cryptmodes_64.T with type key = Cryptsystem.key;;


(* Key management functions:
 *
 * Although only 56 bits count a DES key is written with 64 bits. The
 * extra bits are used for parity checking.
 *)

val check_parity : string -> unit
  (* Checks that the parity of the key given as string is ok. The string
   * must have a length of 8 characters.
   *)

val set_parity : string -> string
  (* Sets the parity bits in the given key and returns a new string. *)


(* - DES has been heavily studied and seems to have a rather good design
 * - DES has only 56 bit keys (this means DES should be considered a
 *   weak algorithm today, but the variant 3DES is still secure)
 * - DES has been designed to be implemented in hardware, and software
 *   implementations are relatively slow
 * - DES is an ANSI and (indirectly) an ISO standard.
 * - There are very few weak keys.
 *)

(* ======================================================================
 * History:
 * 
 * $Log: crypt_des.mli,v $
 * Revision 1.1  1999/06/17 14:55:04  gerd
 * 	Added module for DES.
 *
 *)

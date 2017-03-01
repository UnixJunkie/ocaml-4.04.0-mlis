(* $Id: crypt_3des.mli,v 1.1 1999/06/18 00:23:58 gerd Exp $
 * ----------------------------------------------------------------------
 * This module is part of the cryptgps package by Gerd Stolpmann.
 *)

(* DES, created by the NSA, is a 64 bit block cipher. Triple-DES is a
 * variant that applies DES three times to the same input block. More
 * precisely, first the block is encrypted with key k1, then decrypted
 * with key k2 and then encrypted again with key k3.
 * It is best to choose k1,k2,k3 distinct. Sometimes k1=k3.
 * This implementation accepts strings with 8, 16, or 24 bytes as keys,
 * taking the first 8 bytes as k1, the second as k2, and the third as k3.
 * Please note that some bits are used as parity bits, such that the
 * effective key length is 56, resp. 112, resp. 168 bits.
 *
 * Triple-DES is 3 times slower than DES. 
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
   * must have a length of 8 or 16 or 24 characters.
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
 * $Log: crypt_3des.mli,v $
 * Revision 1.1  1999/06/18 00:23:58  gerd
 * 	First release.
 *
 *)

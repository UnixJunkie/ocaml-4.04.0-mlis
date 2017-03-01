(* $Id: crypt_blowfish.mli,v 1.1 1999/06/04 20:42:01 gerd Exp $
 * ----------------------------------------------------------------------
 * This module is part of the cryptgps package by Gerd Stolpmann.
 *)

(* "Blowfish", created by Bruce Schneier, is a 64 bit block cipher,
 * so the following applies:
 *)

module Cryptsystem : Cryptsystem_64.T;;

module Cryptmodes : Cryptmodes_64.T with type key = Cryptsystem.key;;


(* - Blowfish is one of the fastest ciphers.
 * - Blowfish supports keys from 8 bits to 448 bits. With longer keys
 *   the algorithm does not slow down.
 * - There are weak keys. It is possible to recover some initialization
 *   data for weak keys, but no way to exploit this knowlege has been
 *   reported.
 * - Blowfish is public domain, i.e. free from patents.
 *)

(* ======================================================================
 * History:
 * 
 * $Log: crypt_blowfish.mli,v $
 * Revision 1.1  1999/06/04 20:42:01  gerd
 * 	Initial revision.
 *
 * 
 *)

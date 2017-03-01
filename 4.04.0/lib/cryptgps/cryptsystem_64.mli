(* $Id: cryptsystem_64.mli,v 1.2 2001/03/10 16:43:21 gerd Exp $
 * ----------------------------------------------------------------------
 * This module is part of the cryptgps package by Gerd Stolpmann.
 *)

(* The module type of a cryptsystem using 64 bit block ciphers. 
 * Such a module is normally not used directly to encrypt messages.
 * Only interesting for the average programmer are the key handling
 * functions:
 * - prepare: preprocesses the string representation of the key and get
 *   the internal representation. For some ciphers, this is a very expensive
 *   operation (e.g. for blowfish).
 * - is_weak: for some ciphers, some keys have bad characteristics and 
 *   should not be used to encrypt messages. These keys are called weak.
 *
 * HOW TO TRANSFORM A PASSWORD/PASSPHRASE INTO A KEY:
 *
 * It is not recommended to apply 'prepare' directly to the passphrase that
 * the user types in. As letters are much more likely than other characters,
 * and some bits are never used, the number of different keys would be
 * MUCH smaller than the number of possible keys. To avoid this, apply
 * an MD5 hash function on the ascii representation of the passphrase before
 * passing the value to 'prepare', i.e.
 *
 * let k = prepare (Digest.substring passphrase 0 (String.length passphrase))
 *
 * Note, that the resulting key has not more than 128 bits, even if the
 * passphrase is longer. 
 * To get a 256 bit key, you can concatenate the MD5 of the passphrase
 * and the MD5 of the reverted passphrase.
 * Up to now, 128 bit keys are secure.
 *
 *)

module type T =
  sig
    
    type key
      (* This is the internal, often preprocessed representation of keys. *)

    val encrypt_ecb : 
	key -> (int * int * int * int) -> (int * int * int * int)
      (* This is the ECB mode of the encryption function. The four ints
       * are numbers from 0 to 65535, and given from MSB to LSB.
       *)

    val encrypt_ecb_int32 : 
	key -> int32 -> int32 -> int32 ref -> int32 ref -> unit
      (* The same as encrypt_ecb, but with an int32 interface *)

    val decrypt_ecb : 
	key -> (int * int * int * int) -> (int * int * int * int)
      (* This is the ECB mode of the decryption function. The four ints
       * are numbers from 0 to 65535, and given from MSB to LSB.
       *)

    val decrypt_ecb_int32 : 
        key -> int32 -> int32 -> int32 ref -> int32 ref -> unit
      (* The same as decrypt_ecb, but with an int32 interface *)

    val prepare : string -> key
      (* Prepares the string representation of a key. *)

    val textkey : key -> string
      (* Gets the string representation back *)

    val is_weak : key -> bool
      (* Determines whether the key is known as being weak. Do not use
       * such keys.
       *)
  end
;;
                             


(* ======================================================================
 * History:
 * 
 * $Log: cryptsystem_64.mli,v $
 * Revision 1.2  2001/03/10 16:43:21  gerd
 * 	int32 experiments
 *
 * Revision 1.1  1999/06/04 20:42:01  gerd
 * 	Initial revision.
 *
 * 
 *)

(* $Id: cryptmodes_64.mli,v 1.2 2001/03/10 16:43:21 gerd Exp $
 * ----------------------------------------------------------------------
 * This module is part of the cryptgps package by Gerd Stolpmann.
 *)

(* OVERVIEW:
 *
 * A block cipher encrypts or decrypts a fixed amount of bits on every
 * invocation. Here, we assume that the underlying cipher handles 64 bit
 * blocks as elementary units.
 * If you have a message which is a multiple of 64 bits, you could encrypt
 * every block independently. IT IS STRONGLY RECOMMENDED NOT TO USE THIS
 * SIMPLE APPROACH. This method, often called ECB ("electronic code book"),
 * is vulnerable by plaintext attacks, even if a strong cipher is used.
 * This module implements the following, much better alternatives.
 *
 * ---------------------
 * CIPHER BLOCK CHAINING
 * ---------------------
 *
 * USAGE, LIMITATIONS:
 *
 * - buffer size: the buffer to be en/decrypted must be a multiple of 64 bits.
 * - initialization vector: the ivec used for encryption must be same as the
 *   ivec for decryption. The ivec is not secret, it can be transmitted along
 *   with the ciphertext. It is recommended to use the timestamp as ivec,
 *   or some random bits.
 *
 * SECURITY:
 *
 * - good: 
 *   plaintext patterns (i.e. text structure in the plaintext) is hidden in
 *   the ciphertext 
 * - bad: some manipulations in the ciphertext at the end of the message
 *   are possible. To avoid this, compute an MD5 hash of the message, and
 *   PREPEND the hash value to the message.
 *
 * FAULT-TOLERANCE:
 * 
 * - a bit error in the ciphertext affects the corresponding plaintext and
 *   the following block
 * - no recovery from synchronisation errors possible (missing or extra bits)
 *
 * --------------------
 * CIPHER-FEEDBACK MODE
 * --------------------
 *
 * USAGE, LIMITATIONS:
 *
 * - buffer size: no restrictions
 * - initialization vector: the ivec used for encryption must be same as the
 *   ivec for decryption. The ivec is not secret, it can be transmitted along
 *   with the ciphertext. A different ivec must be used for every transmitted
 *   message, e.g. MD5(timestamp + serial number).
 *
 * SECURITY:
 *
 * - good: 
 *   plaintext patterns (i.e. text structure in the plaintext) is hidden in
 *   the ciphertext 
 * - bad: some manipulations in the ciphertext at the end of the message
 *   are possible. To avoid this, compute an MD5 hash of the message, and
 *   PREPEND the hash value to the message.
 * 
 * FAULT TOLERANCE:
 *
 * - a bit error in the ciphertext affects the corresponding plaintext and
 *   the following block
 * - n-bit CFB can recover from missing n or extra n bits.
 *
 * --------------------
 * OUTPUT-FEEDBACK MODE
 * --------------------
 *
 * USAGE, LIMITATIONS:
 *
 * - buffer size: no restrictions
 * - initialization vector: the ivec used for encryption must be same as the
 *   ivec for decryption. The ivec is not secret, it can be transmitted along
 *   with the ciphertext. A different ivec must be used for every transmitted
 *   message, e.g. MD5(timestamp + serial number).
 *
 * SECURITY:
 *
 * - good: 
 *   plaintext patterns (i.e. text structure in the plaintext) is hidden in
 *   the ciphertext 
 * - bad:
 *   manipulation of bits in the ciphertext directly affects the corresponding
 *   bits in the plaintext
 * 
 * FAULT TOLERANCE:
 *
 * - a bit error in the ciphertext affects only corresponding plaintext bit
 * - n-bit CFB cannot recover from missing or extra bits.
 *
 * --------------
 * RECOMMENDATION
 * --------------
 *
 * - If the encrypted messages are transmitted on a serial line, use CFB-8.
 *   This is the only mode which can recover from synchronization errors on
 *   byte level.
 * - If your message is a multiple of 64 bits, use CBC. If possible, pad
 *   the message to fill up to the next 64 bit multiple, and send the length
 *   of the message, too. 
 * - Otherwise, use CFB-64.
 *)


module type T =
    sig
      
      type key

      (* CIPHER BLOCK CHAINING MODE *)

      val encrypt_cbc :
	  key -> (int * int * int * int) -> string -> 
	    ((int * int * int * int) * string)
	(* Encrypts the string whose length MUST be a multiple of 8 bytes
	 * with the given key and initialization vector, resulting in the
	 * output vector (for the next CBC cascade) and the encrypted string.
	 * The size of the string remains unchanged when it is encrypted.
	 *)

      val decrypt_cbc :
	  key -> (int * int * int * int) -> string -> 
	    ((int * int * int * int) * string)
	(* Decrypts the string whose length MUST be a multiple of 8 bytes
	 * with the given key and initialization vector, resulting in the
	 * output vector (for the next CBC cascade) and the decrypted string.
	 *)

      (* 8 BIT CIPHER-FEEDBACK MODE *)

	      (* This is 8 times slower than CBC *)

      val encrypt_cfb8 :
	  key -> (int * int * int * int) -> string -> 
	    ((int * int * int * int) * string)
	(* Encrypts the string (with arbitraty length) with the given key
	 * and initialization vector, resulting in the output vector (for
	 * the next CFB cascade) and the encrypted string (of the same
	 * length).
	 *)

      val decrypt_cfb8 :
	  key -> (int * int * int * int) -> string -> 
	    ((int * int * int * int) * string)
	(* Decrypts the string (with arbitraty length) with the given key
	 * and initialization vector, resulting in the output vector (for
	 * the next CFB cascade) and the decrypted string (of the same
	 * length).
	 *)

      (* 64 BIT CIPHER-FEEDBACK MODE *)

      val encrypt_cfb64 :
	  key -> (int * int * int * int) -> int -> string -> 
	    ((int * int * int * int) * int * string)
	(* Encrypts the string (with arbitraty length) with the given key
	 * and initialization vector, resulting in the output vector (for
	 * the next CFB cascade) and the encrypted string (of the same
	 * length).
	 * Compared with cfb8, there is an additional int that it passed
	 * to and from this function. It is always in the range 0..7 and
	 * indicates which byte of the 64 bit block comes next. In doubt,
         * pass a 0 as this int.
	 *)

      val decrypt_cfb64 :
	  key -> (int * int * int * int) -> int -> string -> 
	    ((int * int * int * int) * int * string)
	(* Decrypts the string (with arbitraty length) with the given key
	 * and initialization vector, resulting in the output vector (for
	 * the next CFB cascade) and the decrypted string (of the same
	 * length).
	 *)

      (* OUTPUT-FEEDBACK MODE (64 bit) *)

      val crypt_ofb :
	  key -> (int * int * int * int) -> int -> string -> 
	    ((int * int * int * int) * int * string)
	(* Encrypts/Decrypts the string 
	 * with the given key and initialization vector, resulting in the
	 * output vector (for the next OFB cascade) and the encrypted string.
	 * The size of the string remains unchanged when it is encrypted.
	 *)
    end
;;


(* Derives the other modes from the basic ECB mode:
 * 
 * Make_modes: This version is efficient for cryptsystems based on
 *   encrypt_ecb
 * Make_modes_int32: This version is efficient for cryptsystems based on
 *   encrypt_ecb_int32
 *
 * Both functors behave in an equivalent way; the only difference is that
 * Make_modes is fast if M.encrypt_ecb is fast, and that Make_modes_int32
 * is fast if M.encrypt_ecb_int32 is fast.
 *)

module Make_modes (M : Cryptsystem_64.T) : T with type key = M.key
;;


module Make_modes_int32 (M : Cryptsystem_64.T) : T with type key = M.key
;;



(* ======================================================================
 * History:
 * 
 * $Log: cryptmodes_64.mli,v $
 * Revision 1.2  2001/03/10 16:43:21  gerd
 * 	int32 experiments
 *
 * Revision 1.1  1999/06/04 20:42:01  gerd
 * 	Initial revision.
 *
 * 
 *)

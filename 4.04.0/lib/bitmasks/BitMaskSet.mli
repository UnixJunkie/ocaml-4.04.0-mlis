(* ********************************************************************************************** *
 * MetaStack Solutions Ltd.                                                                       *
 * ********************************************************************************************** *
 * BitMask Sets                                                                                   *
 * ********************************************************************************************** *
 * Copyright (c) 2013-14 MetaStack Solutions Ltd.                                                 *
 * ********************************************************************************************** *
 * Author: David Allsopp                                                                          *
 * 27-Dec-2013                                                                                    *
 * ********************************************************************************************** *
 * Redistribution and use in source and binary forms, with or without modification, are permitted *
 * provided that the following two conditions are met:                                            *
 *     1. Redistributions of source code must retain the above copyright notice, this list of     *
 *        conditions and the following disclaimer.                                                *
 *     2. Neither the name of MetaStack Solutions Ltd. nor the names of its contributors may be   *
 *        used to endorse or promote products derived from this software without specific prior   *
 *        written permission.                                                                     *
 *                                                                                                *
 * This software is provided by the Copyright Holder 'as is' and any express or implied           *
 * warranties, including, but not limited to, the implied warranties of merchantability and       *
 * fitness for a particular purpose are disclaimed. In no event shall the Copyright Holder be     *
 * liable for any direct, indirect, incidental, special, exemplary, or consequential damages      *
 * (including, but not limited to, procurement of substitute goods or services; loss of use,      *
 * data, or profits; or business interruption) however caused and on any theory of liability,     *
 * whether in contract, strict liability, or tort (including negligence or otherwise) arising in  *
 * any way out of the use of this software, even if advised of the possibility of such damage.    *
 * ********************************************************************************************** *)

(**
 * Bitmasks exposed as sets. See the documentation for [Set.S] in the Standard Library for details
 * of functions. The ordering used for elements in the set has the least significant bit as the
 * smallest element which corresponds with constructor order of the sum type used for the elements.
 * A complete example may be found in {!Make}.
 *)

module type S =
  sig
    (** All functions and types from the Standard Library's [Set] implementation are available. *)
    include Set.S

    (**/**)

    (* ****************************************************************************************** *
     * Set.Make.find was only added in 4.01.0. It's always exposed in this implementation,        *
     * regardless of OCaml version.                                                               *
     * ****************************************************************************************** *)
    val find : elt -> t -> elt

    (* ****************************************************************************************** *
     * Set.Make.of_list was only added in 4.02.0. It's always exposed in this implementation,     *
     * regardless of OCaml version.                                                               *
     * ****************************************************************************************** *)
    val of_list : elt list -> t

    (**/**)

    type storage
    (** The underlying storage type for the bitmask. *)

    val invalid : t -> t
    (**
     * Returns an empty bitmask where the underlying storage only has any invalid bits remaining.
     *)
  end
(**
 * Signature for Bitmask Sets.
 *)

module type Storage =
  sig
    type storage
    (** The storage type. *)
    val zero : storage
    (** The value for [0]. *)
    val one : storage
    (** The value for [1]. *)
    val logand : storage -> storage -> storage
    (** The [land] operator. *)
    val logor : storage -> storage -> storage
    (** The [lor] operator. *)
    val lognot : storage -> storage
    (** The [lnot] operator. *)
    val shift_left : storage -> int -> storage
    (** The [lsl] operator. *)
    val shift_right_logical : storage -> int -> storage
    (** The [lsr] operator. *)
    val compare : storage -> storage -> int
    (** The [compare] operator. *)
    val toString : storage -> string
    (** Conversion to [string] for the [storage] type. *)
  end
(**
 * Underlying storage type and operations on the storage type. Default implementations are provided
 * in {!Int} and {!Int64}.
 *)

module type BitMask =
  sig
    (**
     * This signature extends the {!Storage} signature.
     *
     * For example, given [type t = A | B | C | D | E] where [A] and [B] correspond to bits 0 and 1,
     * bits 2-4 are unused, [C] corresponds to bit 5, bits 6-10 are unused, and [D] and [E]
     * correspond to bits 11 and 12 then the values would be as follow:
     * - [mask = 0b1100000100011]
     * - [highest = 0b1000000000000]
     * - [lowest = 0b1]
     * - [topbit = 12]
     * - [shifts = [(2, 3); (3, 5)]]
     *)
    include Storage
    type t
    (**
     * The (sum) type used for the elements in the bitmask. Constructors may include values, but the
     * bitmask itself will only be over the constant constructors. Inconsistent values in the
     * structure will break type safety and using a type whose underlying representation is not an
     * integer will almost certainly cause segfaults.
     *)
    val mask : storage
    (** Binary mask of valid bits. *)
    val highest : storage
    (** Value of the highest valid bit. *)
    val lowest : storage
    (** Value of the lowest valid bit. *)
    val topbit : int
    (**
     * The numeric value of the position of the highest valid bit. For example, if {!highest} is
     * [0b10000] then [topbit] is [4]. 
     *)
    val shifts : (int * int) list
    (**
     * Allows for gaps of unused bits in the bitmask without needing dummy constructors in {!t}.
     * Each item specifies a bit number (numbering from [0]) and the number of bits which are
     * unused. The bit number is specified {b as though the unused bits were not present}. See the
     * example above.
     *)
  end
(**
 * Input signature for {!Make} combining {!Storage} with the actual details of a bitmask.
 * See {!Make} for details of its use.
 *)

module Int : Storage with type storage = int
(** Implementation of {!Storage} for type [int] *)
module Int64 : Storage with type storage = int64
(** Implementation of {!Storage} for type [int64] *)

module Make (Mask : BitMask) :
  sig
    type storage = Mask.storage
    (** The underlying storage type. *)
    type t = Mask.storage
    (**
     * The type of bitmasks. This is separate from {!storage} as it will typically be exposed as a
     * [private] type.
     *)
    val create : storage -> t
    (** Creates a bitmask from the storage type (this operation is simply the identity function). *)
    val invalid : Mask.storage -> Mask.storage
    (**
     * [invalid mask] returns a mask where only the bits which are invalid remain set. The handling
     * of invalid bits in the subsequent functions is a compromise between performance and type
     * safety. In general, invalid bits are only ignored if they must ignored to maintain type
     * safety: for use cases where the invalid bits should be ignored, use this function to remove
     * them from the bit mask (e.g. [diff (create x) (invalid x)]).
     *)
    val empty : Mask.storage
    (** The empty bitmask. *)
    val is_empty : Mask.storage -> bool
    (** Tests whether a bitmask is empty or not (includes invalid bits). *)
    val mem : Mask.t -> Mask.storage -> bool
    (** [mem x s] tests whether [x] is set in [s]. *)
    val add : Mask.t -> Mask.storage -> Mask.storage
    (**
     * [add x s] returns a bitmask containing all the elements of [s] with [x] set. If [x] was
     * already set in [s], [s] is returned unchanged.
     *)
    val singleton : Mask.t -> Mask.storage
    (** [singleton x] returns the bitmask with only [x] set. *)
    val remove : Mask.t -> Mask.storage -> Mask.storage
    (**
     * [remove x s] returns a bitmask containing all the elements of [s] with [x] not set. If [x]
     * was not set in [s], [s] is returned unchanged.
     *)
    val union : Mask.storage -> Mask.storage -> Mask.storage
    (** Bitmask union (invalid bits are included). *)
    val inter : Mask.storage -> Mask.storage -> Mask.storage
    (** Bitmask intersection (invalid bits are included). *)
    val diff : Mask.storage -> Mask.storage -> Mask.storage
    (** Bitmask difference (invalid bits are included). *)
    val compare : Mask.storage -> Mask.storage -> int
    (**
     * Total ordering between bitmasks. The presence of differing invalid bits may make two
     * otherwise identical bitmasks differ.
     *)
    val equal : Mask.storage -> Mask.storage -> bool
    (**
     * [equal s1 s2] tests whether the bitmasks [s1] and [s2] are equal, that is, contain the same
     * set elements. The presence of differing invalid bits may make two otherwise identical
     * bitmasks differ.
     *)
    val subset : Mask.storage -> Mask.storage -> bool
    (**
     * [subset s1 s2] tests whether the bitmask [s1] is a subset of the bitmask [s2] (invalid bits
     * are included).
     *)
    val iter : (Mask.t -> unit) -> Mask.storage -> unit
    (**
     * [iter f s] applies [f] in turn to all elements of [s]. The elements of [s] are presented to
     * [f] in increasing ordering with respect to the bit number (i.e. the constructor position
     * within type [Mask.t]).
     *)
    val fold : (Mask.t -> 'a -> 'a) -> Mask.storage -> 'a -> 'a
    (**
     * [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)], where [x1 ... xN] are the elements of
     * [s], in increasing order with respect to the bit number (i.e. the constructor position within
     * type [Mask.t]).
     *)
    val for_all : (Mask.t -> bool) -> Mask.storage -> bool
    (** [for_all p s] checks if all elements of the bitmask satisfy the predicate [p]. *)
    val exists : (Mask.t -> bool) -> Mask.storage -> bool
    (** [exists p s] checks if at least one element of the bitmask satisfies the predicate [p]. *)
    val filter : (Mask.t -> bool) -> Mask.storage -> Mask.storage
    (** [filter p s] returns the bitmask of all elements in [s] that satisfy the predicate [p]. *)
    val partition : (Mask.t -> bool) -> Mask.storage -> Mask.storage * Mask.storage
    (**
     * [partition p s] returns a pair of bitmasks [(s1, s2)], where [s1] is the bitmask of all
     * elements of [s] that satisfy the predicate [p], and [s2] is the bitmask of all the elements
     * of [s] that do not satisfy [p]. [s2] will not contain any invalid bits present in [s].
     *)
    val cardinal : Mask.storage -> int
    (** Return the number of bits which are set in the [bitmask] (does not include invalid bits). *)
    val elements : Mask.storage -> Mask.t list
    (**
     * Return the list of all elements of the given bitmask. The returned list is sorted in
     * increasing order with respect to the bit number (i.e. the constructor position within type
     * Mask.t).
     *)
    val min_elt : Mask.storage -> Mask.t
    (**
     * Return the smallest element of the given bitmask (with respect to the bit number, i.e. the
     * constructor position within type Mask.t) or raise [Not_found] if the bitmask is empty.
     *)
    val max_elt : Mask.storage -> Mask.t
    (** Same as {!min_elt}, but returns the largest element of the given bitmask. *)
    val choose : Mask.storage -> Mask.t
    (**
     * Return one element of the given bitmask, or raise [Not_found] if the bitmask is empty. Which
     * element is chosen is unspecified, but equal elements will be chosen for equal sets (differing
     * invalid bits do not affect this function).
     *)
    val split : Mask.t -> Mask.storage -> Mask.storage * bool * Mask.storage
    (**
     * [split x s] returns a triple [(l, present, r)], where [l] is the bitmask of elements of [s]
     * that are strictly less than [x]; [r] is the bitmask of elements of [s] that are strictly
     * greater than [x] and [present] is [true] if [x] is set in [s]. [l] and [r] will not contain
     * invalid bits.
     *)
    val find : Mask.t -> Mask.storage -> Mask.t
    (** [find x s] returns [x] if [x] is set in [s] or raises [Not_found] if it is not. *)
    val of_list : Mask.t list -> t
    (**
     * [of_list l] creates a bitmask from a list of elements. For bitmasks, this is just a
     * convenience vs folding {!add} over the list.
     *)
  end
(**
 * Functor building an implementation of a BitMaskSet for a given {!BitMask}. Normally, the result
 * of this functor will be constrained to {!S} as:
 * {[module BMSet =
 *  struct
 *    type elt = B0 | B1 | ...
 *
 *    include BitMaskSet.Make(struct ... end)
 *  end]}
 * and the signature may be given as:
 * {[module BMSet :
 *  sig
 *    type elt = B0 | B1 | ...
 *
 *    include BitMaskSet.S with type elt := elt
 *                         with type storage = ...
 *                         with type t = private ...
 *  end]}
 * Note that {!S} does not include {!create}, allowing internal code to create the bitmasks directly
 * but forcing external code to use {!add}, {!singleton} or {!of_list}. This pattern allows you to
 * guarantee that bitmasks will never include invalid bits.
 *)

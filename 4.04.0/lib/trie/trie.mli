(** trie tree
  This module implements strict impure trie tree data structure.
  *)

module type intf =
  sig
    (** type of path point *)
    type path

    (** type of trie node *)
    type 'a node

    (** create a new trie tree with or without an element *)
    val create : 'a option -> 'a node

    (** returns the value associated with the path *)
    val get : 'a node -> path list -> 'a option

    (** associate the value with the path *)
    val set : 'a node -> path list -> 'a -> unit

    (** remove an association of the path *)
    val unset : 'a node -> path list -> unit

    (** returns the sub node associated with the path *)
    val sub: 'a node -> path list -> 'a node option
  end

module Make (H : Core_kernel.Std.Hashtbl.Key): intf with type path:= H.t


(** {1 Type of balanced trees with one custom value} *)
type +'a t = private
  | Leaf
  | Node of int * 'a t * 'a * 'a t
  (** [Node(size, l, x, r)] where:
      - [size] is the number of elements in the tree,
      - [l] is the left sub-tree
      - [x] is a user-defined value
      - [r] is the right sub-tree.

      Trees are guaranteed balanced by construction, the depth of all branches
      is O(log [size]).
  *)

(** Leaf constructor, the empty tree *)
val leaf : 'a t

(** Smart Node constructor, ensuring that the resulting tree is balanced and
    has the appropriate size.

    Cost of [node l x r] is expected to be O(log |[size l] - [size r]|)
    amortized, i.e proportional to the logarithm of the disbalance.
    In particular, if [l] and [r] are similarly-sized, it operates in constant
    time on average.
    NOT PROVEN

    User-values can be moved in different subtrees of the result, but the
    ordering is preserved (so data stay correct if the operation applied on
    values is associative or the relation expected between them is transitive).
*)
val node : 'a t -> 'a -> 'a t -> 'a t

(** {1 Convenience functions} *)

(** Accessor to the size *)
val size : 'a t -> int

(** Concatenate two trees.
    Cost of [join l r] is O(log (min [size l] [size r])).
    NOT PROVEN
*)
val join : 'a t -> 'a t -> 'a t

(** Return the n'th node in tree order *)
val rank : int -> 'a t -> 'a

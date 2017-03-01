
module Acc : sig
  type 'a t
  val create : 'a -> 'a t
  val accum : 'a t -> 'a -> 'a t
  val set_tail : 'a t -> 'a list -> unit
  val return : 'a t -> 'a list
end

module Dacc : sig
  type 'a t
  val create : unit -> 'a t
  val accum : 'a t -> 'a -> 'a t
  val set_tail : 'a t -> 'a list -> unit
  val return : 'a t -> 'a list
end

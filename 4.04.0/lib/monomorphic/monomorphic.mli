(*
Copyright (c) 2014-2015 Jacques-Pascal Deplaix <jp.deplaix@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

module type TY = sig
  type t
end

module MakeInfix (Ty : TY) : sig
  val (=) : Ty.t -> Ty.t -> bool
  val (<>) : Ty.t -> Ty.t -> bool
  val (<) : Ty.t -> Ty.t -> bool
  val (>) : Ty.t -> Ty.t -> bool
  val (<=) : Ty.t -> Ty.t -> bool
  val (>=) : Ty.t -> Ty.t -> bool
end

module MakeCmp (Ty : TY) : sig
  val compare : Ty.t -> Ty.t -> int
  val min : Ty.t -> Ty.t -> Ty.t
  val max : Ty.t -> Ty.t -> Ty.t
end

(** Shadow with specialised functions using [TY.t] *)
module Make (Ty : TY) : sig
  include module type of MakeInfix(Ty)
  include module type of MakeCmp(Ty)
end

(** Almost complete removal of the functions by shadowing *)
module None : module type of Make(struct type t = unit end)

(** Specialize functions with [int] *)
module Int : module type of Make(struct type t = int end)

(** Specialize functions with [float] *)
module Float : module type of Make(struct type t = float end)

type 'a eq = 'a -> 'a -> bool

module Stdlib : sig
  module List : sig
    include module type of List

    val mem : 'a -> eq:'a eq -> 'a list -> bool
    val assoc : 'k -> eq:'k eq -> ('k * 'a) list -> 'a
    val mem_assoc : 'k -> eq:'k eq -> ('k * _) list -> bool
    val remove_assoc : 'k -> eq:'k eq -> ('k * 'a) list -> ('k * 'a) list
  end

  module StdLabels : sig
    include module type of (StdLabels :
                              module type of StdLabels
                            with module List := StdLabels.List
                           )

    module List : sig
      include module type of StdLabels.List

      val mem : 'a -> eq:'a eq -> set:'a list -> bool
      val assoc : 'k -> eq:'k eq -> ('k * 'a) list -> 'a
      val mem_assoc : 'k -> eq:'k eq -> map:('k * _) list -> bool
      val remove_assoc : 'k -> eq:'k eq -> ('k * 'a) list -> ('k * 'a) list
    end
  end

  include module type of None
end

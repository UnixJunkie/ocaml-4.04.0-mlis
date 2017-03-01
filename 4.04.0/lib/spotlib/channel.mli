module Buffer : sig
  type t
  val length : t -> int (* slow *)
  val contents : t -> string
end

(*
class type t = object
  method close : unit
  method read : string -> int -> int -> int
  method read_line : string option
  method read_all : string
  method read_all_lines : string list
end

val in_channel : in_channel -> t
val of_function : ?close: (unit -> unit) -> (string -> int -> int -> int) -> t
val string : string -> t
*)

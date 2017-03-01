open Utils

module Longident : sig
  include module type of struct include Longident end

  val format : Format.t -> t -> unit
  val to_string : t -> string
end
  
module Ident : sig
  include module type of struct include Ident end
      
  val format : Format.formatter -> t -> unit

  val format_verbose : Format.formatter -> t -> unit
  (** Prints also stamp integers *)    
end
  
module Path : sig
  include module type of struct include Path end

  val format : Format.formatter -> t -> unit

  val to_string : t -> string

  val format_verbose : Format.formatter -> t -> unit
  (** Prints also stamp integers *)    
end
  
module Location : sig
  include module type of struct include Location end
      
  val format : Format.formatter -> t -> unit
end

module XParsetree : sig
  open Parsetree

  val iter_core_type : (core_type -> unit) -> core_type -> unit
  (** Iteration on core_type, like Btype.iter_type_expr *)

  val constrs_in_core_type : core_type -> Longident.t list
  (** Referred constrs and classes *)

  val constrs_in_type_declaration : type_declaration -> Longident.t list
  (** Referred constrs and classes *)

  val is_gadt : type_declaration -> bool
  (** Return [true] if it defines a GADT *)

  val scc : ('v * 'v list) list -> 'v list list

end

module Types : sig
  include module type of struct include Types end
      
  val repr_desc : type_expr -> type_desc
  (** repr + desc *)

  val expand_repr_desc : Env.t -> type_expr -> type_desc
  (** expand_head + repr + desc *)

  val with_snapshot : (unit -> 'a) -> 'a
  (** Run the given function. Unifications caused by the function are undo-ed. *)

  val is_constr : Env.t -> type_expr -> (Path.t * type_expr list) option
  (** Check the type is a Tconstr *)

  val is_option_type : Env.t -> type_expr -> type_expr option
  (** Check the type is option *)

  val gen_vars : type_expr -> type_expr list
  (** Generalized tvars *)

  val create_uniq_type : unit -> type_expr
  (** Create a unique data type. Note that the result data type lacks definition. *)

  val close_gen_vars : type_expr -> unit
  (** Unify genvars with unique data types.  Use with_snapshot to recover the original types *)
end

module BytecompOptions(A : sig
  val impl : string -> unit
  val intf : string -> unit
  val anonymous : string -> unit
end) : Main_args.Arg_list

(** The followings are so common in PPX *)

val raise_errorf: ?loc:Location.t -> ?sub:Location.error list -> ?if_highlight:string
            -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** Same as Location.raise_errorf *)

type 'a loc = 'a Location.loc = { txt : 'a; loc : Location.t }
(** Same as Location.loc *)

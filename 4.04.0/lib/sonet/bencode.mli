(*
 * Copyright (C) 2011      Prashanth Mundkur.
 * Author  Prashanth Mundkur <prashanth.mundkur _at_ gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* An implementation of the BitTorrent bencode format. *)

type t =
  | Int of int64
  | String of string
  | List of t list
  | Dict of (string * t) list

val string_of_type: t -> string

val is_int: t -> bool
val is_string: t -> bool
val is_dict: t -> bool
val is_list: t -> bool

val is_scalar: t -> bool

val to_int: t -> int64
val to_string: t -> string
val to_list: t -> t list
val to_dict: t -> (string * t) list

(* first integer argument is the offset into the parse string *)
type error =
  | Unexpected_char of int * char * (* bencode type *) string option
  | Expected_char of int * char * (* bencode type *) string
  | Unterminated_value of int * string
  | Invalid_value of int * string
  | Empty_string of int
  | Invalid_key_type of int * (* bencode type *) string
  | Invalid_string_length of int * string

exception Parse_error of error
val string_of_error: error -> string

val parse: string -> t
val marshal: t -> string

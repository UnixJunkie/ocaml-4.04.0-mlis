(* aio.mli: Linux async I/O interface for libaio-ocaml
 * Copyright (C) 2009 Goswin von Brederlow
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 2.1 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * Under Debian a copy can be found in /usr/share/common-licenses/LGPL-2.1.
 *)

(** libaio-ocaml Linux async I/O interface for ocaml

    This module implements the libaio bindings that interface with the
    Linux system calls.

    {e Version 0.0.0 - goswin-v-b\@web.de }

    {1:top  }
*)

module Buffer : sig
  type t =
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    (** Page aligned buffer for use with Aio. *)

  exception Unaligned
    (** Exception when atempting unaligned access. *)

  external page_size : unit -> int = "caml_aio_buffer_page_size" "noalloc"
    (** Systems page size. Buffers must be multiples of it. *)

  external create : int -> t = "caml_aio_buffer_create"
    (** Allocate an uninitialized buffer. *)

  val clear: t -> unit
    (** zero fill a buffer and rewind *)

  val length : t -> int
    (** Length of the buffer. *)


  val get_int8 : t -> int -> int
  val get_uint8 : t -> int -> int
  val get_int16 : t -> int -> int
  val get_uint16 : t -> int -> int
  val get_int31 : t -> int -> int
  val get_int32 : t -> int -> int32
  val get_int64 : t -> int -> int64
    (** Access with alignment and range checking *)

  val unsafe_get_uint8 : t -> int -> int
  external unsafe_get_int8 : t -> int -> int = "caml_aio_buffer_get_int8" "noalloc"
  external unsafe_get_uint16 : t -> int -> int = "caml_aio_buffer_get_uint16" "noalloc"
  external unsafe_get_int16 : t -> int -> int = "caml_aio_buffer_get_int16" "noalloc"
  external unsafe_get_int31 : t -> int -> int = "caml_aio_buffer_get_int31" "noalloc"
  external unsafe_get_int32 : t -> int -> int32 = "caml_aio_buffer_get_int32"
  external unsafe_get_int64 : t -> int -> int64 = "caml_aio_buffer_get_int64"
    (** Unsafe access without checks. *)

  val set_int8 : t -> int -> int -> unit
  val set_uint8 : t -> int -> int -> unit
  val set_int16 : t -> int -> int -> unit
  val set_uint16 : t -> int -> int -> unit
  val set_int31 : t -> int -> int -> unit
  val set_int32 : t -> int -> int32 -> unit
  val set_int64 : t -> int -> int64 -> unit
    (** Access with alignment and range checking *)

  val unsafe_set_uint8 : t -> int -> int -> unit
  external unsafe_set_int8 : t -> int -> int -> unit = "caml_aio_buffer_set_int8" "noalloc"
  external unsafe_set_uint16 : t -> int -> int -> unit = "caml_aio_buffer_set_uint16" "noalloc"
  external unsafe_set_int16 : t -> int -> int -> unit = "caml_aio_buffer_set_int16" "noalloc"
  val unsafe_set_uint16 : t -> int -> int -> unit
  val unsafe_set_int16 : t -> int -> int -> unit
  external unsafe_set_int31 : t -> int -> int -> unit = "caml_aio_buffer_set_int31" "noalloc"
  external unsafe_set_int32 : t -> int -> int32 -> unit = "caml_aio_buffer_set_int32" "noalloc"
  external unsafe_set_int64 : t -> int -> int64 -> unit = "caml_aio_buffer_set_int64" "noalloc"
    (** Unsafe access without checks. *)

  external unsafe_get_substr : t -> int -> int -> string = "caml_aio_buf_unsafe_get_substr_stub"
    (** unsafe extract string from buffer *)
  val get_substr : t -> int -> int -> string
    (** extract string from buffer *)
  val get_str : t -> string
    (** convert buffer to string *)

  external unsafe_set_substr : t -> int -> string -> unit = "caml_aio_buf_unsafe_set_substr_stub"
    (** unsafe import string from buffer *)
  val set_substr : t -> int -> string -> unit
    (** import string to buffer *)
  val set_str : string -> t
    (** convert string to buffer *)

  (* Big endian byte order *)
  val get_be_int16 : t -> int -> int
  val get_be_uint16 : t -> int -> int
  val get_be_int31 : t -> int -> int
  val get_be_int32 : t -> int -> int32
  val get_be_int64 : t -> int -> int64
    (** Access with alignment and range checking *)

  external unsafe_get_be_int16 : t -> int -> int = "caml_aio_buffer_get_be_int16" "noalloc"
  external unsafe_get_be_uint16 : t -> int -> int = "caml_aio_buffer_get_be_uint16" "noalloc"
  external unsafe_get_be_int31 : t -> int -> int = "caml_aio_buffer_get_be_int31" "noalloc"
  external unsafe_get_be_int32 : t -> int -> int32 = "caml_aio_buffer_get_be_int32"
  external unsafe_get_be_int64 : t -> int -> int64 = "caml_aio_buffer_get_be_int64"
    (** Unsafe access without checks. *)

  val set_be_int16 : t -> int -> int -> unit
  val set_be_uint16 : t -> int -> int -> unit
  val set_be_int31 : t -> int -> int -> unit
  val set_be_int32 : t -> int -> int32 -> unit
  val set_be_int64 : t -> int -> int64 -> unit
    (** Access with alignment and range checking *)

  external unsafe_set_be_int16 : t -> int -> int -> unit = "caml_aio_buffer_set_be_int16" "noalloc"
  external unsafe_set_be_uint16 : t -> int -> int -> unit = "caml_aio_buffer_set_be_uint16" "noalloc"
  external unsafe_set_be_int31 : t -> int -> int -> unit = "caml_aio_buffer_set_be_int31" "noalloc"
  external unsafe_set_be_int32 : t -> int -> int32 -> unit = "caml_aio_buffer_set_be_int32" "noalloc"
  external unsafe_set_be_int64 : t -> int -> int64 -> unit = "caml_aio_buffer_set_be_int64" "noalloc"
    (** Unsafe access without checks. *)


  (* Little endian byte order *)
  val get_le_int16 : t -> int -> int
  val get_le_uint16 : t -> int -> int
  val get_le_int31 : t -> int -> int
  val get_le_int32 : t -> int -> int32
  val get_le_int64 : t -> int -> int64
    (** Access with alignment and range checking *)

  external unsafe_get_le_int16 : t -> int -> int = "caml_aio_buffer_get_le_int16" "noalloc"
  external unsafe_get_le_uint16 : t -> int -> int = "caml_aio_buffer_get_le_uint16" "noalloc"
  external unsafe_get_le_int31 : t -> int -> int = "caml_aio_buffer_get_le_int31" "noalloc"
  external unsafe_get_le_int32 : t -> int -> int32 = "caml_aio_buffer_get_le_int32"
  external unsafe_get_le_int64 : t -> int -> int64 = "caml_aio_buffer_get_le_int64"
    (** Unsafe access without checks. *)

  val set_le_int16 : t -> int -> int -> unit
  val set_le_uint16 : t -> int -> int -> unit
  val set_le_int31 : t -> int -> int -> unit
  val set_le_int32 : t -> int -> int32 -> unit
  val set_le_int64 : t -> int -> int64 -> unit
    (** Access with alignment and range checking *)

  external unsafe_set_le_int16 : t -> int -> int -> unit = "caml_aio_buffer_set_le_int16" "noalloc"
  external unsafe_set_le_uint16 : t -> int -> int -> unit = "caml_aio_buffer_set_le_uint16" "noalloc"
  external unsafe_set_le_int31 : t -> int -> int -> unit = "caml_aio_buffer_set_le_int31" "noalloc"
  external unsafe_set_le_int32 : t -> int -> int32 -> unit = "caml_aio_buffer_set_le_int32" "noalloc"
  external unsafe_set_le_int64 : t -> int -> int64 -> unit = "caml_aio_buffer_set_le_int64" "noalloc"
    (** Unsafe access without checks. *)

(* Network byte order *)
  val get_net_int16 : t -> int -> int
  val get_net_uint16 : t -> int -> int
  val get_net_int31 : t -> int -> int
  val get_net_int32 : t -> int -> int32
  val get_net_int64 : t -> int -> int64
    (** Access with alignment and range checking *)

  val unsafe_get_net_int16 : t -> int -> int
  val unsafe_get_net_uint16 : t -> int -> int
  val unsafe_get_net_int31 : t -> int -> int
  val unsafe_get_net_int32 : t -> int -> int32
  val unsafe_get_net_int64 : t -> int -> int64
    (** Unsafe access without checks. *)

  val set_net_int16 : t -> int -> int -> unit
  val set_net_uint16 : t -> int -> int -> unit
  val set_net_int31 : t -> int -> int -> unit
  val set_net_int32 : t -> int -> int32 -> unit
  val set_net_int64 : t -> int -> int64 -> unit
    (** Access with alignment and range checking *)

  val unsafe_set_net_int16 : t -> int -> int -> unit
  val unsafe_set_net_uint16 : t -> int -> int -> unit
  val unsafe_set_net_int31 : t -> int -> int -> unit
  val unsafe_set_net_int32 : t -> int -> int32 -> unit
  val unsafe_set_net_int64 : t -> int -> int64 -> unit
    (** Unsafe access without checks. *)
end


type result =
    Result of Buffer.t
  | Errno of int
  | Partial of Buffer.t * int
  (** The type for a result of a completed I/O request *)

exception Error of int
  (** An error has occured during a request. *)

exception Incomplete of Buffer.t * int
  (** A request was only partialy completed. *)

val result : result -> Buffer.t
  (** Extract the Buffer.t from a result or throw the proper exception *)

type context
  (** The type for a libaio Context. *)

val context : int -> context
  (** Create a new context for n simultaneous requests. *)

val read : context -> Unix.file_descr -> int64 -> Buffer.t -> (result -> unit) -> unit
  (** fill buffer from file at given offset and call continuation *)

val write : context -> Unix.file_descr -> int64 -> Buffer.t -> (result -> unit) -> unit
  (** write buffer to file at given offset and call continuation *)

val poll : context -> Unix.file_descr -> int -> (Unix.file_descr -> unit) -> unit
  (** poll file descriptor and call continuation *)

val run : context -> unit
  (** run the context till there are no more pending requests *)

val process : context -> unit
  (** process finished events and return *)

val fd : context -> Unix.file_descr
  (** return eventfd associated with the context *)

val sync_read : Unix.file_descr -> int64 -> Buffer.t -> unit
  (** fill buffer from file at given offset, blocking *)

val sync_write : Unix.file_descr -> int64 -> Buffer.t -> unit
  (** write buffer to file at given offset, blocking *)

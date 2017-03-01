(***********************************************************************)
(* The ocaml-libevent library                                          *)
(*                                                                     *)
(* Copyright 2002, 2003, 2004 Maas-Maarten Zeeman. All rights reserved *)
(* Copyright 2010 ygrek                                                *)
(* See LICENCE for details.                                            *)
(***********************************************************************)


(** The ocaml-libevent library provides an interface to the libevent API.

    The libevent API provides a mechanism to execute a function when a
    specific event on a file descriptor occurs or after a given time
    has passed.

    This library is a wrapper of the libevent API made by Nils
    Provos. For more information about this library see:
    http://libevent.org

    Currently, libevent supports kqueue(2), select(2), poll(2) and
    epoll(4). Support for /dev/poll is planned.

    @author Maas-Maarten Zeeman
*)

(** The type of events *)
type event

(** The type of event base *)
type event_base

(** The possible event types *)
type event_flags =
    TIMEOUT (** A timeout occurred. *)
  | READ    (** A read is possible. *)
  | WRITE   (** A write operation is possible. *)
  | SIGNAL  (** A signal occurred. *)

type event_callback = Unix.file_descr -> event_flags -> unit
(** The type of event callbacks *)

(** {5 Basic Libevent Operations} *)

val create : unit -> event
(** Create a new empty event *)

val fd : event -> Unix.file_descr
(** [fd event] returns the file descriptor associated with the event *)

val signal : event -> int
(** [signal event] returns the signal associated with the event *)

val set : event_base -> event ->
  Unix.file_descr -> event_flags list -> persist:bool -> event_callback -> unit
(** [set events event fd type persist callback] initializes the event for use with [events]. The
    flag [persist] makes an event persitent until {!Libevent.del} is
    called. Event can be [set] multiple times, only the last one will be active *)

val set_timer : event_base -> event -> persist:bool -> (unit -> unit) -> unit
(** [set_timer events event persist callback] initializes timer. Flag [persist]
    makes the timer periodic until {!Libevent.del} is called. *)

val set_signal : event_base -> event ->
  signal:int -> persist:bool -> event_callback -> unit
(** [set_signal event signal persist callback] initializes the event. The
    flag [persist] makes an event persistent until {!Libevent.del} is
    called. *)

val add : event -> float option -> unit
(** [add event timeout] makes the [event] pending - schedules the execution
    of the function specified with {!Libevent.set}, or in at least the
    time specified in the [timeout]. If [timeout] is [None], no
    timeout occures, and the function will only be called if a
    matching event occurs on the file descriptor. Addition of the already
    scheduled (added) event will reschedule the timeout. *)

val del : event -> unit
(** Delete the event. After event was deleted it should be first
    reinitialized with [set] before next [add]. *)

val pending : event -> event_flags list -> bool
(** @return whether event is in the pending state for the given type of events,
    i.e. whether it was [add]ed *)

val activate : event -> event_flags list -> unit
(** make an event active, so that the corresponding callback is run. Event may be in
    pending or non-pending state *)

(** {5 Process Events} *)

val dispatch : event_base -> unit
(** In order to process events, an application needs to call dispatch. This
 *  function only returns on error, and should replace the event core of the
 *  application
 *)

type loop_flag = ONCE | NONBLOCK
val loop : event_base -> loop_flag -> unit
(** Obsolete, use [loops] *)

val loops : event_base -> loop_flag list -> unit
(** Provides an interface for single pass execution of pending events *)

val init : unit -> event_base
(** Initialize event base. *)

val reinit : event_base -> unit
(** Reinitialize event base (use after fork) *)

val free : event_base -> unit
(** destroy event base *)

(** Compatibility *)
module Global : sig

val base : event_base
val init : unit -> unit

val set : event -> Unix.file_descr -> event_flags list -> persist:bool -> event_callback -> unit
val dispatch : unit -> unit
val loop : loop_flag -> unit
val loops : loop_flag list -> unit

end

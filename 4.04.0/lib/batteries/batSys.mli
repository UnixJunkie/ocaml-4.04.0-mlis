# 1 "src/batSys.mliv"
(*
 * BatSys - additional and modified functions for System
 * Copyright (C) 1996 Xavier Leroy
 * Copyright (C) 2009 David Teller, LIFO, Universite d'Orleans
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** System interface.

    This module defines higher-level functions than the {!Unix} module
    and should, wherever possible, be used rather than the {!Unix} module
    to ensure portability.

    @author Xavier Leroy (Base module)
    @author David Teller
*)

val argv : string array
(** The command line arguments given to the process.
    The first element is the command name used to invoke the program.
    The following elements are the command-line arguments
    given to the program. *)

val executable_name : string
(** The name of the file containing the executable currently running. *)

external file_exists : string -> bool = "caml_sys_file_exists"
(** Test if a file with the given name exists. *)

external is_directory : string -> bool = "caml_sys_is_directory"
(** Returns [true] if the given name refers to a directory,
    [false] if it refers to another kind of file.
    @raise Sys_error if no file exists with the given name.
    @since 3.10.0
*)

external remove : string -> unit = "caml_sys_remove"
(** Remove the given file name from the file system. *)

external rename : string -> string -> unit = "caml_sys_rename"
(** Rename a file. The first argument is the old name and the
    second is the new name. If there is already another file
    under the new name, [rename] may replace it, or raise an
    exception, depending on your operating system. *)

external getenv : string -> string = "caml_sys_getenv"
(** Return the value associated to a variable in the process
    environment. @raise Not_found if the variable is unbound. *)

external command : string -> int = "caml_sys_system_command"
(** Execute the given shell command and return its exit code. *)

external time : unit -> (float [@unboxed]) =
  "caml_sys_time" "caml_sys_time_unboxed" [@@noalloc]
# 70 "src/batSys.mliv"
(** Return the processor time, in seconds, used by the program
    since the beginning of execution. *)

external chdir : string -> unit = "caml_sys_chdir"
(** Change the current working directory of the process. *)

external getcwd : unit -> string = "caml_sys_getcwd"
(** Return the current working directory of the process. *)

external readdir : string -> string array = "caml_sys_read_directory"
(** Return the names of all files present in the given directory.
    Names denoting the current directory and the parent directory
    (["."] and [".."] in Unix) are not returned.  Each string in the
    result is a file name rather than a complete path.  There is no
    guarantee that the name strings in the resulting array will appear
    in any specific order; they are not, in particular, guaranteed to
    appear in alphabetical order. *)

val interactive : bool ref
(** This reference is initially set to [false] in standalone
    programs and to [true] if the code is being executed under
    the interactive toplevel system [ocaml]. *)

val os_type : string
(** Operating system currently executing the OCaml program. One of
    -  ["Unix"] (for all Unix versions, including Linux and Mac OS X),
    -  ["Win32"] (for MS-Windows, OCaml compiled with MSVC++ or Mingw),
    -  ["Cygwin"] (for MS-Windows, OCaml compiled with Cygwin). *)

type backend_type = Sys.backend_type =
  | Native
  | Bytecode
  | Other of string (**)
(** Currently, the official distribution only supports [Native] and
    [Bytecode], but it can be other backends with alternative
    compilers, for example, javascript.

    @since 2.5.3 and 4.04 *)

val backend_type : backend_type
(** Backend type currently executing the OCaml program.
    @ since 2.5.3 and 4.04
 *)
# 113 "src/batSys.mliv"

val unix : bool
(** True if [Sys.os_type = "Unix"].
    @since 4.01.0 *)

val win32 : bool
(** True if [Sys.os_type = "Win32"].
    @since 4.01.0 *)

val cygwin : bool
(** True if [Sys.os_type = "Cygwin"].
    @since 4.01.0 *)
# 125 "src/batSys.mliv"

val word_size : int
(** Size of one word on the machine currently executing the OCaml
    program, in bits: 32 or 64. *)

val int_size : int
(** Size of an int.  It is 31 bits (resp. 63 bits) when using the
    OCaml compiler on a 32 bits (resp. 64 bits) platform.  It may
    differ for other compilers, e.g. it is 32 bits when compiling to
    JavaScript.
    @since 2.5.0 and OCaml 4.03.0 *)
# 136 "src/batSys.mliv"

val big_endian : bool
(** Whether the machine currently executing the OCaml program is big-endian.
    @since 4.00.0 *)

val max_string_length : int
(** Maximum length of a string. *)

val max_array_length : int
(** Maximum length of a normal array.  The maximum length of a float
    array is [max_array_length/2] on 32-bit machines and
    [max_array_length] on 64-bit machines. *)

external runtime_variant : unit -> string = "caml_runtime_variant"
(** Return the name of the runtime variant the program is running on.
    This is normally the argument given to [-runtime-variant] at compile
    time, but for byte-code it can be changed after compilation.
    @since 2.5.0 and OCaml 4.03.0 *)

external runtime_parameters : unit -> string = "caml_runtime_parameters"
(** Return the value of the runtime parameters, in the same format
    as the contents of the [OCAMLRUNPARAM] environment variable.
    @since 2.5.0 and OCaml 4.03.0 *)
# 159 "src/batSys.mliv"


(** {6 Signal handling} *)


type signal_behavior = Sys.signal_behavior =
    Signal_default
  | Signal_ignore
  | Signal_handle of (int -> unit)
  (** What to do when receiving a signal:
      - [Signal_default]: take the default behavior
       (usually: abort the program)
      - [Signal_ignore]: ignore the signal
      - [Signal_handle f]: call function [f], giving it the signal
      number as argument. *)

external signal :
  int -> signal_behavior -> signal_behavior = "caml_install_signal_handler"
(** Set the behavior of the system on receipt of a given signal.  The
    first argument is the signal number.  Return the behavior
    previously associated with the signal.
    @raise Invalid_argument If the signal number is
    invalid (or not available on your system). *)

val set_signal : int -> signal_behavior -> unit
(** Same as {!Sys.signal} but return value is ignored. *)


(** {7 Signal numbers for the standard POSIX signals.} *)

val sigabrt : int
(** Abnormal termination *)

val sigalrm : int
(** Timeout *)

val sigfpe : int
(** Arithmetic exception *)

val sighup : int
(** Hangup on controlling terminal *)

val sigill : int
(** Invalid hardware instruction *)

val sigint : int
(** Interactive interrupt (ctrl-C) *)

val sigkill : int
(** Termination (cannot be ignored) *)

val sigpipe : int
(** Broken pipe *)

val sigquit : int
(** Interactive termination *)

val sigsegv : int
(** Invalid memory reference *)

val sigterm : int
(** Termination *)

val sigusr1 : int
(** Application-defined signal 1 *)

val sigusr2 : int
(** Application-defined signal 2 *)

val sigchld : int
(** Child process terminated *)

val sigcont : int
(** Continue *)

val sigstop : int
(** Stop *)

val sigtstp : int
(** Interactive stop *)

val sigttin : int
(** Terminal read from background process *)

val sigttou : int
(** Terminal write from background process *)

val sigvtalrm : int
(** Timeout in virtual time *)

val sigprof : int
(** Profiling interrupt *)

val sigbus : int
(** Bus error
    @since 2.5.0 *)

val sigpoll : int
(** Pollable event
    @since 2.5.0 *)

val sigsys : int
(** Bad argument to routine
    @since 2.5.0 *)

val sigtrap : int
(** Trace/breakpoint trap
    @since 2.5.0 *)

val sigurg : int
(** Urgent condition on socket
    @since 2.5.0 *)

val sigxcpu : int
(** Timeout in cpu time
    @since 2.5.0 *)

val sigxfsz : int
(** File size limit exceeded
    @since 2.5.0 *)


exception Break
(** Exception raised on interactive interrupt if {!Sys.catch_break}
    is on. *)


val catch_break : bool -> unit
(** [catch_break] governs whether interactive interrupt (ctrl-C)
    terminates the program or raises [Break].
    Call [catch_break true] to enable raising [Break],
    and [catch_break false] to let the system
    terminate the program on user interrupt. *)


val ocaml_version : string;;
(** [ocaml_version] is the version of OCaml.
    It is a string of the form ["major.minor[.patchlevel][+additional-info]"],
    where [major], [minor], and [patchlevel] are integers, and
    [additional-info] is an arbitrary string. The [[.patchlevel]] and
    [[+additional-info]] parts may be absent. *)


val files_of: string -> string BatEnum.t
    (**As {!readdir} but the results are presented as an enumeration
       of names.*)

val enable_runtime_warnings: bool -> unit
(** Control whether the OCaml runtime system can emit warnings
    on stderr.  Currently, the only supported warning is triggered
    when a channel created by [open_*] functions is finalized without
    being closed.  Runtime warnings are enabled by default.
    @since 2.5.0 and OCaml 4.03 *)
# 312 "src/batSys.mliv"

val runtime_warnings_enabled: unit -> bool
(** Return whether runtime warnings are currently enabled.
    @since 2.5.0 and OCaml 4.03 *)
# 316 "src/batSys.mliv"

(** {6 Optimization} *)

external opaque_identity : 'a -> 'a = "%opaque"
# 321 "src/batSys.mliv"
(** For the purposes of optimization, [opaque_identity] behaves like an
    unknown (and thus possibly side-effecting) function.

    At runtime, [opaque_identity] disappears altogether.

    A typical use of this function is to prevent pure computations from being
    optimized away in benchmarking loops.  For example:
    {[
      for _round = 1 to 100_000 do
        ignore (Sys.opaque_identity (my_pure_computation ()))
      done
    ]}

    The compiler primitive was added to OCaml 4.03, but we emulate it
    under 4.02 using the -opaque compilation flag. There is no easy
    way for Batteries to emulate it correctly under older OCaml
    versions.

    @since 2.5.0 and OCaml 4.02
 *)

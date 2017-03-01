open Ast_mapper

val handle_error : (unit -> 'a) -> 'a
(** [handle_error f] executes [f ()] and if it raises exception
    it reports it and exit with status 2. *)

val run :
  (string * Arg.spec * string) list
  -> string
  -> (unit -> unit)
  -> mapper
  -> unit
(** [run opts name initf mapepr] runs the [mapper]. 
    [name] is used for reporting errors raised by [mapper].
    [opts] is command line options for the PPXX command additional to
    the default ones provided by Ppxx.

    [initf] is for initialization which is called prior to the processing of each input file.
*)

val cp : string list -> Unix.process_status * unit

val mv : string list -> Unix.process_status * unit

val rm : string list -> Unix.process_status * unit

val cat : string list -> Unix.process_status * unit

val cmp : string -> string -> [`Same | `Different | `Error]
(** Execute "cmp", the file comparison unix command by execvp *)
  
val file : string -> (string option, Unix.process_status) Result.t
(** Execute "file path" *)

val grep : 
  string list
  -> init:'state
  -> f: ('state -> [ `Err | `Out ] * [ `EOF | `Read of string ] -> 'state) 
  -> Unix.process_status * 'state
(** Run grep command *)

val grep_ : string list -> Unix.process_status * unit
(** Run grep command but just returns the result *)

module V2 : sig
  val com : string -> string list -> unit
  val cp : string list -> unit
  val mv : string list -> unit
  val rm : string list -> unit
  val cat : string list -> unit
  val file :
    string -> [> `Error of Unix.process_status | `Ok of string option ]
  val grep :
    string list ->
    'a -> (Command.output -> 'a -> 'a) -> 'a * Unix.process_status
  val grep_ : string list -> unit * Unix.process_status
  val cmp : string -> string -> [> `Different | `Error | `Same ]
end

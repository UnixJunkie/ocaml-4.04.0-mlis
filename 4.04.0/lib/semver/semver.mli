(** Simple semantic versioning module *)

(** Semantic version consisting of major/minor/patch components *)
type t = int * int * int

(** Part labels *)
type version_part = [
  | `Major (** First part of version *)
  | `Minor (** Second part of version *)
  | `Patch (** Third part of version *)
]

(** Compare two versions *)
val compare : t -> t -> int

(** succ [vpart] [v] increments [vpart] component in [v] *)
val succ  : version_part -> t -> t

(** pred [vpart] [v] decrements [vpart] component in [v] *)
val pred : version_part -> t -> t

(** Parse a semantic version from a string *)
val of_string : string -> t option

(** Convert a semantic version to a string *)
val to_string : t -> string

module Query : sig
  type t = [
    | `Patch of int * int * int
    | `Minor of int * int
    | `Major of int
  ]

  val of_string : string -> t option
  val to_string : t -> string
end

val query : Query.t -> t list -> t option

val query_str : string -> t list -> t option

val untyped_identity : Ast_mapper.mapper
(** The identity of untyped transformation *)

module Typemod : S.Typemod
(** The vanilla OCaml's type inference algorithm *)

module Typed_identity : S.TypedTransformation
(** The identity of typed transformation *)

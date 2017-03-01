open Camlp4.PreCast.Ast

(** { 6 Tools } *)

val from_to : int -> int -> int list
(** [ from_to f t = [f; f+1; ..; t] ] *)

val (--) : int -> int -> int list
(** Same as [from_to] *)



(** { 6 Idents and Paths } *)

val mk_idents : string -> int -> ident list
(** [mk_idents name n] creates idents from name1 to namen *) 

val change_id : (string -> string) -> ident -> ident
(** A.x => A.y where y = f x *)

val label_of_path : ident -> string
(** Abcc.x => abc_dot_x *)

val name_of_ident : ident -> string
(** a => a *)

val convert_path : string list -> ident
(** ["A"; "B"; 'c'] => A.B.c *)

val expr_of_id : ident -> expr
val patt_of_id : ident -> patt
(** [expr_of_id id] and [patt_of_id id] create an expr or patt of [id] correspondingly *)


val strip_locs_of_ident : ident -> ident
(** strips off location information to normalize [ident] *)

val same_idents : ident -> ident -> bool
(** returns true if two idents are the same, ignoring the locations *)

(** { 6 Tvars } *)

val patt_of_tvar : ctyp -> patt
(** [patt_of_tvar tv] creates a pattern variable for a type variable [tv] *)
val expr_of_tvar : ctyp -> expr
(** [expr_of_tvar tv] creates an expression variable for a type variable [tv] *)




(** { 6 Creators } *)

val create_patt_app : patt -> patt list -> patt
(** [create_patt_app const args] creates a pattern of variant constructor like 
    const (arg1,..,argn) *)

val create_expr_app : expr -> expr list -> expr
(** [create_expr_app const args] creates an expr of variant constructor like 
    const (arg1,..,argn) 
  This is a variant of Gen.create_expr_app *)

val create_tuple : expr list -> expr
(** e1, e2, ..., en => (e1,...,en) 
    Do not use this for variant creations, since variants are curried in P4.
*)

val create_patt_tuple : patt list -> patt
(** p1, p2, ..., pn => (p1,...,pn) 
    Do not use this for variant creations, since variants are curried in P4.
*)

val create_list : expr list -> expr
(** e1, e2, ..., en => [e1;...,en] *)

val create_patt_list : patt list -> patt
(** p1, p2, ..., pn => [p1;...,pn] *)

val create_record : (ident * expr) list -> expr
(** l1,e1, ... ln,en => { l1:e1; ...; ln:en } *)

val create_object : (string * expr) list -> expr
(** l1,e1, ... ln,en => object method l1 = e1; ...; method ln = en end *)

val create_top_let : bool -> binding list -> str_item



(** { 6 Concatenations } *)

val concat_let_bindings : binding list -> binding
val concat_class_str_items : class_str_item list -> class_str_item
val concat_str_items : str_item list -> str_item
val concat_sig_items : sig_item list -> sig_item




(** { 6 Deconstruction } *)

val split_by_comma : expr -> expr list

val deconstr_tydef : ctyp 
  -> ([> `Alias of Loc.t * ctyp
      | `Mani of Loc.t * ctyp * 'a
      | `Nil of Loc.t
      | `Record of Loc.t * (loc * ident * ctyp) list
      | `Sum of Loc.t * (loc * ident * ctyp list) list
      | `Variant of Loc.t * (loc * string * ctyp list) list ] as 'a)

val deconstr_variant_type : ctyp -> loc * (loc * string * ctyp list) list

val deconstr_object_type : ctyp -> loc * (loc * ident * ctyp) list * row_var_flag

val type_definitions_are_recursive : bool -> ctyp -> bool


(** { 6 Type construction } *)

val create_param_type : ctyp list -> string -> ctyp
(** (p1, p2, .., pn) name *)

val create_for_all : ctyp list -> ctyp -> ctyp
(** p1 ... pn . ty *)

val create_type_quant : ctyp list -> ctyp -> ctyp
(** type p1 ... pn . ty *)

val create_object_type : bool (** poly or not *) -> (string * ctyp) list -> ctyp
(** < x : t; ...; .. > *)



(** { 6 Strippers } *)

val strip_field_flags : ctyp -> ctyp
(** strip mutable+private flags *)

val strip_ident_loc : ident -> ident
(** strip location *)



(** { 6 Error } *)

val errorf : Loc.t -> ('failure, unit, string, 'a) format4 -> 'failure

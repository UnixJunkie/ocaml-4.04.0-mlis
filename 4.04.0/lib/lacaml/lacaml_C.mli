(* File: CZ.mli

   Copyright (C) 2010-

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umons.ac.be/an/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

(** This module [Lacaml.C] contains linear algebra routines for
    complex numbers (precision: complex32).  It is recommended to use this
    module by writing
    {[
    open Lacaml.C
    ]}
    at the top of your file.  *)

open Bigarray

type prec = complex32_elt
type num_type = Complex.t

type vec = (Complex.t, complex32_elt, fortran_layout) Array1.t
(** Complex vectors (precision: complex32). *)

type rvec = (float, float32_elt, fortran_layout) Array1.t
(** Vectors of reals (precision: float32). *)

type mat = (Complex.t, complex32_elt, fortran_layout) Array2.t
(** Complex matrices (precision: complex32). *)

type trans3 = [ `C | `N | `T ]
(** Transpose parameter (conjugate transposed, normal, or transposed). *)

val prec : (Complex.t, complex32_elt) Bigarray.kind
(** Precision for this submodule {!C}.  Allows to write precision
    independent code. *)

module Vec : sig
  type t = vec

  (** {5 Vector operations} *)

  open Lacaml_complex32

  (** {6 Creation of vectors} *)

  val random :
    ?rnd_state : Random.State.t ->
    ?re_from : float -> ?re_range : float ->
    ?im_from : float -> ?im_range : float ->
    int
    -> vec
  (** [random ?rnd_state ?re_from ?re_range ?im_from ?im_range n]
      @return a vector of size [n] initialized with random elements sampled
      uniformly from [re_range] and [im_range] starting at [re_from] and
      [im_from] for real and imaginary numbers respectively.  A random state
      [rnd_state] can be passed.

      @param rnd_state default = Random.get_state ()
      @param re_from default = -1.0
      @param re_range default = 2.0
      @param im_from default = -1.0
      @param im_range default = 2.0 *)

  open Lacaml_common
  open Lacaml_complex32
  open Types.Vec

  (** {6 Creation/conversion of vectors and dimension accessor} *)

  val create : int -> vec
  (** [create n] @return a vector with [n] rows (not initialized). *)

  val make : int -> num_type -> vec
  (** [make n x] @return a vector with [n] rows initialized with value [x]. *)

  val make0 : int -> vec
  (** [make0 n x] @return a vector with [n] rows initialized with the zero
      element. *)

  val init : int -> (int -> num_type) -> vec
  (** [init n f] @return a vector containing [n] elements, where each
      element at position [i] is initialized by the result of calling
      [f i]. *)

  val of_array : num_type array -> vec
  (** [of_array ar] @return a vector initialized from array [ar]. *)

  val to_array : vec -> num_type array
  (** [to_array v] @return an array initialized from vector [v]. *)

  val of_list : num_type list -> vec
  (** [of_list l] @return a vector initialized from list [l]. *)

  val to_list : vec -> num_type list
  (** [to_list v] @return a list initialized from vector [v]. *)

  val append : vec -> vec -> vec
  (** [append v1 v2] @return the vector resulting from appending vector
      [v2] to [v1]. *)

  val concat : vec list -> vec
  (** [concat vs] @return the concatenation of vectors [vs]. *)

  val empty : vec
  (** [empty], the empty vector. *)

  val linspace : ?y : vec -> num_type -> num_type -> int -> vec
  (** [linspace ?z a b n] @return the vector [y] overwritten with [n]
      linearly spaced points between and including [a] and [b].
      @param y default = fresh vector of dim [n] *)

  val logspace : ?y : vec -> num_type -> num_type -> ?base : float -> int -> vec
  (** [logspace ?z a b base n] @return the vector [y] overwritten with [n]
      points logarithmically spaced using base [b] between and including
      [base] ** [a] and [base] ** [b].
      @param y default = fresh vector of dim [n]
      @param base default = 10.0 *)

  val dim : vec -> int
  (** [dim x] @return the dimension of vector [x]. *)

  val has_zero_dim : vec -> bool
  (** [has_zero_dim vec] checks whether vector [vec] has a dimension of size
      [zero].  In this case it cannot contain data. *)


  (** {6 Iterators over vectors} *)

  val map :
    (num_type -> num_type) ->
    ?n : int ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec
    -> vec
  (** [map f ?n ?ofsx ?incx x] @return a new vector resulting from the
      application of [f] to each element of [x].
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
      @param y default = new vector with [ofsy+(n-1)(abs incy)] rows
      @param ofsx default = 1
      @param incx default = 1 *)

  val iter :
    (num_type -> unit) ->
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    vec
    -> unit
  (** [iter ?n ?ofsx ?incx f x] applies function [f] in turn to all elements
      of vector [x].
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)

  val iteri :
    (int -> num_type -> unit) ->
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    vec
    -> unit
  (** [iteri ?n ?ofsx ?incx f x] same as [iter] but additionally passes
      the index of the element as first argument and the element itself
      as second argument. *)

  val fold :
    ('a -> num_type -> 'a) ->
    'a ->
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    vec
    -> 'a
  (** [fold f a ?n ?ofsx ?incx x] is
      [f (... (f (f a x.{ofsx}) x.{ofsx + incx}) ...) x.{ofsx + (n-1)*incx}]
      if [incx > 0] and the same in the reverse order of appearance of the
      [x] values if [incx < 0].
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)


  (** {6 Operations on one vector} *)

  val rev : vec -> vec
  (** [rev x] reverses vector [x] (non-destructive). *)

  val max : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
  (** [max ?n ?ofsx ?incx x] computes the greater of the [n] elements
      in vector [x] (2-norm), separated by [incx] incremental steps. NaNs
      are ignored. If only NaNs are encountered, the negative [infinity]
      value will be returned.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)

  val min : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
  (** [min ?n ?ofsx ?incx x] computes the smaller of the [n] elements
      in vector [x] (2-norm), separated by [incx] incremental steps.
      NaNs are ignored. If only NaNs are encountered, the [infinity] value
      will be returned.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)

  val sort :
    ?cmp : (num_type -> num_type -> int) ->
    ?decr : bool ->
    ?n : int ->
    ?ofsp : int ->
    ?incp : int ->
    ?p : int_vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec
    -> unit
  (** [sort ?cmp ?n ?ofsx ?incx x] sorts the array [x] in increasing
      order according to the comparison function [cmp].

      @param cmp a function such that [cmp a b < 0] if [a] is less than
        [b], [cmp a b = 0] if [a] equal [b] and [cmp a b > 0] if [a] is
        greater than [b] for the desired order.  Default: the usual
        order on floating point values or the lexicographic order on
        complex ones (a special routine makes it fast).  Whatever the
        order you choose, NaNs (in any component for complex numbers)
        are considered larger than any other value (so they will be
        last, in no specified order, in the sorted vector).  Therefore,
        NaN are never passed to [cmp].

      @param p if you pass a vector of size [ofsp+(n - 1)(abs incp)],
        the vector [x] will be unchanged and the permutation to sort it
        will be stored in [p].  Thus [x.{p.{ofsp + (i-1) * incp}}] will
        give the elements of [x] in increasing order.  Default: no
        vector is provided.

      @param decr sort in decreasing order (stays fast for the default [cmp]).
      @param n default = greater [n] s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsp default = 1
      @param incp default = 1
      @param ofsx default = 1
      @param incx default = 1
   *)

  val fill : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type -> unit
  (** [fill ?n ?ofsx ?incx x a] fills vector [x] with value [a] in the
      designated range.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)

  val sum : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
  (** [sum ?n ?ofsx ?incx x] computes the sum of the [n] elements in
      vector [x], separated by [incx] incremental steps.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)

  val prod : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
  (** [prod ?n ?ofsx ?incx x] computes the product of the [n] elements
      in vector [x], separated by [incx] incremental steps.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)

  val add_const : num_type -> unop
  (** [add_const c ?n ?ofsy ?incy ?y ?ofsx ?incx x] adds constant [c] to the [n]
      elements of vector [x] and stores the result in [y], using [incx] and [incy]
      as incremental steps respectively.  If [y] is given, the result will
      be stored in there using increments of [incy], otherwise a fresh
      vector will be used.  The resulting vector is returned.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
      @param ofsx default = 1
      @param incx default = 1 *)

  val sqr_nrm2 :
    ?stable : bool -> ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
  (** [sqr_nrm2 ?stable ?n ?c ?ofsx ?incx x] computes the square of
      the 2-norm (Euclidean norm) of vector [x] separated by [incx]
      incremental steps.  If [stable] is true, this is equivalent to
      squaring the result of calling the BLAS-function [nrm2], which
      avoids over- and underflow if possible.  If [stable] is false
      (default), [dot] will be called instead for greatly improved
      performance.

      @param stable default = [false]
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
  *)

  val ssqr :
    ?n : int ->
    ?c : num_type ->
    ?ofsx : int ->
    ?incx : int ->
    vec
    -> num_type
  (** [ssqr ?n ?c ?ofsx ?incx x] computes the sum of squared differences
      of the [n] elements in vector [x] from constant [c], separated
      by [incx] incremental steps.  Please do not confuse with
      {!sqr_nrm2}!  The current function behaves differently with
      complex numbers when zero is passed in for [c].  It computes
      the square for each entry then, whereas {!sqr_nrm2} uses the
      conjugate transpose in the product.  The latter will therefore
      always return a real number.

      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param c default = zero
      @param ofsx default = 1
      @param incx default = 1
  *)

  val neg : unop
  (** [neg ?n ?ofsy ?incy ?y ?ofsx ?incx x] negates [n] elements of the
      vector [x] using [incx] as incremental steps.   If [y] is given,
      the result will be stored in there using increments of [incy],
      otherwise a fresh vector will be used.  The resulting vector is returned.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
      @param ofsx default = 1
      @param incx default = 1 *)

  val reci : unop
  (** [reci ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the reciprocal value
      of [n] elements of the vector [x] using [incx] as incremental steps.
      If [y] is given, the result will be stored in there using increments of
      [incy], otherwise a fresh vector will be used.  The resulting vector
      is returned.

      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
      @param ofsx default = 1
      @param incx default = 1
  *)


  (** {6 Operations on two vectors} *)

  val add : binop
  (** [add ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] adds [n]
      elements of vectors [x] and [y] elementwise, using [incx] and [incy]
      as incremental steps respectively. If [z] is given, the result will
      be stored in there using increments of [incz], otherwise a fresh
      vector will be used. The resulting vector is returned.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsz default = 1
      @param incz default = 1
      @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

  val sub : binop
  (** [sub ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] subtracts [n]
      elements of vectors [x] and [y] elementwise, using [incx] and [incy]
      as incremental steps respectively. If [z] is given, the result will
      be stored in there using increments of [incz], otherwise a fresh
      vector will be used. The resulting vector is returned.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsz default = 1
      @param incz default = 1
      @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

  val mul : binop
  (** [mul ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] multiplies
      [n] elements of vectors [x] and [y] elementwise, using [incx]
      and [incy] as incremental steps respectively. If [z] is given, the
      result will be stored in there using increments of [incz], otherwise
      a fresh vector will be used. The resulting vector is returned.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsz default = 1
      @param incz default = 1
      @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

  val div : binop
  (** [div ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] divides [n]
      elements of vectors [x] and [y] elementwise, using [incx] and [incy]
      as incremental steps respectively. If [z] is given, the result will
      be stored in there using increments of [incz], otherwise a fresh
      vector will be used. The resulting vector is returned.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsz default = 1
      @param incz default = 1
      @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

  val zpxy :
    ?n : int ->
    ?ofsz : int ->
    ?incz : int ->
    vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec
    -> unit
  (** [zpxy ?n ?ofsz ?incz z ?ofsx ?incx x ?ofsy ?incy y] multiplies [n]
      elements of vectors [x] and [y] elementwise, using [incx] and [incy]
      as incremental steps respectively, and adds the result to and stores it
      in the specified range in [z].  This function is useful for convolutions.

      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsz default = 1
      @param incz default = 1
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

  val zmxy :
    ?n : int ->
    ?ofsz : int ->
    ?incz : int ->
    vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec
    -> unit
  (** [zmxy ?n ?ofsz ?incz z ?ofsx ?incx x ?ofsy ?incy y] multiplies [n]
      elements of vectors [x] and [y] elementwise, using [incx] and [incy]
      as incremental steps respectively, and substracts the result from
      and stores it in the specified range in [z].  This function is
      useful for convolutions.

      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsz default = 1
      @param incz default = 1
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

  val ssqr_diff :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec
    -> num_type
  (** [ssqr_diff ?n ?ofsx ?incx x ?ofsy ?incy y] returns the sum of
      squared differences of [n] elements of vectors [x] and [y], using
      [incx] and [incy] as incremental steps respectively.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

end

module Mat : sig
  type t = mat

  (** {5 Matrix operations} *)

  open Lacaml_complex32

  (** {6 Creation of matrices} *)

  val random :
    ?rnd_state : Random.State.t ->
    ?re_from : float -> ?re_range : float ->
    ?im_from : float -> ?im_range : float ->
    int -> int
    -> mat
  (** [random ?rnd_state ?re_from ?re_range ?im_from ?im_range m n]
      @return an [m]x[n] matrix initialized with random elements sampled
      uniformly from [re_range] and [im_range] starting at [re_from] and
      [im_from] for real and imaginary numbers respectively.  A random state
      [rnd_state] can be passed.

      @param rnd_state default = Random.get_state ()
      @param re_from default = -1.0
      @param re_range default = 2.0
      @param im_from default = -1.0
      @param im_range default = 2.0 *)

  open Lacaml_common
  open Lacaml_complex32
  open Types.Mat

  (** {6 Creation of matrices and accessors} *)

  val create : int -> int -> mat
  (** [create m n] @return a matrix containing [m] rows and [n] columns. *)

  val make : int -> int -> num_type -> mat
  (** [make m n x] @return a matrix containing [m] rows and [n] columns
      initialized with value [x]. *)

  val make0 : int -> int -> mat
  (** [make0 m n x] @return a matrix containing [m] rows and [n] columns
      initialized with the zero element. *)

  val of_array : num_type array array -> mat
  (** [of_array ar] @return a matrix initialized from the array of arrays
      [ar].  It is assumed that the OCaml matrix is in row major order
      (standard). *)

  val to_array : mat -> num_type array array
  (** [to_array mat] @return an array of arrays initialized from matrix
      [mat]. *)

  val of_list : num_type list list -> mat
  (** [of_list ls] @return a matrix initialized from the list of lists
      [ls].  Each sublist of [ls] represents a row of the desired matrix,
      and must be of the same length. *)

  val to_list : mat -> num_type list list
  (** [to_array mat] @return [mat] in row major order as lists. *)

  val of_col_vecs : vec array -> mat
  (** [of_col_vecs ar] @return a matrix whose columns are initialized from
      the array of vectors [ar].  The vectors must be of same length. *)

  val to_col_vecs : mat -> vec array
  (** [to_col_vecs mat] @return an array of column vectors initialized
      from matrix [mat]. *)

  val of_col_vecs_list : vec list -> mat
  (** [of_col_vecs_list ar] @return a matrix whose columns are initialized from
      the list of vectors [ar]. The vectors must be of same length. *)

  val to_col_vecs_list : mat -> vec list
  (** [to_col_vecs_list mat] @return a list of column vectors initialized
      from matrix [mat]. *)

  val as_vec : mat -> vec
  (** [as_vec mat] @return a vector containing all elements of the
      matrix in column-major order.  The data is shared. *)

  val init_rows : int -> int -> (int -> int -> num_type) -> mat
  (** [init_cols m n f] @return a matrix containing [m] rows and [n]
      columns, where each element at [row] and [col] is initialized by the
      result of calling [f row col]. The elements are passed row-wise. *)

  val init_cols : int -> int -> (int -> int -> num_type) -> mat
  (** [init_cols m n f] @return a matrix containing [m] rows and [n]
      columns, where each element at [row] and [col] is initialized by the
      result of calling [f row col]. The elements are passed column-wise. *)

  val create_mvec : int -> mat
  (** [create_mvec m] @return a matrix with one column containing [m] rows. *)

  val make_mvec : int -> num_type -> mat
  (** [make_mvec m x] @return a matrix with one column containing [m] rows
      initialized with value [x]. *)

  val mvec_of_array : num_type array -> mat
  (** [mvec_of_array ar] @return a matrix with one column
      initialized with values from array [ar]. *)

  val mvec_to_array : mat -> num_type array
  (** [mvec_to_array mat] @return an array initialized with values from
      the first (not necessarily only) column vector of matrix [mat]. *)

  val from_col_vec : vec -> mat
  (** [from_col_vec v] @return a matrix with one column representing vector [v].
      The data is shared. *)

  val from_row_vec : vec -> mat
  (** [from_row_vec v] @return a matrix with one row representing vector [v].
      The data is shared. *)

  val empty : mat
  (** [empty], the empty matrix. *)

  val identity : int -> mat
  (** [identity n] @return the [n]x[n] identity matrix. *)

  val of_diag :
    ?n : int ->
    ?br : int -> ?bc : int -> ?b : mat ->
    ?ofsx : int -> ?incx : int -> vec ->
    mat
  (** [of_diag ?n ?br ?bc ?b ?ofsx ?incx x] @return matrix [b] with diagonal
      elements in the designated sub-matrix coming from the designated sub-vector
      in [x].

      @param n default = greater [n] s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param br default = [1]
      @param bc default = [1]
      @param b default = minimal fresh matrix consistent with [n], [br], and [bc]
      @param ofsx default = 1
      @param incx default = 1
  *)

  val dim1 : mat -> int
  (** [dim1 m] @return the first dimension of matrix [m] (number of rows). *)

  val dim2 : mat -> int
  (** [dim2 m] @return the second dimension of matrix [m] (number of columns). *)

  val has_zero_dim : mat -> bool
  (** [has_zero_dim mat] checks whether matrix [mat] has a dimension of size
      [zero].  In this case it cannot contain data. *)

  val col : mat -> int -> vec
  (** [col m n] @return the [n]th column of matrix [m] as a vector.
      The data is shared. *)

  val copy_row : ?vec : vec -> mat -> int -> vec
  (** [copy_row ?vec mat int] @return a copy of the [n]th row of matrix [m]
      in vector [vec].

      @param vec default = fresh vector of length [dim2 mat]
  *)


  (** {6 Matrix transformations} *)

  val swap :
    ?uplo : [ `U | `L ] ->
    ?m : int -> ?n : int ->
    ?ar : int -> ?ac : int -> mat ->
    ?br : int -> ?bc : int -> mat ->
    unit
    (** [swap ?m ?n ?ar ?ac a ?br ?bc b] swaps the contents of (sub-matrices)
        [a] and [b].

        @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
        @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
        @param ar default = [1]
        @param ac default = [1]
        @param br default = [1]
        @param bc default = [1]
    *)

  val transpose_copy : unop
  (** [transpose_copy ?m ?n ?br ?bc ?b ?ar ?ac a] @return the transpose of
      (sub-)matrix [a].  If [b] is given, the result will be stored in there
      using offsets [br] and [bc], otherwise a fresh matrix will be used.
      NOTE: this operations does _not_ support in-place transposes!

      @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
      @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
      @param br default = [1]
      @param bc default = [1]
      @param b default = fresh matrix with [br + m - 1] rows and
                         [bc + n - 1] columns
      @param ar default = [1]
      @param ac default = [1]
  *)

  val detri : ?up : bool -> ?n : int -> ?ar : int -> ?ac : int -> mat -> unit
  (** [detri ?up ?n ?ar ?ac a] takes a triangular (sub-)matrix [a], i.e. one
      where only the upper (iff [up] is true) or lower triangle is defined,
      and makes it a symmetric matrix by mirroring the defined triangle
      along the diagonal.

      @param up default = [true]
      @param n default = [Mat.dim1 a]
      @param ar default = [1]
      @param ac default = [1]
  *)

  val packed : ?up : bool -> ?n : int -> ?ar : int -> ?ac : int -> mat -> vec
  (** [packed ?up ?n ?ar ?ac a] @return (sub-)matrix [a] in packed
      storage format.

      @param up default = [true]
      @param n default = [Mat.dim2 a]
      @param ar default = [1]
      @param ac default = [1]
  *)

  val unpacked : ?up : bool -> ?n : int -> vec -> mat
  (** [unpacked ?up x] @return an upper or lower (depending on [up])
      triangular matrix from packed representation [vec].  The other
      triangle of the matrix will be filled with zeros.

      @param up default = [true]
      @param n default = [Vec.dim x]
  *)


  (** {6 Operations on one matrix} *)

  val fill :
    ?m : int -> ?n : int -> ?ar : int -> ?ac : int -> mat -> num_type -> unit
  (** [fill ?m ?n ?ar ?ac a x] fills the specified sub-matrix in [a] with value
      [x]. *)

  val sum : ?m : int -> ?n : int -> ?ar : int -> ?ac : int -> mat -> num_type
  (** [sum ?m ?n ?ar ?ac a] computes the sum of all elements in
      the [m]-by-[n] submatrix starting at row [ar] and column [ac]. *)

  val add_const : num_type -> unop
  (** [add_const c ?m ?n ?br ?bc ?b ?ar ?ac a] adds constant [c] to the
      designated [m] by [n] submatrix in [a] and stores the result in the
      designated submatrix in [b].

      @param m default = [Mat.dim1 a]
      @param n default = [Mat.dim2 a]
      @param ar default = [1]
      @param ac default = [1]
      @param br default = [1]
      @param bc default = [1]
      @param b default = fresh matrix of size [m] by [n]
  *)

  val neg : unop
  (** [neg ?m ?n ?br ?bc ?b ?ar ?ac a] computes the negative of the elements in
      the [m] by [n] (sub-)matrix of the matrix [a] starting in row [ar]
      and column [ac].  If [b] is given, the result will be stored in there
      using offsets [br] and [bc], otherwise a fresh matrix will be used.
      The resulting matrix is returned.

      @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
      @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
      @param br default = 1
      @param bc default = 1
      @param b default = fresh matrix with [br + m - 1] rows and
                         [bc + n - 1] columns
      @param ar default = 1
      @param ac default = 1
  *)

  val reci : unop
  (** [reci ?m ?n ?br ?bc ?b ?ar ?ac a] computes the reciprocal of the elements in
      the [m] by [n] (sub-)matrix of the matrix [a] starting in row [ar]
      and column [ac].  If [b] is given, the result will be stored in there
      using offsets [br] and [bc], otherwise a fresh matrix will be used.
      The resulting matrix is returned.

      @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
      @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
      @param br default = 1
      @param bc default = 1
      @param b default = fresh matrix with [br + m - 1] rows and
                         [bc + n - 1] columns
      @param ar default = 1
      @param ac default = 1
  *)

  val copy_diag :
    ?n : int ->
    ?ofsy : int -> ?incy : int -> ?y : vec ->
    ?ar : int -> ?ac : int -> mat ->
    vec
  (** [copy_diag ?n ?ofsy ?incy ?y ?ar ?ac a] @return the diagonal of the
      (sub-)matrix [a] in a (sub-)vector.

      @param n default = greatest [n] that does not exceed matrix dimensions
      @param ofsy default = 1
      @param incy default = 1
      @param y default = fresh vector of length [n]
      @param ar default = 1
      @param ac default = 1
  *)

  val trace : mat -> num_type
  (** [trace m] @return the trace of matrix [m].  If [m] is not a
      square matrix, the sum of the longest possible sequence of
      diagonal elements will be returned. *)

  val scal :
    ?m : int -> ?n : int -> num_type -> ?ar : int -> ?ac : int -> mat -> unit
  (** [scal ?m ?n alpha ?ar ?ac a] BLAS [scal] function for (sub-)matrices. *)

  val scal_cols :
    ?m : int -> ?n : int ->
    ?ar : int -> ?ac : int -> mat ->
    ?ofs : int -> vec ->
    unit
  (** [scal_cols ?m ?n ?ar ?ac a ?ofs alphas] column-wise [scal]
      function for matrices. *)

  val scal_rows :
    ?m : int -> ?n : int ->
    ?ofs : int -> vec ->
    ?ar : int -> ?ac : int -> mat ->
    unit
  (** [scal_rows ?m ?n ?ofs alphas ?ar ?ac a] row-wise [scal]
      function for matrices. *)

  val syrk_trace :
    ?n : int ->
    ?k : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    num_type
  (** [syrk_trace ?n ?k ?ar ?ac a] computes the trace of either [a' * a]
      or [a * a'], whichever is more efficient (results are identical), of the
      (sub-)matrix [a] multiplied by its own transpose.  This is the same as
      the square of the Frobenius norm of a matrix.  [n] is the number of rows
      to consider in [a], and [k] the number of columns to consider.

      @param n default = number of rows of [a]
      @param k default = number of columns of [a]
      @param ar default = [1]
      @param ac default = [1]
  *)

  val syrk_diag :
    ?n : int ->
    ?k : int ->
    ?beta : num_type ->
    ?ofsy : int ->
    ?y : vec ->
    ?trans : trans2 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    vec
  (** [syrk_diag ?n ?k ?beta ?ofsy ?y ?trans ?alpha ?ar ?ac a]
      computes the diagonal of the symmetric rank-k product of the
      (sub-)matrix [a], multiplying it with [alpha] and adding [beta]
      times [y], storing the result in [y] starting at the specified
      offset.  [n] elements of the diagonal will be computed, and [k]
      elements of the matrix will be part of the dot product associated
      with each diagonal element.

      @param n default = number of rows of [a] (or tr[a])
      @param k default = number of columns of [a] (or tr[a])
      @param beta default = [0]
      @param ofsy default = [1]
      @param y default = fresh vector of size [n + ofsy - 1]
      @param trans default = [`N]
      @param alpha default = [1]
      @param ar default = [1]
      @param ac default = [1]
  *)


  (** {6 Operations on two matrices} *)

  val add : binop
  (** [add ?m ?n ?cr ?cc ?c ?ar ?ac a ?br ?bc b] computes the sum of the [m]
      by [n] sub-matrix of the matrix [a] starting in row [ar] and column [ac]
      with the corresponding sub-matrix of the matrix [b] starting in row
      [br] and column [bc].  If [c] is given, the result will be stored in
      there starting in row [cr] and column [cc], otherwise a fresh matrix
      will be used.  The resulting matrix is returned.

      @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
      @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
      @param cr default = 1
      @param cc default = 1
      @param c default = fresh matrix with [cr + m - 1] rows and
                         [cc + n - 1] columns
      @param br default = 1
      @param bc default = 1
      @param ar default = 1
      @param ac default = 1
  *)

  val sub : binop
  (** [sub ?m ?n ?cr ?cc ?c ?ar ?ac a ?br ?bc b] computes the difference of the
      [m] by [n] sub-matrix of the matrix [a] starting in row [ar] and column
      [ac] with the corresponding sub-matrix of the matrix [b] starting in row
      [br] and column [bc].  If [c] is given, the result will be stored in
      there starting in row [cr] and column [cc], otherwise a fresh matrix
      will be used.  The resulting matrix is returned.

      @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
      @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
      @param cr default = 1
      @param cc default = 1
      @param c default = fresh matrix with [cr + m - 1] rows and
                         [cc + n - 1] columns
      @param br default = 1
      @param bc default = 1
      @param ar default = 1
      @param ac default = 1
  *)

  val mul : binop
  (** [mul ?m ?n ?cr ?cc ?c ?ar ?ac a ?br ?bc b] computes the element-wise
      product of the [m] by [n] sub-matrix of the matrix [a] starting in row
      [ar] and column [ac] with the corresponding sub-matrix of the matrix
      [b] starting in row [br] and column [bc].  If [c] is given, the result
      will be stored in there starting in row [cr] and column [cc], otherwise
      a fresh matrix will be used.  The resulting matrix is returned.

      NOTE: please do not confuse this function with matrix multiplication!
      The LAPACK-function for matrix multiplication is called [gemm],
      e.g. [Lacaml.D.gemm].

      @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
      @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
      @param cr default = 1
      @param cc default = 1
      @param c default = fresh matrix with [cr + m - 1] rows and
                         [cc + n - 1] columns
      @param br default = 1
      @param bc default = 1
      @param ar default = 1
      @param ac default = 1
  *)

  val div : binop
  (** [div ?m ?n ?cr ?cc ?c ?ar ?ac a ?br ?bc b] computes the division of the
      [m] by [n] sub-matrix of the matrix [a] starting in row [ar] and column
      [ac] with the corresponding sub-matrix of the matrix [b] starting in row
      [br] and column [bc].  If [c] is given, the result will be stored in
      there starting in row [cr] and column [cc], otherwise a fresh matrix
      will be used.  The resulting matrix is returned.

      @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
      @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
      @param cr default = 1
      @param cc default = 1
      @param c default = fresh matrix with [cr + m - 1] rows and
                         [cc + n - 1] columns
      @param br default = 1
      @param bc default = 1
      @param ar default = 1
      @param ac default = 1
  *)

  val axpy :
    ?alpha : num_type ->
    ?m : int ->
    ?n : int ->
    ?xr : int ->
    ?xc : int ->
    mat ->
    ?yr : int ->
    ?yc : int ->
    mat
    -> unit
  (** [axpy ?alpha ?m ?n ?xr ?xc x ?yr ?yc y] BLAS [axpy] function for
      matrices. *)

  val gemm_diag :
    ?n : int ->
    ?k : int ->
    ?beta : num_type ->
    ?ofsy : int ->
    ?y : vec ->
    ?transa : trans3 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?transb : trans3 ->
    ?br : int ->
    ?bc : int ->
    mat ->
    vec
  (** [gemm_diag ?n ?k ?beta ?ofsy ?y ?transa ?transb ?alpha ?ar ?ac a ?br ?bc b]
      computes the diagonal of the product of the (sub-)matrices [a]
      and [b] (taking into account potential transposing), multiplying
      it with [alpha] and adding [beta] times [y], storing the result in
      [y] starting at the specified offset.  [n] elements of the diagonal
      will be computed, and [k] elements of the matrices will be part of
      the dot product associated with each diagonal element.

      @param n default = number of rows of [a] (or tr [a]) and
                         number of columns of [b] (or tr [b])
      @param k default = number of columns of [a] (or tr [a]) and
                         number of rows of [b] (or tr [b])
      @param beta default = [0]
      @param ofsy default = [1]
      @param y default = fresh vector of size [n + ofsy - 1]
      @param transa default = [`N]
      @param alpha default = [1]
      @param ar default = [1]
      @param ac default = [1]
      @param transb default = [`N]
      @param br default = [1]
      @param bc default = [1]
  *)

  val gemm_trace :
    ?n : int ->
    ?k : int ->
    ?transa : trans3 ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?transb : trans3 ->
    ?br : int ->
    ?bc : int ->
    mat ->
    num_type
  (** [gemm_trace ?n ?k ?transa ?ar ?ac a ?transb ?br ?bc b] computes
      the trace of the product of the (sub-)matrices [a] and [b] (taking
      into account potential transposing).  When transposing [a], this
      yields the so-called Frobenius product of [a] and [b].  [n] is the
      number of rows (columns) to consider in [a] and the number of columns
      (rows) in [b].  [k] is the inner dimension to use for the product.

      @param n default = number of rows of [a] (or tr [a]) and
                         number of columns of [b] (or tr [b])
      @param k default = number of columns of [a] (or tr [a]) and
                         number of rows of [b] (or tr [b])
      @param transa default = [`N]
      @param ar default = [1]
      @param ac default = [1]
      @param transb default = [`N]
      @param br default = [1]
      @param bc default = [1]
  *)

  val symm2_trace :
    ?n : int ->
    ?upa : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?upb : bool ->
    ?br : int ->
    ?bc : int ->
    mat ->
    num_type
  (** [symm2_trace ?n ?upa ?ar ?ac a ?upb ?br ?bc b] computes the
      trace of the product of the symmetric (sub-)matrices [a] and
      [b].  [n] is the number of rows and columns to consider in [a]
      and [b].

      @param n default = dimensions of [a] and [b]
      @param upa default = true (upper triangular portion of [a] is accessed)
      @param ar default = [1]
      @param ac default = [1]
      @param upb default = true (upper triangular portion of [b] is accessed)
      @param br default = [1]
      @param bc default = [1]
  *)

  val ssqr_diff :
    ?m : int ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    num_type
  (** [ssqr_diff ?m ?n ?ar ?ac a ?br ?bc b] @return the sum of squared
      differences between the [m] by [n] sub-matrix of the matrix
      [a] starting in row [ar] and column [ac] with the corresponding
      sub-matrix of the matrix [b] starting in row [br] and column [bc].

      @param m default = greater n s.t. [ar + m - 1 <= dim1 a]
      @param n default = greater n s.t. [ac + n - 1 <= dim2 a]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1
  *)


  (** {6 Iterators over matrices} *)

  val map :
    (num_type -> num_type) ->
    ?m : int ->
    ?n : int ->
    ?br : int ->
    ?bc : int ->
    ?b : mat ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> mat
  (** [map f ?m ?n ?br ?bc ?b ?ar ?ac a]
      @return matrix with [f] applied to each element of [a].
      @param m default = number of rows of [a]
      @param n default = number of columns of [a]
      @param b default = fresh matrix of size m by n *)

  val fold_cols : ('a -> vec -> 'a) -> ?n : int -> ?ac : int -> 'a -> mat -> 'a
  (** [fold_cols f ?n ?ac acc a]
      @return accumulator resulting from folding over each column vector.
      @param ac default = 1
      @param n default = number of columns of [a] *)

end

val pp_num : Format.formatter -> Complex.t -> unit
(** [pp_num ppf el] is equivalent to [fprintf ppf "(%G, %Gi)"
    el.re el.im]. *)

val pp_vec : (Complex.t, 'a) Lacaml_io.pp_vec
(** Pretty-printer for column vectors. *)

val pp_mat : (Complex.t, 'a) Lacaml_io.pp_mat
(** Pretty-printer for matrices. *)


open Lacaml_common
open Lacaml_complex32

(** {6 BLAS-1 interface} *)

val dotu :
  ?n : int ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  ?ofsy : int ->
  ?incy : int ->
  vec
  -> num_type
(** [dotu ?n ?ofsx ?incx x ?ofsy ?incy y] see BLAS documentation!

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1
*)

val dotc :
  ?n : int ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  ?ofsy : int ->
  ?incy : int ->
  vec
  -> num_type
(** [dotc ?n ?ofsx ?incx x ?ofsy ?incy y] see BLAS documentation!

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1
*)


(** {6 LAPACK interface} *)

val lansy_min_lwork : int -> norm4 -> int
(** [lansy_min_lwork m norm]
    @return the minimum length of the work array used by the [lansy]-function.
    @param norm type of norm that will be computed by [lansy]
    @param n the number of columns (and rows) in the matrix *)

val lansy :
  ?n : int ->
  ?up : bool ->
  ?norm : norm4 ->
  ?work : rvec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  float
(** [lansy ?n ?up ?norm ?work ?ar ?ac a] see LAPACK documentation!
    @param norm default = `O
    @param up default = true (reference upper triangular part of [a])
    @param n default = number of columns of matrix [a]
    @param work default = allocated work space for norm `I *)

val gecon_min_lwork : int -> int
(** [gecon_min_lwork n] @return the minimum length of the work array
    used by the [gecon]-function.
    @param n the logical dimensions of the matrix given to
             the [gecon]-function *)

val gecon_min_lrwork : int -> int
(** [gecon_min_lrwork n] @return the minimum length of the rwork array
    used by the [gecon]-function.
    @param n the logical dimensions of the matrix given to [gecon]-function *)

val gecon :
  ?n : int ->
  ?norm : norm2 ->
  ?anorm : float ->
  ?work : vec ->
  ?rwork : rvec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  float
(** [gecon ?n ?norm ?anorm ?work ?rwork ?ar ?ac a]
    @return estimate of the reciprocal of the condition number of matrix [a]
    @param n default = available number of columns of matrix [a]
    @param norm default = 1-norm
    @param anorm default = norm of the matrix [a] as returned by [lange]
    @param work default = automatically allocated workspace
    @param rwork default = automatically allocated workspace
    @param ar default = 1
    @param ac default = 1 *)

val sycon_min_lwork : int -> int
(** [sycon_min_lwork n] @return the minimum length of the work array
    used by the [sycon]-function.
    @param n the logical dimensions of the matrix given to
             the [sycon]-function *)

val sycon :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int32_vec ->
    ?anorm : float ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    float
(** [sycon ?n ?up ?ipiv ?anorm ?work ?ar ?ac a]
    @return estimate of the reciprocal of the
            condition number of symmetric matrix [a]
    @param n default = available number of columns of matrix [a]
    @param up default = upper triangle of the factorization of [a] is stored
    @param ipiv default = vec of length [n]
    @param anorm default = 1-norm of the matrix [a] as returned by [lange]
    @param work default = automatically allocated workspace *)

val pocon_min_lwork : int -> int
(** [pocon_min_lwork n] @return the minimum length of the work array
    used by the [pocon]-function.
    @param n the logical dimensions of the matrix given to
             the [pocon]-function *)

val pocon_min_lrwork : int -> int
(** [pocon_min_lrwork n] @return the minimum length of the rwork array
    used by the [pocon]-function.
    @param n the logical dimensions of the matrix given to [pocon]-function *)

val pocon :
    ?n : int ->
    ?up : bool ->
    ?anorm : float ->
    ?work : vec ->
    ?rwork : rvec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    float
(** [pocon ?n ?up ?anorm ?work ?rwork ?ar ?ac a]
    @return estimate of the reciprocal of the condition number of
            complex Hermitian positive definite matrix [a]
    @param n default = available number of columns of matrix [a]
    @param up default = upper triangle of Cholesky factorization
                        of [a] is stored
    @param work default = automatically allocated workspace
    @param rwork default = automatically allocated workspace
    @param anorm default = 1-norm of the matrix [a] as returned by [lange] *)

(** {7 General Schur factorization} *)

val gees :
  ?n : int ->
  ?jobvs : Lacaml_common.schur_vectors ->
  ?sort : Lacaml_common.eigen_value_sort ->
  ?w : vec ->
  ?vsr : int -> ?vsc : int -> ?vs : mat ->
  ?work : vec ->
  ?ar : int -> ?ac : int ->
  mat -> int * vec * mat
  (** [gees ?n ?jobvs ?sort ?w ?vsr ?vsc ?vs ?work ?ar ?ac a]
      See [gees]-function for details about arguments.
      @return (sdim, w, vs) *)


(** {7 General SVD routines} *)

val gesvd_min_lwork : m : int -> n : int -> int
(** [gesvd_min_lwork ~m ~n] @return the minimum length of the work array
    used by the [gesvd]-function for matrices with [m] rows and [n]
    columns. *)

val gesvd_lrwork : m : int -> n : int -> int
(** [gesvd_lrwork m n] @return the (minimum) length of the rwork array
    used by the [gesvd]-function. *)

val gesvd_opt_lwork :
  ?m : int -> ?n : int ->
  ?jobu : svd_job ->
  ?jobvt : svd_job ->
  ?s : rvec ->
  ?ur : int -> ?uc : int -> ?u : mat ->
  ?vtr : int -> ?vtc : int -> ?vt : mat ->
  ?ar : int -> ?ac : int -> mat ->
  int

val gesvd :
  ?m : int -> ?n : int ->
  ?jobu : svd_job ->
  ?jobvt : svd_job ->
  ?s : rvec ->
  ?ur : int -> ?uc : int -> ?u : mat ->
  ?vtr : int -> ?vtc : int -> ?vt : mat ->
  ?work : vec ->
  ?rwork : rvec ->
  ?ar : int -> ?ac : int -> mat ->
  rvec * mat * mat


(** {7 General eigenvalue problem (simple drivers)} *)

val geev_min_lwork : int -> int
(** [geev_min_lwork n] @return the minimum length of the work array
    used by the [geev]-function.
    @param n the logical dimensions of the matrix given to [geev]-function *)

val geev_min_lrwork : int -> int
(** [geev_min_lrwork n] @return the minimum length of the rwork array
    used by the [geev]-function.
    @param n the logical dimensions of the matrix given to [geev]-function *)

val geev_opt_lwork :
  ?n : int ->
  ?vlr : int -> ?vlc  : int -> ?vl : mat option ->
  ?vrr : int -> ?vrc : int -> ?vr : mat option ->
  ?ofsw : int -> ?w : vec ->
  ?ar : int -> ?ac : int -> mat ->
  int
  (** [geev ?work ?rwork ?n ?vlr ?vlc ?vl ?vrr ?vrc ?vr ?ofsw w ?ar ?ac a]
      See [geev]-function for details about arguments.
      @return "optimal" work size *)

val geev :
  ?n : int ->
  ?work : vec ->
  ?rwork : vec ->
  ?vlr : int -> ?vlc  : int -> ?vl : mat option ->
  ?vrr : int -> ?vrc : int -> ?vr : mat option ->
  ?ofsw : int -> ?w : vec ->
  ?ar : int -> ?ac : int -> mat ->
  mat * vec * mat
(** [geev ?work ?rwork ?n
      ?vlr ?vlc ?vl
      ?vrr ?vrc ?vr
      ?ofsw w
      ?ar ?ac a]
    @return [(lv, w, rv)], where [lv] and [rv] correspond to the left and
      right eigenvectors respectively, [w] to the eigenvalues. [lv] ([rv])
      is the empty matrix if [vl] ([vr]) is set to [None].
    @raise Failure if the function fails to converge
    @param n default = available number of columns of matrix [a]
    @param work default = automatically allocated workspace
    @param rwork default = automatically allocated workspace
    @param vl default = Automatically allocated left eigenvectors.
                        Pass [None] if you do not want to compute them,
                        [Some lv] if you want to provide the storage.
                        You can set [vlr], [vlc] in the last case.
    (See LAPACK GEEV docs for details about storage of complex eigenvectors)
    @param vr default = Automatically allocated right eigenvectors.
                         Pass [None] if you do not want to compute them,
                         [Some rv] if you want to provide the storage.
                         You can set [vrr], [vrc] in the last case.
    @param w default = automatically allocate eigenvalues
    @param a the matrix whose eigensystem is computed *)

open Lacaml_common
open Lacaml_complex32

(** {6 BLAS-1 interface} *)

val swap :
  ?n : int ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  ?ofsy : int ->
  ?incy : int ->
  vec ->
  unit
(** [swap ?n ?ofsx ?incx x ?ofsy ?incy y] see BLAS documentation!
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1 *)

val scal : ?n : int -> num_type -> ?ofsx : int -> ?incx : int -> vec -> unit
(** [scal ?n alpha ?ofsx ?incx x] see BLAS documentation!
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)

val copy :
  ?n : int ->
  ?ofsy : int ->
  ?incy : int ->
  ?y : vec ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  vec
(** [copy ?n ?ofsy ?incy ?y ?ofsx ?incx x] see BLAS documentation!
    @return vector [y], which is overwritten.
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = new vector with [ofsy+(n-1)(abs incy)] rows
    @param ofsx default = 1
    @param incx default = 1 *)

val nrm2 : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
(** [nrm2 ?n ?ofsx ?incx x] see BLAS documentation!
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1
*)

val axpy :
  ?alpha : num_type ->
  ?n : int ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  ?ofsy : int ->
  ?incy : int ->
  vec ->
  unit
(** [axpy ?alpha ?n ?ofsx ?incx x ?ofsy ?incy y] see BLAS documentation!
    @param alpha default = [{ re = 1.; im = 0. }]
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1
    @param ofsy default = 1
    @param incy default = 1 *)

val iamax : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> int
(** [iamax ?n ?ofsx ?incx x] see BLAS documentation!
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)

val amax :
  ?n : int ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  num_type
(** [amax ?n ?ofsx ?incx x] @return the greater of the absolute
    values of the elements of the vector [x].
    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1 *)


(** {6 BLAS-2 interface} *)

val gemv :
  ?m : int ->
  ?n : int ->
  ?beta : num_type  ->
  ?ofsy : int ->
  ?incy : int ->
  ?y : vec ->
  ?trans : trans3 ->
  ?alpha : num_type ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  vec
(** [gemv ?m ?n ?beta ?ofsy ?incy ?y ?trans ?alpha ?ar ?ac a ?ofsx ?incx x]
    see BLAS documentation!  BEWARE that the 1988 BLAS-2 specification
    mandates that this function has no effect when [n=0] while the
    mathematically expected behabior is [y ← beta * y].
    @return vector [y], which is overwritten.
    @param m default = number of available rows in matrix [a]
    @param n default = available columns in matrix [a]
    @param beta default = [{ re = 0.; im = 0. }]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = vector with minimal required length (see BLAS)
    @param trans default = `N
    @param alpha default = [{ re = 1.; im = 0. }]
    @param ar default = 1
    @param ac default = 1
    @param ofsx default = 1
    @param incx default = 1 *)

val gbmv :
  ?m : int ->
  ?n : int ->
  ?beta : num_type ->
  ?ofsy : int ->
  ?incy : int ->
  ?y : vec ->
  ?trans : trans3 ->
  ?alpha : num_type ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  int ->
  int ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  vec
(** [gbmv
      ?m ?n ?beta ?ofsy ?incy ?y ?trans ?alpha ?ar ?ac a kl ku ?ofsx ?incx x]
    see BLAS documentation!
    @return vector [y], which is overwritten.
    @param m default = same as [n] (i.e., [a] is a square matrix)
    @param n default = available number of columns in matrix [a]
    @param beta default = [{ re = 0.; im = 0. }]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = vector with minimal required length (see BLAS)
    @param trans default = `N
    @param alpha default = [{ re = 1.; im = 0. }]
    @param ar default = 1
    @param ac default = 1
    @param ofsx default = 1
    @param incx default = 1 *)

val symv :
  ?n : int ->
  ?beta : num_type ->
  ?ofsy : int ->
  ?incy : int ->
  ?y : vec ->
  ?up : bool ->
  ?alpha : num_type ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  vec
(** [symv ?n ?beta ?ofsy ?incy ?y ?up ?alpha ?ar ?ac a ?ofsx ?incx x]
    see BLAS documentation!
    @return vector [y], which is overwritten.
    @param n default = dimension of symmetric matrix [a]
    @param beta default = [{ re = 0.; im = 0. }]
    @param ofsy default = 1
    @param incy default = 1
    @param y default = vector with minimal required length (see BLAS)
    @param up default = true (upper triangular portion of [a] is accessed)
    @param alpha default = [{ re = 1.; im = 0. }]
    @param ar default = 1
    @param ac default = 1
    @param ofsx default = 1
    @param incx default = 1 *)

val trmv :
  ?n : int ->
  ?trans : trans3 ->
  ?diag : diag ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  unit
(** [trmv ?n ?trans ?diag ?up ?ar ?ac a ?ofsx ?incx x]
    see BLAS documentation!
    @param n default = dimension of triangular matrix [a]
    @param trans default = `N
    @param diag default = false (not a unit triangular matrix)
    @param up default = true (upper triangular portion of [a] is accessed)
    @param ar default = 1
    @param ac default = 1
    @param ofsx default = 1
    @param incx default = 1 *)

val trsv :
  ?n : int ->
  ?trans : trans3 ->
  ?diag : diag ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  unit
(** [trsv ?n ?trans ?diag ?up ?ar ?ac a ?ofsx ?incx x]
    see BLAS documentation!
    @param n default = dimension of triangular matrix [a]
    @param trans default = `N
    @param diag default = false (not a unit triangular matrix)
    @param up default = true (upper triangular portion of [a] is accessed)
    @param ar default = 1
    @param ac default = 1
    @param ofsx default = 1
    @param incx default = 1 *)

val tpmv :
  ?n : int ->
  ?trans : trans3 ->
  ?diag : diag ->
  ?up : bool ->
  ?ofsap : int ->
  vec ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  unit
(** [tpmv ?n ?trans ?diag ?up ?ofsap ap ?ofsx ?incx x]
    see BLAS documentation!
    @param n default = dimension of packed triangular matrix [ap]
    @param trans default = `N
    @param diag default = false (not a unit triangular matrix)
    @param up default = true (upper triangular portion of [ap] is accessed)
    @param ofsap default = 1
    @param ofsx default = 1
    @param incx default = 1 *)

val tpsv :
  ?n : int ->
  ?trans : trans3 ->
  ?diag : diag ->
  ?up : bool ->
  ?ofsap : int ->
  vec ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  unit
(** [tpsv ?n ?trans ?diag ?up ?ofsap ap ?ofsx ?incx x]
    see BLAS documentation!
    @param n default = dimension of packed triangular matrix [ap]
    @param trans default = `N
    @param diag default = false (not a unit triangular matrix)
    @param up default = true (upper triangular portion of [ap] is accessed)
    @param ofsap default = 1
    @param ofsx default = 1
    @param incx default = 1 *)


(** {6 BLAS-3 interface} *)

val gemm :
  ?m : int ->
  ?n : int ->
  ?k : int ->
  ?beta : num_type ->
  ?cr : int ->
  ?cc : int ->
  ?c : mat ->
  ?transa : trans3 ->
  ?alpha : num_type ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?transb : trans3 ->
  ?br : int ->
  ?bc : int ->
  mat ->
  mat
(** [gemm ?m ?n ?k ?beta ?cr ?cc ?c ?transa ?alpha ?ar ?ac a ?transb ?br ?bc b]
    see BLAS documentation!
    @return matrix [c], which is overwritten.
    @param m default = number of rows of [a] (or tr [a]) and [c]
    @param n default = number of columns of [b] (or tr [b]) and [c]
    @param k default = number of columns of [a] (or tr [a]) and
                       number of rows of [b] (or tr [b])
    @param beta default = [{ re = 0.; im = 0. }]
    @param cr default = 1
    @param cc default = 1
    @param c default = matrix with minimal required dimension
    @param transa default = `N
    @param alpha default = [{ re = 1.; im = 0. }]
    @param ar default = 1
    @param ac default = 1
    @param transb default = `N
    @param br default = 1
    @param bc default = 1 *)

val symm :
  ?m : int ->
  ?n : int ->
  ?side : side ->
  ?up : bool ->
  ?beta : num_type ->
  ?cr : int ->
  ?cc : int ->
  ?c : mat ->
  ?alpha : num_type ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?br : int ->
  ?bc : int ->
  mat ->
  mat
(** [symm ?m ?n ?side ?up ?beta ?cr ?cc ?c ?alpha ?ar ?ac a ?br ?bc b]
    see BLAS documentation!
    @return matrix [c], which is overwritten.
    @param m default = number of rows of [c]
    @param n default = number of columns of [c]
    @param side default = `L (left - multiplication is [a][b])
    @param up default = true (upper triangular portion of [a] is accessed)
    @param beta default = [{ re = 0.; im = 0. }]
    @param cr default = 1
    @param cc default = 1
    @param c default = matrix with minimal required dimension
    @param alpha default = [{ re = 1.; im = 0. }]
    @param ar default = 1
    @param ac default = 1
    @param br default = 1
    @param bc default = 1 *)

val trmm :
  ?m : int ->
  ?n : int ->
  ?side : side ->
  ?up : bool ->
  ?transa : trans3 ->
  ?diag : diag ->
  ?alpha : num_type ->
  ?ar : int ->
  ?ac : int ->
  a : mat ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [trmm ?m ?n ?side ?up ?transa ?diag ?alpha ?ar ?ac ~a ?br ?bc b]
    see BLAS documentation!
    @param m default = number of rows of [b]
    @param n default = number of columns of [b]
    @param side default = `L (left - multiplication is [a][b])
    @param up default = true (upper triangular portion of [a] is accessed)
    @param transa default = `N
    @param diag default = `N (non-unit)
    @param alpha default = [{ re = 1.; im = 0. }]
    @param ar default = 1
    @param ac default = 1
    @param br default = 1
    @param bc default = 1 *)

val trsm :
  ?m : int ->
  ?n : int ->
  ?side : side ->
  ?up : bool ->
  ?transa : trans3 ->
  ?diag : diag ->
  ?alpha : num_type ->
  ?ar : int ->
  ?ac : int ->
  a : mat ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [trsm ?m ?n ?side ?up ?transa ?diag ?alpha ?ar ?ac ~a ?br ?bc b]
    see BLAS documentation!
    @return matrix [b], which is overwritten.
    @param m default = number of rows of [b]
    @param n default = number of columns of [b]
    @param side default = `L (left - multiplication is [a][b])
    @param up default = true (upper triangular portion of [a] is accessed)
    @param transa default = `N
    @param diag default = `N (non-unit)
    @param alpha default = [{ re = 1.; im = 0. }]
    @param ar default = 1
    @param ac default = 1
    @param br default = 1
    @param bc default = 1 *)

val syrk :
  ?n : int ->
  ?k : int ->
  ?up : bool ->
  ?beta : num_type ->
  ?cr : int ->
  ?cc : int ->
  ?c : mat ->
  ?trans : trans2 ->
  ?alpha : num_type ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  mat
(** [syrk ?n ?k ?up ?beta ?cr ?cc ?c ?trans ?alpha ?ar ?ac a]
    see BLAS documentation!
    @return matrix [c], which is overwritten.
    @param n default = number of rows of [a] (or [a]'), [c]
    @param k default = number of columns of [a] (or [a]')
    @param up default = true (upper triangular portion of [c] is accessed)
    @param beta default = [{ re = 0.; im = 0. }]
    @param cr default = 1
    @param cc default = 1
    @param c default = matrix with minimal required dimension
    @param trans default = `N
    @param alpha default = [{ re = 1.; im = 0. }]
    @param ar default = 1
    @param ac default = 1 *)

val syr2k :
  ?n : int ->
  ?k : int ->
  ?up : bool ->
  ?beta : num_type ->
  ?cr : int ->
  ?cc : int ->
  ?c : mat ->
  ?trans : trans2 ->
  ?alpha : num_type ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?br : int ->
  ?bc : int ->
  mat ->
  mat
(** [syr2k ?n ?k ?up ?beta ?cr ?cc ?c ?trans ?alpha ?ar ?ac a ?br ?bc b]
    see BLAS documentation!
    @return matrix [c], which is overwritten.
    @param n default = number of rows of [a] (or [a]'), [c]
    @param k default = number of columns of [a] (or [a]')
    @param up default = true (upper triangular portion of [c] is accessed)
    @param beta default = [{ re = 0.; im = 0. }]
    @param cr default = 1
    @param cc default = 1
    @param c default = matrix with minimal required dimension
    @param trans default = `N
    @param alpha default = [{ re = 1.; im = 0. }]
    @param ar default = 1
    @param ac default = 1
    @param br default = 1
    @param bc default = 1
*)


(** {6 LAPACK interface} *)

(** {7 Auxiliary routines} *)

val lacpy :
  ?uplo : [ `U | `L ] ->
  ?m : int ->
  ?n : int ->
  ?br : int ->
  ?bc : int ->
  ?b : mat ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  mat
(** [lacpy ?uplo ?m ?n ?br ?bc ?b ?ar ?ac a] copy a (triangular)
    (sub-)matrix [a] (to an optional (sub-)matrix [b]).

    @param uplo default = whole matrix
*)

val laswp :
  ?n : int ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?k1 : int ->
  ?k2 : int ->
  ?incx : int ->
  int32_vec ->
  unit
(** [laswp ?n ?ar ?ac a ?k1 ?k2 ?incx ipiv] swap rows of [a] according to
    [ipiv].
    See LAPACK-documentation for details!

    @param n default = number of columns of matrix
    @param ar default = 1
    @param ac default = 1
    @param k1 default = 1
    @param k2 default = dimension of ipiv
    @param incx default = 1
    @param ipiv is a vector of sequential row interchanges.
*)

val lapmt :
  ?forward : bool ->
  ?m : int ->
  ?n : int ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  int32_vec ->
  unit
(** [lapmt ?forward ?n ?m ?ar ?ac a k] swap columns of [a]
    according to the permutations in [k].
    See LAPACK-documentation for details!

    @param forward default = true
    @param m default = number of rows of matrix
    @param n default = number of columns of matrix
    @param ar default = 1
    @param ac default = 1
    @param k is vector of column permutations and must be of length [n].  Note
      that checking for duplicates in [k] is not performed and this could lead
      to {b undefined} behavior. Furthermore, LAPACK uses [k] as a workspace and
      restore it upon completion, sharing this permutation array is not thread
      safe.
*)

val lassq :
  ?n : int ->
  ?scale : float ->
  ?sumsq : float ->
  ?ofsx : int ->
  ?incx : int ->
  vec ->
  float * float
(** [lassq ?n ?ofsx ?incx ?scale ?sumsq] @return [(scl, ssq)], where
    [scl] is a scaling factor and [ssq] the sum of squares of vector
    [x] starting at [ofs] and using increment [incx] and initial
    [scale] and [sumsq].  The following equality holds:
    [scl**2. *. ssq = x.{1}**2. +. ... +. x.{n}**2. +. scale**2. *. sumsq].
    See LAPACK-documentation for details!

    @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
    @param ofsx default = 1
    @param incx default = 1
    @param scale default = 0.
    @param sumsq default = 1.
*)

val larnv :
  ?idist : [ `Uniform0 | `Uniform1 | `Normal ] ->
  ?iseed : int32_vec ->
  ?n : int ->
  ?ofsx : int ->
  ?x : vec ->
  unit ->
  vec
(** [larnv ?idist ?iseed ?n ?ofsx ?x ()] @return a random vector with random
    distribution as specifified by [idist], random seed [iseed], vector offset
    [ofsx] and optional vector [x].

    @param idist default = [`Normal]
    @param iseed default = integer vector of size 4 with all ones.
    @param n default = [dim x - ofsx + 1] if [x] is provided, [1] otherwise.
    @param ofsx default = [1]
    @param x default = vector of length [ofsx - 1 + n] if [n] is provided.
*)

val lange_min_lwork : int -> norm4 -> int
(** [lange_min_lwork m norm]
    @return the minimum length of the work array used by the [lange]-function.
    @param m the number of rows in the matrix
    @param norm type of norm that will be computed by [lange] *)

val lange :
  ?m : int ->
  ?n : int ->
  ?norm : norm4 ->
  ?work : rvec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  float
(** [lange ?m ?n ?norm ?work ?ar ?ac a] @return the value of the one
    norm ([norm = `O]), or the Frobenius norm ([norm = `F]), or the infinity
    norm ([norm = `I]), or the element of largest absolute value
    ([norm = `M]) of a real matrix [a].

    @param m default = number of rows of matrix [a]
    @param n default = number of columns of matrix [a]
    @param norm default = [`O]
    @param work default = allocated work space for norm [`I]
    @param ar default = 1
    @param ac default = 1 *)

val lauum :
  ?up : bool ->
  ?n : int ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  unit
(** [lauum ?up ?n ?ar ?ac a] computes the product U * U**T or L**T * L,
    where the triangular factor U or L is stored in the upper or lower
    triangular part of the array [a].  The upper or lower part of [a]
    is overwritten.

    @param up default = [true]
    @param n default = minimum of available number of rows/columns in matrix [a]
    @param ar default = 1
    @param ac default = 1 *)


(** {7 Linear equations (computational routines)} *)

val getrf :
  ?m : int ->
  ?n : int ->
  ?ipiv : int32_vec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  int32_vec
(** [getrf ?m ?n ?ipiv ?ar ?ac a] computes an LU factorization of a
    general [m]-by-[n] matrix [a] using partial pivoting with row
    interchanges.  See LAPACK documentation.
    @return [ipiv], the  pivot indices.
    @raise Failure if the matrix is singular.
    @param m default = number of rows in matrix [a]
    @param n default = number of columns in matrix [a]
    @param ipiv = vec of length [min(m, n)]
    @param ar default = 1
    @param ac default = 1 *)

val getrs :
  ?n : int ->
  ?ipiv : int32_vec ->
  ?trans : trans3 ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [getrs ?n ?ipiv ?trans ?ar ?ac a ?nrhs ?br ?bc b] solves a system
    of linear equations [a] * X = [b] or [a]' * X = [b] with a general
    [n]-by-[n] matrix [a] using the LU factorization computed by
    {!getrf}.
    Note that matrix [a] will be passed to {!getrf} if [ipiv] was not
    provided.
    @raise Failure if the matrix is singular.
    @param n default = number of columns in matrix [a]
    @param ipiv default = result from [getrf] applied to [a]
    @param trans default = `N
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val getri_min_lwork : int -> int
(** [getri_min_lwork n] @return the minimum length of the
    work array used by the {!getri}-function if the matrix has [n] columns. *)

val getri_opt_lwork :
  ?n : int ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  int
(** [getri_opt_lwork ?n ?ar ?ac a] @return the optimal size of the
    work array used by the {!getri}-function.
    @param n default = number of columns of matrix [a]
    @param ar default = 1
    @param ac default = 1 *)

val getri :
  ?n : int ->
  ?ipiv : int32_vec ->
  ?work : vec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  unit
(** [getri ?n ?ipiv ?work ?ar ?ac a] computes the inverse of a matrix
    using the LU factorization computed by {!getrf}.  Note that matrix
    [a] will be passed to {!getrf} if [ipiv] was not provided.
    @raise Failure if the matrix is singular.
    @param n default = number of columns in matrix [a]
    @param ipiv default = vec of length [m] from getri
    @param work default = vec of optimum length
    @param ar default = 1
    @param ac default = 1 *)

val sytrf_min_lwork : unit -> int
(** [sytrf_min_lwork ()] @return the minimum length of the
    work array used by the {!sytrf}-function. *)

val sytrf_opt_lwork :
  ?n : int ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  int
(** [sytrf_opt_lwork ?n ?up ?ar ?ac a] @return the optimal size of the
    work array used by the {!sytrf}-function.
    @param n default = number of columns of matrix [a]
    @param up default = true (store upper triangle in [a])
    @param a the matrix [a]
    @param ar default = 1
    @param ac default = 1 *)

val sytrf :
  ?n : int ->
  ?up : bool ->
  ?ipiv : int32_vec ->
  ?work : vec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  int32_vec
(** [sytrf ?n ?up ?ipiv ?work ?ar ?ac a] computes the factorization of
    the real symmetric matrix [a] using the Bunch-Kaufman diagonal
    pivoting method.
    @raise Failure if D in [a] = U*D*U' or L*D*L' is singular.
    @param n default = number of columns in matrix [a]
    @param up default = true (store upper triangle in [a])
    @param ipiv = vec of length n
    @param work default = vec of optimum length
    @param ar default = 1
    @param ac default = 1 *)

val sytrs :
  ?n : int ->
  ?up : bool ->
  ?ipiv : int32_vec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [sytrs ?n ?up ?ipiv ?ar ?ac a ?nrhs ?br ?bc b] solves a system of
    linear equations [a]*X = [b] with a real symmetric matrix [a]
    using the factorization [a] = U*D*U**T or [a] = L*D*L**T computed
    by {!sytrf}.  Note that matrix [a] will be passed to {!sytrf} if
    [ipiv] was not provided.
    @raise Failure if the matrix is singular.
    @param n default = number of columns in matrix [a]
    @param up default = true (store upper triangle in [a])
    @param ipiv default = vec of length [n]
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val sytri_min_lwork : int -> int
(** [sytri_min_lwork n] @return the minimum length of the
    work array used by the {!sytri}-function if the matrix has [n] columns. *)

val sytri :
  ?n : int ->
  ?up : bool ->
  ?ipiv : int32_vec ->
  ?work : vec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  unit
(** [sytri ?n ?up ?ipiv ?work ?ar ?ac a] computes the inverse of the
    real symmetric indefinite matrix [a] using the factorization [a] =
    U*D*U**T or [a] = L*D*L**T computed by {!sytrf}.  Note that matrix
    [a] will be passed to {!sytrf} if [ipiv] was not provided.

    @raise Failure if the matrix is singular.
    @param n default = number of columns in matrix [a]
    @param up default = true (store upper triangle in [a])
    @param ipiv default = vec of length [n] from {!sytrf}
    @param work default = vec of optimum length
    @param ar default = 1
    @param ac default = 1 *)

val potrf :
  ?n : int ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  ?jitter : num_type ->
  mat ->
  unit
(** [potrf ?n ?up ?ar ?ac ?jitter a] factorizes symmetric positive
    definite matrix [a] (or the designated submatrix) using Cholesky
    factorization.

    Due to rounding errors ill-conditioned matrices may actually appear
    as if they were not positive definite, thus leading to an exception.
    One remedy for this problem is to add a small [jitter] to the
    diagonal of the matrix, which will usually allow Cholesky to complete
    successfully (though at a small bias).  For extremely ill-conditioned
    matrices it is recommended to use (symmetric) eigenvalue decomposition
    instead of this function for a numerically more stable factorization.

    @raise Failure if the matrix is singular.

    @param n default = number of columns in matrix [a]
    @param up default = true (store upper triangle in [a])
    @param ar default = 1
    @param ac default = 1
    @param jitter default = nothing
*)

val potrs :
  ?n : int ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  ?factorize : bool ->
  ?jitter : num_type ->
  mat ->
  unit
(** [potrs ?n ?up ?ar ?ac a ?nrhs ?br ?bc ?factorize ?jitter b] solves
    a system of linear equations [a]*X = [b], where [a] is symmetric
    positive definite matrix, using the Cholesky factorization [a] =
    U**T*U or [a] = L*L**T computed by {!potrf}.

    @raise Failure if the matrix is singular.

    @param n default = number of columns in matrix [a]
    @param up default = true
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1
    @param factorize default = true (calls {!potrf} implicitly)
    @param jitter default = nothing
*)

val potri :
  ?n : int ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  ?factorize : bool ->
  ?jitter : num_type ->
  mat ->
  unit
(** [potri ?n ?up ?ar ?ac ?factorize ?jitter a] computes the inverse
    of the real symmetric positive definite matrix [a] using the
    Cholesky factorization [a] = U**T*U or [a] = L*L**T computed by
    {!potrf}.

    @raise Failure if the matrix is singular.

    @param n default = number of columns in matrix [a]
    @param up default = true (upper triangle stored in [a])
    @param ar default = 1
    @param ac default = 1
    @param factorize default = true (calls {!potrf} implicitly)
    @param jitter default = nothing
*)

val trtrs :
  ?n : int ->
  ?up : bool ->
  ?trans : trans3 ->
  ?diag : diag ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [trtrs ?n ?up ?trans ?diag ?ar ?ac a ?nrhs ?br ?bc b] solves a
    triangular system of the form [a] * X = [b] or [a]**T * X = [n],
    where [a] is a triangular matrix of order [n], and [b] is an
    [n]-by-[nrhs] matrix.

    @raise Failure if the matrix [a] is singular.

    @param n default = number of columns in matrix [a]
    @param up default = true
    @param trans default = `N
    @param diag default = `N
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1
*)

val tbtrs :
  ?n : int ->
  ?kd : int ->
  ?up : bool ->
  ?trans : trans3 ->
  ?diag : diag ->
  ?abr : int ->
  ?abc : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [tbtrs ?n ?kd ?up ?trans ?diag ?abr ?abc ab ?nrhs ?br ?bc b]
    solves a triangular system of the form [a] * X = [b] or [a]**T * X = [b],
    where [a] is a triangular band matrix of order [n], and [b] is
    an [n]-by-[nrhs] matrix.

    @raise Failure if the matrix [a] is singular.

    @param n default = number of columns in matrix [ab]
    @param kd default = number of rows in matrix [ab] - 1
    @param up default = true
    @param trans default = `N
    @param diag default = `N
    @param abr default = 1
    @param abc default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1
*)

val trtri :
  ?n : int ->
  ?up : bool ->
  ?diag : diag ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  unit
(** [trtri ?n ?up ?diag ?ar ?ac a] computes the inverse of a real
    upper or lower triangular matrix [a].

    @raise Failure if the matrix [a] is singular.

    @param n default = number of columns in matrix [a]
    @param up default = true (upper triangle stored in [a])
    @param diag default = `N
    @param ar default = 1
    @param ac default = 1
*)

val geqrf_opt_lwork :
  ?m : int ->
  ?n : int ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  int
(** [geqrf_opt_lwork ?m ?n ?ar ?ac a] @return the optimum
    length of the work-array used by the {!geqrf}-function given matrix
    [a] and optionally its logical dimensions [m] and [n].

    @param m default = number of rows in matrix [a]
    @param n default = number of columns in matrix [a]
    @param ar default = 1
    @param ac default = 1
*)

val geqrf_min_lwork : n : int -> int
(** [geqrf_min_lwork ~n] @return the minimum length of the
    work-array used by the {!geqrf}-function if the matrix has [n]
    columns. *)

val geqrf :
  ?m : int ->
  ?n : int ->
  ?work : vec ->
  ?tau : vec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  vec
(** [geqrf ?m ?n ?work ?tau ?ar ?ac a] computes a QR factorization of
    a real [m]-by-[n] matrix [a].  See LAPACK documentation.

    @return [tau], the scalar factors of the elementary reflectors.
    @param m default = number of rows in matrix [a]
    @param n default = number of columns in matrix [a]
    @param work default = vec of optimum length
    @param tau default = vec of required length
    @param ar default = 1
    @param ac default = 1 *)


(** {7 Linear equations (simple drivers)} *)

val gesv :
  ?n : int ->
  ?ipiv : int32_vec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [gesv ?n ?ipiv ?ar ?ac a ?nrhs ?br ?bc b] computes the solution to
    a real system of linear equations [a] * X = [b], where [a] is an
    [n]-by-[n] matrix and X and [b] are [n]-by-[nrhs] matrices.  The
    LU decomposition with partial pivoting and row interchanges is
    used to factor [a] as [a] = P * L * U, where P is a permutation
    matrix, L is unit lower triangular, and U is upper triangular.
    The factored form of [a] is then used to solve the system of
    equations [a] * X = [b].  On exit, [b] contains the solution matrix X.

    @raise Failure if the matrix [a] is singular.
    @param n default = available number of columns in matrix [a]
    @param ipiv default = vec of length [n]
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val gbsv :
  ?n : int ->
  ?ipiv : int32_vec ->
  ?abr : int ->
  ?abc : int ->
  mat ->
  int ->
  int ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [gbsv ?n ?ipiv ?abr ?abc ab kl ku ?nrhs ?br ?bc b] computes the
    solution to a real system of linear equations [a] * X = [b], where
    [a] is a band matrix of order [n] with [kl] subdiagonals and [ku]
    superdiagonals, and X and [b] are [n]-by-[nrhs] matrices.  The LU
    decomposition with partial pivoting and row interchanges is used
    to factor [a] as [a] = L * U, where L is a product of permutation and
    unit lower triangular matrices with [kl] subdiagonals, and U is
    upper triangular with [kl+ku] superdiagonals.  The factored form of
    [a] is then used to solve the system of equations [a] * X = [b].

    @raise Failure if the matrix [a] is singular.
    @param n default = available number of columns in matrix [ab]
    @param ipiv default = vec of length [n]
    @param abr default = 1
    @param abc default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val gtsv :
  ?n : int ->
  ?ofsdl : int ->
  vec ->
  ?ofsd : int ->
  vec ->
  ?ofsdu : int ->
  vec ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [gtsv ?n ?ofsdl dl ?ofsd d ?ofsdu du ?nrhs ?br ?bc b] solves the
    equation [a] * X = [b] where [a] is an [n]-by-[n] tridiagonal
    matrix, by Gaussian elimination with partial pivoting.  Note that
    the equation [A]'*X = [b] may be solved by interchanging the order
    of the arguments [du] and [dl].

    @raise Failure if the matrix is singular.
    @param n default = available length of vector [d]
    @param ofsdl default = 1
    @param ofsd default = 1
    @param ofsdu default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val posv :
  ?n : int ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [posv ?n ?up ?ar ?ac a ?nrhs ?br ?bc b] computes the solution to a
    real system of linear equations [a] * X = [b], where [a] is an
    [n]-by-[n] symmetric positive definite matrix and X and [b] are
    [n]-by-[nrhs] matrices.  The Cholesky decomposition is used to
    factor [a] as
    [a] = U**T * U,  if [up = true], or
    [a] = L * L**T,  if [up = false],
    where U is an upper triangular matrix and L is a lower triangular
    matrix.  The factored form of [a] is then used to solve the system
    of equations [a] * X = [b].

    @raise Failure if the matrix is singular.
    @param n default = available number of columns in matrix [a]
    @param up default = true i.e., upper triangle of [a] is stored
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val ppsv :
  ?n : int ->
  ?up : bool ->
  ?ofsap : int ->
  vec ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [ppsv ?n ?up ?ofsap ap ?nrhs ?br ?bc b] computes the solution to
    the real system of linear equations [a] * X = [b], where [a] is an
    [n]-by-[n] symmetric positive definite matrix stored in packed
    format and X and [b] are [n]-by-[nrhs] matrices.  The Cholesky
    decomposition is used to factor [a] as
    [a] = U**T * U,  if [up = true], or
    [a] = L * L**T,  if [up = false],
    where U is an upper triangular matrix and L is a lower triangular
    matrix.  The factored form of [a] is then used to solve the system
    of equations [a] * X = [b].

    @raise Failure if the matrix is singular.
    @param n default = the greater n s.t. n(n+1)/2 <= [Vec.dim ap]
    @param up default = true i.e., upper triangle of [ap] is stored
    @param ofsap default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val pbsv :
  ?n : int ->
  ?up : bool ->
  ?kd : int ->
  ?abr : int ->
  ?abc : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [pbsv ?n ?up ?kd ?abr ?abc ab ?nrhs ?br ?bc b] computes the
    solution to a real system of linear equations [a] * X = [b], where
    [a] is an [n]-by-[n] symmetric positive definite band matrix and X
    and [b] are [n]-by-[nrhs] matrices.  The Cholesky decomposition is
    used to factor [a] as
    [a] = U**T * U,  if [up = true], or
    [a] = L * L**T,  if [up = false],
    where U is an upper triangular band matrix, and L is a lower
    triangular band matrix, with the same number of superdiagonals or
    subdiagonals as [a].  The factored form of [a] is then used to
    solve the system of equations [a] * X = [b].

    @raise Failure if the matrix is singular.
    @param n default = available number of columns in matrix [ab]
    @param up default = true i.e., upper triangle of [ab] is stored
    @param kd default = available number of rows in matrix [ab] - 1
    @param abr default = 1
    @param abc default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val ptsv :
  ?n : int ->
  ?ofsd : int ->
  vec ->
  ?ofse : int ->
  vec ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [ptsv ?n ?ofsd d ?ofse e ?nrhs ?br ?bc b] computes the solution to
    the real system of linear equations [a]*X = [b], where [a] is an
    [n]-by-[n] symmetric positive definite tridiagonal matrix, and X
    and [b] are [n]-by-[nrhs] matrices.  A is factored as [a] =
    L*D*L**T, and the factored form of [a] is then used to solve the
    system of equations.

    @raise Failure if the matrix is singular.
    @param n default = available length of vector [d]
    @param ofsd default = 1
    @param ofse default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val sysv_opt_lwork :
  ?n : int ->
  ?up : bool ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  int
(** [sysv_opt_lwork ?n ?up ?ar ?ac a ?nrhs ?br ?bc b] @return the optimum
    length of the work-array used by the [sysv]-function given matrix
    [a], optionally its logical dimension [n] and given right hand side
    matrix [b] with an optional number [nrhs] of vectors.
    @param n default = available number of columns in matrix [a]
    @param up default = true i.e., upper triangle of [a] is stored
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val sysv :
  ?n : int ->
  ?up : bool ->
  ?ipiv : int32_vec ->
  ?work : vec ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [sysv ?n ?up ?ipiv ?work ?ar ?ac a ?nrhs ?br ?bc b] computes the
    solution to a real system of linear equations [a] * X = [b], where
    [a] is an N-by-N symmetric matrix and X and [b] are [n]-by-[nrhs]
    matrices.  The diagonal pivoting method is used to factor [a] as
    [a] = U * D * U**T,  if [up = true], or
    [a] = L * D * L**T,  if [up = false],
    where U (or L) is a product of permutation and unit upper (lower)
    triangular matrices, and D is symmetric and block diagonal with
    1-by-1 and 2-by-2 diagonal blocks.  The factored form of [a] is
    then used to solve the system of equations [a] * X = [b].

    @raise Failure if the matrix is singular.
    @param n default = available number of columns in matrix [a]
    @param up default = true i.e., upper triangle of [a] is stored
    @param ipiv default = vec of length [n]
    @param work default = vec of optimum length (-> [sysv_opt_lwork])
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val spsv :
  ?n : int ->
  ?up : bool ->
  ?ipiv : int32_vec ->
  ?ofsap : int ->
  vec ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [spsv ?n ?up ?ipiv ?ofsap ap ?nrhs ?br ?bc b] computes the
    solution to the real system of linear equations [a] * X = [b],
    where [a] is an [n]-by-[n] symmetric matrix stored in packed
    format and X and [b] are [n]-by-[nrhs] matrices.  The diagonal
    pivoting method is used to factor [a] as
    [a] = U * D * U**T,  if [up = true], or
    [a] = L * D * L**T,  if [up = false],
    where U (or L) is a product of permutation and unit upper (lower)
    triangular matrices, D is symmetric and block diagonal with 1-by-1
    and 2-by-2 diagonal blocks.  The factored form of [a] is then used
    to solve the system of equations [a] * X = [b].

    @raise Failure if the matrix is singular.
    @param n default = the greater n s.t. n(n+1)/2 <= [Vec.dim ap]
    @param up default = true i.e., upper triangle of [ap] is stored
    @param ipiv default = vec of length [n]
    @param ofsap default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)


(** {7 Least squares (simple drivers)} *)

val gels_min_lwork : m : int -> n : int -> nrhs : int -> int
(** [gels_min_lwork ~m ~n ~nrhs] @return the minimum length of the
    work-array used by the [gels]-function if the logical dimensions
    of the matrix are [m] rows and [n] columns and if there are [nrhs]
    right hand side vectors. *)

val gels_opt_lwork :
  ?m : int ->
  ?n : int ->
  ?trans : trans2 ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  int
(** [gels_opt_lwork ?m ?n ?trans ?ar ?ac a ?nrhs ?br ?bc b] @return
    the optimum length of the work-array used by the [gels]-function given
    matrix [a], optionally its logical dimensions [m] and [n] and given
    right hand side matrix [b] with an optional number [nrhs] of vectors.
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns in matrix [a]
    @param trans default = `N
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)

val gels :
  ?m : int ->
  ?n : int ->
  ?work : vec ->
  ?trans : trans2 ->
  ?ar : int ->
  ?ac : int ->
  mat ->
  ?nrhs : int ->
  ?br : int ->
  ?bc : int ->
  mat ->
  unit
(** [gels ?m ?n ?work ?trans ?ar ?ac a ?nrhs ?br ?bc b] see
    LAPACK documentation!
    @param m default = available number of rows in matrix [a]
    @param n default = available number of columns of matrix [a]
    @param work default = vec of optimum length (-> {!gels_opt_lwork})
    @param trans default = `N
    @param ar default = 1
    @param ac default = 1
    @param nrhs default = available number of columns in matrix [b]
    @param br default = 1
    @param bc default = 1 *)


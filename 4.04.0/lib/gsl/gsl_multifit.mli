(* gsl-ocaml - OCaml interface to GSL                       *)
(* Copyright (©) 2002-2012 - Olivier Andrieu                *)
(* Distributed under the terms of the GPL version 3         *)

(** Multi-parameter Least-Squares Fitting *)

open Gsl_vectmat

type ws
val make : n:int -> p:int -> ws

external _linear :
  ?weight:vec ->
  x:mat -> y:vec ->
  c:vec -> cov:mat -> ws -> float
  = "ml_gsl_multifit_linear_bc" "ml_gsl_multifit_linear"

val linear :
  ?weight:vec -> mat -> vec -> 
    Gsl_vector.vector * Gsl_matrix.matrix * float

external linear_est : x:vec -> c:vec -> cov:mat -> Gsl_fun.result
    = "ml_gsl_multifit_linear_est"

val fit_poly : 
    ?weight:float array -> x:float array -> y:float array -> int ->
      float array * float array array * float

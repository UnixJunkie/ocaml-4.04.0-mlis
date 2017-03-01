(* gsl-ocaml - OCaml interface to GSL                       *)
(* Copyright (©) 2002-2012 - Olivier Andrieu                *)
(* Distributed under the terms of the GPL version 3         *)

(** {1 Special functions} *)

open Gsl_fun

(** {2 Airy functions} *)

external airy_Ai : float -> mode -> float = "ml_gsl_sf_airy_Ai"
external airy_Ai_e : float -> mode -> result = "ml_gsl_sf_airy_Ai_e"

external airy_Bi : float -> mode -> float = "ml_gsl_sf_airy_Bi"
external airy_Bi_e : float -> mode -> result = "ml_gsl_sf_airy_Bi_e"

external airy_Ai_scaled : float -> mode -> float = "ml_gsl_sf_airy_Ai_scaled"
external airy_Ai_scaled_e : float -> mode -> result
  = "ml_gsl_sf_airy_Ai_scaled_e"

external airy_Bi_scaled : float -> mode -> float = "ml_gsl_sf_airy_Bi_scaled"
external airy_Bi_scaled_e : float -> mode -> result
  = "ml_gsl_sf_airy_Bi_scaled_e"

external airy_Ai_deriv : float -> mode -> float = "ml_gsl_sf_airy_Ai_deriv"
external airy_Ai_deriv_e : float -> mode -> result
  = "ml_gsl_sf_airy_Ai_deriv_e"

external airy_Bi_deriv : float -> mode -> float = "ml_gsl_sf_airy_Bi_deriv"
external airy_Bi_deriv_e : float -> mode -> result
  = "ml_gsl_sf_airy_Bi_deriv_e"

external airy_Ai_deriv_scaled : float -> mode -> float
  = "ml_gsl_sf_airy_Ai_deriv_scaled"
external airy_Ai_deriv_scaled_e : float -> mode -> result
  = "ml_gsl_sf_airy_Ai_deriv_scaled_e"

external airy_Bi_deriv_scaled : float -> mode -> float
  = "ml_gsl_sf_airy_Bi_deriv_scaled"
external airy_Bi_deriv_scaled_e : float -> mode -> result
  = "ml_gsl_sf_airy_Bi_deriv_scaled_e"


external airy_zero_Ai : int -> float = "ml_gsl_sf_airy_zero_Ai"
external airy_zero_Ai_e : int -> result = "ml_gsl_sf_airy_zero_Ai_e"

external airy_zero_Bi : int -> float = "ml_gsl_sf_airy_zero_Bi"
external airy_zero_Bi_e : int -> result = "ml_gsl_sf_airy_zero_Bi_e"


(** {2 Bessel functions} *)

external bessel_J0 : float -> float = "ml_gsl_sf_bessel_J0"
external bessel_J0_e : float -> result = "ml_gsl_sf_bessel_J0_e"
external bessel_J1 : float -> float = "ml_gsl_sf_bessel_J1"
external bessel_J1_e : float -> result = "ml_gsl_sf_bessel_J1_e"
external bessel_Jn : int -> float -> float = "ml_gsl_sf_bessel_Jn"
external bessel_Jn_e : int -> float -> result = "ml_gsl_sf_bessel_Jn_e"
external bessel_Jn_array : int -> float -> float array -> unit
  = "ml_gsl_sf_bessel_Jn_array"

external bessel_Y0 : float -> float = "ml_gsl_sf_bessel_Y0"
external bessel_Y0_e : float -> result = "ml_gsl_sf_bessel_Y0_e"
external bessel_Y1 : float -> float = "ml_gsl_sf_bessel_Y1"
external bessel_Y1_e : float -> result = "ml_gsl_sf_bessel_Y1_e"
external bessel_Yn : int -> float -> float = "ml_gsl_sf_bessel_Yn"
external bessel_Yn_e : int -> float -> result = "ml_gsl_sf_bessel_Yn_e"
external bessel_Yn_array : int -> float -> float array -> unit
  = "ml_gsl_sf_bessel_Yn_array"

external bessel_I0 : float -> float = "ml_gsl_sf_bessel_I0"
external bessel_I0_e : float -> result = "ml_gsl_sf_bessel_I0_e"
external bessel_I1 : float -> float = "ml_gsl_sf_bessel_I1"
external bessel_I1_e : float -> result = "ml_gsl_sf_bessel_I1_e"
external bessel_In : int -> float -> float = "ml_gsl_sf_bessel_In"
external bessel_In_e : int -> float -> result = "ml_gsl_sf_bessel_In_e"
external bessel_In_array : int -> float -> float array -> unit
  = "ml_gsl_sf_bessel_In_array"

external bessel_K0 : float -> float = "ml_gsl_sf_bessel_K0"
external bessel_K0_e : float -> result = "ml_gsl_sf_bessel_K0_e"
external bessel_K1 : float -> float = "ml_gsl_sf_bessel_K1"
external bessel_K1_e : float -> result = "ml_gsl_sf_bessel_K1_e"
external bessel_Kn : int -> float -> float = "ml_gsl_sf_bessel_Kn"
external bessel_Kn_e : int -> float -> result = "ml_gsl_sf_bessel_Kn_e"
external bessel_Kn_array : int -> float -> float array -> unit
  = "ml_gsl_sf_bessel_Kn_array"

external bessel_I0_scaled : float -> float = "ml_gsl_sf_bessel_I0_scaled"
external bessel_I0_scaled_e : float -> result
  = "ml_gsl_sf_bessel_I0_scaled_e"
external bessel_I1_scaled : float -> float = "ml_gsl_sf_bessel_I1_scaled"
external bessel_I1_scaled_e : float -> result
  = "ml_gsl_sf_bessel_I1_scaled_e"
external bessel_In_scaled : int -> float -> float
  = "ml_gsl_sf_bessel_In_scaled"
external bessel_In_scaled_e : int -> float -> result
  = "ml_gsl_sf_bessel_In_scaled_e"
external bessel_In_scaled_array : int -> float -> float array -> unit
  = "ml_gsl_sf_bessel_In_scaled_array"

external bessel_K0_scaled : float -> float = "ml_gsl_sf_bessel_K0_scaled"
external bessel_K0_scaled_e : float -> result
  = "ml_gsl_sf_bessel_K0_scaled_e"
external bessel_K1_scaled : float -> float = "ml_gsl_sf_bessel_K1_scaled"
external bessel_K1_scaled_e : float -> result
  = "ml_gsl_sf_bessel_K1_scaled_e"
external bessel_Kn_scaled : int -> float -> float
  = "ml_gsl_sf_bessel_Kn_scaled"
external bessel_Kn_scaled_e : int -> float -> result
  = "ml_gsl_sf_bessel_Kn_scaled_e"
external bessel_Kn_scaled_array : int -> float -> float array -> unit
  = "ml_gsl_sf_bessel_Kn_scaled_array"


external bessel_j0 : float -> float = "ml_gsl_sf_bessel_j0"
external bessel_j0_e : float -> result = "ml_gsl_sf_bessel_j0_e"
external bessel_j1 : float -> float = "ml_gsl_sf_bessel_j1"
external bessel_j1_e : float -> result = "ml_gsl_sf_bessel_j1_e"
external bessel_j2 : float -> float = "ml_gsl_sf_bessel_j2"
external bessel_j2_e : float -> result = "ml_gsl_sf_bessel_j2_e"
external bessel_jl : int -> float -> float = "ml_gsl_sf_bessel_jl"
external bessel_jl_e : int -> float -> result = "ml_gsl_sf_bessel_jl_e"
external bessel_jl_array : int -> float -> float array -> unit
  = "ml_gsl_sf_bessel_jl_array"

external bessel_jl_steed_array : float -> float array -> unit
  = "ml_gsl_sf_bessel_jl_steed_array"

external bessel_y0 : float -> float = "ml_gsl_sf_bessel_y0"
external bessel_y0_e : float -> result = "ml_gsl_sf_bessel_y0_e"
external bessel_y1 : float -> float = "ml_gsl_sf_bessel_y1"
external bessel_y1_e : float -> result = "ml_gsl_sf_bessel_y1_e"
external bessel_y2 : float -> float = "ml_gsl_sf_bessel_y2"
external bessel_y2_e : float -> result = "ml_gsl_sf_bessel_y2_e"
external bessel_yl : int -> float -> float = "ml_gsl_sf_bessel_yl"
external bessel_yl_e : int -> float -> result = "ml_gsl_sf_bessel_yl_e"
external bessel_yl_array : int -> float -> float array -> unit
  = "ml_gsl_sf_bessel_yl_array"

external bessel_i0_scaled : float -> float = "ml_gsl_sf_bessel_i0_scaled"
external bessel_i0_scaled_e : float -> result
  = "ml_gsl_sf_bessel_i0_scaled_e"
external bessel_i1_scaled : float -> float = "ml_gsl_sf_bessel_i1_scaled"
external bessel_i1_scaled_e : float -> result
  = "ml_gsl_sf_bessel_i1_scaled_e"
external bessel_il_scaled : int -> float -> float
  = "ml_gsl_sf_bessel_il_scaled"
external bessel_il_scaled_e : int -> float -> result
  = "ml_gsl_sf_bessel_il_scaled_e"
external bessel_il_scaled_array : int -> float -> float array -> unit
  = "ml_gsl_sf_bessel_il_scaled_array"

external bessel_k0_scaled : float -> float = "ml_gsl_sf_bessel_k0_scaled"
external bessel_k0_scaled_e : float -> result
  = "ml_gsl_sf_bessel_k0_scaled_e"
external bessel_k1_scaled : float -> float = "ml_gsl_sf_bessel_k1_scaled"
external bessel_k1_scaled_e : float -> result
  = "ml_gsl_sf_bessel_k1_scaled_e"
external bessel_kl_scaled : int -> float -> float
  = "ml_gsl_sf_bessel_kl_scaled"
external bessel_kl_scaled_e : int -> float -> result
  = "ml_gsl_sf_bessel_kl_scaled_e"
external bessel_kl_scaled_array : int -> float -> float array -> unit
  = "ml_gsl_sf_bessel_kl_scaled_array"


external bessel_Jnu : float -> float -> float = "ml_gsl_sf_bessel_Jnu"
external bessel_Jnu_e : float -> float -> result = "ml_gsl_sf_bessel_Jnu_e"

external bessel_sequence_Jnu_e : float -> mode -> float array -> unit
  = "ml_gsl_sf_bessel_sequence_Jnu_e"

external bessel_Ynu : float -> float -> float = "ml_gsl_sf_bessel_Ynu"
external bessel_Ynu_e : float -> float -> result = "ml_gsl_sf_bessel_Ynu_e"

external bessel_Inu : float -> float -> float = "ml_gsl_sf_bessel_Inu"
external bessel_Inu_e : float -> float -> result = "ml_gsl_sf_bessel_Inu_e"

external bessel_Inu_scaled : float -> float -> float
  = "ml_gsl_sf_bessel_Inu_scaled"
external bessel_Inu_scaled_e : float -> float -> result
  = "ml_gsl_sf_bessel_Inu_scaled_e"

external bessel_Knu : float -> float -> float = "ml_gsl_sf_bessel_Knu"
external bessel_Knu_e : float -> float -> result = "ml_gsl_sf_bessel_Knu_e"

external bessel_lnKnu : float -> float -> float = "ml_gsl_sf_bessel_lnKnu"
external bessel_lnKnu_e : float -> float -> result
  = "ml_gsl_sf_bessel_lnKnu_e"

external bessel_Knu_scaled : float -> float -> float
  = "ml_gsl_sf_bessel_Knu_scaled"
external bessel_Knu_scaled_e : float -> float -> result
  = "ml_gsl_sf_bessel_Knu_scaled_e"


external bessel_zero_J0 : int -> float = "ml_gsl_sf_bessel_zero_J0"
external bessel_zero_J0_e : int -> result = "ml_gsl_sf_bessel_zero_J0_e"

external bessel_zero_J1 : int -> float = "ml_gsl_sf_bessel_zero_J1"
external bessel_zero_J1_e : int -> result = "ml_gsl_sf_bessel_zero_J1_e"

external bessel_zero_Jnu : float -> int -> float
  = "ml_gsl_sf_bessel_zero_Jnu"
external bessel_zero_Jnu_e : float -> int -> result
  = "ml_gsl_sf_bessel_zero_Jnu_e"


(** {2 Clausen functions} *)

external clausen : float -> float = "ml_gsl_sf_clausen"
external clausen_e : float -> result = "ml_gsl_sf_clausen_e"


(** {2 Coulomb functions} *)

external hydrogenicR_1 : float -> float -> float = "ml_gsl_sf_hydrogenicR_1"
external hydrogenicR_1_e : float -> float -> result
  = "ml_gsl_sf_hydrogenicR_1_e"

external hydrogenicR : int -> int -> float -> float -> float
  = "ml_gsl_sf_hydrogenicR"
external hydrogenicR_e : int -> int -> float -> float -> result
  = "ml_gsl_sf_hydrogenicR_e"


(* FIXME: COULOMB wave functions *)

external coulomb_CL_e : float -> float -> result = "ml_gsl_sf_coulomb_CL_e"

external coulomb_CL_array : float -> float -> float array -> unit
  = "ml_gsl_sf_coulomb_CL_array"


(* FIXME: coupling coeffs *)

(** {2 Dawson functions} *)

external dawson : float -> float = "ml_gsl_sf_dawson"
external dawson_e : float -> result = "ml_gsl_sf_dawson_e"


(** {2 Debye functions} *)

external debye_1 : float -> float = "ml_gsl_sf_debye_1"
external debye_1_e : float -> result = "ml_gsl_sf_debye_1_e"

external debye_2 : float -> float = "ml_gsl_sf_debye_2"
external debye_2_e : float -> result = "ml_gsl_sf_debye_2_e"

external debye_3 : float -> float = "ml_gsl_sf_debye_3"
external debye_3_e : float -> result = "ml_gsl_sf_debye_3_e"

external debye_4 : float -> float = "ml_gsl_sf_debye_4"
external debye_4_e : float -> result = "ml_gsl_sf_debye_4_e"

external debye_5 : float -> float = "ml_gsl_sf_debye_5"
external debye_5_e : float -> result = "ml_gsl_sf_debye_5_e"

external debye_6 : float -> float = "ml_gsl_sf_debye_6"
external debye_6_e : float -> result = "ml_gsl_sf_debye_6_e"


(** {2 Dilogarithm} *)

external dilog : float -> float = "ml_gsl_sf_dilog"
external dilog_e : float -> result = "ml_gsl_sf_dilog_e"

external complex_dilog_xy_e : float -> float -> result * result
  = "ml_gsl_sf_complex_dilog_xy_e"

external complex_dilog_e : float -> float -> result * result
  = "ml_gsl_sf_complex_dilog_e"

external complex_spence_xy_e : float -> float -> result * result
  = "ml_gsl_sf_complex_spence_xy_e"


(** {2 Elementary operations} *)

external multiply_e : float -> float -> result = "ml_gsl_sf_multiply_e"

external multiply_err_e : x:float -> dx:float -> y:float -> dy:float -> result
  = "ml_gsl_sf_multiply_err_e"


(** {2 Elliptic integrals} *)

external ellint_Kcomp : float -> mode -> float = "ml_gsl_sf_ellint_Kcomp"
external ellint_Kcomp_e : float -> mode -> result
  = "ml_gsl_sf_ellint_Kcomp_e"

external ellint_Ecomp : float -> mode -> float = "ml_gsl_sf_ellint_Ecomp"
external ellint_Ecomp_e : float -> mode -> result
  = "ml_gsl_sf_ellint_Ecomp_e"

external ellint_Pcomp : float -> float -> mode -> float
  = "ml_gsl_sf_ellint_Pcomp"
external ellint_Pcomp_e : float -> float -> mode -> result
  = "ml_gsl_sf_ellint_Pcomp_e"

external ellint_Dcomp : float -> mode -> float = "ml_gsl_sf_ellint_Dcomp"
external ellint_Dcomp_e : float -> mode -> result
  = "ml_gsl_sf_ellint_Dcomp_e"

external ellint_F : float -> float -> mode -> float = "ml_gsl_sf_ellint_F"
external ellint_F_e : float -> float -> mode -> result
  = "ml_gsl_sf_ellint_F_e"

external ellint_E : float -> float -> mode -> float = "ml_gsl_sf_ellint_E"
external ellint_E_e : float -> float -> mode -> result
  = "ml_gsl_sf_ellint_E_e"

external ellint_P : float -> float -> float -> mode -> float
  = "ml_gsl_sf_ellint_P"
external ellint_P_e : float -> float -> float -> mode -> result
  = "ml_gsl_sf_ellint_P_e"

external ellint_D : float -> float -> mode -> float = "ml_gsl_sf_ellint_D"
external ellint_D_e : float -> float -> mode -> result
  = "ml_gsl_sf_ellint_D_e"

external ellint_RC : float -> float -> mode -> float = "ml_gsl_sf_ellint_RC"
external ellint_RC_e : float -> float -> mode -> result
  = "ml_gsl_sf_ellint_RC_e"

external ellint_RD : float -> float -> float -> mode -> float
  = "ml_gsl_sf_ellint_RD"
external ellint_RD_e : float -> float -> float -> mode -> result
  = "ml_gsl_sf_ellint_RD_e"

external ellint_RF : float -> float -> float -> mode -> float
  = "ml_gsl_sf_ellint_RF"
external ellint_RF_e : float -> float -> float -> mode -> result
  = "ml_gsl_sf_ellint_RF_e"

external ellint_RJ : float -> float -> float -> float -> mode -> float
  = "ml_gsl_sf_ellint_RJ"
external ellint_RJ_e : float -> float -> float -> float -> mode -> result
  = "ml_gsl_sf_ellint_RJ_e"

(* FIXME: elljac_e *)

(** {2 Error function} *)

external erf : float -> float = "ml_gsl_sf_erf" "gsl_sf_erf" "float"
external erf_e : float -> result = "ml_gsl_sf_erf_e"

external erfc : float -> float = "ml_gsl_sf_erfc" "gsl_sf_erfc" "float"
external erfc_e : float -> result = "ml_gsl_sf_erfc_e"

external log_erfc : float -> float
  = "ml_gsl_sf_log_erfc" "gsl_sf_log_erfc" "float"
external log_erfc_e : float -> result = "ml_gsl_sf_log_erfc_e"

external erf_Z : float -> float = "ml_gsl_sf_erf_Z" "gsl_sf_erf_Z" "float"
external erf_Z_e : float -> result = "ml_gsl_sf_erf_Z_e"

external erf_Q : float -> float = "ml_gsl_sf_erf_Q" "gsl_sf_erf_Q" "float"
external erf_Q_e : float -> result = "ml_gsl_sf_erf_Q_e"


(** {2 Exponential functions} *)

external exp : float -> float = "ml_gsl_sf_exp" "gsl_sf_exp" "float"
external exp_e : float -> result = "ml_gsl_sf_exp_e"

(** [exp x] computes the exponential function eˣ using GSL semantics
    and error checking.  *)

external exp_e10 : float -> result_e10 = "ml_gsl_sf_exp_e10_e"

(** [exp_e10 x] computes the exponential eˣ and returns a result with
    extended range. This function may be useful if the value of eˣ
    would overflow the numeric range of double.  *)

external exp_mult : float -> float -> float = "ml_gsl_sf_exp_mult"
external exp_mult_e : float -> float -> result = "ml_gsl_sf_exp_mult_e"

(** [exp_mult x y] exponentiate [x] and multiply by the factor [y] to
    return the product y eˣ.  *)

external exp_mult_e10 : float -> float -> result_e10
  = "ml_gsl_sf_exp_mult_e10_e"

(** Same as {!exp_e10} but return a result with extended numeric range. *)

external expm1 : float -> float = "ml_gsl_sf_expm1"
external expm1_e : float -> result = "ml_gsl_sf_expm1_e"

(** [expm1 x] compute the quantity eˣ-1 using an algorithm that is
    accurate for small [x].  *)

external exprel : float -> float = "ml_gsl_sf_exprel"
external exprel_e : float -> result = "ml_gsl_sf_exprel_e"

(** [exprel x] compute the quantity (eˣ-1)/x using an algorithm that
    is accurate for small [x].  For small [x] the algorithm is based
    on the expansion (eˣ-1)/x = 1 + x/2 + x²/(2*3) + x³/(2*3*4) + ⋯  *)

external exprel_2 : float -> float = "ml_gsl_sf_exprel_2"
external exprel_2_e : float -> result = "ml_gsl_sf_exprel_2_e"

(** [exprel_2 x] compute the quantity 2(eˣ-1-x)/x² using an algorithm
    that is accurate for small [x].  For small x the algorithm is
    based on the expansion 2(eˣ-1-x)/x^2 = 1 + x/3 + x²/(3*4) +
    x³/(3*4*5) + ⋯ *)

external exprel_n : int -> float -> float = "ml_gsl_sf_exprel_n"
external exprel_n_e : int -> float -> result = "ml_gsl_sf_exprel_n_e"

(** [exprel_n x] compute the [n]-relative exponential, which is the
    n-th generalization of the functions {!exprel} and
    {!exprel_2}. The N-relative exponential is given by,
    {[
                             n-1
    exprel_n x = n!/xⁿ (aˣ -  ∑ xᵏ/k!)
                             k=0
               = 1 + x/(N+1) + x²/((N+1)(N+2)) + ⋯
    ]}*)

external exp_err_e : x:float -> dx:float -> result = "ml_gsl_sf_exp_err_e"

external exp_err_e10 : x:float -> dx:float -> result_e10
  = "ml_gsl_sf_exp_err_e10_e"

external exp_mult_err_e : x:float -> dx:float -> y:float -> dy:float -> result
  = "ml_gsl_sf_exp_mult_err_e"

external exp_mult_err_e10_e : x:float -> dx:float -> y:float -> dy:float -> result_e10
  = "ml_gsl_sf_exp_mult_err_e10_e"


(** {2 Exponential integrals} *)

external expint_E1 : float -> float = "ml_gsl_sf_expint_E1"
external expint_E1_e : float -> result = "ml_gsl_sf_expint_E1_e"

external expint_E2 : float -> float = "ml_gsl_sf_expint_E2"
external expint_E2_e : float -> result = "ml_gsl_sf_expint_E2_e"

external expint_E1_scaled : float -> float = "ml_gsl_sf_expint_E1_scaled"
external expint_E1_scaled_e : float -> result
  = "ml_gsl_sf_expint_E1_scaled_e"

external expint_E2_scaled : float -> float = "ml_gsl_sf_expint_E2_scaled"
external expint_E2_scaled_e : float -> result
  = "ml_gsl_sf_expint_E2_scaled_e"

external expint_Ei : float -> float = "ml_gsl_sf_expint_Ei"
external expint_Ei_e : float -> result = "ml_gsl_sf_expint_Ei_e"

external expint_Ei_scaled : float -> float = "ml_gsl_sf_expint_Ei_scaled"
external expint_Ei_scaled_e : float -> result
  = "ml_gsl_sf_expint_Ei_scaled_e"

external shi : float -> float = "ml_gsl_sf_Shi"

external chi : float -> float = "ml_gsl_sf_Chi"

external expint_3 : float -> float = "ml_gsl_sf_expint_3"
external expint_3_e : float -> result = "ml_gsl_sf_expint_3_e"

external si : float -> float = "ml_gsl_sf_Si"

external ci : float -> float = "ml_gsl_sf_Ci"

external atanint : float -> float = "ml_gsl_sf_atanint"
external atanint_e : float -> result = "ml_gsl_sf_atanint_e"


(** {2 Fermi-Dirac function} *)

external fermi_dirac_m1 : float -> float = "ml_gsl_sf_fermi_dirac_m1"
external fermi_dirac_m1_e : float -> result = "ml_gsl_sf_fermi_dirac_m1_e"

external fermi_dirac_0 : float -> float = "ml_gsl_sf_fermi_dirac_0"
external fermi_dirac_0_e : float -> result = "ml_gsl_sf_fermi_dirac_0_e"

external fermi_dirac_1 : float -> float = "ml_gsl_sf_fermi_dirac_1"
external fermi_dirac_1_e : float -> result = "ml_gsl_sf_fermi_dirac_1_e"

external fermi_dirac_2 : float -> float = "ml_gsl_sf_fermi_dirac_2"
external fermi_dirac_2_e : float -> result = "ml_gsl_sf_fermi_dirac_2_e"

external fermi_dirac_int : int -> float -> float
  = "ml_gsl_sf_fermi_dirac_int"
external fermi_dirac_int_e : int -> float -> result
  = "ml_gsl_sf_fermi_dirac_int_e"

external fermi_dirac_mhalf : float -> float = "ml_gsl_sf_fermi_dirac_mhalf"
external fermi_dirac_mhalf_e : float -> result
  = "ml_gsl_sf_fermi_dirac_mhalf_e"

external fermi_dirac_half : float -> float = "ml_gsl_sf_fermi_dirac_half"
external fermi_dirac_half_e : float -> result
  = "ml_gsl_sf_fermi_dirac_half_e"

external fermi_dirac_3half : float -> float = "ml_gsl_sf_fermi_dirac_3half"
external fermi_dirac_3half_e : float -> result
  = "ml_gsl_sf_fermi_dirac_3half_e"

external fermi_dirac_inc_0 : float -> float -> float
  = "ml_gsl_sf_fermi_dirac_inc_0"
external fermi_dirac_inc_0_e : float -> float -> result
  = "ml_gsl_sf_fermi_dirac_inc_0_e"


(** {2 Gamma function} *)

external gamma : float -> float = "ml_gsl_sf_gamma"

external gamma_e : float -> result = "ml_gsl_sf_gamma_e"

external lngamma : float -> float = "ml_gsl_sf_lngamma"
external lngamma_e : float -> result = "ml_gsl_sf_lngamma_e"

external lngamma_sgn_e : float -> result * float = "ml_gsl_sf_lngamma_sgn_e"

external gammastar : float -> float = "ml_gsl_sf_gammastar"
external gammastar_e : float -> result = "ml_gsl_sf_gammastar_e"

external gammainv : float -> float = "ml_gsl_sf_gammainv"
external gammainv_e : float -> result = "ml_gsl_sf_gammainv_e"

external lngamma_complex_e : float -> float -> result * result
  = "ml_gsl_sf_lngamma_complex_e"

external taylorcoeff : int -> float -> float = "ml_gsl_sf_taylorcoeff"
external taylorcoeff_e : int -> float -> result = "ml_gsl_sf_taylorcoeff_e"

external fact : int -> float = "ml_gsl_sf_fact"
external fact_e : int -> result = "ml_gsl_sf_fact_e"

external doublefact : int -> float = "ml_gsl_sf_doublefact"
external doublefact_e : int -> result = "ml_gsl_sf_doublefact_e"

external lnfact : int -> float = "ml_gsl_sf_lnfact"
external lnfact_e : int -> result = "ml_gsl_sf_lnfact_e"

external lndoublefact : int -> float = "ml_gsl_sf_lndoublefact"
external lndoublefact_e : int -> result = "ml_gsl_sf_lndoublefact_e"

external choose : int -> int -> float = "ml_gsl_sf_choose"
external choose_e : int -> int -> result = "ml_gsl_sf_choose_e"

external lnchoose : int -> int -> float = "ml_gsl_sf_lnchoose"
external lnchoose_e : int -> int -> result = "ml_gsl_sf_lnchoose_e"

external poch : float -> float -> float = "ml_gsl_sf_poch"
external poch_e : float -> float -> result = "ml_gsl_sf_poch_e"

external lnpoch : float -> float -> float = "ml_gsl_sf_lnpoch"
external lnpoch_e : float -> float -> result = "ml_gsl_sf_lnpoch_e"

external lnpoch_sgn_e : float -> float -> result * float
  = "ml_gsl_sf_lngamma_sgn_e"

external pochrel : float -> float -> float = "ml_gsl_sf_pochrel"
external pochrel_e : float -> float -> result = "ml_gsl_sf_pochrel_e"

external gamma_inc_Q : float -> float -> float = "ml_gsl_sf_gamma_inc_Q"
external gamma_inc_Q_e : float -> float -> result = "ml_gsl_sf_gamma_inc_Q_e"

external gamma_inc_P : float -> float -> float = "ml_gsl_sf_gamma_inc_P"
external gamma_inc_P_e : float -> float -> result = "ml_gsl_sf_gamma_inc_P_e"

external gamma_inc : float -> float -> float = "ml_gsl_sf_gamma_inc"
external gamma_inc_e : float -> float -> result = "ml_gsl_sf_gamma_inc_e"

external beta : float -> float -> float = "ml_gsl_sf_beta"
external beta_e : float -> float -> result = "ml_gsl_sf_beta_e"

external lnbeta : float -> float -> float = "ml_gsl_sf_lnbeta"
external lnbeta_e : float -> float -> result = "ml_gsl_sf_lnbeta_e"

external lnbeta_sgn_e : float -> float -> result * float
  = "ml_gsl_sf_lnbeta_sgn_e"

external beta_inc : float -> float -> float -> float = "ml_gsl_sf_beta_inc"
external beta_inc_e : float -> float -> float -> result
  = "ml_gsl_sf_beta_inc_e"


(** {2 Gegenbauer functions aka Ultraspherical polynomials}

    Gegenbauer functions are defined in {{:http://dlmf.nist.gov/18.3} DLMF}. *)

external gegenpoly_1 : float -> float -> float = "ml_gsl_sf_gegenpoly_1"
external gegenpoly_1_e : float -> float -> result = "ml_gsl_sf_gegenpoly_1_e"

(** [gegenpoly_1 l x] = C₁⁽ˡ⁾(x). *)

external gegenpoly_2 : float -> float -> float = "ml_gsl_sf_gegenpoly_2"
external gegenpoly_2_e : float -> float -> result = "ml_gsl_sf_gegenpoly_2_e"

(** [gegenpoly_2 l x] = C₂⁽ˡ⁾(x). *)

external gegenpoly_3 : float -> float -> float = "ml_gsl_sf_gegenpoly_3"
external gegenpoly_3_e : float -> float -> result = "ml_gsl_sf_gegenpoly_3_e"

(** [gegenpoly_3 l x] = C₃⁽ˡ⁾(x). *)

external gegenpoly_n : int -> float -> float -> float
  = "ml_gsl_sf_gegenpoly_n"
external gegenpoly_n_e : int -> float -> float -> result
  = "ml_gsl_sf_gegenpoly_n_e"

(** [gegenpoly_n n l x] = Cₙ⁽ˡ⁾(x).  Constraints: l > -1/2, n ≥ 0. *)

external gegenpoly_array : float -> float -> float array -> unit
  = "ml_gsl_sf_gegenpoly_array"

(** [gegenpoly_array l x c] computes an array of Gegenbauer
    polynomials c.(n) = Cₙ⁽ˡ⁾(x) for n = 0, 1, 2,̣..., [Array.length c - 1].
    Constraints: l > -1/2. *)


(** {2 Hypergeometric functions} *)

(* FIXME *)

(** {2 Laguerre functions} *)

external laguerre_1 : float -> float -> float = "ml_gsl_sf_laguerre_1"
external laguerre_1_e : float -> float -> result = "ml_gsl_sf_laguerre_1_e"

external laguerre_2 : float -> float -> float = "ml_gsl_sf_laguerre_2"
external laguerre_2_e : float -> float -> result = "ml_gsl_sf_laguerre_2_e"

external laguerre_3 : float -> float -> float = "ml_gsl_sf_laguerre_3"
external laguerre_3_e : float -> float -> result = "ml_gsl_sf_laguerre_3_e"

external laguerre_n : int -> float -> float -> float = "ml_gsl_sf_laguerre_n"
external laguerre_n_e : int -> float -> float -> result
  = "ml_gsl_sf_laguerre_n_e"


(** {2 Lambert W functions} *)

external lambert_W0 : float -> float = "ml_gsl_sf_lambert_W0"
external lambert_W0_e : float -> result = "ml_gsl_sf_lambert_W0_e"

external lambert_Wm1 : float -> float = "ml_gsl_sf_lambert_Wm1"
external lambert_Wm1_e : float -> result = "ml_gsl_sf_lambert_Wm1_e"


(** {2 Legendre functions} *)

external legendre_P1 : float -> float = "ml_gsl_sf_legendre_P1"
external legendre_P1_e : float -> result = "ml_gsl_sf_legendre_P1_e"

external legendre_P2 : float -> float = "ml_gsl_sf_legendre_P2"
external legendre_P2_e : float -> result = "ml_gsl_sf_legendre_P2_e"

external legendre_P3 : float -> float = "ml_gsl_sf_legendre_P3"
external legendre_P3_e : float -> result = "ml_gsl_sf_legendre_P3_e"

external legendre_Pl : int -> float -> float = "ml_gsl_sf_legendre_Pl"
external legendre_Pl_e : int -> float -> result = "ml_gsl_sf_legendre_Pl_e"

external legendre_Pl_array : float -> float array -> unit
  = "ml_gsl_sf_legendre_Pl_array"

external legendre_Q0 : float -> float = "ml_gsl_sf_legendre_Q0"
external legendre_Q0_e : float -> result = "ml_gsl_sf_legendre_Q0_e"

external legendre_Q1 : float -> float = "ml_gsl_sf_legendre_Q1"
external legendre_Q1_e : float -> result = "ml_gsl_sf_legendre_Q1_e"

external legendre_Ql : int -> float -> float = "ml_gsl_sf_legendre_Ql"
external legendre_Ql_e : int -> float -> result = "ml_gsl_sf_legendre_Ql_e"


(** {2 Associated Legendre functions and Spherical Harmonics} *)

(** Normalization of Legendre functions.
    See {{:https://www.gnu.org/software/gsl/manual/html_node/Associated-Legendre-Polynomials-and-Spherical-Harmonics.html#Associated-Legendre-Polynomials-and-Spherical-Harmonics}
    the GSL documentation}. *)
type legendre_t =
  | Schmidt (** Specifies the computation of the Schmidt semi-normalized
                associated Legendre polynomials Sₗᵐ(x). *)
  | Spharm (** Specifies the computation of the spherical harmonic
               associated Legendre polynomials Yₗᵐ(x). *)
  | Full (** Specifies the computation of the fully normalized associated
             Legendre polynomials Nₗᵐ(x). *)
  | None (** Specifies the computation of the unnormalized associated
             Legendre polynomials Pₗᵐ(x). *)
(* FIXME: keep in the same order as the C definition in gsl_sf_legendre.h *)

(* FIXME: to avoid repetitive checks on the length of the array,
   it would be better to structure the code in a more abstract way. *)
external legendre_array : legendre_t -> int -> float -> float array -> unit
  = "ml_gsl_sf_legendre_array"

(** [legendre_array norm lmax x result] calculate all normalized
    associated Legendre polynomials for 0 ≤ [l] ≤ [lmax] and
    [0 ≤ m ≤ l] for [|x| ≤ 1].  The [norm] parameter specifies which
    normalization is used.  The normalized Pₗᵐ(x) values are stored in
    [result], whose minimum size can be obtained from calling
    {!legendre_array_n}.  The array index of Pₗᵐ(x) is obtained from
    calling {!legendre_array_index}[(l, m)].  To include or exclude
    the Condon-Shortley phase factor of (-1)ᵐ, set the parameter
    csphase to either -1 or 1 respectively in the _e function. This
    factor is included by default.  *)

(* FIXME: more associated Legendre functions to bind. *)

external legendre_array_n : int -> int = "ml_gsl_sf_legendre_array_n"

(** [legendre_array_n lmax] returns the minimum array size for maximum
    degree lmax needed for the array versions of the associated
    Legendre functions.  Size is calculated as the total number of
    Pₗᵐ(x) functions, plus extra space for precomputing multiplicative
    factors used in the recurrence relations.  *)

external legendre_array_index : int -> int -> int
  = "ml_gsl_sf_legendre_array_index"

(** [legendre_array_index l m] returns the index into the [result]
    array of {!legendre_array}, {!legendre_deriv_array},
    {!legendre_deriv_alt_array}, {!legendre_deriv2_array}, and
    {!legendre_deriv2_alt_array} corresponding to Pₗᵐ(x), ∂ₓPₗᵐ(x),
    or ∂ₓ²Pₗₗᵐ(x).  The index is given by l(l+1)/2 + m.  *)
(* FIXME: it would likely be more efficient to implement this function
   directly in OCaml. *)

external legendre_Plm : int -> int -> float -> float
  = "ml_gsl_sf_legendre_Plm"
external legendre_Plm_e : int -> int -> float -> result
  = "ml_gsl_sf_legendre_Plm_e"

(** [legendre_Plm l m x] and [legendre_Plm_e l m x] compute the
    associated Legendre polynomial Pₗᵐ(x) for [m ≥ 0], [l ≥ m],
    [|x| ≤ 1].  *)

external legendre_sphPlm : int -> int -> float -> float
  = "ml_gsl_sf_legendre_sphPlm"
external legendre_sphPlm_e : int -> int -> float -> result
  = "ml_gsl_sf_legendre_sphPlm_e"

(** [legendre_sphPlm l m x] and [legendre_Plm_e] compute the
    normalized associated Legendre polynomial √((2l+1)/(4\pi))
    √((l-m)!/(l+m)!) Pₗᵐ(x) suitable for use in spherical harmonics.
    The parameters must satisfy [m ≥ 0], [l ≥ m], [|x| ≤ 1].  Theses
    routines avoid the overflows that occur for the standard
    normalization of Pₗᵐ(x).  *)


(** {2 Logarithm and related functions} *)

external log : float -> float = "ml_gsl_sf_log"
external log_e : float -> result = "ml_gsl_sf_log_e"

external log_abs : float -> float = "ml_gsl_sf_log_abs"
external log_abs_e : float -> result = "ml_gsl_sf_log_abs_e"

external log_complex_e : float -> float -> result * result
  = "ml_gsl_sf_complex_log_e"

external log_1plusx : float -> float = "ml_gsl_sf_log_1plusx"
external log_1plusx_e : float -> result = "ml_gsl_sf_log_1plusx_e"

external log_1plusx_mx : float -> float = "ml_gsl_sf_log_1plusx_mx"
external log_1plusx_mx_e : float -> result = "ml_gsl_sf_log_1plusx_mx_e"


(** {2 Power function} *)

external pow_int : float -> int -> float = "ml_gsl_sf_pow_int"
external pow_int_e : float -> int -> result = "ml_gsl_sf_pow_int_e"


(** {2 Psi (Digamma) function} *)

external psi_int : int -> float = "ml_gsl_sf_psi_int"
external psi_int_e : int -> result = "ml_gsl_sf_psi_int_e"

external psi : float -> float = "ml_gsl_sf_psi"
external psi_e : float -> result = "ml_gsl_sf_psi_e"

external psi_1piy : float -> float = "ml_gsl_sf_psi_1piy"
external psi_1piy_e : float -> result = "ml_gsl_sf_psi_1piy_e"

external psi_complex_e : float -> float -> result * result
  = "ml_gsl_sf_complex_psi_e"

external psi_1_int : int -> float = "ml_gsl_sf_psi_1_int"
external psi_1_int_e : int -> result = "ml_gsl_sf_psi_1_int_e"

external psi_1 : float -> float = "ml_gsl_sf_psi_1"
external psi_1_e : float -> result = "ml_gsl_sf_psi_1_e"

external psi_n : int -> float -> float = "ml_gsl_sf_psi_n"
external psi_n_e : int -> float -> result = "ml_gsl_sf_psi_n_e"


(** {2 Synchrotron functions} *)

external synchrotron_1 : float -> float = "ml_gsl_sf_synchrotron_1"
external synchrotron_1_e : float -> result = "ml_gsl_sf_synchrotron_1_e"

external synchrotron_2 : float -> float = "ml_gsl_sf_synchrotron_2"
external synchrotron_2_e : float -> result = "ml_gsl_sf_synchrotron_2_e"


(** {2 Transport functions} *)

external transport_2 : float -> float = "ml_gsl_sf_transport_2"
external transport_2_e : float -> result = "ml_gsl_sf_transport_2_e"

external transport_3 : float -> float = "ml_gsl_sf_transport_3"
external transport_3_e : float -> result = "ml_gsl_sf_transport_3_e"

external transport_4 : float -> float = "ml_gsl_sf_transport_4"
external transport_4_e : float -> result = "ml_gsl_sf_transport_4_e"

external transport_5 : float -> float = "ml_gsl_sf_transport_5"
external transport_5_e : float -> result = "ml_gsl_sf_transport_5_e"


(** {2 Trigonometric functions} *)

external sin : float -> float = "ml_gsl_sf_sin" "gsl_sf_sin" "float"
external sin_e : float -> result = "ml_gsl_sf_sin_e"

external cos : float -> float = "ml_gsl_sf_cos" "gsl_sf_cos" "float"
external cos_e : float -> result = "ml_gsl_sf_cos_e"

external hypot : float -> float -> float = "ml_gsl_sf_hypot"
external hypot_e : float -> float -> result = "ml_gsl_sf_hypot_e"

external sinc : float -> float = "ml_gsl_sf_sinc" "gsl_sf_sinc" "float"
external sinc_e : float -> result = "ml_gsl_sf_sinc_e"

external complex_sin_e : float -> float -> result * result
  = "ml_gsl_sf_complex_sin_e"

external complex_cos_e : float -> float -> result * result
  = "ml_gsl_sf_complex_cos_e"

external complex_logsin_e : float -> float -> result * result
  = "ml_gsl_sf_complex_logsin_e"

external lnsinh : float -> float = "ml_gsl_sf_lnsinh"
external lnsinh_e : float -> result = "ml_gsl_sf_lnsinh_e"

external lncosh : float -> float = "ml_gsl_sf_lncosh"
external lncosh_e : float -> result = "ml_gsl_sf_lncosh_e"

external rect_of_polar : r:float -> theta:float -> result * result
  = "ml_gsl_sf_polar_to_rect"

external polar_of_rect : x:float -> y:float -> result * result
  = "ml_gsl_sf_rect_to_polar"

external angle_restrict_symm : float -> float
  = "ml_gsl_sf_angle_restrict_symm"

external angle_restrict_pos : float -> float = "ml_gsl_sf_angle_restrict_pos"

external sin_err_e : float -> dx:float -> result = "ml_gsl_sf_sin_err_e"

external cos_err_e : float -> dx:float -> result = "ml_gsl_sf_cos_err_e"


(** {2 Zeta functions} *)

external zeta_int : int -> float = "ml_gsl_sf_zeta_int"
external zeta_int_e : int -> result = "ml_gsl_sf_zeta_int_e"

external zeta : float -> float = "ml_gsl_sf_zeta"
external zeta_e : float -> result = "ml_gsl_sf_zeta_e"

external hzeta : float -> float -> float = "ml_gsl_sf_hzeta"
external hzeta_e : float -> float -> result = "ml_gsl_sf_hzeta_e"

external eta_int : int -> float = "ml_gsl_sf_eta_int"
external eta_int_e : int -> result = "ml_gsl_sf_eta_int_e"

external eta : float -> float = "ml_gsl_sf_eta"
external eta_e : float -> result = "ml_gsl_sf_eta_e"


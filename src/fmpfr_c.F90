!   This file is part of the FMPFR library.
! 
!   Copyright 2022 by Thomas Koenig
! 
!   Permission is hereby granted, free of charge, to any person
!   obtaining a copy of this software and associated documentation
!   files (the "Software"), to deal in the Software without
!   restriction, including without limitation the rights to use, copy,
!   modify, merge, publish, distribute, sublicense, and/or sell copies
!   of the Software, and to permit persons to whom the Software is
!   furnished to do so, subject to the following conditions:
! 
!   The above copyright notice and this permission notice shall be
!   included in all copies or substantial portions of the Software.
! 
!   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
!   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
!   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
!   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
!   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
!   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!   SOFTWARE.

module fmpfr_c
  use fmpfr_kinds
  interface
    pure subroutine mpfr_init2 (x, prec) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: x
      integer (mpfr_prec_kind), value, intent(in) :: prec
    end subroutine mpfr_init2
    pure subroutine mpfr_clear (x) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: x
    end subroutine mpfr_clear
    subroutine mpfr_init (x) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
    end subroutine mpfr_init
    subroutine mpfr_set_default_prec (prec) bind(c)
      import
      implicit none
      integer (mpfr_prec_kind), value :: prec
    end subroutine mpfr_set_default_prec
    function mpfr_get_default_prec () result (ret) bind(c)
      import
      implicit none
      integer (mpfr_prec_kind) :: ret
    end function mpfr_get_default_prec
    pure subroutine mpfr_set_prec (x, prec) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: x
      integer (mpfr_prec_kind), value, intent(in) :: prec
    end subroutine mpfr_set_prec
    pure function mpfr_get_prec (x) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: x
      integer (mpfr_prec_kind) :: ret
    end function mpfr_get_prec
    pure function mpfr_set (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: rop
      type (mpfr_t), intent(in) :: op
      integer (c_int), value, intent(in) :: rnd
      integer (c_int) :: ret
    end function mpfr_set
    pure function mpfr_set_si (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: rop
      integer (c_long), value, intent(in) :: op
      integer (c_int), value, intent(in) :: rnd
      integer (c_int) :: ret
    end function mpfr_set_si
    pure function mpfr_set_flt (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: rop
      real (c_float), value, intent(in) :: op
      integer (c_int), value, intent(in) :: rnd
      integer (c_int) :: ret
    end function mpfr_set_flt
    pure function mpfr_set_d (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: rop
      real (c_double), value, intent(in) :: op
      integer (c_int), value, intent(in) :: rnd
      integer (c_int) :: ret
    end function mpfr_set_d
#if USE_LONG_DOUBLE
    pure function mpfr_set_ld (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: rop
      real (c_long_double), value, intent(in) :: op
      integer (c_int), value, intent(in) :: rnd
      integer (c_int) :: ret
    end function mpfr_set_ld
#endif

#if USE_FLOAT128
    pure function mpfr_set_float128 (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: rop
      real (qp), value, intent(in) :: op
      integer (c_int), value, intent(in) :: rnd
      integer (c_int) :: ret
    end function mpfr_set_float128
#endif

    function mpfr_set_si_2exp (rop, op, e, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      integer (c_long), value :: op
      integer (mpfr_exp_kind), value :: e
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_set_si_2exp
    pure function mpfr_set_str (rop, s, base, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: rop
      type (c_ptr), value, intent(in) :: s
      integer (c_int), value, intent(in) :: base
      integer (c_int), value, intent(in) :: rnd
      integer (c_int) :: ret
    end function mpfr_set_str
    function mpfr_strtofr (rop, nptr, endptr, base, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (c_ptr), value :: nptr
      type (c_ptr) :: endptr
      integer (c_int), value :: base
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_strtofr
    subroutine mpfr_set_nan (x) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
    end subroutine mpfr_set_nan
    subroutine mpfr_set_inf (x, sign) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      integer (c_int), value :: sign
    end subroutine mpfr_set_inf
    subroutine mpfr_set_zero (x, sign) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      integer (c_int), value :: sign
    end subroutine mpfr_set_zero
    subroutine mpfr_swap (x, y) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      type (mpfr_t) :: y
    end subroutine mpfr_swap
    function mpfr_init_set_str (x, s, base, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      type (c_ptr), value :: s
      integer (c_int), value :: base
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_init_set_str
    pure function mpfr_get_flt (op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: op
      integer (c_int), value, intent(in) :: rnd
      real (c_float) :: ret
    end function mpfr_get_flt
    pure function mpfr_get_d (op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: op
      integer (c_int), value, intent(in) :: rnd
      real (c_double) :: ret
    end function mpfr_get_d
#if USE_LONG_DOUBLE
    pure function mpfr_get_ld (op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: op
      integer (c_int), value, intent(in) :: rnd
      real (c_long_double) :: ret
    end function mpfr_get_ld
#endif

#if USE_FLOAT128
    pure function mpfr_get_float128 (op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: op
      integer (c_int), value, intent(in) :: rnd
      real (qp) :: ret
    end function mpfr_get_float128
#endif

    pure function mpfr_get_si (op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: op
      integer (c_int), value, intent(in) :: rnd
      integer (c_long) :: ret
    end function mpfr_get_si
    function mpfr_get_sj (op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_intmax_t) :: ret
    end function mpfr_get_sj
    function mpfr_get_d_2exp (exp, op, rnd) result (ret) bind(c)
      import
      implicit none
      integer (c_long), intent(out) :: exp
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      real (c_double) :: ret
    end function mpfr_get_d_2exp
#if USE_LONG_DOUBLE
    function mpfr_get_ld_2exp (exp, op, rnd) result (ret) bind(c)
      import
      implicit none
      integer (c_long), intent(out) :: exp
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      real (c_long_double) :: ret
    end function mpfr_get_ld_2exp
#endif

    function mpfr_frexp (exp, y, x, rnd) result (ret) bind(c)
      import
      implicit none
      integer (mpfr_exp_kind) :: exp
      type (mpfr_t) :: y
      type (mpfr_t) :: x
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_frexp
    function mpfr_get_str_ndigits (b, p) result (ret) bind(c)
      import
      implicit none
      integer (c_int), value :: b
      integer (mpfr_prec_kind), value :: p
      integer (c_size_t) :: ret
    end function mpfr_get_str_ndigits
    function mpfr_get_str (str, expptr, base, n, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (c_ptr), value :: str
      integer (mpfr_exp_kind) :: expptr
      integer (c_int), value :: base
      integer (c_size_t), value :: n
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      type (c_ptr) :: ret
    end function mpfr_get_str
    subroutine mpfr_free_str (str) bind(c)
      import
      implicit none
      type (c_ptr), value :: str
    end subroutine mpfr_free_str
    function mpfr_fits_ulong_p (op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_fits_ulong_p
    function mpfr_fits_slong_p (op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_fits_slong_p
    function mpfr_fits_uint_p (op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_fits_uint_p
    function mpfr_fits_sint_p (op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_fits_sint_p
    function mpfr_fits_ushort_p (op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_fits_ushort_p
    function mpfr_fits_sshort_p (op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_fits_sshort_p
    function mpfr_fits_uintmax_p (op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_fits_uintmax_p
    function mpfr_fits_intmax_p (op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_fits_intmax_p
    function mpfr_add (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_add
    function mpfr_add_si (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      integer (c_long), value :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_add_si
    function mpfr_add_d (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      real (c_double), value :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_add_d
    function mpfr_sub (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_sub
    function mpfr_si_sub (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      integer (c_long), value :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_si_sub
    function mpfr_sub_si (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      integer (c_long), value :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_sub_si
    function mpfr_d_sub (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      real (c_double), value :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_d_sub
    function mpfr_sub_d (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      real (c_double), value :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_sub_d
    function mpfr_mul (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_mul
    function mpfr_mul_si (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      integer (c_long), value :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_mul_si
    function mpfr_mul_d (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      real (c_double), value :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_mul_d
    function mpfr_sqr (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_sqr
    function mpfr_div (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_div
    function mpfr_si_div (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      integer (c_long), value :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_si_div
    function mpfr_div_si (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      integer (c_long), value :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_div_si
    function mpfr_d_div (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      real (c_double), value :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_d_div
    function mpfr_div_d (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      real (c_double), value :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_div_d
    function mpfr_sqrt (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_sqrt
    function mpfr_rec_sqrt (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_rec_sqrt
    function mpfr_cbrt (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_cbrt
    function mpfr_neg (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_neg
    function mpfr_abs (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_abs
    function mpfr_dim (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_dim
    function mpfr_mul_2si (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      integer (c_long), value :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_mul_2si
    function mpfr_div_2si (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      integer (c_long), value :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_div_2si
    function mpfr_fma (rop, op1, op2, op3, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      type (mpfr_t) :: op3
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_fma
    function mpfr_fms (rop, op1, op2, op3, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      type (mpfr_t) :: op3
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_fms
    function mpfr_fmma (rop, op1, op2, op3, op4, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      type (mpfr_t) :: op3
      type (mpfr_t) :: op4
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_fmma
    function mpfr_fmms (rop, op1, op2, op3, op4, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      type (mpfr_t) :: op3
      type (mpfr_t) :: op4
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_fmms
    function mpfr_hypot (rop, x, y, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: x
      type (mpfr_t) :: y
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_hypot
    function mpfr_cmp (op1, op2) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int) :: ret
    end function mpfr_cmp
    function mpfr_cmp_si (op1, op2) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op1
      integer (c_long), value :: op2
      integer (c_int) :: ret
    end function mpfr_cmp_si
    function mpfr_cmp_d (op1, op2) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op1
      real (c_double), value :: op2
      integer (c_int) :: ret
    end function mpfr_cmp_d
#if USE_LONG_DOUBLE
    function mpfr_cmp_ld (op1, op2) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op1
      real (c_long_double), value :: op2
      integer (c_int) :: ret
    end function mpfr_cmp_ld
#endif

    function mpfr_cmp_si_2exp (op1, op2, e) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op1
      integer (c_long), value :: op2
      integer (mpfr_exp_kind), value :: e
      integer (c_int) :: ret
    end function mpfr_cmp_si_2exp
    function mpfr_cmpabs (op1, op2) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int) :: ret
    end function mpfr_cmpabs
    function mpfr_nan_p (op) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int) :: ret
    end function mpfr_nan_p
    function mpfr_inf_p (op) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int) :: ret
    end function mpfr_inf_p
    function mpfr_number_p (op) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int) :: ret
    end function mpfr_number_p
    function mpfr_zero_p (op) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int) :: ret
    end function mpfr_zero_p
    function mpfr_regular_p (op) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int) :: ret
    end function mpfr_regular_p
    pure function mpfr_greater_p (op1, op2) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int) :: ret
    end function mpfr_greater_p
    pure function mpfr_greaterequal_p (op1, op2) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int) :: ret
    end function mpfr_greaterequal_p
    pure function mpfr_less_p (op1, op2) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int) :: ret
    end function mpfr_less_p
    pure function mpfr_lessequal_p (op1, op2) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int) :: ret
    end function mpfr_lessequal_p
    pure function mpfr_equal_p (op1, op2) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int) :: ret
    end function mpfr_equal_p
    pure function mpfr_lessgreater_p (op1, op2) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int) :: ret
    end function mpfr_lessgreater_p
    function mpfr_unordered_p (op1, op2) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int) :: ret
    end function mpfr_unordered_p
    function mpfr_total_order_p (x, y) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      type (mpfr_t) :: y
      integer (c_int) :: ret
    end function mpfr_total_order_p
    function mpfr_log (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_log
    function mpfr_log2 (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_log2
    function mpfr_log10 (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_log10
    function mpfr_log1p (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_log1p
    function mpfr_exp (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_exp
    function mpfr_exp2 (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_exp2
    function mpfr_exp10 (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_exp10
    function mpfr_expm1 (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_expm1
    function mpfr_pow (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_pow
    function mpfr_pow_si (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      integer (c_long), value :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_pow_si
    function mpfr_cos (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_cos
    function mpfr_sin (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_sin
    function mpfr_tan (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_tan
    function mpfr_sin_cos (sop, cop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: sop
      type (mpfr_t) :: cop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_sin_cos
    function mpfr_sec (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_sec
    function mpfr_csc (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_csc
    function mpfr_cot (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_cot
    function mpfr_acos (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_acos
    function mpfr_asin (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_asin
    function mpfr_atan (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_atan
    function mpfr_atan2 (rop, y, x, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: y
      type (mpfr_t) :: x
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_atan2
    function mpfr_cosh (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_cosh
    function mpfr_sinh (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_sinh
    function mpfr_tanh (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_tanh
    function mpfr_sinh_cosh (sop, cop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: sop
      type (mpfr_t) :: cop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_sinh_cosh
    function mpfr_sech (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_sech
    function mpfr_csch (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_csch
    function mpfr_coth (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_coth
    function mpfr_acosh (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_acosh
    function mpfr_asinh (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_asinh
    function mpfr_atanh (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_atanh
    function mpfr_eint (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_eint
    function mpfr_li2 (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_li2
    function mpfr_gamma (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_gamma
    function mpfr_gamma_inc (rop, op, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_gamma_inc
    function mpfr_lngamma (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_lngamma
    function mpfr_lgamma (rop, signp, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      integer (c_int) :: signp
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_lgamma
    function mpfr_digamma (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_digamma
    function mpfr_beta (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_beta
    function mpfr_zeta (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_zeta
    function mpfr_erf (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_erf
    function mpfr_erfc (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_erfc
    function mpfr_j0 (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_j0
    function mpfr_j1 (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_j1
    function mpfr_jn (rop, n, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      integer (c_long), value :: n
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_jn
    function mpfr_y0 (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_y0
    function mpfr_y1 (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_y1
    function mpfr_yn (rop, n, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      integer (c_long), value :: n
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_yn
    function mpfr_agm (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_agm
    function mpfr_ai (rop, x, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: x
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_ai
    function mpfr_const_log2 (rop, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_const_log2
    function mpfr_const_pi (rop, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_const_pi
    function mpfr_const_euler (rop, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_const_euler
    function mpfr_const_catalan (rop, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_const_catalan
    subroutine mpfr_dump (op) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
    end subroutine mpfr_dump
    function mpfr_rint (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_rint
    function mpfr_ceil (rop, op) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int) :: ret
    end function mpfr_ceil
    function mpfr_floor (rop, op) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int) :: ret
    end function mpfr_floor
    function mpfr_round (rop, op) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int) :: ret
    end function mpfr_round
    function mpfr_roundeven (rop, op) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int) :: ret
    end function mpfr_roundeven
    function mpfr_trunc (rop, op) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int) :: ret
    end function mpfr_trunc
    function mpfr_rint_ceil (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_rint_ceil
    function mpfr_rint_floor (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_rint_floor
    function mpfr_rint_round (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_rint_round
    function mpfr_rint_roundeven (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_rint_roundeven
    function mpfr_rint_trunc (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_rint_trunc
    function mpfr_frac (rop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_frac
    function mpfr_modf (iop, fop, op, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: iop
      type (mpfr_t) :: fop
      type (mpfr_t) :: op
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_modf
    function mpfr_fmod (r, x, y, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: r
      type (mpfr_t) :: x
      type (mpfr_t) :: y
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_fmod
    function mpfr_fmodquo (r, q, x, y, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: r
      integer (c_long) :: q
      type (mpfr_t) :: x
      type (mpfr_t) :: y
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_fmodquo
    function mpfr_remainder (r, x, y, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: r
      type (mpfr_t) :: x
      type (mpfr_t) :: y
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_remainder
    function mpfr_remquo (r, q, x, y, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: r
      integer (c_long) :: q
      type (mpfr_t) :: x
      type (mpfr_t) :: y
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_remquo
    function mpfr_integer_p (op) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int) :: ret
    end function mpfr_integer_p
    subroutine mpfr_set_default_rounding_mode (rnd) bind(c)
      import
      implicit none
      integer (c_int), value :: rnd
    end subroutine mpfr_set_default_rounding_mode
    function mpfr_get_default_rounding_mode () result (ret) bind(c)
      import
      implicit none
      integer (c_int) :: ret
    end function mpfr_get_default_rounding_mode
    function mpfr_prec_round (x, prec, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      integer (mpfr_prec_kind), value :: prec
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_prec_round
    function mpfr_can_round (b, err, rnd1, rnd2, prec) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: b
      integer (mpfr_exp_kind), value :: err
      integer (c_int), value :: rnd1
      integer (c_int), value :: rnd2
      integer (mpfr_prec_kind), value :: prec
      integer (c_int) :: ret
    end function mpfr_can_round
    function mpfr_min_prec (x) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      integer (mpfr_prec_kind) :: ret
    end function mpfr_min_prec
    function mpfr_print_rnd_mode (rnd) result (ret) bind(c)
      import
      implicit none
      integer (c_int), value :: rnd
      type (c_ptr) :: ret
    end function mpfr_print_rnd_mode
    subroutine mpfr_nexttoward (x, y) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      type (mpfr_t) :: y
    end subroutine mpfr_nexttoward
    subroutine mpfr_nextabove (x) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
    end subroutine mpfr_nextabove
    subroutine mpfr_nextbelow (x) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
    end subroutine mpfr_nextbelow
    function mpfr_min (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_min
    function mpfr_max (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_max
    function mpfr_get_exp (x) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      integer (mpfr_exp_kind) :: ret
    end function mpfr_get_exp
    function mpfr_set_exp (x, e) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      integer (mpfr_exp_kind), value :: e
      integer (c_int) :: ret
    end function mpfr_set_exp
    function mpfr_signbit (op) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: op
      integer (c_int) :: ret
    end function mpfr_signbit
    function mpfr_setsign (rop, op, s, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op
      integer (c_int), value :: s
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_setsign
    function mpfr_copysign (rop, op1, op2, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_copysign
    function mpfr_get_version () result (ret) bind(c)
      import
      implicit none
      type (c_ptr) :: ret
    end function mpfr_get_version
    function mpfr_get_patches () result (ret) bind(c)
      import
      implicit none
      type (c_ptr) :: ret
    end function mpfr_get_patches
    function mpfr_buildopt_tls_p () result (ret) bind(c)
      import
      implicit none
      integer (c_int) :: ret
    end function mpfr_buildopt_tls_p
    function mpfr_buildopt_float128_p () result (ret) bind(c)
      import
      implicit none
      integer (c_int) :: ret
    end function mpfr_buildopt_float128_p
    function mpfr_buildopt_gmpinternals_p () result (ret) bind(c)
      import
      implicit none
      integer (c_int) :: ret
    end function mpfr_buildopt_gmpinternals_p
    function mpfr_buildopt_sharedcache_p () result (ret) bind(c)
      import
      implicit none
      integer (c_int) :: ret
    end function mpfr_buildopt_sharedcache_p
    function mpfr_buildopt_tune_case () result (ret) bind(c)
      import
      implicit none
      type (c_ptr) :: ret
    end function mpfr_buildopt_tune_case
    function mpfr_get_emin () result (ret) bind(c)
      import
      implicit none
      integer (mpfr_exp_kind) :: ret
    end function mpfr_get_emin
    function mpfr_get_emax () result (ret) bind(c)
      import
      implicit none
      integer (mpfr_exp_kind) :: ret
    end function mpfr_get_emax
    function mpfr_set_emin (exp) result (ret) bind(c)
      import
      implicit none
      integer (mpfr_exp_kind), value :: exp
      integer (c_int) :: ret
    end function mpfr_set_emin
    function mpfr_set_emax (exp) result (ret) bind(c)
      import
      implicit none
      integer (mpfr_exp_kind), value :: exp
      integer (c_int) :: ret
    end function mpfr_set_emax
    function mpfr_get_emin_min () result (ret) bind(c)
      import
      implicit none
      integer (mpfr_exp_kind) :: ret
    end function mpfr_get_emin_min
    function mpfr_get_emin_max () result (ret) bind(c)
      import
      implicit none
      integer (mpfr_exp_kind) :: ret
    end function mpfr_get_emin_max
    function mpfr_get_emax_min () result (ret) bind(c)
      import
      implicit none
      integer (mpfr_exp_kind) :: ret
    end function mpfr_get_emax_min
    function mpfr_get_emax_max () result (ret) bind(c)
      import
      implicit none
      integer (mpfr_exp_kind) :: ret
    end function mpfr_get_emax_max
    function mpfr_check_range (x, t, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      integer (c_int), value :: t
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_check_range
    function mpfr_subnormalize (x, t, rnd) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      integer (c_int), value :: t
      integer (c_int), value :: rnd
      integer (c_int) :: ret
    end function mpfr_subnormalize
    subroutine mpfr_clear_underflow () bind(c)
      import
      implicit none
    end subroutine mpfr_clear_underflow
    subroutine mpfr_clear_overflow () bind(c)
      import
      implicit none
    end subroutine mpfr_clear_overflow
    subroutine mpfr_clear_divby0 () bind(c)
      import
      implicit none
    end subroutine mpfr_clear_divby0
    subroutine mpfr_clear_nanflag () bind(c)
      import
      implicit none
    end subroutine mpfr_clear_nanflag
    subroutine mpfr_clear_inexflag () bind(c)
      import
      implicit none
    end subroutine mpfr_clear_inexflag
    subroutine mpfr_clear_erangeflag () bind(c)
      import
      implicit none
    end subroutine mpfr_clear_erangeflag
    subroutine mpfr_clear_flags () bind(c)
      import
      implicit none
    end subroutine mpfr_clear_flags
    subroutine mpfr_set_underflow () bind(c)
      import
      implicit none
    end subroutine mpfr_set_underflow
    subroutine mpfr_set_overflow () bind(c)
      import
      implicit none
    end subroutine mpfr_set_overflow
    subroutine mpfr_set_divby0 () bind(c)
      import
      implicit none
    end subroutine mpfr_set_divby0
    subroutine mpfr_set_nanflag () bind(c)
      import
      implicit none
    end subroutine mpfr_set_nanflag
    subroutine mpfr_set_inexflag () bind(c)
      import
      implicit none
    end subroutine mpfr_set_inexflag
    subroutine mpfr_set_erangeflag () bind(c)
      import
      implicit none
    end subroutine mpfr_set_erangeflag
    function mpfr_underflow_p () result (ret) bind(c)
      import
      implicit none
      integer (c_int) :: ret
    end function mpfr_underflow_p
    function mpfr_overflow_p () result (ret) bind(c)
      import
      implicit none
      integer (c_int) :: ret
    end function mpfr_overflow_p
    function mpfr_divby0_p () result (ret) bind(c)
      import
      implicit none
      integer (c_int) :: ret
    end function mpfr_divby0_p
    function mpfr_nanflag_p () result (ret) bind(c)
      import
      implicit none
      integer (c_int) :: ret
    end function mpfr_nanflag_p
    function mpfr_inexflag_p () result (ret) bind(c)
      import
      implicit none
      integer (c_int) :: ret
    end function mpfr_inexflag_p
    function mpfr_erangeflag_p () result (ret) bind(c)
      import
      implicit none
      integer (c_int) :: ret
    end function mpfr_erangeflag_p
    subroutine mpfr_flags_clear (mask) bind(c)
      import
      implicit none
      integer (c_int), value :: mask
    end subroutine mpfr_flags_clear
    subroutine mpfr_flags_set (mask) bind(c)
      import
      implicit none
      integer (c_int), value :: mask
    end subroutine mpfr_flags_set
    function mpfr_flags_test (mask) result (ret) bind(c)
      import
      implicit none
      integer (c_int), value :: mask
      integer (c_int) :: ret
    end function mpfr_flags_test
    function mpfr_flags_save () result (ret) bind(c)
      import
      implicit none
      integer (c_int) :: ret
    end function mpfr_flags_save
    subroutine mpfr_flags_restore (flags, mask) bind(c)
      import
      implicit none
      integer (c_int), value :: flags
      integer (c_int), value :: mask
    end subroutine mpfr_flags_restore
    subroutine mpfr_free_cache () bind(c)
      import
      implicit none
    end subroutine mpfr_free_cache
    subroutine mpfr_free_cache2 (way) bind(c)
      import
      implicit none
      integer (c_int), value :: way
    end subroutine mpfr_free_cache2
    subroutine mpfr_free_pool () bind(c)
      import
      implicit none
    end subroutine mpfr_free_pool
    function mpfr_mp_memory_cleanup () result (ret) bind(c)
      import
      implicit none
      integer (c_int) :: ret
    end function mpfr_mp_memory_cleanup
    subroutine mpfr_set_prec_raw (x, prec) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      integer (mpfr_prec_kind), value :: prec
    end subroutine mpfr_set_prec_raw
    subroutine mpfr_reldiff (rop, op1, op2, rnd) bind(c)
      import
      implicit none
      type (mpfr_t) :: rop
      type (mpfr_t) :: op1
      type (mpfr_t) :: op2
      integer (c_int), value :: rnd
    end subroutine mpfr_reldiff
    function mpfr_custom_get_size (prec) result (ret) bind(c)
      import
      implicit none
      integer (mpfr_prec_kind), value :: prec
      integer (c_size_t) :: ret
    end function mpfr_custom_get_size
    subroutine mpfr_custom_init (significand, prec) bind(c)
      import
      implicit none
      type (c_ptr), value :: significand
      integer (mpfr_prec_kind), value :: prec
    end subroutine mpfr_custom_init
    subroutine mpfr_custom_init_set (x, kind, exp, prec, significand) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      integer (c_int), value :: kind
      integer (mpfr_exp_kind), value :: exp
      integer (mpfr_prec_kind), value :: prec
      type (c_ptr), value :: significand
    end subroutine mpfr_custom_init_set
    function mpfr_custom_get_kind (x) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      integer (c_int) :: ret
    end function mpfr_custom_get_kind
    function mpfr_custom_get_significand (x) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      type (c_ptr) :: ret
    end function mpfr_custom_get_significand
    function mpfr_custom_get_exp (x) result (ret) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      integer (mpfr_exp_kind) :: ret
    end function mpfr_custom_get_exp
    subroutine mpfr_custom_move (x, new_position) bind(c)
      import
      implicit none
      type (mpfr_t) :: x
      type (c_ptr), value :: new_position
    end subroutine mpfr_custom_move
  end interface
end module fmpfr_c

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

module fmpfr_oper
  use fmpfr_c
  implicit none
  private
  type fmpfr
    type (mpfr_t) :: mp
    logical :: initialized = .false.
  contains
    final :: fmpfr_cleanup
  end type fmpfr

  enum, bind(c)
     enumerator :: mpfr_rndn = 0, mpfr_rndz, mpfr_rndu, mpfr_rndd, mpfr_rnda, &
          mpfr_rndf, mpfr_rndna = -1
  end enum
  integer (c_int) :: default_rnd

  interface
     function strlen (s) bind(c) result(res)
       import
       implicit none
       integer (c_size_t) :: res
       type (c_ptr), value :: s
     end function strlen
  end interface

  interface
    pure subroutine fmpfr_set (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_set

    pure subroutine fmpfr_set_si (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      integer (c_long), value:: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_set_si

    pure subroutine fmpfr_set_flt (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      real (c_float), value:: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_set_flt

    pure subroutine fmpfr_set_d (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      real (c_double), value:: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_set_d

#if USE_LONG_DOUBLE
    pure subroutine fmpfr_set_ld (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      real (c_long_double), value:: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_set_ld

#endif
#if USE_FLOAT128
    pure subroutine fmpfr_set_float128 (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      real (qp), value:: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_set_float128

#endif
    pure subroutine fmpfr_set_str (rop, s, base, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (c_ptr), value:: s
      integer (c_int), value:: base
      integer (c_int), value:: rnd
    end subroutine fmpfr_set_str

    pure subroutine fmpfr_add (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_add

    pure subroutine fmpfr_add_si (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      integer (c_long), value:: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_add_si

    pure subroutine fmpfr_add_d (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      real (c_double), value:: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_add_d

    pure subroutine fmpfr_sub (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_sub

    pure subroutine fmpfr_si_sub (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      integer (c_long), value:: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_si_sub

    pure subroutine fmpfr_sub_si (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      integer (c_long), value:: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_sub_si

    pure subroutine fmpfr_d_sub (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      real (c_double), value:: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_d_sub

    pure subroutine fmpfr_sub_d (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      real (c_double), value:: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_sub_d

    pure subroutine fmpfr_mul (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_mul

    pure subroutine fmpfr_mul_si (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      integer (c_long), value:: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_mul_si

    pure subroutine fmpfr_mul_d (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      real (c_double), value:: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_mul_d

    pure subroutine fmpfr_div (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_div

    pure subroutine fmpfr_si_div (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      integer (c_long), value:: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_si_div

    pure subroutine fmpfr_div_si (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      integer (c_long), value:: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_div_si

    pure subroutine fmpfr_d_div (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      real (c_double), value:: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_d_div

    pure subroutine fmpfr_div_d (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      real (c_double), value:: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_div_d

    pure subroutine fmpfr_sqrt (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_sqrt

    pure subroutine fmpfr_abs (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_abs

    pure subroutine fmpfr_greater_p (op1, op2) bind(c)
      import
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
    end subroutine fmpfr_greater_p

    pure subroutine fmpfr_greaterequal_p (op1, op2) bind(c)
      import
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
    end subroutine fmpfr_greaterequal_p

    pure subroutine fmpfr_less_p (op1, op2) bind(c)
      import
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
    end subroutine fmpfr_less_p

    pure subroutine fmpfr_lessequal_p (op1, op2) bind(c)
      import
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
    end subroutine fmpfr_lessequal_p

    pure subroutine fmpfr_equal_p (op1, op2) bind(c)
      import
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
    end subroutine fmpfr_equal_p

    pure subroutine fmpfr_lessgreater_p (op1, op2) bind(c)
      import
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
    end subroutine fmpfr_lessgreater_p

    pure subroutine fmpfr_log (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_log

    pure subroutine fmpfr_log10 (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_log10

    pure subroutine fmpfr_exp (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_exp

    pure subroutine fmpfr_pow (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_pow

    pure subroutine fmpfr_pow_si (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      integer (c_long), value:: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_pow_si

    pure subroutine fmpfr_cos (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_cos

    pure subroutine fmpfr_sin (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_sin

    pure subroutine fmpfr_tan (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_tan

    pure subroutine fmpfr_acos (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_acos

    pure subroutine fmpfr_asin (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_asin

    pure subroutine fmpfr_atan (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_atan

    pure subroutine fmpfr_atan2 (rop, y, x, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: y
      type (mpfr_t), intent(in) :: x
      integer (c_int), value:: rnd
    end subroutine fmpfr_atan2

    pure subroutine fmpfr_gamma (rop, op, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op
      integer (c_int), value:: rnd
    end subroutine fmpfr_gamma

    pure subroutine fmpfr_min (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_min

    pure subroutine fmpfr_max (rop, op1, op2, rnd) bind(c)
      import
      type (mpfr_t), intent(out) :: rop
      type (mpfr_t), intent(in) :: op1
      type (mpfr_t), intent(in) :: op2
      integer (c_int), value:: rnd
    end subroutine fmpfr_max

  end interface
  public :: abs
  interface abs
    module procedure fun_abs
  end interface abs

  public :: acos
  interface acos
    module procedure fun_acos
  end interface acos

  public :: add
  interface add
    module procedure fun_add
    module procedure fun_add_si
#if SIZEOF_INT < SIZEOF_LONG
    module procedure fun_add_si_int
#endif
    module procedure fun_add_si_short
    module procedure fun_add_d
    module procedure fun_add_d_float
  end interface add

  public :: asin
  interface asin
    module procedure fun_asin
  end interface asin

  public :: atan
  interface atan
    module procedure fun_atan
  end interface atan

  public :: atan2
  interface atan2
    module procedure fun_atan2
  end interface atan2

  public :: cos
  interface cos
    module procedure fun_cos
  end interface cos

  public :: div
  interface div
    module procedure fun_div
    module procedure fun_si_div
#if SIZEOF_INT < SIZEOF_LONG
    module procedure fun_si_div_int
#endif
    module procedure fun_si_div_short
    module procedure fun_div_si
#if SIZEOF_INT < SIZEOF_LONG
    module procedure fun_div_si_int
#endif
    module procedure fun_div_si_short
    module procedure fun_d_div
    module procedure fun_d_div_float
    module procedure fun_div_d
    module procedure fun_div_d_float
  end interface div

  public :: exp
  interface exp
    module procedure fun_exp
  end interface exp

  public :: fmpfr
  interface fmpfr
    module procedure fun_set_si
    module procedure fun_set_flt
    module procedure fun_set_d
#if USE_LONG_DOUBLE
    module procedure fun_set_ld
#endif
#if USE_FLOAT128
    module procedure fun_set_float128
#endif
  module procedure fun_set_str
  end interface fmpfr

  public :: gamma
  interface gamma
    module procedure fun_gamma
  end interface gamma

  public :: init
  interface init
    module procedure init_long
#if SIZEOF_INT < SIZEOF_LONG
    module procedure init_int
#endif
    module procedure init_short
    module procedure init_default
  end interface init

  public :: log
  interface log
    module procedure fun_log
  end interface log

  public :: log10
  interface log10
    module procedure fun_log10
  end interface log10

  public :: max
  interface max
    module procedure fun_max
  end interface max

  public :: min
  interface min
    module procedure fun_min
  end interface min

  public :: mul
  interface mul
    module procedure fun_mul
    module procedure fun_mul_si
#if SIZEOF_INT < SIZEOF_LONG
    module procedure fun_mul_si_int
#endif
    module procedure fun_mul_si_short
    module procedure fun_mul_d
    module procedure fun_mul_d_float
  end interface mul

  public :: pow
  interface pow
    module procedure fun_pow
    module procedure fun_pow_si
#if SIZEOF_INT < SIZEOF_LONG
    module procedure fun_pow_si_int
#endif
    module procedure fun_pow_si_short
  end interface pow

  public :: set_default_prec
  interface set_default_prec
    module procedure set_default_prec_long
    module procedure set_default_prec_int
    module procedure set_default_prec_short
  end interface set_default_prec

  public :: sin
  interface sin
    module procedure fun_sin
  end interface sin

  public :: sqrt
  interface sqrt
    module procedure fun_sqrt
  end interface sqrt

  public :: sub
  interface sub
    module procedure fun_sub
    module procedure fun_si_sub
#if SIZEOF_INT < SIZEOF_LONG
    module procedure fun_si_sub_int
#endif
    module procedure fun_si_sub_short
    module procedure fun_sub_si
#if SIZEOF_INT < SIZEOF_LONG
    module procedure fun_sub_si_int
#endif
    module procedure fun_sub_si_short
    module procedure fun_d_sub
    module procedure fun_d_sub_float
    module procedure fun_sub_d
    module procedure fun_sub_d_float
  end interface sub

  public :: tan
  interface tan
    module procedure fun_tan
  end interface tan

  public :: operator (+)
  interface operator (+)
    module procedure op_add
    module procedure op_add_si
#if SIZEOF_INT < SIZEOF_LONG
    module procedure op_add_si_int
#endif
    module procedure op_add_si_short
    module procedure op_si_add
#if SIZEOF_INT < SIZEOF_LONG
    module procedure op_si_add_int
#endif
    module procedure op_si_add_short
    module procedure op_add_d
    module procedure op_add_d_float
    module procedure op_d_add
    module procedure op_d_add_float
  end interface operator (+)

  public :: operator (/)
  interface operator (/)
    module procedure op_div
    module procedure op_si_div
#if SIZEOF_INT < SIZEOF_LONG
    module procedure op_si_div_int
#endif
    module procedure op_si_div_short
    module procedure op_div_si
#if SIZEOF_INT < SIZEOF_LONG
    module procedure op_div_si_int
#endif
    module procedure op_div_si_short
    module procedure op_d_div
    module procedure op_d_div_float
    module procedure op_div_d
    module procedure op_div_d_float
  end interface operator (/)

  public :: operator (*)
  interface operator (*)
    module procedure op_mul
    module procedure op_mul_si
#if SIZEOF_INT < SIZEOF_LONG
    module procedure op_mul_si_int
#endif
    module procedure op_mul_si_short
    module procedure op_si_mul
#if SIZEOF_INT < SIZEOF_LONG
    module procedure op_si_mul_int
#endif
    module procedure op_si_mul_short
    module procedure op_mul_d
    module procedure op_mul_d_float
    module procedure op_d_mul
    module procedure op_d_mul_float
  end interface operator (*)

  public :: operator (**)
  interface operator (**)
    module procedure op_pow
    module procedure op_pow_si
#if SIZEOF_INT < SIZEOF_LONG
    module procedure op_pow_si_int
#endif
    module procedure op_pow_si_short
  end interface operator (**)

  public :: operator (-)
  interface operator (-)
    module procedure op_sub
    module procedure op_si_sub
#if SIZEOF_INT < SIZEOF_LONG
    module procedure op_si_sub_int
#endif
    module procedure op_si_sub_short
    module procedure op_sub_si
#if SIZEOF_INT < SIZEOF_LONG
    module procedure op_sub_si_int
#endif
    module procedure op_sub_si_short
    module procedure op_d_sub
    module procedure op_d_sub_float
    module procedure op_sub_d
    module procedure op_sub_d_float
  end interface operator (-)

  public :: operator (/=)
  interface operator (/=)
    module procedure lessgreater_p
  end interface

  public :: operator (<)
  interface operator (<)
    module procedure less_p
  end interface

  public :: operator (<=)
  interface operator (<=)
    module procedure lessequal_p
  end interface

  public :: operator (==)
  interface operator (==)
    module procedure equal_p
  end interface

  public :: operator (>)
  interface operator (>)
    module procedure greater_p
  end interface

  public :: operator (>=)
  interface operator (>=)
    module procedure greaterequal_p
  end interface

  public :: assignment(=)
  interface assignment(=)
    module procedure ass_set
    module procedure ass_set_si
    module procedure ass_set_si_int
    module procedure ass_set_si_short
    module procedure ass_set_flt
    module procedure ass_set_d
#if USE_LONG_DOUBLE
    module procedure ass_set_ld
#endif
#if USE_FLOAT128
    module procedure ass_set_float128
#endif
    module procedure ass_get_flt
    module procedure ass_get_d
#if USE_LONG_DOUBLE
    module procedure ass_get_ld
#endif
#if USE_FLOAT128
    module procedure ass_get_float128
#endif
    module procedure ass_get_si
end interface assignment(=)

  public :: write(formatted)
  interface write(formatted)
    module procedure write_formatted
  end interface write(formatted)

  public :: mpfr_rndn
  public :: mpfr_rndz
  public :: mpfr_rndu
  public :: mpfr_rndd
  public :: mpfr_rnda
  public :: mpfr_rndf
  public :: mpfr_rndna
  public :: get_flt
  public :: get_d
#if USE_LONG_DOUBLE
  public :: get_ld
#endif
#if USE_FLOAT128
  public :: get_float128
#endif
  public :: get_si
  public :: fmpfr_cleanup
  public :: set_default_rounding_mode
contains
    elemental function fun_set (op, rnd) result (rop)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rc
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        call mpfr_init2 (rop%mp, default_prec)
        rop%initialized = .true.
      end if
      call fmpfr_set (rop%mp, op%mp, rnd_val)
    end function fun_set

    elemental function fun_set_si (op, rnd) result (rop)
      integer (c_long), intent(in) :: op
      integer (c_int), intent(in), optional :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rc
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        call mpfr_init2 (rop%mp, default_prec)
        rop%initialized = .true.
      end if
      call fmpfr_set_si (rop%mp, op, rnd_val)
    end function fun_set_si

    elemental subroutine ass_set_si (rop, op)
      type (fmpfr), intent(inout) :: rop
      integer (c_long), intent(in) :: op

      if (.not. rop%initialized) then
        call mpfr_init2 (rop%mp, default_prec)
        rop%initialized = .true.
      end if
      call fmpfr_set_si (rop%mp, op, default_rnd)
    end subroutine ass_set_si

#if SIZEOF_INT < SIZEOF_LONG
  elemental subroutine ass_set_si_int (rop, op)
    type (fmpfr), intent(inout) :: rop
    integer (c_int), intent(in) :: op
    integer (c_long) :: tmp_op;

    tmp_op = op;
    if (.not. rop%initialized) then
      call mpfr_init2 (rop%mp, default_prec)
      rop%initialized = .true.
    end if
    call fmpfr_set_si (rop%mp, tmp_op, default_rnd)
  end subroutine ass_set_si_int
#endif
  elemental subroutine ass_set_si_short (rop, op)
    type (fmpfr), intent(inout) :: rop
    integer (c_short), intent(in) :: op
    integer (c_long) :: tmp_op;

    tmp_op = op;
    if (.not. rop%initialized) then
      call mpfr_init2 (rop%mp, default_prec)
      rop%initialized = .true.
    end if
    call fmpfr_set_si (rop%mp, tmp_op, default_rnd)
  end subroutine ass_set_si_short
    elemental function fun_set_flt (op, rnd) result (rop)
      real (c_float), intent(in) :: op
      integer (c_int), intent(in), optional :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rc
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        call mpfr_init2 (rop%mp, default_prec)
        rop%initialized = .true.
      end if
      call fmpfr_set_flt (rop%mp, op, rnd_val)
    end function fun_set_flt

    elemental subroutine ass_set_flt (rop, op)
      type (fmpfr), intent(inout) :: rop
      real (c_float), intent(in) :: op

      if (.not. rop%initialized) then
        call mpfr_init2 (rop%mp, default_prec)
        rop%initialized = .true.
      end if
      call fmpfr_set_flt (rop%mp, op, default_rnd)
    end subroutine ass_set_flt

    elemental function fun_set_d (op, rnd) result (rop)
      real (c_double), intent(in) :: op
      integer (c_int), intent(in), optional :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rc
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        call mpfr_init2 (rop%mp, default_prec)
        rop%initialized = .true.
      end if
      call fmpfr_set_d (rop%mp, op, rnd_val)
    end function fun_set_d

    elemental subroutine ass_set_d (rop, op)
      type (fmpfr), intent(inout) :: rop
      real (c_double), intent(in) :: op

      if (.not. rop%initialized) then
        call mpfr_init2 (rop%mp, default_prec)
        rop%initialized = .true.
      end if
      call fmpfr_set_d (rop%mp, op, default_rnd)
    end subroutine ass_set_d

#if USE_LONG_DOUBLE
    elemental function fun_set_ld (op, rnd) result (rop)
      real (c_long_double), intent(in) :: op
      integer (c_int), intent(in), optional :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rc
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        call mpfr_init2 (rop%mp, default_prec)
        rop%initialized = .true.
      end if
      call fmpfr_set_ld (rop%mp, op, rnd_val)
    end function fun_set_ld

    elemental subroutine ass_set_ld (rop, op)
      type (fmpfr), intent(inout) :: rop
      real (c_long_double), intent(in) :: op

      if (.not. rop%initialized) then
        call mpfr_init2 (rop%mp, default_prec)
        rop%initialized = .true.
      end if
      call fmpfr_set_ld (rop%mp, op, default_rnd)
    end subroutine ass_set_ld

#endif
#if USE_FLOAT128
    elemental function fun_set_float128 (op, rnd) result (rop)
      real (qp), intent(in) :: op
      integer (c_int), intent(in), optional :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rc
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        call mpfr_init2 (rop%mp, default_prec)
        rop%initialized = .true.
      end if
      call fmpfr_set_float128 (rop%mp, op, rnd_val)
    end function fun_set_float128

    elemental subroutine ass_set_float128 (rop, op)
      type (fmpfr), intent(inout) :: rop
      real (qp), intent(in) :: op

      if (.not. rop%initialized) then
        call mpfr_init2 (rop%mp, default_prec)
        rop%initialized = .true.
      end if
      call fmpfr_set_float128 (rop%mp, op, default_rnd)
    end subroutine ass_set_float128

#endif
    elemental function get_flt (op, rnd) result (rval)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional :: rnd
      real (c_float) :: rval
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      rval = mpfr_get_flt (op%mp, rnd_val)
    end function get_flt

  elemental subroutine ass_get_flt (rval, op)
    real (c_float), intent(out) :: rval
    type (fmpfr), intent(in) :: op

    rval = get_flt (op)
  end subroutine ass_get_flt

    elemental function get_d (op, rnd) result (rval)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional :: rnd
      real (c_double) :: rval
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      rval = mpfr_get_d (op%mp, rnd_val)
    end function get_d

  elemental subroutine ass_get_d (rval, op)
    real (c_double), intent(out) :: rval
    type (fmpfr), intent(in) :: op

    rval = get_d (op)
  end subroutine ass_get_d

#if USE_LONG_DOUBLE
    elemental function get_ld (op, rnd) result (rval)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional :: rnd
      real (c_long_double) :: rval
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      rval = mpfr_get_ld (op%mp, rnd_val)
    end function get_ld

  elemental subroutine ass_get_ld (rval, op)
    real (c_long_double), intent(out) :: rval
    type (fmpfr), intent(in) :: op

    rval = get_ld (op)
  end subroutine ass_get_ld

#endif
#if USE_FLOAT128
    elemental function get_float128 (op, rnd) result (rval)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional :: rnd
      real (qp) :: rval
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      rval = mpfr_get_float128 (op%mp, rnd_val)
    end function get_float128

  elemental subroutine ass_get_float128 (rval, op)
    real (qp), intent(out) :: rval
    type (fmpfr), intent(in) :: op

    rval = get_float128 (op)
  end subroutine ass_get_float128

#endif
    elemental function get_si (op, rnd) result (rval)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional :: rnd
      integer (c_long) :: rval
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      rval = mpfr_get_si (op%mp, rnd_val)
    end function get_si

  elemental subroutine ass_get_si (rval, op)
    integer (c_long), intent(out) :: rval
    type (fmpfr), intent(in) :: op

    rval = get_si (op)
  end subroutine ass_get_si

    elemental function fun_add (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1, prec_op2, prec
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        prec_op2 = mpfr_get_prec (op2%mp)
        prec = max (prec_op1, prec_op2)
        call mpfr_init2 (rop%mp, prec)
      end if
      call fmpfr_add (rop%mp, op1%mp, op2%mp, rnd_val)
    end function fun_add

    elemental function op_add (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_add (op1, op2)
    end function op_add

    elemental function fun_add_si (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_long), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        call mpfr_init2 (rop%mp, prec_op1)
      end if
      call fmpfr_add_si (rop%mp, op1%mp, op2, rnd_val)
    end function fun_add_si

    elemental function op_add_si (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_long), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_add_si (op1, op2)
    end function op_add_si

#if SIZEOF_INT < SIZEOF_LONG
  elemental function fun_add_si_int (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_int), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (c_long) :: op2_tmp
      op2_tmp = op2
      rop = fun_add_si (op1, op2_tmp, rnd)
  end function fun_add_si_int

  elemental function op_add_si_int (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_int), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_add_si_int (op1, op2)
  end function op_add_si_int

#endif

  elemental function fun_add_si_short (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_short), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (c_long) :: op2_tmp
      op2_tmp = op2
      rop = fun_add_si (op1, op2_tmp, rnd)
  end function fun_add_si_short

  elemental function op_add_si_short (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_short), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_add_si_short (op1, op2)
  end function op_add_si_short

    elemental function op_si_add (op2, op1) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_long), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_add_si (op1, op2)
    end function op_si_add

#if SIZEOF_INT < SIZEOF_LONG
    elemental function op_si_add_int (op2, op1) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_int), intent(in) :: op2
      type (fmpfr) :: rop
      integer (c_long) :: op2_tmp
      op2_tmp = op2
      rop =  fun_add_si (op1, op2_tmp)
    end function op_si_add_int

#endif

    elemental function op_si_add_short (op2, op1) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_short), intent(in) :: op2
      type (fmpfr) :: rop
      integer (c_long) :: op2_tmp
      op2_tmp = op2
      rop =  fun_add_si (op1, op2_tmp)
    end function op_si_add_short

    elemental function fun_add_d (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_double), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        call mpfr_init2 (rop%mp, prec_op1)
      end if
      call fmpfr_add_d (rop%mp, op1%mp, op2, rnd_val)
    end function fun_add_d

    elemental function op_add_d (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_double), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_add_d (op1, op2)
    end function op_add_d

  elemental function fun_add_d_float (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_float), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      real (c_double) :: op2_tmp
      op2_tmp = op2
      rop = fun_add_d (op1, op2_tmp, rnd)
  end function fun_add_d_float

  elemental function op_add_d_float (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_float), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_add_d_float (op1, op2)
  end function op_add_d_float

    elemental function op_d_add (op2, op1) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_double), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_add_d (op1, op2)
    end function op_d_add

    elemental function op_d_add_float (op2, op1) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_float), intent(in) :: op2
      type (fmpfr) :: rop
      real (c_double) :: op2_tmp
      op2_tmp = op2
      rop =  fun_add_d (op1, op2_tmp)
    end function op_d_add_float

    elemental function fun_sub (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1, prec_op2, prec
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        prec_op2 = mpfr_get_prec (op2%mp)
        prec = max (prec_op1, prec_op2)
        call mpfr_init2 (rop%mp, prec)
      end if
      call fmpfr_sub (rop%mp, op1%mp, op2%mp, rnd_val)
    end function fun_sub

    elemental function op_sub (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_sub (op1, op2)
    end function op_sub

    elemental function fun_si_sub (op1, op2, rnd) result (rop)
      integer (c_long), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op2
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op2 = mpfr_get_prec (op2%mp)
        call mpfr_init2 (rop%mp, prec_op2)
      end if
      call fmpfr_si_sub (rop%mp, op1, op2%mp, rnd_val)
    end function fun_si_sub

    elemental function op_si_sub (op1, op2) result (rop)
      integer (c_long), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_si_sub (op1, op2)
    end function op_si_sub

#if SIZEOF_INT < SIZEOF_LONG
  elemental function fun_si_sub_int (op1, op2, rnd) result (rop)
      integer (c_int), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (c_long) :: op1_tmp
      op1_tmp = op1
      rop = fun_si_sub (op1_tmp, op2, rnd)
  end function fun_si_sub_int

  elemental function op_si_sub_int (op1, op2) result (rop)
      integer (c_int), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_si_sub_int (op1, op2)
  end function op_si_sub_int

#endif

  elemental function fun_si_sub_short (op1, op2, rnd) result (rop)
      integer (c_short), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (c_long) :: op1_tmp
      op1_tmp = op1
      rop = fun_si_sub (op1_tmp, op2, rnd)
  end function fun_si_sub_short

  elemental function op_si_sub_short (op1, op2) result (rop)
      integer (c_short), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_si_sub_short (op1, op2)
  end function op_si_sub_short

    elemental function fun_sub_si (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_long), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        call mpfr_init2 (rop%mp, prec_op1)
      end if
      call fmpfr_sub_si (rop%mp, op1%mp, op2, rnd_val)
    end function fun_sub_si

    elemental function op_sub_si (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_long), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_sub_si (op1, op2)
    end function op_sub_si

#if SIZEOF_INT < SIZEOF_LONG
  elemental function fun_sub_si_int (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_int), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (c_long) :: op2_tmp
      op2_tmp = op2
      rop = fun_sub_si (op1, op2_tmp, rnd)
  end function fun_sub_si_int

  elemental function op_sub_si_int (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_int), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_sub_si_int (op1, op2)
  end function op_sub_si_int

#endif

  elemental function fun_sub_si_short (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_short), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (c_long) :: op2_tmp
      op2_tmp = op2
      rop = fun_sub_si (op1, op2_tmp, rnd)
  end function fun_sub_si_short

  elemental function op_sub_si_short (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_short), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_sub_si_short (op1, op2)
  end function op_sub_si_short

    elemental function fun_d_sub (op1, op2, rnd) result (rop)
      real (c_double), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op2
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op2 = mpfr_get_prec (op2%mp)
        call mpfr_init2 (rop%mp, prec_op2)
      end if
      call fmpfr_d_sub (rop%mp, op1, op2%mp, rnd_val)
    end function fun_d_sub

    elemental function op_d_sub (op1, op2) result (rop)
      real (c_double), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_d_sub (op1, op2)
    end function op_d_sub

  elemental function fun_d_sub_float (op1, op2, rnd) result (rop)
      real (c_float), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      real (c_double) :: op1_tmp
      op1_tmp = op1
      rop = fun_d_sub (op1_tmp, op2, rnd)
  end function fun_d_sub_float

  elemental function op_d_sub_float (op1, op2) result (rop)
      real (c_float), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_d_sub_float (op1, op2)
  end function op_d_sub_float

    elemental function fun_sub_d (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_double), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        call mpfr_init2 (rop%mp, prec_op1)
      end if
      call fmpfr_sub_d (rop%mp, op1%mp, op2, rnd_val)
    end function fun_sub_d

    elemental function op_sub_d (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_double), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_sub_d (op1, op2)
    end function op_sub_d

  elemental function fun_sub_d_float (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_float), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      real (c_double) :: op2_tmp
      op2_tmp = op2
      rop = fun_sub_d (op1, op2_tmp, rnd)
  end function fun_sub_d_float

  elemental function op_sub_d_float (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_float), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_sub_d_float (op1, op2)
  end function op_sub_d_float

    elemental function fun_mul (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1, prec_op2, prec
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        prec_op2 = mpfr_get_prec (op2%mp)
        prec = max (prec_op1, prec_op2)
        call mpfr_init2 (rop%mp, prec)
      end if
      call fmpfr_mul (rop%mp, op1%mp, op2%mp, rnd_val)
    end function fun_mul

    elemental function op_mul (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_mul (op1, op2)
    end function op_mul

    elemental function fun_mul_si (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_long), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        call mpfr_init2 (rop%mp, prec_op1)
      end if
      call fmpfr_mul_si (rop%mp, op1%mp, op2, rnd_val)
    end function fun_mul_si

    elemental function op_mul_si (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_long), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_mul_si (op1, op2)
    end function op_mul_si

#if SIZEOF_INT < SIZEOF_LONG
  elemental function fun_mul_si_int (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_int), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (c_long) :: op2_tmp
      op2_tmp = op2
      rop = fun_mul_si (op1, op2_tmp, rnd)
  end function fun_mul_si_int

  elemental function op_mul_si_int (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_int), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_mul_si_int (op1, op2)
  end function op_mul_si_int

#endif

  elemental function fun_mul_si_short (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_short), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (c_long) :: op2_tmp
      op2_tmp = op2
      rop = fun_mul_si (op1, op2_tmp, rnd)
  end function fun_mul_si_short

  elemental function op_mul_si_short (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_short), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_mul_si_short (op1, op2)
  end function op_mul_si_short

    elemental function op_si_mul (op2, op1) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_long), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_mul_si (op1, op2)
    end function op_si_mul

#if SIZEOF_INT < SIZEOF_LONG
    elemental function op_si_mul_int (op2, op1) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_int), intent(in) :: op2
      type (fmpfr) :: rop
      integer (c_long) :: op2_tmp
      op2_tmp = op2
      rop =  fun_mul_si (op1, op2_tmp)
    end function op_si_mul_int

#endif

    elemental function op_si_mul_short (op2, op1) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_short), intent(in) :: op2
      type (fmpfr) :: rop
      integer (c_long) :: op2_tmp
      op2_tmp = op2
      rop =  fun_mul_si (op1, op2_tmp)
    end function op_si_mul_short

    elemental function fun_mul_d (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_double), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        call mpfr_init2 (rop%mp, prec_op1)
      end if
      call fmpfr_mul_d (rop%mp, op1%mp, op2, rnd_val)
    end function fun_mul_d

    elemental function op_mul_d (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_double), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_mul_d (op1, op2)
    end function op_mul_d

  elemental function fun_mul_d_float (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_float), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      real (c_double) :: op2_tmp
      op2_tmp = op2
      rop = fun_mul_d (op1, op2_tmp, rnd)
  end function fun_mul_d_float

  elemental function op_mul_d_float (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_float), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_mul_d_float (op1, op2)
  end function op_mul_d_float

    elemental function op_d_mul (op2, op1) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_double), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_mul_d (op1, op2)
    end function op_d_mul

    elemental function op_d_mul_float (op2, op1) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_float), intent(in) :: op2
      type (fmpfr) :: rop
      real (c_double) :: op2_tmp
      op2_tmp = op2
      rop =  fun_mul_d (op1, op2_tmp)
    end function op_d_mul_float

    elemental function fun_div (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1, prec_op2, prec
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        prec_op2 = mpfr_get_prec (op2%mp)
        prec = max (prec_op1, prec_op2)
        call mpfr_init2 (rop%mp, prec)
      end if
      call fmpfr_div (rop%mp, op1%mp, op2%mp, rnd_val)
    end function fun_div

    elemental function op_div (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_div (op1, op2)
    end function op_div

    elemental function fun_si_div (op1, op2, rnd) result (rop)
      integer (c_long), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op2
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op2 = mpfr_get_prec (op2%mp)
        call mpfr_init2 (rop%mp, prec_op2)
      end if
      call fmpfr_si_div (rop%mp, op1, op2%mp, rnd_val)
    end function fun_si_div

    elemental function op_si_div (op1, op2) result (rop)
      integer (c_long), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_si_div (op1, op2)
    end function op_si_div

#if SIZEOF_INT < SIZEOF_LONG
  elemental function fun_si_div_int (op1, op2, rnd) result (rop)
      integer (c_int), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (c_long) :: op1_tmp
      op1_tmp = op1
      rop = fun_si_div (op1_tmp, op2, rnd)
  end function fun_si_div_int

  elemental function op_si_div_int (op1, op2) result (rop)
      integer (c_int), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_si_div_int (op1, op2)
  end function op_si_div_int

#endif

  elemental function fun_si_div_short (op1, op2, rnd) result (rop)
      integer (c_short), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (c_long) :: op1_tmp
      op1_tmp = op1
      rop = fun_si_div (op1_tmp, op2, rnd)
  end function fun_si_div_short

  elemental function op_si_div_short (op1, op2) result (rop)
      integer (c_short), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_si_div_short (op1, op2)
  end function op_si_div_short

    elemental function fun_div_si (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_long), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        call mpfr_init2 (rop%mp, prec_op1)
      end if
      call fmpfr_div_si (rop%mp, op1%mp, op2, rnd_val)
    end function fun_div_si

    elemental function op_div_si (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_long), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_div_si (op1, op2)
    end function op_div_si

#if SIZEOF_INT < SIZEOF_LONG
  elemental function fun_div_si_int (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_int), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (c_long) :: op2_tmp
      op2_tmp = op2
      rop = fun_div_si (op1, op2_tmp, rnd)
  end function fun_div_si_int

  elemental function op_div_si_int (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_int), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_div_si_int (op1, op2)
  end function op_div_si_int

#endif

  elemental function fun_div_si_short (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_short), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (c_long) :: op2_tmp
      op2_tmp = op2
      rop = fun_div_si (op1, op2_tmp, rnd)
  end function fun_div_si_short

  elemental function op_div_si_short (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_short), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_div_si_short (op1, op2)
  end function op_div_si_short

    elemental function fun_d_div (op1, op2, rnd) result (rop)
      real (c_double), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op2
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op2 = mpfr_get_prec (op2%mp)
        call mpfr_init2 (rop%mp, prec_op2)
      end if
      call fmpfr_d_div (rop%mp, op1, op2%mp, rnd_val)
    end function fun_d_div

    elemental function op_d_div (op1, op2) result (rop)
      real (c_double), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_d_div (op1, op2)
    end function op_d_div

  elemental function fun_d_div_float (op1, op2, rnd) result (rop)
      real (c_float), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      real (c_double) :: op1_tmp
      op1_tmp = op1
      rop = fun_d_div (op1_tmp, op2, rnd)
  end function fun_d_div_float

  elemental function op_d_div_float (op1, op2) result (rop)
      real (c_float), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_d_div_float (op1, op2)
  end function op_d_div_float

    elemental function fun_div_d (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_double), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        call mpfr_init2 (rop%mp, prec_op1)
      end if
      call fmpfr_div_d (rop%mp, op1%mp, op2, rnd_val)
    end function fun_div_d

    elemental function op_div_d (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_double), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_div_d (op1, op2)
    end function op_div_d

  elemental function fun_div_d_float (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_float), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      real (c_double) :: op2_tmp
      op2_tmp = op2
      rop = fun_div_d (op1, op2_tmp, rnd)
  end function fun_div_d_float

  elemental function op_div_d_float (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      real (c_float), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_div_d_float (op1, op2)
  end function op_div_d_float

  elemental function fun_sqrt (op, rnd) result (rop)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op = mpfr_get_prec (op%mp)
        call mpfr_init2 (rop%mp, prec_op)
      end if
      call fmpfr_sqrt (rop%mp, op%mp, rnd_val)
  end function fun_sqrt

  elemental function fun_abs (op, rnd) result (rop)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op = mpfr_get_prec (op%mp)
        call mpfr_init2 (rop%mp, prec_op)
      end if
      call fmpfr_abs (rop%mp, op%mp, rnd_val)
  end function fun_abs

    elemental function greater_p (op1, op2) result(ret)
      type (fmpfr), intent(in) :: op1, op2
      logical :: ret
      integer :: rc

      call fmpfr_greater_p (op1%mp, op2%mp)
      if (rc /= 0) then
        ret = .true.
      else
        ret = .false.
      end if
    end function greater_p

    elemental function greaterequal_p (op1, op2) result(ret)
      type (fmpfr), intent(in) :: op1, op2
      logical :: ret
      integer :: rc

      call fmpfr_greaterequal_p (op1%mp, op2%mp)
      if (rc /= 0) then
        ret = .true.
      else
        ret = .false.
      end if
    end function greaterequal_p

    elemental function less_p (op1, op2) result(ret)
      type (fmpfr), intent(in) :: op1, op2
      logical :: ret
      integer :: rc

      call fmpfr_less_p (op1%mp, op2%mp)
      if (rc /= 0) then
        ret = .true.
      else
        ret = .false.
      end if
    end function less_p

    elemental function lessequal_p (op1, op2) result(ret)
      type (fmpfr), intent(in) :: op1, op2
      logical :: ret
      integer :: rc

      call fmpfr_lessequal_p (op1%mp, op2%mp)
      if (rc /= 0) then
        ret = .true.
      else
        ret = .false.
      end if
    end function lessequal_p

    elemental function equal_p (op1, op2) result(ret)
      type (fmpfr), intent(in) :: op1, op2
      logical :: ret
      integer :: rc

      call fmpfr_equal_p (op1%mp, op2%mp)
      if (rc /= 0) then
        ret = .true.
      else
        ret = .false.
      end if
    end function equal_p

    elemental function lessgreater_p (op1, op2) result(ret)
      type (fmpfr), intent(in) :: op1, op2
      logical :: ret
      integer :: rc

      call fmpfr_lessgreater_p (op1%mp, op2%mp)
      if (rc /= 0) then
        ret = .true.
      else
        ret = .false.
      end if
    end function lessgreater_p

  elemental function fun_log (op, rnd) result (rop)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op = mpfr_get_prec (op%mp)
        call mpfr_init2 (rop%mp, prec_op)
      end if
      call fmpfr_log (rop%mp, op%mp, rnd_val)
  end function fun_log

  elemental function fun_log10 (op, rnd) result (rop)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op = mpfr_get_prec (op%mp)
        call mpfr_init2 (rop%mp, prec_op)
      end if
      call fmpfr_log10 (rop%mp, op%mp, rnd_val)
  end function fun_log10

  elemental function fun_exp (op, rnd) result (rop)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op = mpfr_get_prec (op%mp)
        call mpfr_init2 (rop%mp, prec_op)
      end if
      call fmpfr_exp (rop%mp, op%mp, rnd_val)
  end function fun_exp

    elemental function fun_pow (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1, prec_op2, prec
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        prec_op2 = mpfr_get_prec (op2%mp)
        prec = max (prec_op1, prec_op2)
        call mpfr_init2 (rop%mp, prec)
      end if
      call fmpfr_pow (rop%mp, op1%mp, op2%mp, rnd_val)
    end function fun_pow

    elemental function op_pow (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_pow (op1, op2)
    end function op_pow

    elemental function fun_pow_si (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_long), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        call mpfr_init2 (rop%mp, prec_op1)
      end if
      call fmpfr_pow_si (rop%mp, op1%mp, op2, rnd_val)
    end function fun_pow_si

    elemental function op_pow_si (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_long), intent(in) :: op2
      type (fmpfr) :: rop

      rop = fun_pow_si (op1, op2)
    end function op_pow_si

#if SIZEOF_INT < SIZEOF_LONG
  elemental function fun_pow_si_int (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_int), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (c_long) :: op2_tmp
      op2_tmp = op2
      rop = fun_pow_si (op1, op2_tmp, rnd)
  end function fun_pow_si_int

  elemental function op_pow_si_int (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_int), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_pow_si_int (op1, op2)
  end function op_pow_si_int

#endif

  elemental function fun_pow_si_short (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_short), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (c_long) :: op2_tmp
      op2_tmp = op2
      rop = fun_pow_si (op1, op2_tmp, rnd)
  end function fun_pow_si_short

  elemental function op_pow_si_short (op1, op2) result (rop)
      type (fmpfr), intent(in) :: op1
      integer (c_short), intent(in) :: op2
      type (fmpfr) :: rop
      rop = fun_pow_si_short (op1, op2)
  end function op_pow_si_short

  elemental function fun_cos (op, rnd) result (rop)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op = mpfr_get_prec (op%mp)
        call mpfr_init2 (rop%mp, prec_op)
      end if
      call fmpfr_cos (rop%mp, op%mp, rnd_val)
  end function fun_cos

  elemental function fun_sin (op, rnd) result (rop)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op = mpfr_get_prec (op%mp)
        call mpfr_init2 (rop%mp, prec_op)
      end if
      call fmpfr_sin (rop%mp, op%mp, rnd_val)
  end function fun_sin

  elemental function fun_tan (op, rnd) result (rop)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op = mpfr_get_prec (op%mp)
        call mpfr_init2 (rop%mp, prec_op)
      end if
      call fmpfr_tan (rop%mp, op%mp, rnd_val)
  end function fun_tan

  elemental function fun_acos (op, rnd) result (rop)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op = mpfr_get_prec (op%mp)
        call mpfr_init2 (rop%mp, prec_op)
      end if
      call fmpfr_acos (rop%mp, op%mp, rnd_val)
  end function fun_acos

  elemental function fun_asin (op, rnd) result (rop)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op = mpfr_get_prec (op%mp)
        call mpfr_init2 (rop%mp, prec_op)
      end if
      call fmpfr_asin (rop%mp, op%mp, rnd_val)
  end function fun_asin

  elemental function fun_atan (op, rnd) result (rop)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op = mpfr_get_prec (op%mp)
        call mpfr_init2 (rop%mp, prec_op)
      end if
      call fmpfr_atan (rop%mp, op%mp, rnd_val)
  end function fun_atan

  elemental function fun_atan2 (y, x, rnd) result (rop)
      type (fmpfr), intent(in) :: y
      type (fmpfr), intent(in) :: x
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_y, prec_x, prec
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_y = mpfr_get_prec (y%mp)
        prec_x = mpfr_get_prec (x%mp)
        prec = max (prec_y, prec_x)
        call mpfr_init2 (rop%mp, prec)
      end if
      call fmpfr_atan2 (rop%mp, y%mp, x%mp, rnd_val)
  end function fun_atan2

  elemental function fun_gamma (op, rnd) result (rop)
      type (fmpfr), intent(in) :: op
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op = mpfr_get_prec (op%mp)
        call mpfr_init2 (rop%mp, prec_op)
      end if
      call fmpfr_gamma (rop%mp, op%mp, rnd_val)
  end function fun_gamma

  elemental function fun_min (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1, prec_op2, prec
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        prec_op2 = mpfr_get_prec (op2%mp)
        prec = max (prec_op1, prec_op2)
        call mpfr_init2 (rop%mp, prec)
      end if
      call fmpfr_min (rop%mp, op1%mp, op2%mp, rnd_val)
  end function fun_min

  elemental function fun_max (op1, op2, rnd) result (rop)
      type (fmpfr), intent(in) :: op1
      type (fmpfr), intent(in) :: op2
      integer (c_int), intent(in), optional  :: rnd
      type (fmpfr) :: rop
      integer (mpfr_prec_kind) :: prec_op1, prec_op2, prec
      integer (c_int) :: rnd_val
      rnd_val = default_rnd
      if (present(rnd)) rnd_val = rnd
      if (.not. rop%initialized) then
        prec_op1 = mpfr_get_prec (op1%mp)
        prec_op2 = mpfr_get_prec (op2%mp)
        prec = max (prec_op1, prec_op2)
        call mpfr_init2 (rop%mp, prec)
      end if
      call fmpfr_max (rop%mp, op1%mp, op2%mp, rnd_val)
  end function fun_max

  function fun_set_str (s, rnd) result(rop)
    type (fmpfr) :: rop
    character(kind=c_char,len=*), intent(in) :: s
    integer (kind=kind(c_int)), intent(in), optional :: rnd
    integer, volatile :: rc
    integer :: rnd_val
    character(kind=c_char), dimension(:), allocatable, target :: s_arg
    rnd_val = default_rnd
    if (present(rnd)) rnd_val = rnd
    if (.not. rop%initialized) then
       call mpfr_init2 (rop%mp, default_prec)
       rop%initialized = .true.
     end if
    
    allocate (s_arg(len(s)+1))
    s_arg(1:len(s)) = transfer(s,s_arg)
    s_arg(len(s)+1) = char(0)
    call fmpfr_set_str (rop%mp, c_loc(s_arg), 10, rnd_val)
    deallocate (s_arg)
  end function fun_set_str

  subroutine write_formatted (dtv, unit, iotype, vlist, iostat, iomsg)
    class (fmpfr), intent(in) :: dtv
    integer, intent(in) :: unit
    character (len=*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character (len=*), intent(inout) :: iomsg
    character, pointer :: p(:)
    type (c_ptr) :: pc
    integer (mpfr_exp_kind) :: exp_val
    integer (c_size_t) :: slen, start
  
    if (mpfr_nan_p(dtv%mp) /= 0) then
       write (unit,fmt='(A)',advance="no",iostat=iostat,iomsg=iomsg) "NaN"
       return
    else if (mpfr_inf_p(dtv%mp) /= 0) then
       if (mpfr_signbit(dtv%mp) == 0) then
          write (unit,fmt='(A)',advance="no",iostat=iostat,iomsg=iomsg) "+Inf"
       else
          write (unit,fmt='(A)',advance="no",iostat=iostat,iomsg=iomsg) "-Inf"
       end if
       return
    end if

    pc = mpfr_get_str (c_null_ptr, exp_val, 10, 0_c_size_t, dtv%mp, default_rnd)
    slen = strlen(pc)
    call c_f_pointer (pc,p,shape=[slen])
    if (p(1) == '-' .or. p(1) == '+') then
       write (unit,fmt='(A)',advance="no",iostat=iostat,iomsg=iomsg) p(1)
       start = 2
    else
       start = 1
    end if
    write (unit,fmt='(*(A))',advance="no",iostat=iostat,iomsg=iomsg) p(start)
    if (iostat /= 0) return
    write (unit,fmt='(A)', advance="no", iostat=iostat, iomsg=iomsg) '.'
    if (iostat /= 0) return
    write (unit,fmt='(*(A))',advance="no",iostat=iostat,iomsg=iomsg) p(start+1:),"E"
    if (iostat /= 0) return
    write (unit,fmt='(I0,1X)',iostat=iostat,iomsg=iomsg) exp_val-1
    call mpfr_free_str(pc)
  end subroutine write_formatted

  elemental subroutine ass_set (rop, op)
    type (fmpfr), intent(inout) :: rop
    type (fmpfr), intent(in) :: op
    integer (c_int) :: rc

    if (.not. rop%initialized) then
      call mpfr_init2 (rop%mp, default_prec)
      rop%initialized = .true.
    end if
    call fmpfr_set (rop%mp, op%mp, default_rnd)    
  end subroutine ass_set

  elemental subroutine fmpfr_cleanup (self)
    type (fmpfr), intent(inout) :: self
    if (self%initialized) call mpfr_clear(self%mp)
    self%initialized = .false.
  end subroutine fmpfr_cleanup

  elemental subroutine init_default (op)
    type (fmpfr), intent(inout) :: op
    if (op%initialized) then
      call mpfr_set_prec (op%mp, default_prec)
    else
      call mpfr_init2 (op%mp, default_prec)
    end if
  end subroutine init_default

  subroutine set_default_rounding_mode (op)
    integer (c_int), intent(in) :: op
    default_rnd = op
    call mpfr_set_default_rounding_mode (op)
  end subroutine set_default_rounding_mode
  
  elemental subroutine init_long (op, prec)
    type (fmpfr), intent(inout) :: op
    integer (c_long), intent(in) :: prec
    integer (mpfr_prec_kind) :: prec_val
    prec_val = prec
    if (op%initialized) then
      call mpfr_set_prec (op%mp, prec_val)
    else
      call mpfr_init2 (op%mp, prec_val)
    end if
   end subroutine init_long

  subroutine set_default_prec_long (prec)
    integer (c_long), intent(in) :: prec
    default_prec = prec
  end subroutine set_default_prec_long

#if SIZEOF_INT < SIZEOF_LONG
  elemental subroutine init_int (op, prec)
    type (fmpfr), intent(inout) :: op
    integer (c_int), intent(in) :: prec
    integer (mpfr_prec_kind) :: prec_val
    prec_val = prec
    if (op%initialized) then
      call mpfr_set_prec (op%mp, prec_val)
    else
      call mpfr_init2 (op%mp, prec_val)
    end if
   end subroutine init_int

  subroutine set_default_prec_int (prec)
    integer (c_int), intent(in) :: prec
    default_prec = prec
  end subroutine set_default_prec_int

#endif
  elemental subroutine init_short (op, prec)
    type (fmpfr), intent(inout) :: op
    integer (c_short), intent(in) :: prec
    integer (mpfr_prec_kind) :: prec_val
    prec_val = prec
    if (op%initialized) then
      call mpfr_set_prec (op%mp, prec_val)
    else
      call mpfr_init2 (op%mp, prec_val)
    end if
   end subroutine init_short

  subroutine set_default_prec_short (prec)
    integer (c_short), intent(in) :: prec
    default_prec = prec
  end subroutine set_default_prec_short

end module fmpfr_oper

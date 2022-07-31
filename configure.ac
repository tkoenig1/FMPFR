AC_INIT([Fortran MPFR biding], 0.0,,[libfmpfr])

AC_CONFIG_SRCDIR(src/fmpfr_kinds.f90.in)
AC_CONFIG_FILES([
Makefile
src/fmpfr_kinds.f90
src/Makefile
test/Makefile
])

AM_INIT_AUTOMAKE([foreign])
AC_LANG_C
AC_PROG_CC
AC_PROG_FC
AC_CHECK_TOOL(AR, ar)
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_CHECK_HEADER(mpfr.h,,[AC_MSG_ERROR([Cannot find mpfr.h])])
AC_CHECK_LIB([mpfr],[mpfr_add],,[AC_MSG_ERROR([mpfr not usable])])
AC_CHECK_HEADER(gmp.h,,[AC_MSG_ERROR([Cannot find gmp.h])])
AC_CHECK_LIB([gmp],[__gmpz_init],,[AC_MSG_ERROR([gmp not usable])])
AC_MSG_CHECKING([for mp_limb_t])
mpfr_limb_t="c_long"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <gmp.h>
#include <mpfr.h>
#if !defined(__GMP_SHORT_LIMB)
#error
#endif
]])],[mpfr_limb_t=c_int])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <gmp.h>
#include <mpfr.h>
#if !defined(_LONG_LONG_LIMB)
#error
#endif
]])],[mpfr_limb_t=c_long_long])
AC_MSG_RESULT($mpfr_limb_t)
AC_SUBST(mpfr_limb_t)

AC_MSG_CHECKING([for mpfr_prec_t])
mpfr_prec_t="none"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <gmp.h>
#include <mpfr.h>
#if _MPFR_PREC_FORMAT != 2
#error
#endif
]])],[mpfr_prec_t=c_int])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <gmp.h>
#include <mpfr.h>
#if _MPFR_PREC_FORMAT != 3
#error
#endif
]])],[mpfr_prec_t=c_long])
if test "$mpfr_prec_t" == "none"; then
  AC_MSG_ERROR("Cannot determine mpfr_prec_t")
else
  AC_MSG_RESULT($mpfr_prec_t)
  AC_SUBST(mpfr_prec_t)
fi

AC_MSG_CHECKING([for mpfr_exp_t])
mpfr_exp_t="none"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <gmp.h>
#include <mpfr.h>
#if _MPFR_EXP_FORMAT != 1
#error
#endif
]])],[mpfr_exp_t=c_short])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <gmp.h>
#include <mpfr.h>
#if _MPFR_EXP_FORMAT != 2
#error
#endif
]])],[mpfr_exp_t=c_int])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <gmp.h>
#include <mpfr.h>
#if _MPFR_EXP_FORMAT != 3
#error
#endif
]])],[mpfr_exp_t=c_long])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <gmp.h>
#include <mpfr.h>
#if _MPFR_EXP_FORMAT != 4
#error
#endif
]])],[mpfr_exp_t=c_long])
if test "$mpfr_exp_t" == "none"; then
  AC_MSG_ERROR("Cannot determine mpfr_exp_t")
else
  AC_MSG_RESULT($mpfr_exp_t)
  AC_SUBST(mpfr_exp_t)
fi

AC_CHECK_SIZEOF([int])
AC_CHECK_SIZEOF([long])
AC_FC_PP_SRCEXT([F90])
# AC_SUBST(ac_cv_fc_pp_srcext_F90)
if test "$ac_cv_fc_pp_srcext_F90" == "none"; then
  fc_preprocessor_flag=""
else
  fc_preprocessor_flag="$ac_cv_fc_pp_srcext_F90"
fi
AC_SUBST(fc_preprocessor_flag)
AC_FC_PP_DEFINE
fc_preflags="$ac_cv_fc_pp_define""SIZEOF_INT=$ac_cv_sizeof_int $ac_cv_fc_pp_define""SIZEOF_LONG=$ac_cv_sizeof_long"
AC_MSG_CHECKING([if __float128 is supported])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
  __float128 foo (__float128 x)
  {
     __float128 a, b;
     a = x/7.Q;
     b = a + a *2.3e10Q;
  }
  ]],[[foo(1.2Q)]])],
  [
     have_float128=yes
  ],[
     have_float128=no
  ]
)
AC_MSG_RESULT([$have_float128])
mpfr_have_float128="no"
if test "$have_float128" = "yes"; then
  AC_MSG_CHECKING([if mpfr has __float128 support])
  AC_LINK_IFELSE([AC_LANG_PROGRAM([
#define MPFR_WANT_FLOAT128 1
#include <mpfr.h>
],
[
  mpfr_t a;
  __float128 x;
  x = 3.7Q;
  mpfr_set_float128 (a, x, MPFR_RNDN)
])],[mpfr_have_float128="yes"])
  AC_MSG_RESULT([$mpfr_have_float128])
fi

AC_LANG_PUSH([Fortran])
AC_MSG_CHECKING([if derived-type I/O is supported])
AC_COMPILE_IFELSE([
      module x
      implicit none
      type, public:: foo
      character(len=2) :: c
      end type foo
      interface read(formatted)
      module procedure read_formatted
      end interface read(formatted)
      contains
      subroutine read_formatted (dtv, unit, iotype, vlist, iostat,
     &     iomsg)
      class (foo), intent(inout) :: dtv
      integer, intent(in) :: unit
      character (len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character (len=*), intent(inout) :: iomsg
      read (unit,'(A)',iostat=iostat,iomsg=iomsg) dtv%c
      end subroutine read_formatted
      end module x
      
      program main
      use x
      implicit none
      type(foo) :: a, b
      open (10,file="testfile.dat",status="replace")
      write (10,'(A)') '','aa bb'
      rewind (10)
      a%c = "x"
      b%c = "y"
      read (10,*) a, b
      if (a%c .eq. "aa" .and. b%c .eq. "bb") then
         close (10,status="delete")
      else
         close (10,status="keep")
      end if
      end program main
],
[derived_io=yes],[derived_io=no])
AC_MSG_RESULT([$derived_io])
if test "$derived_io" == "yes"; then
  fc_preflags="$fc_preflags $ac_cv_fc_pp_define""USE_DERIVED_IO=1"
fi
AC_MSG_CHECKING([if the Fortran compiler supports finalization])
AC_COMPILE_IFELSE([
      module x
        type foo
          integer :: a
        contains
          final :: cleanup
        end type foo
      contains
        subroutine cleanup(self)
          type(foo), intent(inout) :: self
          self%a = 0
      end subroutine cleanup
      end module x
],[AC_MSG_RESULT([yes])],[AC_MSG_ERROR([Finalization not supported])])
AC_MSG_CHECKING([if the Fortran compiler supports -I])
save_FCFLAGS=$FCFLAGS
AC_COMPILE_IFELSE([
      error stop
      end
],[AC_MSG_RESULT([yes])],[AC_MSG_ERROR([-I not supported.])])
FCFLAGS=$save_FCFLAGS
AC_MSG_CHECKING([if Fortran supports ERROR STOP])
AC_COMPILE_IFELSE([
      error stop
      end
],[AC_MSG_RESULT([yes])],AC_MSG_ERROR([ERROR STOP required for testsuite for now]))
AC_MSG_CHECKING([if Fortran supports IEEE 128-bit])
AC_COMPILE_IFELSE([
      integer, parameter :: qp = selected_real_kind(33,4000)
      real(kind=qp) :: a
      end
],[fortran_have_ieee128=yes],[fortran_have_ieee128=no])
AC_MSG_RESULT([$fortran_have_ieee128])
if test "$fortran_have_ieee128""$mpfr_have_float128" == yesyes; then
fc_preflags="$fc_preflags $ac_cv_fc_pp_define""USE_FLOAT128=1"
CFLAGS="$CFLAGS -DUSE_FLOAT128=1"
fi
AC_MSG_CHECKING([if c_long_double is useful])
AC_COMPILE_IFELSE([
      module x
      use iso_c_binding
      interface inter
      module procedure f_d, f_l
      end interface inter
      contains
      subroutine f_d(arg)
      real(kind=c_double) :: arg
      end subroutine f_d
      subroutine f_l(arg)
      real(kind=c_long_double) :: arg
      end subroutine f_l
      end module x

],[fortran_use_c_long_double=yes],[fortarn_use_c_long_double=no])
AC_MSG_RESULT([$fortran_use_c_long_double])
if test x"$fortran_use_c_long_double" == xyes; then
fc_preflags="$fc_preflags $ac_cv_fc_pp_define""USE_LONG_DOUBLE=1"
CFLAGS="$CFLAGS -DUSE_LONG_DOUBLE=1"
fi

AC_SUBST(use_float128)
AC_LANG_POP([Fortran])
AC_ARG_WITH([default-precision],
  [AS_HELP_STRING([--with-default-precision],
     [default-precision (default is 113)])],
  [default_precision=$withval],
  [default_precision=113])
AC_SUBST(default_precision)
AC_MSG_CHECKING([flags to pass for Fortran files to be preprocessed])
AC_MSG_RESULT([$fc_preflags])
AC_SUBST(fc_preflags)
AC_MSG_NOTICE([Default preicision is $default_precision])
AC_OUTPUT
rm -f x.mod

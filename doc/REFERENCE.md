# FMPFR library reference

This is the refrence for the FMPFR library.  Like the library itself,
it is still incomplete and contain bugs.  The interface is not
guaranteed to be stable.

## Modules

The library provides two modules: `fmpfr_kinds`, which declares the kinds
of the integer variables, `fmpfr_c` which provides interfaces to the
MPFR functions which can reasonably be used from Fortran; for example,
all functions pertaining to unsigned integer types have been removed.
Finally, `fmpfr_oper` provides all the functions that a user would
normally need.

## I/O

On compilers which support user-derived I/O, list-directed output
is supported.  Output can also be done using the function `get_str`,
which takes two optional arguments: The number of decimal digits,
`n`, and the rounding mode, `rnd`.  Note that `get_str` is not
elemental, so it has to operate on individual array elements.

Input has to be done by reading strings, and then using the `fmpfr`
function on them.

## Arithmetic

### Supported operators

The Fortran operators `+`, `-`, `*`, `/` and `**` are supported.  For
the elementary arithmetic operations, both sides can be `type(fmpr)`
and `integer(c_short)`, `integer(c_int)`, `integer(c_long)`,
`real(c_real)` and `real(c_double)` (which usually includes Fortran's
`REAL`, `INTEGER` and `DOUBLE PRECISION` types).

If you use these operators, they use the default rounding as set by
`set_rounding_mode`, which defaults to`MPFR_RNDN`.

Comparision operators are supported, but both sides have to be of
`type (fmpr)`.  This mirrors the usage of MPFR.  The best way to
compare to a constant would be
```
  if (a < fmpfr("1.2"))
```

### Rounding modes

Default rounding can be set via a call to `set_default_rounding_mode`.
The constants `MPFR_RNDN`, `MPFR_RNDZ`, `MPFR_RNDU`, `MPFR_RNDD`,
`MPFR_RNDA` and `MPFR_RNDF` are available.

Here is an example:

```
program memain
  use fmpfr_oper
  implicit none
  call foo
contains
  subroutine foo
    type (fmpfr), dimension(:), allocatable :: a,b
    integer :: i
    allocate (a(10),b(10))
    call set_default_prec (64)
    a(1) = fmpfr("1.3")
    b(1) = a(1)
    do i=2,size(a)
       a(i) = a(i-1) + a(i-1)/2
    end do
    call set_default_rounding_mode (mpfr_rndz)
    do i=2,size(b)
       b(i) = b(i-1) + b(i-1)/2
    end do
    do i=1,size(a)
       print *,a(i),b(i),a(i)-b(i)
    end do
  end subroutine foo
end program memain
```
which has the output
```
 1.29999999999999999995E0  1.29999999999999999995E0  0.00000000000000000000E-1 
 1.94999999999999999993E0  1.94999999999999999993E0  0.00000000000000000000E-1 
 2.92499999999999999995E0  2.92499999999999999973E0  2.16840434497100886801E-19 
 4.38749999999999999982E0  4.38749999999999999939E0  4.33680868994201773602E-19 
 6.58124999999999999973E0  6.58124999999999999887E0  8.67361737988403547205E-19 
 9.87187499999999999982E0  9.87187499999999999809E0  1.73472347597680709441E-18 
 1.48078124999999999993E1  1.48078124999999999967E1  2.60208521396521064161E-18 
 2.22117187499999999989E1  2.22117187499999999937E1  5.20417042793042128323E-18 
 3.33175781249999999993E1  3.33175781249999999888E1  1.04083408558608425664E-17 
 4.99763671874999999972E1  4.99763671874999999833E1  1.38777878078144567552E-17 
```

## Initialization of variables

Each variable in MPFR has a precision, given in bits.  When a variable is
first assigned to, it gets the default precision as specified by
`set_default_prec`.  To mimic Fortran semantics, the precision of the
variable does *not* depend on the precision of the right-hand side.
If a different precision is required, it is possible to initialize a
variable `a` using `call init(a, prec)` where prec is the number of binary
digits to be used.

## Assignment

## Functions

### Supported intrinsic functions

The following of Fortran's intrinsic functions are supported: `abs`,
`acos`, `asin`, `atan`, `atan2`, `cos`, `div`, `exp`, `gamma`, `log`,
`log10`, `max`, `min`, `sin`, `sqrt`, `tan`.

All these functions take an optional second argument for rounding, and return
a value of the same accuracy as its first arguments.

### Conversion to fmpfr

The `fmpfr` function can be used to convert a character variable to
an fmpfr value.  It takes two optional arguments: `prec`, which
specifies the number of arguments, and `rnd`, which specifies the
rounding mode.

### Arithmetic functions with rounding control

The functions `add`, `sub`, `mul`, `div` and `pow` perform the
normal arithmetic operations, but allow specification of a rounding
mode.  Example:
```
program memain
  use fmpfr_oper
  implicit none
  type (fmpfr) :: a, b
  call set_default_prec (64)
  a = fmpfr("1.4")
  b = fmpfr("1.2")
  print *,add(a,b,mpfr_rndn)
  print *,add(a,b,mpfr_rndz)
  print *,add(a,b,mpfr_rnda)
end program memain
```
with the output
```
 2.59999999999999999991E0 
 2.59999999999999999991E0 
 2.60000000000000000013E0
```

### Functions to convert from Fortran data types

The functions `get_flt`, `get_d`, `get_ld` and `get_si` return a
`real(c_float)`, a `real(c_double)`, a `real(c_long_double)` and an
`integer(c_long)`, respectively, again with optional rounding.

Example:
```
program memain
  use fmpfr_oper
  implicit none
  type (fmpfr) :: a
  call set_default_prec (64)
  a = fmpfr("1.4")
  print *,get_si (a, mpfr_rndz)
  print *,get_si (a, mpfr_rndu)
end program memain
```
with the output (the number of spaces will vary according
to your Fortran compiler)
```
                    1
                    2
```


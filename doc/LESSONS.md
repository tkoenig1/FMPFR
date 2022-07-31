# Lessons learned from the implementation of FMPFR

This documents some of the design choices for, and lessons from, the
implementation of MPFR, reflecting both strenghs and weaknesses of
Fortran and the other tools that are used.

The aim of the project was to make MPFR easy to use for a Fortran
programmer by providing a "thick binding", using overloading,
elemental functions and subroutines and user-defined I/O to
make the code look as close as possible to "natural" Fortran
code, and to facilitate conversion of existing code to use
the library.

The big strength of Fortran is that this is indeed possible.

## Why generate the code with a script?

### Automated generation of interfaces to C

MPFR is a large library, with more than 300 functions.  Picking out
those functions which are required and translating them for hand
would have been time-consuming and error-prone.  Fortunately, the
interface to MPFR is very regular, and its interfaces from the Texinfo
source to the documentation, for example
```
@deftypefun void mpfr_init2 (mpfr_t @var{x}, mpfr_prec_t @var{prec})
```
are easy to find with `grep` and easy to parse with a script
language.  Also, functions which are not suitable to use with Fortran
(for example those containing unsigned integers) can easily be
removed.  In this project, perl is used, but other script languages
would be equally suitable as long as they have associative arrays.

### Generating Fortran code for similar functions

MPFR contains many functions which serve a similar purpose, with
different data types.  As an example, `mpfr_add_si` adds a variable
of type `mpfr_t` and an `unsigned long`, and `mpfr_add_d` adds an
`mpfr_t` and a `double`.  Also, operator overloading should work
for both `a+b` and `b+a` where `a` is a multi-precision variable
and `b` is one of Fortran's types.

This is also an advantage when a change is made to these functions.
It is then enough to change one place, and not change the customary
n-1 places when n would have been needed.

### Lack of generics

A use would probably like to write `a+3` instead of `a + 3_c_long`,
especially since ISO C binding is not otherwise needed on the user side.
This was solved by generating appropriate functions from the script.

## Why the configure scripts and preprocessing?

While a Fortran processor that implements C binding has to provide
both `c_int` and `c_long`, these might be the same, and an interface like

```
  interface foo
    module procedure foo_int
    module procedure foo_long
  end interface

   subroutine foo_int (a)
     integer(c_int) :: a
...
   subroutine foo_long
     integer(c_long) :: a
```

would be rejected. There is no way known to the author do solve this
issue within the bounds of the Fortran standard.

So, there is a need for a preprocessing step.  This was done using
the de-facto-standard of using the C preprocessor, but the
preprocessore needed to get its information from somewhere.

For this, autoconf and the rest of the GNU autotools suggested
itself - checking for sizes of C types is implemented there.

Autoconf is now also used for checking features (not all compilers
support user-defined I/O) and for getting information on the internal
types of MPFR.  Also, the testsuite uses autotools.

For the author, autotools are not easy to use because of documentation which
is aimed at describing the features, and less towards how to reach
specific goals.

## Other issues

### User-defined I/O

I found user-defined I/O to be difficult.  For list-directed
output, it is usable (if not implemented on all compiler versions I tried
this on).  Input appears to be poorly specified, and different compiler
writers appear to have different ideas what is specified.  I therefore had
to leave that out.

### Generics with two interger arguments

One design issue was how to design generics where two integer
arguments (the precision and the rounding mode) could both be
optional arguments, and the compiler could differentiate between
them.  The solution implemented is somewhat of a hack, but if there
is a cleaner solution, I would like to hear about it.

The constants for `mpfr_rnd_t`are specified as `integer(int8)`, and
the precision can be specified as `integer(c_short)`,
`integer(c_int)` or `integer(c_long)`.

For conversion from a string to a `type(fmpfr)`. the interface
then contains the functions

```
  function fun_set_str (s, rnd) result(rop)
    type (fmpfr) :: rop
    integer (kind=int8), optional :: rnd
    character(kind=c_char,len=*), intent(in) :: s
!...
  function fun_set_str_long (s, prec, rnd) result(rop)
    type (fmpfr) :: rop
    character(kind=c_char,len=*), intent(in) :: s
    integer (c_long), intent(in) :: prec
    integer (kind=int8), intent(in), optional :: rnd
```
plus two versions for `integer(c_int)` and `integer(c_short)`.

### Pure functions and return codes

Many MPFR functions return a value indicating the direction of
rounding.  In order to be able to use them in an elemental
procedure, they have to be declared pure.  However, an
optimizting compiler will remove a pure function call
like
```
  rc = mpfr_add_si (rop&mp, op1%mp, op2, rnd_val)
```
when no further use is made of `rc`.  While it is normal style
in C to discard the return value of a function, Fortran C
binding does not allow this.  Therefore, the line above
was replaced with
```
 call fmpfr_add_si (rop%mp, op1%mp, op2, rnd_val)
```
where `fmpfr_add_si` is a glue function containing only the
single call to `mpfr_add_si` with the same arguments, which
is translated into a single jump.

### `ELEMENTAL` and `RECURSIVE`

In Fortran 2008, a procedure could not both be elemental and
recursive.  This restriction was lifted in Fortran 2018, but that
is not implemented by many compilers up to now.

### Array intrinsics

It would have been nice to be able to use array intrinsics like
`MAXLOC` or `CSHIFT`.  Unfortunately, it is not possible to write
these intrinsics in Fortran unless a version is written for each
rank.

The extended C interoperability of Fortran 2018 could be used to
write such an implementation (as libgfortran shows), but this would
be a much larger job than the current implementation, and the code
from libgfortran cannot be used due to the difference in license.

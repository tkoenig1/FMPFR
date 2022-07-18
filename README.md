# FMPFR - a Fortran binding for MPFR

This library offers a Fortran binding to the [GNU MPFR
Library](https://www.mpfr.org/) multi-precision library.  The aim of
the library is to make the use of MPFR convenient for a Fortran user.

## What is supported

* Arithmetic expressions can be used the usual way; expressions
can be mixed with real and integer expressions.

* Mathematical intrinsics, like `sin`, `cos` etc.

* Array expressions

* Setting and changing a default precision of variables

* List-directed output

* Reading values from strings

## What is *not* supported

* Input (user-defined I/O is too unevenly implemented in existing
  compilers)

* Array intrinsics like `MAXLOC` and `MAXVAL`

## An example

Here is a short exampe:
```
program memain
  use fmpfr_oper
  implicit none
  type (fmpfr), dimension(:), allocatable :: a
  call set_default_prec (128)
  allocate (a(2))
  a(1) = fmpfr("1.3")
  a(2) = a(1) + 2
  print *,a
  print *,sin(a)
end program memain
```
whose output is
```
 1.299999999999999999999999999999999999998E0  3.299999999999999999999999999999999999998E0 
 9.635581854171929647013486300395548153418E-1  -1.577456941432483820116542776024823708430E-1
```

## Status

This is an initial work, expect bugs and (especially) missing features.

## Modifying the code

If you want to modify the code, it is best if you make your changes to
the `src/generate.pl` file and run
```
$ grep ^@deftypefun mfpr.texi | perl generate.pl
```
where mpfr.texi is the MPFR documentation.

## License

This is covered under the MIT license.
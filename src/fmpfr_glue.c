/*
  This file is part of the FMPFR library.

  Copyright 2022 by Thomas Koenig

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation
  files (the "Software"), to deal in the Software without
  restriction, including without limitation the rights to use, copy,
  modify, merge, publish, distribute, sublicense, and/or sell copies
  of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
*/

#include <mpfr.h>

void fmpfr_set (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
{
  mpfr_set (rop, op, rnd);
}

void fmpfr_set_si (mpfr_t rop, long int op, mpfr_rnd_t rnd)
{
  mpfr_set_si (rop, op, rnd);
}

void fmpfr_set_flt (mpfr_t rop, float op, mpfr_rnd_t rnd)
{
  mpfr_set_flt (rop, op, rnd);
}

void fmpfr_set_d (mpfr_t rop, double op, mpfr_rnd_t rnd)
{
  mpfr_set_d (rop, op, rnd);
}

void fmpfr_set_ld (mpfr_t rop, long double op, mpfr_rnd_t rnd)
{
  mpfr_set_ld (rop, op, rnd);
}

void fmpfr_set_float128 (mpfr_t rop, _Float128 op, mpfr_rnd_t rnd)
{
  mpfr_set_float128 (rop, op, rnd);
}

void fmpfr_set_str (mpfr_t rop, const char * s, int base, mpfr_rnd_t rnd)
{
  mpfr_set_str (rop, s, base, rnd);
}

void fmpfr_add (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
{
  mpfr_add (rop, op1, op2, rnd);
}

void fmpfr_add_si (mpfr_t rop, mpfr_t op1, long int op2, mpfr_rnd_t rnd)
{
  mpfr_add_si (rop, op1, op2, rnd);
}

void fmpfr_add_d (mpfr_t rop, mpfr_t op1, double op2, mpfr_rnd_t rnd)
{
  mpfr_add_d (rop, op1, op2, rnd);
}

void fmpfr_sub (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
{
  mpfr_sub (rop, op1, op2, rnd);
}

void fmpfr_si_sub (mpfr_t rop, long int op1, mpfr_t op2, mpfr_rnd_t rnd)
{
  mpfr_si_sub (rop, op1, op2, rnd);
}

void fmpfr_sub_si (mpfr_t rop, mpfr_t op1, long int op2, mpfr_rnd_t rnd)
{
  mpfr_sub_si (rop, op1, op2, rnd);
}

void fmpfr_d_sub (mpfr_t rop, double op1, mpfr_t op2, mpfr_rnd_t rnd)
{
  mpfr_d_sub (rop, op1, op2, rnd);
}

void fmpfr_sub_d (mpfr_t rop, mpfr_t op1, double op2, mpfr_rnd_t rnd)
{
  mpfr_sub_d (rop, op1, op2, rnd);
}

void fmpfr_mul (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
{
  mpfr_mul (rop, op1, op2, rnd);
}

void fmpfr_mul_si (mpfr_t rop, mpfr_t op1, long int op2, mpfr_rnd_t rnd)
{
  mpfr_mul_si (rop, op1, op2, rnd);
}

void fmpfr_mul_d (mpfr_t rop, mpfr_t op1, double op2, mpfr_rnd_t rnd)
{
  mpfr_mul_d (rop, op1, op2, rnd);
}

void fmpfr_div (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
{
  mpfr_div (rop, op1, op2, rnd);
}

void fmpfr_si_div (mpfr_t rop, long int op1, mpfr_t op2, mpfr_rnd_t rnd)
{
  mpfr_si_div (rop, op1, op2, rnd);
}

void fmpfr_div_si (mpfr_t rop, mpfr_t op1, long int op2, mpfr_rnd_t rnd)
{
  mpfr_div_si (rop, op1, op2, rnd);
}

void fmpfr_d_div (mpfr_t rop, double op1, mpfr_t op2, mpfr_rnd_t rnd)
{
  mpfr_d_div (rop, op1, op2, rnd);
}

void fmpfr_div_d (mpfr_t rop, mpfr_t op1, double op2, mpfr_rnd_t rnd)
{
  mpfr_div_d (rop, op1, op2, rnd);
}

void fmpfr_sqrt (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
{
  mpfr_sqrt (rop, op, rnd);
}

void fmpfr_abs (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
{
  mpfr_abs (rop, op, rnd);
}

void fmpfr_greater_p (mpfr_t op1, mpfr_t op2)
{
  mpfr_greater_p (op1, op2);
}

void fmpfr_greaterequal_p (mpfr_t op1, mpfr_t op2)
{
  mpfr_greaterequal_p (op1, op2);
}

void fmpfr_less_p (mpfr_t op1, mpfr_t op2)
{
  mpfr_less_p (op1, op2);
}

void fmpfr_lessequal_p (mpfr_t op1, mpfr_t op2)
{
  mpfr_lessequal_p (op1, op2);
}

void fmpfr_equal_p (mpfr_t op1, mpfr_t op2)
{
  mpfr_equal_p (op1, op2);
}

void fmpfr_lessgreater_p (mpfr_t op1, mpfr_t op2)
{
  mpfr_lessgreater_p (op1, op2);
}

void fmpfr_log (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
{
  mpfr_log (rop, op, rnd);
}

void fmpfr_log10 (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
{
  mpfr_log10 (rop, op, rnd);
}

void fmpfr_exp (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
{
  mpfr_exp (rop, op, rnd);
}

void fmpfr_pow (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
{
  mpfr_pow (rop, op1, op2, rnd);
}

void fmpfr_pow_si (mpfr_t rop, mpfr_t op1, long int op2, mpfr_rnd_t rnd)
{
  mpfr_pow_si (rop, op1, op2, rnd);
}

void fmpfr_cos (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
{
  mpfr_cos (rop, op, rnd);
}

void fmpfr_sin (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
{
  mpfr_sin (rop, op, rnd);
}

void fmpfr_tan (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
{
  mpfr_tan (rop, op, rnd);
}

void fmpfr_acos (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
{
  mpfr_acos (rop, op, rnd);
}

void fmpfr_asin (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
{
  mpfr_asin (rop, op, rnd);
}

void fmpfr_atan (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
{
  mpfr_atan (rop, op, rnd);
}

void fmpfr_atan2 (mpfr_t rop, mpfr_t y, mpfr_t x, mpfr_rnd_t rnd)
{
  mpfr_atan2 (rop, y, x, rnd);
}

void fmpfr_gamma (mpfr_t rop, mpfr_t op, mpfr_rnd_t rnd)
{
  mpfr_gamma (rop, op, rnd);
}

void fmpfr_min (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
{
  mpfr_min (rop, op1, op2, rnd);
}

void fmpfr_max (mpfr_t rop, mpfr_t op1, mpfr_t op2, mpfr_rnd_t rnd)
{
  mpfr_max (rop, op1, op2, rnd);
}


program memain
  use fmpfr_oper
  implicit none
  type (fmpfr) :: c
  c = fmpfr("-0.5")  + (-0.5)
  call error_check (c, fmpfr (-1))

  c = (-0.5) + fmpfr("-0.5")
 call error_check (c, fmpfr (-1))

  c = fmpfr("-0.5")  - (-0.5)
  call error_check (c, fmpfr (0))

  c = (-0.5) - fmpfr("-0.5")
 call error_check (c, fmpfr (0))

  c = fmpfr("-0.5")  * (-0.5)
  call error_check (c, fmpfr (0.25))

  c = (-0.5) * fmpfr("-0.5")
 call error_check (c, fmpfr (0.25))

  c = fmpfr("-0.5")  / (-0.5)
  call error_check (c, fmpfr (1))

  c = (-0.5) / fmpfr("-0.5")
 call error_check (c, fmpfr (1))

  c = fmpfr("-0.5")  + (-0.5)
  call error_check (c, fmpfr (-1))

  c = (-0.5) + fmpfr("-0.5")
 call error_check (c, fmpfr (-1))

  c = fmpfr("-0.5")  - (-0.5)
  call error_check (c, fmpfr (0))

  c = (-0.5) - fmpfr("-0.5")
 call error_check (c, fmpfr (0))

  c = fmpfr("-0.5")  * (-0.5)
  call error_check (c, fmpfr (0.25))

  c = (-0.5) * fmpfr("-0.5")
 call error_check (c, fmpfr (0.25))

  c = fmpfr("-0.5")  / (-0.5)
  call error_check (c, fmpfr (1))

  c = (-0.5) / fmpfr("-0.5")
 call error_check (c, fmpfr (1))

  c = fmpfr("-0.5")  + (0.5)
  call error_check (c, fmpfr (0))

  c = (0.5) + fmpfr("-0.5")
 call error_check (c, fmpfr (0))

  c = fmpfr("-0.5")  - (0.5)
  call error_check (c, fmpfr (-1))

  c = (0.5) - fmpfr("-0.5")
 call error_check (c, fmpfr (1))

  c = fmpfr("-0.5")  * (0.5)
  call error_check (c, fmpfr (-0.25))

  c = (0.5) * fmpfr("-0.5")
 call error_check (c, fmpfr (-0.25))

  c = fmpfr("-0.5")  / (0.5)
  call error_check (c, fmpfr (-1))

  c = (0.5) / fmpfr("-0.5")
 call error_check (c, fmpfr (-1))

  c = fmpfr("-0.5")  + (0.5)
  call error_check (c, fmpfr (0))

  c = (0.5) + fmpfr("-0.5")
 call error_check (c, fmpfr (0))

  c = fmpfr("-0.5")  - (0.5)
  call error_check (c, fmpfr (-1))

  c = (0.5) - fmpfr("-0.5")
 call error_check (c, fmpfr (1))

  c = fmpfr("-0.5")  * (0.5)
  call error_check (c, fmpfr (-0.25))

  c = (0.5) * fmpfr("-0.5")
 call error_check (c, fmpfr (-0.25))

  c = fmpfr("-0.5")  / (0.5)
  call error_check (c, fmpfr (-1))

  c = (0.5) / fmpfr("-0.5")
 call error_check (c, fmpfr (-1))

  c = fmpfr("-0.5")  + (1.0)
  call error_check (c, fmpfr (0.5))

  c = (1.0) + fmpfr("-0.5")
 call error_check (c, fmpfr (0.5))

  c = fmpfr("-0.5")  - (1.0)
  call error_check (c, fmpfr (-1.5))

  c = (1.0) - fmpfr("-0.5")
 call error_check (c, fmpfr (1.5))

  c = fmpfr("-0.5")  * (1.0)
  call error_check (c, fmpfr (-0.5))

  c = (1.0) * fmpfr("-0.5")
 call error_check (c, fmpfr (-0.5))

  c = fmpfr("-0.5")  / (1.0)
  call error_check (c, fmpfr (-0.5))

  c = (1.0) / fmpfr("-0.5")
 call error_check (c, fmpfr (-2))

  c = fmpfr("-0.5")  + (1.0)
  call error_check (c, fmpfr (0.5))

  c = (1.0) + fmpfr("-0.5")
 call error_check (c, fmpfr (0.5))

  c = fmpfr("-0.5")  - (1.0)
  call error_check (c, fmpfr (-1.5))

  c = (1.0) - fmpfr("-0.5")
 call error_check (c, fmpfr (1.5))

  c = fmpfr("-0.5")  * (1.0)
  call error_check (c, fmpfr (-0.5))

  c = (1.0) * fmpfr("-0.5")
 call error_check (c, fmpfr (-0.5))

  c = fmpfr("-0.5")  / (1.0)
  call error_check (c, fmpfr (-0.5))

  c = (1.0) / fmpfr("-0.5")
 call error_check (c, fmpfr (-2))

  c = fmpfr("0.5")  + (-0.5)
  call error_check (c, fmpfr (0))

  c = (-0.5) + fmpfr("0.5")
 call error_check (c, fmpfr (0))

  c = fmpfr("0.5")  - (-0.5)
  call error_check (c, fmpfr (1))

  c = (-0.5) - fmpfr("0.5")
 call error_check (c, fmpfr (-1))

  c = fmpfr("0.5")  * (-0.5)
  call error_check (c, fmpfr (-0.25))

  c = (-0.5) * fmpfr("0.5")
 call error_check (c, fmpfr (-0.25))

  c = fmpfr("0.5")  / (-0.5)
  call error_check (c, fmpfr (-1))

  c = (-0.5) / fmpfr("0.5")
 call error_check (c, fmpfr (-1))

  c = fmpfr("0.5")  + (-0.5)
  call error_check (c, fmpfr (0))

  c = (-0.5) + fmpfr("0.5")
 call error_check (c, fmpfr (0))

  c = fmpfr("0.5")  - (-0.5)
  call error_check (c, fmpfr (1))

  c = (-0.5) - fmpfr("0.5")
 call error_check (c, fmpfr (-1))

  c = fmpfr("0.5")  * (-0.5)
  call error_check (c, fmpfr (-0.25))

  c = (-0.5) * fmpfr("0.5")
 call error_check (c, fmpfr (-0.25))

  c = fmpfr("0.5")  / (-0.5)
  call error_check (c, fmpfr (-1))

  c = (-0.5) / fmpfr("0.5")
 call error_check (c, fmpfr (-1))

  c = fmpfr("0.5")  + (0.5)
  call error_check (c, fmpfr (1))

  c = (0.5) + fmpfr("0.5")
 call error_check (c, fmpfr (1))

  c = fmpfr("0.5")  - (0.5)
  call error_check (c, fmpfr (0))

  c = (0.5) - fmpfr("0.5")
 call error_check (c, fmpfr (0))

  c = fmpfr("0.5")  * (0.5)
  call error_check (c, fmpfr (0.25))

  c = (0.5) * fmpfr("0.5")
 call error_check (c, fmpfr (0.25))

  c = fmpfr("0.5")  / (0.5)
  call error_check (c, fmpfr (1))

  c = (0.5) / fmpfr("0.5")
 call error_check (c, fmpfr (1))

  c = fmpfr("0.5")  + (0.5)
  call error_check (c, fmpfr (1))

  c = (0.5) + fmpfr("0.5")
 call error_check (c, fmpfr (1))

  c = fmpfr("0.5")  - (0.5)
  call error_check (c, fmpfr (0))

  c = (0.5) - fmpfr("0.5")
 call error_check (c, fmpfr (0))

  c = fmpfr("0.5")  * (0.5)
  call error_check (c, fmpfr (0.25))

  c = (0.5) * fmpfr("0.5")
 call error_check (c, fmpfr (0.25))

  c = fmpfr("0.5")  / (0.5)
  call error_check (c, fmpfr (1))

  c = (0.5) / fmpfr("0.5")
 call error_check (c, fmpfr (1))

  c = fmpfr("0.5")  + (1.0)
  call error_check (c, fmpfr (1.5))

  c = (1.0) + fmpfr("0.5")
 call error_check (c, fmpfr (1.5))

  c = fmpfr("0.5")  - (1.0)
  call error_check (c, fmpfr (-0.5))

  c = (1.0) - fmpfr("0.5")
 call error_check (c, fmpfr (0.5))

  c = fmpfr("0.5")  * (1.0)
  call error_check (c, fmpfr (0.5))

  c = (1.0) * fmpfr("0.5")
 call error_check (c, fmpfr (0.5))

  c = fmpfr("0.5")  / (1.0)
  call error_check (c, fmpfr (0.5))

  c = (1.0) / fmpfr("0.5")
 call error_check (c, fmpfr (2))

  c = fmpfr("0.5")  + (1.0)
  call error_check (c, fmpfr (1.5))

  c = (1.0) + fmpfr("0.5")
 call error_check (c, fmpfr (1.5))

  c = fmpfr("0.5")  - (1.0)
  call error_check (c, fmpfr (-0.5))

  c = (1.0) - fmpfr("0.5")
 call error_check (c, fmpfr (0.5))

  c = fmpfr("0.5")  * (1.0)
  call error_check (c, fmpfr (0.5))

  c = (1.0) * fmpfr("0.5")
 call error_check (c, fmpfr (0.5))

  c = fmpfr("0.5")  / (1.0)
  call error_check (c, fmpfr (0.5))

  c = (1.0) / fmpfr("0.5")
 call error_check (c, fmpfr (2))

  c = fmpfr("1.0")  + (-0.5)
  call error_check (c, fmpfr (0.5))

  c = (-0.5) + fmpfr("1.0")
 call error_check (c, fmpfr (0.5))

  c = fmpfr("1.0")  - (-0.5)
  call error_check (c, fmpfr (1.5))

  c = (-0.5) - fmpfr("1.0")
 call error_check (c, fmpfr (-1.5))

  c = fmpfr("1.0")  * (-0.5)
  call error_check (c, fmpfr (-0.5))

  c = (-0.5) * fmpfr("1.0")
 call error_check (c, fmpfr (-0.5))

  c = fmpfr("1.0")  / (-0.5)
  call error_check (c, fmpfr (-2))

  c = (-0.5) / fmpfr("1.0")
 call error_check (c, fmpfr (-0.5))

  c = fmpfr("1.0")  + (-0.5)
  call error_check (c, fmpfr (0.5))

  c = (-0.5) + fmpfr("1.0")
 call error_check (c, fmpfr (0.5))

  c = fmpfr("1.0")  - (-0.5)
  call error_check (c, fmpfr (1.5))

  c = (-0.5) - fmpfr("1.0")
 call error_check (c, fmpfr (-1.5))

  c = fmpfr("1.0")  * (-0.5)
  call error_check (c, fmpfr (-0.5))

  c = (-0.5) * fmpfr("1.0")
 call error_check (c, fmpfr (-0.5))

  c = fmpfr("1.0")  / (-0.5)
  call error_check (c, fmpfr (-2))

  c = (-0.5) / fmpfr("1.0")
 call error_check (c, fmpfr (-0.5))

  c = fmpfr("1.0")  + (0.5)
  call error_check (c, fmpfr (1.5))

  c = (0.5) + fmpfr("1.0")
 call error_check (c, fmpfr (1.5))

  c = fmpfr("1.0")  - (0.5)
  call error_check (c, fmpfr (0.5))

  c = (0.5) - fmpfr("1.0")
 call error_check (c, fmpfr (-0.5))

  c = fmpfr("1.0")  * (0.5)
  call error_check (c, fmpfr (0.5))

  c = (0.5) * fmpfr("1.0")
 call error_check (c, fmpfr (0.5))

  c = fmpfr("1.0")  / (0.5)
  call error_check (c, fmpfr (2))

  c = (0.5) / fmpfr("1.0")
 call error_check (c, fmpfr (0.5))

  c = fmpfr("1.0")  + (0.5)
  call error_check (c, fmpfr (1.5))

  c = (0.5) + fmpfr("1.0")
 call error_check (c, fmpfr (1.5))

  c = fmpfr("1.0")  - (0.5)
  call error_check (c, fmpfr (0.5))

  c = (0.5) - fmpfr("1.0")
 call error_check (c, fmpfr (-0.5))

  c = fmpfr("1.0")  * (0.5)
  call error_check (c, fmpfr (0.5))

  c = (0.5) * fmpfr("1.0")
 call error_check (c, fmpfr (0.5))

  c = fmpfr("1.0")  / (0.5)
  call error_check (c, fmpfr (2))

  c = (0.5) / fmpfr("1.0")
 call error_check (c, fmpfr (0.5))

  c = fmpfr("1.0")  + (1.0)
  call error_check (c, fmpfr (2))

  c = (1.0) + fmpfr("1.0")
 call error_check (c, fmpfr (2))

  c = fmpfr("1.0")  - (1.0)
  call error_check (c, fmpfr (0))

  c = (1.0) - fmpfr("1.0")
 call error_check (c, fmpfr (0))

  c = fmpfr("1.0")  * (1.0)
  call error_check (c, fmpfr (1))

  c = (1.0) * fmpfr("1.0")
 call error_check (c, fmpfr (1))

  c = fmpfr("1.0")  / (1.0)
  call error_check (c, fmpfr (1))

  c = (1.0) / fmpfr("1.0")
 call error_check (c, fmpfr (1))

  c = fmpfr("1.0")  + (1.0)
  call error_check (c, fmpfr (2))

  c = (1.0) + fmpfr("1.0")
 call error_check (c, fmpfr (2))

  c = fmpfr("1.0")  - (1.0)
  call error_check (c, fmpfr (0))

  c = (1.0) - fmpfr("1.0")
 call error_check (c, fmpfr (0))

  c = fmpfr("1.0")  * (1.0)
  call error_check (c, fmpfr (1))

  c = (1.0) * fmpfr("1.0")
 call error_check (c, fmpfr (1))

  c = fmpfr("1.0")  / (1.0)
  call error_check (c, fmpfr (1))

  c = (1.0) / fmpfr("1.0")
 call error_check (c, fmpfr (1))

contains
  subroutine error_check(a,b)
    type (fmpfr), intent(in) :: a, b
    if (a /= b) then
      print *,get_str(a), get_str(b)
      error stop
    end if
  end subroutine error_check
end program memain


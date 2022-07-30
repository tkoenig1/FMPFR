program memain
  use fmpfr_oper
  implicit none
  integer :: i
  character(len=80) :: line
  character(len=80), dimension(6) :: res
  type (fmpfr)  :: a
  call set_default_prec (113)
  res(1) = "1.42857142857143E+0"
  res(2) = "1.4285714285714E+0"
  res(3) = "1.428571428571E+0"
  res(4) = "1.42857142857E+0"
  res(5) = "1.4285714286E+0"
  res(6) = "1.428571429E+0"

  a = fmpfr("1.0") / fmpfr("0.7")
  do i=1,6
     line = get_str(a,16-i)
     if (line /= res(i)) then
        error stop
     end if
  end do
end program memain

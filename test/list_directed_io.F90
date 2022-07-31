program memain
#if USE_DERIVED_IO
  use fmpfr_oper
  type (fmpfr) :: c, d
  character (len=80) :: line
  open (10,status="scratch")
  c = fmpfr("1.0") / fmpfr("7.0")
  write (10,*) c
  rewind 10
  read (10,'(A)') line
  d = fmpfr(line)
  if (c /= d) stop 1
#endif
end program

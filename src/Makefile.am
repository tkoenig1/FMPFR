lib_LIBRARIES = libfmpfr.a
libfmpfr_a_SOURCES = fmpfr_kinds.f90 fmpfr_oper.F90 fmpfr_c.F90 fmpfr_glue.c
FC_PREPROCESSOR_FLAG = @fc_preprocessor_flag@ @fc_preflags@

fmpfr_kinds.mod fmpfr_kinds.o: fmpfr_kinds.f90
	$(FC) -c $(FCFLAGS) fmpfr_kinds.f90

fmpfr_c.mod fmpfr_c.o: fmpfr_c.F90 fmpfr_kinds.mod
	$(FC) $(FC_PREPROCESSOR_FLAG) $(FCFLAGS) -c fmpfr_c.F90

fmpfr_oper.mod fmpfr_oper.o: fmpfr_oper.F90 fmpfr_c.mod fmpfr_kinds.mod
	$(FC) $(FC_PREPROCESSOR_FLAG) $(FCFLAGS) -c fmpfr_oper.F90

clean:
	rm -f *.mod *~ *.o *~ libmpfr.a

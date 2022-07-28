TESTS = basic_arithmetic$(EXEEXT)

%: %.f90
	pwd
	$(FC) -I ../src -o $@ $< ../src/libfmpfr.a -lmpfr

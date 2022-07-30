TESTS = basic_arithmetic$(EXEEXT) list_directed_io$(EXEEXT) get_str$(EXEEXT)

FC_PREPROCESSOR_FLAG = @fc_preprocessor_flag@ @fc_preflags@

%: %.f90 ../src/libfmpfr.a
	$(FC) -g -I ../src -o $@ $< ../src/libfmpfr.a -lmpfr

%: %.F90 ../src/libfmpfr.a
	$(FC) $(FC_PREPROCESSOR_FLAG) -g -I ../src -o $@ $< ../src/libfmpfr.a -lmpfr

distclean: clean

clean:
	rm -f $(TESTS) *.mod *.log *.trs *~

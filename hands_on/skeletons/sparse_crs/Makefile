COMPILER=Intel
COMPILER=NAG
COMPILER=GCC


include ../../../make.inc

#FOPT+=$(DEBUG)


test_sparse.exe : mod_sparse.o test_sparse.o
	$(F90) $(FOPT) -o $@ $^

test_sparse_overload.exe : mod_sparse.o test_sparse_overload.o
	$(F90) $(FOPT) -o $@ $^

%.o : %.f90
	$(F90) $(FOPT) -c -o $@ $^

clean :
	rm -f *.exe *.o *~ *.mod log_*.txt *.pnm


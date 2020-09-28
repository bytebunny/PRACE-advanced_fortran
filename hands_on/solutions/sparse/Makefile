#COMPILER=Intel
#COMPILER=NAG
COMPILER=GCC

include ../../../make.inc

#FOPT+=$(DEBUG)
#FOPT+=$(OMP)

all : test_sparse_simple.exe  test_sparse.exe test_sparse_io.exe


test_sparse.exe : mod_sparse.o test_sparse.o
	$(F90) $(FOPT) -o $@ $^

test_sparse_simple.exe : mod_sparse_simple.o test_sparse_simple.o
	$(F90) $(FOPT) -o $@ $^

test_sparse_io.exe : mod_sparse_io.o test_sparse_io.o
	$(F90) $(FOPT) -o $@ $^

%.o : %.f90
	$(F90) $(FOPT) -c -o $@ $^

clean :
	rm -f *.exe *.o *~ *.mod log_*.txt *.ckp *.cfg *.dat *.prot *.pnm

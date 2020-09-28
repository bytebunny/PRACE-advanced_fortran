#COMPILER=NAG
#COMPILER=Intel
COMPILER=GCC
#COMPILER=IBM
#COMPILER=PGI

include ../../make.inc

test_union.exe : mod_union.f90 test_union.f90
	$(F90) $(FFLAGS) -o $@ $^

clean :
	rm -f *.mod *.exe *~ *.o

COMPILER=Intel
COMPILER=NAG
#COMPILER=GCC

include ../../../make.inc


triangular.exe : triangular.o
	$(F90) $(FOPT) -o $@ $^

%.o : %.f90
	$(F90) $(FOPT) -c -o $@ $^


clean :
	rm -f *.exe *.o *~ *.mod

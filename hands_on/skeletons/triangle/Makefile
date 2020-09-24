COMPILER=Intel
#COMPILER=NAG
#COMPILER=GCC

include ../../../make.inc


triangular.exe : triangular.o
	$(CAF) $(FOPT) -o $@ $^

%.o : %.f90
	$(CAF) $(FOPT) -c -o $@ $^


clean :
	rm -f *.exe *.o *~ *.mod

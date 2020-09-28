#COMPILER=Intel
#COMPILER=NAG
COMPILER=GCC

include ../../../make.inc


integration.exe : mod_integration.f90 prog_integration.f90
	$(F90) $(OPT) $(OMP) -o $@ $^


clean :
	rm -f *.exe *.o *~ *.mod

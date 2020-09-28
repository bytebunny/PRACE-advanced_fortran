COMPILER=Intel
COMPILER=NAG
COMPILER=GCC

include ../../../make.inc


raytracef.exe : raytracef.f90
	$(F90) $(FOPT) -o $@ $^
#	$(F90) $(FOPT) -threads -o $@ $^

clean :
	rm -f *.exe *.mod *~ *.inc *.pnm

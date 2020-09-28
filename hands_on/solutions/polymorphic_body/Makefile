COMPILER=Intel
COMPILER=NAG
COMPILER=GCC

include ../../../make.inc

tbody.exe : mod_body.f90 tbody.f90
	$(F90) $(FOPT) -o $@ $^

clean :
	rm -f *.exe *.mod *~ *.inc 

COMPILER=Intel
COMPILER=GCC
#COMPILER=NAG

include ../../../make.inc

VPATH=../timer

heat.exe : timer.o mod_heat.o heat.o
	$(F90) $(FOPT) -o $@ $^

mod_heat.o : mod_heat.f90
	$(F90) $(FOPT) -c -o $@ $^

%.o : %.f90
	$(F90) $(FOPT) -c -o $@ $^


clean :
	rm -f *.exe *.o *~ *.mod

COMPILER=Intel
COMPILER=GCC
#COMPILER=NAG

include ../../../make.inc

VPATH=../timer

heat_static.exe : timer.o mod_heat_static.o heat_static.o
	$(CAF) $(FOPT) -o $@ $^

mod_heat_static.o : mod_heat_static.f90
	$(CAF) $(FOPT) -c -o $@ $^

%.o : %.f90
	$(CAF) $(FOPT) -c -o $@ $^


clean :
	rm -f *.exe *.o *~ *.mod

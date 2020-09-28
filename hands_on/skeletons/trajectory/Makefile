COMPILER=Intel
COMPILER=NAG
COMPILER=GCC

VPATH=../polymorphic_body

include ../../../make.inc

all : test_trajectory.exe


test_trajectory.exe : mod_body.o mod_trajectory.o test_trajectory.o
	$(F90) -o $@ $^


%.o : %.f90
	$(F90) -c -o $@ $(DEBUG) $(FFLAGS) $^

clean :
	rm -f a.out *.o *.mod *~ *.exe *.dat

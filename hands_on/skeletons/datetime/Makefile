COMPILER=Intel
#COMPILER=NAG
COMPILER=GCC

include ../../../make.inc

all :: test_date.exe person.exe

test_date.exe : mod_date.f90 test_date.f90
        $(F90) -o $@ $(FFLAGS) $^
person.exe : mod_date.f90 person.f90
        $(F90) -o $@ $(FFLAGS) $^

clean :
	rm -f *.exe *.o *.mod *~

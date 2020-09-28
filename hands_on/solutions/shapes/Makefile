COMPILER=Intel
COMPILER=NAG
COMPILER=GCC

include ../../../make.inc

all : shapes_badstyle.exe shapes_improved.exe

shapes_badstyle.exe : shapes_badstyle.f90
	$(F90) $(FFLAGS) -o $@ $^
shapes_improved.exe : shapes_improved.f90
	$(F90) $(FFLAGS) -o $@ $^

clean :
	rm -f *.exe *.mod *~ *.inc 

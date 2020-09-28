COMPILER=Intel
COMPILER=NAG
COMPILER=GCC

include ../../../make.inc

all : triangular.exe triangular_full.exe triangular_collective.exe triangular_library.exe 

triangular.exe : triangular.o
	$(CAF) $(FOPT) -o $@ $^
triangular_full.exe : triangular_full.o
	$(CAF) $(FOPT) -o $@ $^
triangular_collective.exe : triangular_collective.o
	$(CAF) $(FOPT) -o $@ $^
triangular_library.exe : triangular_library.o
	$(CAF) $(FOPT) -o $@ $^

%.o : %.f90
	$(CAF) $(FOPT) -c -o $@ $^


clean :
	rm -f *.exe *.o *~ *.mod

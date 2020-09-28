COMPILER=Intel
COMPILER=GCC
#COMPILER=NAG
#COMPILER=PGI

include ../../make.inc


all : ptype.exe

ptype.exe : ptype.o 
	$(F90) -o $@ $^ 


%.o : %.f90
	$(F90) -c -o $@ $^

clean :
	rm -f *.o *~ *.mod *.exe *.dat

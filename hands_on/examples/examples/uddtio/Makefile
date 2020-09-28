COMPILER=Intel
COMPILER=GCC
COMPILER=NAG
#COMPILER=PGI

include ../../make.inc


all : write_list.exe

write_list.exe : mod_list_person.o write_list.o
	$(F90) -o $@ $^ 


%.o : %.f90
	$(F90) -c -o $@ $^

clean :
	rm -f *.o *~ *.mod *.exe *.dat

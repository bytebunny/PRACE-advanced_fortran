COMPILER=NAG
#COMPILER=Intel
COMPILER=GCC

include ../../../make.inc

OBJS=prog_client.o mod_handle.o mod_file_handle.o smod_handle.o
prog_client.exe : $(OBJS) 
	$(F90) -o $@ $^ 

smod_handle.o : smod_handle.f90 mod_handle.o
	$(F90) -c -o $@ smod_handle.f90
mod_file_handle.o : mod_file_handle.f90  mod_handle.o
	$(F90) -c -o $@ mod_file_handle.f90
prog_client.o : prog_client.f90 mod_handle.o 
	$(F90) -c -o $@ prog_client.f90

%.o : %.f90 
	$(F90) -c -o $@ $^


clean :
	rm -rf *~ *.o *.mod *.smod *.dat *.exe *.sub

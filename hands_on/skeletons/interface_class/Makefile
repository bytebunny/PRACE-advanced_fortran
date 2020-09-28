COMPILER=Intel
COMPILER=NAG
COMPILER=GCC

include ../../../make.inc

prog_client.exe : mod_handle.f90 mod_file_handle.f90 mod_obp.f90 prog_client.f90
	$(F90) -o $@ $^ 

clean :
	rm -rf *~ *.o *.mod *.dat *.exe

COMPILER=Intel
COMPILER=NAG
COMPILER=GCC

include ../../../make.inc

prog_passwd.exe : mod_passwd.f90 prog_passwd.f90
	$(F90)  -o $@ $^

clean :
	rm -f *.exe *.mod *~ *.inc

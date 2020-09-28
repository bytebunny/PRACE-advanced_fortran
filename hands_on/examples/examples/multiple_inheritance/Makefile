COMPILER=NAG

include ../../make.inc


all : test_admin_funds.exe test_override.exe


test_admin_funds.exe : mod_date.o mod_funds.o mod_admin_funds.o test_admin_funds.o
	$(F90) -o $@ $^

test_override.exe : mod_date.o mod_funds.o mod_admin_funds.o test_override.o
	$(F90) -o $@ $^



%.o : %.f90
	$(F90) -c -o $@ $(FFLAGS) $^


clean :
	rm -rf *.o *.mod *.exe *~ a.out

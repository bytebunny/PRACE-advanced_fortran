COMPILER=Intel
COMPILER=NAG
COMPILER=GCC
include ../../make.inc

all : prog_f.exe prog_c.exe

prog_f.exe : mod_dyn.o prog_f.o c_dyn.o
	$(F90) $(DEBUG) -o $@ $^

prog_c.exe :  mod_dyn.o prog_c.o c_dyn.o
	$(F90) $(DEBUG) $(LDCMAIN) -o $@ $^

%.o : %.c
	$(CC) $(CDBG) -c -o $@ $^

%.o : %.f90
	$(F90) $(DEBUG) -c -o $@ $^

clean :
	rm -rf *.o *~ *.mod *.exe

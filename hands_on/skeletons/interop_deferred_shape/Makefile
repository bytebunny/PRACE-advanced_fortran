COMPILER=Intel
COMPILER=NAG
COMPILER=GCC

include ../../../make.inc

ifeq ($(F90),ifort)
	CC=icc
endif

all : fmain.exe cmain.exe


fmain.exe : mod_deferred.f90 fmain.f90
	$(F90) -o $@ $^

cmain.exe : cmain.o  mod_deferred.f90
ifeq ($(F90),ifort)
	$(F90) -nofor-main -o $@ $^
else
	$(F90) -o $@ $(CMAIN_LD) $^
endif

mod_deferred.o: mod_deferred.f90
	$(F90) -c -o $@ $^

cmain.o : cmain.c
	$(CC) -c -o $@ $^

clean :
	rm -f *.exe *~ *.mod *.o

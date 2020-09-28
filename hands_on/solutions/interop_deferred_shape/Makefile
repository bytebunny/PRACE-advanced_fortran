COMPILER=Intel
COMPILER=NAG
COMPILER=GCC
#COMPILER=IBM

include ../../../make.inc

ifeq ($(F90),ifort)
	CC=icc
endif
ifeq ($(F90),xlf2003_r)
	CC=xlc_r -I/opt/ibm/xlf/15.1.5/include/
endif

all : fmain.exe cmain.exe cmain_f18.exe


fmain.exe : mod_deferred.f90 fmain.f90
	$(F90) -o $@ $^

cmain.exe : cmain.o  mod_deferred.f90
ifeq ($(F90),ifort)
	$(F90) -nofor-main -o $@ $^
else
	$(F90) -o $@ $(CMAIN_LD) $^
endif
cmain_f18.exe : cmain_f18.o  mod_deferred_f18.f90
ifeq ($(F90),ifort)
	$(F90) -nofor-main -o $@ $^
else
	$(F90) -o $@ $(CMAIN_LD) $^
endif

mod_deferred.o: mod_deferred.f90
	$(F90) -c -o $@ $^
mod_deferred_f18.o: mod_deferred_f18.f90
	$(F90) -c -o $@ $^

cmain.o : cmain.c
	$(CC) -c -o $@ $^

clean :
	rm -f *.exe *~ *.mod *.o

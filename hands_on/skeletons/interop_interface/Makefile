COMPILER=Intel
COMPILER=NAG
#COMPILER=GCC

include ../../../make.inc

all : cmain.exe fmain.exe

cmain.exe : cmain.o c_libcall.o
	gcc -o $@ $^

fmain.exe : fmain.o f_lib.o c_libcall.o
	$(F90) -o $@ $^

fmain.o : fmain.f90 f_lib.o
	$(F90) -c -o $@ $<

%.o : %.c 
	gcc -c -o $@ $^

%.o : %.f90
	$(F90) -c -o $@ $^

clean :
	rm -f *~ *.o *.mod *.exe

COMPILER=Intel
COMPILER=NAG
COMPILER=GCC

include ../../../make.inc

all : cmain.exe fmain.exe fmain_f18.exe

cmain.exe : cmain.o c_libcall.o
	$(CC) -o $@ $^

fmain.exe : fmain.o f_lib.o c_libcall.o
	$(F90) -o $@ $^
fmain_f18.exe : fmain_f18.o f_lib_f18.o c_libcall.o
	$(F90) -o $@ $^

fmain.o : fmain.f90 f_lib.o
	$(F90) -c -o $@ $<
fmain_f18.o : fmain_f18.f90 f_lib_f18.o
	$(F90) -c -o $@ $<

%.o : %.c 
	$(CC) -c -o $@ $^

%.o : %.f90
	$(F90) -c -o $@ $^

clean :
	rm -f *~ *.o *.mod *.exe

COMPILER=Intel
#COMPILER=GCC
#COMPILER=NAG

VPATH=../timer
include ../../../make.inc

all : raytrace_caf_compalloc.exe raytrace_caf_tilelist.exe raytrace_caf_events.exe

%.exe : %.f90 timer.o
	$(CAF) $(FOPT) -o $@ $^

%.o : %.f90
	$(CAF) $(FOPT) -c -o $@ $^


clean :
	rm -f *.exe *.o *~ *.mod *.pnm

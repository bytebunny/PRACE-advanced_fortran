COMPILER=Intel
COMPILER=NAG
COMPILER=GCC

include ../../../make.inc

#FOPT+=$(DEBUG)
#FOPT+=$(OMP)
GSL_LIB=-lgsl -lgslcblas


all : calc_erf.exe


calc_erf.exe : calc_erf.f90
	$(F90) $(FOPT) -o $@ $^ $(GSL_LIB)

%.o : %.f90
	$(F90) $(FOPT) -c -o $@ $^

clean :
	rm -f *.exe *.o *~ *.mod log_*.txt *.ckp *.cfg *.dat *.prot *.pnm

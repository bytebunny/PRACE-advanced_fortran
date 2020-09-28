COMPILER=Intel
COMPILER=GCC
include ../../../make.inc

GSL_LIB=-lgsl -lgslcblas

all : calc_erf.exe

calc_erf.exe : calc_erf.c
	$(CC) $(GSL_INC) -o $@ $^ $(GSL_LIB)

clean :
	rm -f *.exe *.o *~ *.mod log_*.txt *.ckp *.cfg *.dat *.prot *.pnm

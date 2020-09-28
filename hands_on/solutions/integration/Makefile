COMPILER=Intel
COMPILER=GCC
#COMPILER=PGI
#COMPILER=NAG

include ../../../make.inc

CC=gcc
# Linux Cluster
# please do a "module load gsl" before building
# Other systems:
GSL_LIB=-lgsl
GSL_BLAS_LIB=-lgslcblas

test_qdr.exe : fgsl.f90  qdr.f90  test_qdr.f90  util.o
	$(F90) -o $@ $^ $(GSL_LIB) $(GSL_BLAS_LIB)

util.o : util.c
	$(CC) -c $(GSL_INC) util.c

clean :
	rm -f *.o *.mod *.exe *~

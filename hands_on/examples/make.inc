ifndef COMPILER
COMPILER=GCC
endif

ifeq ($(COMPILER),Intel)
F90=ifort
CC=icc
FFLAGS=-standard-semantics
CAF=ifort -coarray
DEBUG=-g -traceback #-check all
FPP=-fpp
#FOPT=-O3 -xAVX       # for Sandy Bridge
#FOPT=-O3 -xCORE-AVX2 # for Haswell
#FOPT=-O3 -xSSE2
FOPT=-O3
F_IEEE=-assume fpe_summary -assume ieee_fpe_flags
# the above is implied by -standard-semantics
#F_IEEE=
SOPT=-shared -fpic
OMP=-qopenmp
endif

ifeq ($(COMPILER),GCC)
F90=gfortran
CC=gcc
FFLAGS=
CAF=caf
DEBUG=-g -fbacktrace
FPP=-cpp
#FOPT=-O3
#FOPT=-O3 -march=opteron -msse3 -funroll-loops 
FOPT=-O3 -march=corei7-avx -ftree-vectorize
SOPT=-shared -fpic
OMP=-fopenmp
endif

ifeq ($(COMPILER),NAG)
F90=nagfor
CC=gcc
FFLAGS=-f2018
CAF=nagfor -f2018 -coarray
DEBUG=-g -gline -mtrace -C=all
FPP=-fpp
FOPT=-O3
F_IEEE=-ieee=full
SOPT=-Wl,-shared -pic
OMP=-openmp
endif

ifeq ($(COMPILER),CRAY)
F90=ftn
CC=cc
FFLAGS=-e m
DEBUG=-g 
endif

ifeq ($(COMPILER),PGI)
F90=pgf90
CC=pgcc
FFLAGS=
DEBUG=-g 
FOPT=-O3 # -tp=shanghai-64
FPP=-Mpreprocess
F_IEEE=-Kieee
# NOTE: the above only impacts precision, not exception handling
SOPT=-shared -fpic
OMP=-openmp
endif

ifeq ($(COMPILER),IBM)
F90=xlf2003_r
CC=xlc_r
FFLAGS=
DEBUG=-g 
FOPT=-O4 -q64 -qhot=vector -qcache=auto -qalias=noaryovrlp
FPP=-qsuffix=cpp=f90
OMP=-mp
endif

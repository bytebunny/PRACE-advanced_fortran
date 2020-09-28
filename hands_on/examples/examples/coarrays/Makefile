COMPILER=Intel
COMPILER=GCC
#COMPILER=NAG

include ../../make.inc

VPATH += ../../timer
#MEM := 
#FL=$(DEBUG)
#FL=-heap-arrays


all : parallel_hello.exe
#all : serial_mv.exe caf_mv_0.exe caf_mv_1.exe caf_mv_2.exe caf_mv_3.exe caf_mv_4.exe caf_mv_5.exe caf_mv_6.exe


serial_mv.exe : timer.f90 serial_mv.f90
	$(F90) -o $@ $^ $(MKL_SHLIB)

caf_mv_0.exe : timer.o caf_mv_0.o
	$(CAFLD) -o $@ $(FL) $(MEM) $(FPP) -DEXPONENT=0 $^ $(MKL_SHLIB) 
caf_mv_1.exe : timer.o caf_mv_1.o
	$(CAFLD) -o $@ $(FL) $(MEM) $(FPP) -DEXPONENT=1 $^ $(MKL_SHLIB)
caf_mv_2.exe : timer.o caf_mv_2.o
	$(CAFLD) -o $@ $(FL) $(MEM) $(FPP) -DEXPONENT=2 $^ $(MKL_SHLIB)
caf_mv_3.exe : timer.o caf_mv_3.o
	$(CAFLD) -o $@ $(FL) $(MEM) $(FPP) -DEXPONENT=3 $^ $(MKL_SHLIB)
caf_mv_4.exe : timer.o caf_mv_4.o
	$(CAFLD) -o $@ $(FL) $(MEM) $(FPP) -DEXPONENT=4 $^ $(MKL_SHLIB)
caf_mv_5.exe : timer.o caf_mv_5.o
	$(CAFLD) -o $@ $(FL) $(MEM) $(FPP) -DEXPONENT=5 $^ $(MKL_SHLIB)
caf_mv_6.exe : timer.o caf_mv_6.o
	$(CAFLD) -o $@ $(FL) $(MEM) $(FPP) -DEXPONENT=6 $^ $(MKL_SHLIB)


caf_mv_0.o : caf_mv.f90
	$(CAF) -c -o $@ $(FL) $(MEM) $(FPP) $(FFLAGS) -DEXPONENT=0 $^
caf_mv_1.o : caf_mv.f90
	$(CAF) -c -o $@ $(FL) $(MEM) $(FPP) $(FFLAGS) -DEXPONENT=1 $^
caf_mv_2.o : caf_mv.f90
	$(CAF) -c -o $@ $(FL) $(MEM) $(FPP) $(FFLAGS) -DEXPONENT=2 $^
caf_mv_3.o : caf_mv.f90
	$(CAF) -c -o $@ $(FL) $(MEM) $(FPP) $(FFLAGS) -DEXPONENT=3 $^
caf_mv_4.o : caf_mv.f90
	$(CAF) -c -o $@ $(FL) $(MEM) $(FPP) $(FFLAGS) -DEXPONENT=4 $^
caf_mv_5.o : caf_mv.f90
	$(CAF) -c -o $@ $(FL) $(MEM) $(FPP) $(FFLAGS) -DEXPONENT=5 $^
caf_mv_6.o : caf_mv.f90
	$(CAF) -c -o $@ $(FL) $(MEM) $(FPP) $(FFLAGS) -DEXPONENT=6 $^

%.exe : %.f90
	$(CAF) -o $@ $^

timer.o : timer.f90
	$(F90) -c -o $@ $^


clean :
	rm -rf *.o *.mod *.exe *~ a.out

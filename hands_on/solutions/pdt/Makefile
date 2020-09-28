F90=nagfor -g

default: pdt_public.exe pdt_private.exe

pdt_public.exe : pdt_public.f90
	$(F90) $(FOPT) -o $@ $^

pdt_private.exe : pdt_private.f90
	$(F90) $(FOPT) -o $@ $^

clean :
	rm -f *.exe *.mod *~ *.inc 

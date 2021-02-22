# Start of the makefile
# Defining variables
objects = mass.o main.o counting.o molarmass.o maxmin.o get_bonds.o
f90comp = gfortran
OPT= -O3
# Makefile
programa.x: $(objects)
	$(f90comp) -o programa.x $(OPT) $(objects)
mass.mod: mass.f90
	$(f90comp) -c $(OPT) mass.f90
main.o: mass.mod main.f90
	$(f90comp) -c $(OPT) main.f90
counting.o: main.o mass.mod molarmass.o
	$(f90comp) -c $(OPT) counting.f90
molarmass.o: mass.mod molarmass.f90
	$(f90comp) -c $(OPT) molarmass.f90
maxmin.o: counting.o maxmin.f90
	$(f90comp) -c $(OPT) maxmin.f90
get_bonds.o: counting.o get_bonds.f90
	$(f90comp) -c $(OPT) get_bonds.f90
# Cleaning everything
clean:
	rm -f constants.mod programa.x
	rm -f $(objects)
# End of the makefile


.SUFFIXES: .f .o

# Select a default Fortran compiler.
ifndef fortran
  fortran=gfortran
endif

ifeq ($(fortran),gfortran)
  FTN = gfortran -g -fno-automatic -fno-second-underscore
else
ifeq ($(fortran),g77)
  FTN = g77 -g -fno-automatic -finit-local-zero -fno-second-underscore
endif
endif

.f.o:
	$(FTN) -c -o $*.o $*.f  

SRC = MAIN.f AVOL.f GEOSHELL.f MAN2.f PDCEX.f BODY.f PDCYLM.f VEHCON.f WING2.f   

#INC = VEHICON.INC LAND.INC STRTCM.INC  BODY.f PDCYLM.f VEHCON.f WING2.f   


OBJ = $(SRC:.f=.o)

default: pdcyl    

# Create Python wrapper.
f2py: $(SRC)
	f2py -c -m pdcylm  --f77flags=' -g -fno-automatic' $(SRC) 

# Build standalone executable.
pdcyl: $(OBJ)
	$(FTN) -o $@ $(OBJ)

# Check against coding standards.
ftnchek:
	ftnchek $(SRC) | \
	sed -e '/0 syntax errors detected/d' >ftnchek.out

# Remove all intermediate build files.
clean:
	/bin/rm -f *.o ftnchek.out

# Remove all build files.
clobber: clean
	/bin/rm -f pdcyl


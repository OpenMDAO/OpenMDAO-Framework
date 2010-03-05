
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

SRC = axod.f axodm.f besfit.f cpa.f cpf.f cpw.f diagt.f etacf.f etapr.f \
	fit1.f fit2.f flow1.f flow2.f gama.f icommon.f init.f input.f inst2.f \
	instg.f loop.f loss1.f loss2.f ovrall.f phim.f ploter.f pratio.f r.f \
	sta01.f sta1.f sta1a.f sta2.f sta2a.f getdata.f tempdata.f

OBJ = $(SRC:.f=.o)

default: axod

# Create Python wrapper.
f2py: $(SRC)
	f2py -c -m axod --f77flags=' -g -fno-automatic' $(SRC)

# Build standalone executable.
axod: $(OBJ)
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
	/bin/rm -f axod


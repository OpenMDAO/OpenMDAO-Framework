# Makefile for pdcyl

.SUFFIXES:	.o .f

# use these flags when debuging
#pdcyl_fflags= -c -g -finit-local-zero
 pdcyl_fflags= -c -g -fno-automatic     
# use these flags otherwise
#pdcyl_fflags= -c -finit-local-zero

pdcyl_root=.

pdcyl_source= \
	$(pdcyl_root)/AVOL.f \
	$(pdcyl_root)/BODY.f \
	$(pdcyl_root)/GEOSHELL.f \
	$(pdcyl_root)/PDCYLM.f \
	$(pdcyl_root)/MAN2.f \
	$(pdcyl_root)/PDCEX.f \
	$(pdcyl_root)/VEHCON.f \
	$(pdcyl_root)/WING2.f 

pdcyl_object=$(pdcyl_source:.f=.o)

# Create Python wrapper.
f2py: $(pdcyl_source)
        f2py -c -m pcdylm --f77flags=' -g -fno-automatic' $(pdcyl_source)

all: pdcyl

pdcyl: msg $(pdcyl_object)
	@echo  "          Linking code..."
	@gfortran $(pdcyl_object) -o pdcyl.exe
	@echo  "   Compilation and linking completed successfully."
	@echo  "          Type pdcyl.exe' to execute."

msg:
	@echo  "          Compiling code..."
.f.o:
	gfortran $(pdcyl_fflags) $< -o $*.o

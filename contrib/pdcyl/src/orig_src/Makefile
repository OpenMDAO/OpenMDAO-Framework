# Makefile for pdcyl

.SUFFIXES:	.o .f

# use these flags when debuging
 pdcyl_fflags= -c -g -finit-local-zero
# use these flags otherwise
#pdcyl_fflags= -c -finit-local-zero

pdcyl_root=.

pdcyl_source= \
	$(pdcyl_root)/avol.f \
	$(pdcyl_root)/body.f \
	$(pdcyl_root)/geoshell.f \
	$(pdcyl_root)/main.f \
	$(pdcyl_root)/man2.f \
	$(pdcyl_root)/pdcex.f \
	$(pdcyl_root)/vehcon.f \
	$(pdcyl_root)/wing2.f 

pdcyl_object=$(pdcyl_source:.f=.o)

all: pdcyl

pdcyl: msg $(pdcyl_object)
	@echo  "          Linking code..."
	@g77 $(pdcyl_object) -o pdcyl.exe
	@echo  "   Compilation and linking completed successfully."
	@echo  "          Type pdcyl.exe' to execute."

msg:
	@echo  "          Compiling code..."
.f.o:
	g77 $(pdcyl_fflags) $< -o $*.o

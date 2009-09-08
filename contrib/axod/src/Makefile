
.SUFFIXES: .f .o

ifdef NPSS_CONFIG

EXE   = axod.$(NPSS_CONFIG)
ODIR  = O.$(NPSS_CONFIG)
VPATH = $(ODIR)

ifeq ($(NPSS_CONFIG),hp)
FTN = f90 +ppu -K +DAportable -g
endif

ifeq ($(NPSS_CONFIG),linux)
FTN = g77 -fno-automatic -finit-local-zero -fno-second-underscore -g
endif

ifeq ($(NPSS_CONFIG),sgi)
FTN = gfortran -static -O -g3 -trapuv
endif

ifeq ($(NPSS_CONFIG),sun)
FTN = f90 -ftrap=%none -g # -O
endif

%.o: %.f
	$(FTN) -c -o $(ODIR)/$*.o $*.f

else

EXE  = axod
ODIR = .
FTN  = gfortran -g -fno-automatic

.f.o:
	$(FTN) -c -o $*.o $*.f

endif

SRC = axod.f axodm.f besfit.f cpa.f cpf.f cpw.f diagt.f etacf.f etapr.f \
	fit1.f fit2.f flow1.f flow2.f gama.f icommon.f init.f input.f inst2.f \
	instg.f loop.f loss1.f loss2.f ovrall.f phim.f ploter.f pratio.f r.f \
	sta01.f sta1.f sta1a.f sta2.f sta2a.f

OBJ = $(SRC:.f=.o)

default: $(ODIR) $(EXE)

f2py: $(SRC)
	f2py -c -m axodm --f77flags=' -g -fno-automatic' $(SRC)


$(EXE): $(OBJ)
	$(FTN) -o $@ $(patsubst %,$(ODIR)/%,$(OBJ))

$(ODIR):
	mkdir $(ODIR)

ftnchek:
	ftnchek $(SRC) | \
	sed -e '/0 syntax errors detected/d' >ftnchek.out

# Remove all intermediate build files.
clean:
	/bin/rm -f $(ODIR)/*.o ftnchek.out

# Remove all build files.
clobber: clean
	/bin/rm -f $(EXE)


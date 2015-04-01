.SUFFIXES:

.SUFFIXES : .o .ftn90 .ftn .cdk .cdk90 .o

SHELL = /bin/sh

COMPILE = compile

FFLAGS =

CFLAGS =

#OPTIMIZ = -O 2
#OPTIMIZ = -O 1
OPTIMIZ = -O 0
#OPTIMIZ = -O 0 -debug

LIBRMN = rmn_013
TEMPLIB = ../$(EC_ARCH)/lib/libezinterpv.a

default: test

.ftn90.o:
	r.compile -includes ../$(EC_ARCH)/include -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" \
	-src $<

.cdk90.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" \
	-src $<

.ftn.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

#.f.o:
#	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

TEST_OBJECTS=\
	VertInterpConstants.o  VerticalGrid.o  VerticalInterpolation.o ViIfc.o TestVI.o

INTERPV_OBJECTS=\
	Interp1D_Constants.o Extrap1D_LapseRate.o Extrap1D_Fixed.o Extrap1D_Surface.o \
	Extrap1D_SurfaceWind.o Interp1D_CubicLagrange.o Interp1D_CubicWithDerivs.o \
	Interp1D_FindPos.o Interp1D_Linear.o Interp1D_NearestNeighbour.o

SPEED_OBJECTS=\
	VertInterpConstants.o  VerticalGrid.o  VerticalInterpolation.o SpeedConvertP.o

copyHlocal:
	cp ../*.h .

removeH:
	rm `cd ..; ls *.h`

testWithInterpv: copyHlocal TestVI.o removeH
	cp ../*.o .
	cp ../../interpv/*.o .
	r.build -o TestVInterp_$(BASE_ARCH).Abs -obj $(INTERPV_OBJECTS) $(TEST_OBJECTS) -arch $(ARCH) \
	-abi$(ABI) -librmn $(LIBRMN)

test:	copyHlocal TestVI.o removeH
	cp ../*.o .
	r.build -o TestVInterp_$(BASE_ARCH).Abs -obj $(TEST_OBJECTS) -arch $(ARCH) \
	-abi$(ABI) -librmn $(LIBRMN)

testlib: TestVI.o
	r.build -includes ../$(EC_ARCH)/include -libpath . ../$(EC_ARCH)/lib -libappl ezinterpv descrip -o TestVInterp_$(BASE_ARCH).Abs -obj TestVI.o -arch $(ARCH) \
	-abi$(ABI) -librmn $(LIBRMN)

copielocale:
	cp ../*.o . 
	cp ../*.mod .

SpeedConvert: copielocale SpeedConvertP.o
	r.build -o SpeedConvertP_$(BASE_ARCH).Abs -obj $(SPEED_OBJECTS) -arch $(ARCH) \
	-abi$(ABI) -librmn $(LIBRMN)

clean:
#Faire le grand menage. On enleve tous les fichiers sources\ninutiles et les .o 
	-if [ "*.ftn" != "`echo *.ftn`" ] ; \
	then \
	for i in *.ftn ; \
	do \
	fn=`r.basename $$i '.ftn'`; \
	rm -f $$fn.f; \
	done \
	fi
	rm -f *.o *.f90 *.mod *.stb  *.Abs
	rm *.h

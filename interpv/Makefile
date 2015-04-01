#!!mak Interpv - makefile for the interp1D package
#!
#!AUTHOR
#!     J.W. Blezius JULY 2002
#!
#!REVISION
#! v1_0    Blezius J.W.          - new code
#! v1_2    Blezius J.W. OCT 2002 - add missing dependencies
#! v1_3    Blezius J.W. OCT 2003 - add Extrap1D_Surface
#!
#!OBJECT
#!        To compile the components of the 1-D interpolation package, for
#!        inclusion in a library
#!
#!!
include /usr/local/env/armnlib/include/$(ARCH)$(ABI)/Makefile_addons

.SUFFIXES:

.SUFFIXES : .o .ftn90 .cdk

SHELL = /bin/sh

RMNLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn.a

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2
#OPTIMIZ = -O 0 -debug

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmnbeta.a

LIB_X = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn_x.a

UPDATEX =

.PRECIOUS: $(RMNLIB) $(MALIB)

default: obj

.ftn90.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.ftn.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

Interp1D_Arguments_X.cdk : Interp1D_Arguments.cdk
Interp1D_Constants.o : Interp1D_Constants.ftn90 Interp1D_Arguments_X.cdk
Extrap1D_LapseRate.o : Interp1D_Constants.o Interp1D_Arguments_X.cdk Extrap1D_LapseRate.ftn90 \
	Extrap1D_LapseRate_Body.ftn90 Interp1D_PreX_Shell.ftn90
Extrap1D_Surface.o : Interp1D_Constants.o Interp1D_Arguments_X.cdk Extrap1D_Surface.ftn90 \
	Extrap1D_Surface_Body.ftn90
Extrap1D_SurfaceWind.o : Interp1D_Constants.o Interp1D_Arguments_X.cdk Extrap1D_SurfaceWind.ftn90 \
	Extrap1D_SurfaceWind_Body.ftn90
Interp1D_CubicLagrange.o : Interp1D_Constants.o Interp1D_Arguments_X.cdk Interp1D_CubicLagrange.ftn90 \
	Interp1D_CubicLagrange_Body.ftn90 Interp1D_PreX_Shell.ftn90
Interp1D_CubicWithDerivs.o : Interp1D_Constants.o Interp1D_Arguments_X.cdk Interp1D_CubicWithDerivs.ftn90 \
	Interp1D_CubicWithDerivs_Body.ftn90 Interp1D_PreX_Shell.ftn90
Interp1D_FindPos.o : Interp1D_Constants.o Interp1D_FindPos.ftn90 Interp1D_FindPos_Body.ftn90
Interp1D_Linear.o : Interp1D_Constants.o Interp1D_Arguments_X.cdk Interp1D_Linear.ftn90 Interp1D_Linear_Body.ftn90 \
	Interp1D_PreX_Shell.ftn90
Interp1D_NearestNeighbour.o : Interp1D_Constants.o Interp1D_Arguments_X.cdk Interp1D_NearestNeighbour.ftn90 \
	Interp1D_NearestNeighbour_Body.ftn90 Interp1D_PreX_Shell.ftn90

OBJECTS= \
        Interp1D_Constants.o Extrap1D_LapseRate.o Extrap1D_Surface.o Extrap1D_SurfaceWind.o\
        Interp1D_CubicLagrange.o Interp1D_CubicWithDerivs.o Interp1D_FindPos.o \
        Interp1D_Linear.o Interp1D_NearestNeighbour.o

FICHIERS = *.ftn90 *.cdk
        
SpeedFindPos:	SpeedFindPos.o $(OBJECTS)
	r.build -o SpeedFindPos_$(ARCH).Abs -obj SpeedFindPos.o $(OBJECTS) \
	-arch $(ARCH) -abi$(ABI) -librmn rmn_005

SpeedInterp:	SpeedInterp.o $(OBJECTS)
	r.build -o SpeedInterp_$(ARCH).Abs -obj SpeedInterp.o $(OBJECTS) -arch $(ARCH) \
	-abi$(ABI) -librmn rmn_005

SpeedExtrap:	SpeedExtrap.o $(OBJECTS)
	r.build -o SpeedExtrap_$(ARCH).Abs -obj SpeedExtrap.o $(OBJECTS) -arch $(ARCH) \
	-abi$(ABI) -librmn rmn_005

Test_Interp1D:	Test_Interp1D.o $(OBJECTS)
	r.build -o Test_Interp1D_$(ARCH).Abs -obj Test_Interp1D.o $(OBJECTS) -arch $(ARCH) \
	-abi$(ABI) -librmn rmn_005

tarball:  $(FICHIERS) Makefile
	tar cfzv /data/armnraid1/www/ssm/interpv_1.0_all.tgz $(FICHIERS) Makefile 

obj: $(OBJECTS)
#Produire les fichiers objets (.o) pour tous les fichiers

genlib: $(OBJECTS)
#Creer ou mettre a jour la programmatheque 
	r.ar -arch $(ARCH) rcv $(MYLIB) $(OBJECTS)

updlib: 
#mettre a jour la programmatheque 
	r.ar -arch $(ARCH) rcv $(MYLIB) *.o
	if [ "$(UPDATEX)" = "1" ] ; \
	then \
	r.ar -arch $(ARCH) rcv $(LIB_X) *.o ; \
	fi

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

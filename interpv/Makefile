#!!mak Interpv - makefile for the interp1D package
#!
#!AUTHOR
#!     J.W. Blezius JULY 2002
#!
#!REVISION
#! v1_0    Blezius J.W.          - new code
#! v1_2    Blezius J.W. OCT 2002 - add missing dependencies
#! v1_3    Blezius J.W. OCT 2003 - add Extrap1D_Surface
#!         Blezius J.W. MAY 2014 - add Extrap1D_Abort
#!
#!OBJECT
#!        To compile the components of the 1-D interpolation package, for
#!        inclusion in a library
#!
#!!

.SUFFIXES:

.SUFFIXES : .o .ftn90 .cdk .a

SHELL = /bin/sh

RMNLIB_BASE_PATH=$(ARMNLIB)

RMNLIB = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmn.a

TEMPLIB = ./$(EC_ARCH)/lib_local.a

FFLAGS =

CFLAGS =

SUPP_OPT = -openmp

OPTIMIZ = 2
#OPTIMIZ = 0 -debug

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmnbeta.a

LIB_X = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmn_x.a

UPDATEX =

TARRLS = beta

.PRECIOUS: $(RMNLIB) $(MALIB)

default: obj

include $(RPN_TEMPLATE_LIBS)/include/makefile_suffix_rules.inc

Interp1D_Arguments_X.cdk : Interp1D_Arguments.cdk
Interp1D_Constants.o : Interp1D_Constants.ftn90 Interp1D_Arguments_X.cdk
Extrap1D_LapseRate.o : Interp1D_Constants.o Interp1D_Arguments_X.cdk Extrap1D_LapseRate.ftn90 \
	Extrap1D_LapseRate_Body.ftn90 Interp1D_PreX_Shell.ftn90
Extrap1D_Fixed.o : Interp1D_Constants.o Interp1D_Arguments_X.cdk Extrap1D_Fixed.ftn90 \
	Extrap1D_Fixed_Body.ftn90 Interp1D_PreX_Shell.ftn90
Extrap1D_Abort.o : Interp1D_Constants.o Interp1D_Arguments_X.cdk Extrap1D_Abort.ftn90 \
	Extrap1D_Abort_Body.ftn90 Interp1D_PreX_Shell.ftn90
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
        Interp1D_Constants.o Extrap1D_LapseRate.o Extrap1D_Fixed.o Extrap1D_Abort.o Extrap1D_Surface.o \
        Extrap1D_SurfaceWind.o Interp1D_CubicLagrange.o Interp1D_CubicWithDerivs.o \
        Interp1D_FindPos.o Interp1D_Linear.o Interp1D_NearestNeighbour.o

FICHIERS = *.ftn90 *.cdk
        
SpeedFindPos:	SpeedFindPos.o $(OBJECTS)
	s.compile -o SpeedFindPos_$(COMP_ARCH).Abs -obj SpeedFindPos.o $(OBJECTS) \
	-abi$(ABI) -librmn rmn_005

SpeedInterp:	SpeedInterp.o $(OBJECTS)
	s.compile -o SpeedInterp_$(COMP_ARCH).Abs -obj SpeedInterp.o $(OBJECTS) \
	-abi$(ABI) -librmn rmn_005

SpeedExtrap:	SpeedExtrap.o $(OBJECTS)
	s.compile -o SpeedExtrap_$(COMP_ARCH).Abs -obj SpeedExtrap.o $(OBJECTS) \
	-abi$(ABI) -librmn rmn_005

Test_Interp1D:	Test_Interp1D.o $(OBJECTS)
	(. s.ssmuse.dot /ssm/net/rpn/libs/4.0; \
	s.compile -o Test_Interp1D_$(COMP_ARCH).Abs -obj Test_Interp1D.o $(OBJECTS) \
	-abi$(ABI) -librmn rmn_014)

tarball:  $(FICHIERS) Makefile
	tar cfzv /data/armnraid1/www/ssm/sources/interpv_$(TARRLS)_all.tgz $(FICHIERS) Makefile 

obj: $(OBJECTS)
#Produire les fichiers objets (.o) pour tous les fichiers

gen_ec_arch_dir:
#Creer le repertoire $EC_ARCH 
	mkdir -p ./$(EC_ARCH)

locallib: gen_ec_arch_dir \
        $(TEMPLIB)(Interp1D_Constants.o) $(TEMPLIB)(Extrap1D_LapseRate.o) \
        $(TEMPLIB)(Extrap1D_Fixed.o) $(TEMPLIB)(Extrap1D_Abort.o) \
        $(TEMPLIB)(Extrap1D_Surface.o) $(TEMPLIB)(Extrap1D_SurfaceWind.o) \
        $(TEMPLIB)(Interp1D_CubicLagrange.o) $(TEMPLIB)(Interp1D_CubicWithDerivs.o) $(TEMPLIB)(Interp1D_FindPos.o) \
        $(TEMPLIB)(Interp1D_Linear.o) $(TEMPLIB)(Interp1D_NearestNeighbour.o)

genlib: $(OBJECTS)
#Creer ou mettre a jour la programmatheque 
	r.ar rcv $(MYLIB) $(OBJECTS)

updlib: 
#mettre a jour la programmatheque 
	r.ar rcv $(MYLIB) *.o
	if [ "$(UPDATEX)" = "1" ] ; \
	then \
	r.ar rcv $(LIB_X) *.o ; \
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
	rm -f *.o *.f90 *.mod *.MOD *.stb  *.Abs

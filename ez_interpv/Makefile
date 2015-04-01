.SUFFIXES:

.SUFFIXES : .o .ftn90 .cdk .cdk90 .c .a

SHELL = /bin/sh

COMPILE = compile

FFLAGS =

FTN90_SUPP_OPT = -includes /users/dor/armn/rmt/data/public/vgrid_descriptors/1.0.9/$(EC_ARCH)/xlf10/mod

CFLAGS =

SUPP_OPT = -openmp

OPTIMIZ = 2
#OPTIMIZ =  1
#OPTIMIZ =  0
#OPTIMIZ =  0 -debug

RMNLIB_BASE_PATH=$(ARMNLIB)

MYLIB = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmnbeta.a

LIB_X = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmn_x.a

TEMPLIB = ./$(EC_ARCH)/lib_local.a

UPDATEX =

TARRLS = beta

.PRECIOUS: $(RMNLIB) $(MALIB)

default: obj

include $(ARMNLIB)/include/makefile_suffix_rules.inc

VertInterpConstants.o: VertInterpConstants.cdk90 ViConstants_f90.h

ViIfc.o: ViIfc.ftn90 ViIfc_Body.ftn90 VertInterpConstants.o VerticalGrid.o VerticalInterpolation.o

VerticalGrid.o: VerticalGrid.cdk90 VerticalGrid_Body.cdk90 VertInterpConstants.o

VerticalInterpolation.o: VerticalInterpolation.ftn90 VerticalInterpolation_Body.ftn90 \
	VertInterpConstants.o VerticalGrid.o


OBJECTS= \
	VertInterpConstants.o VerticalGrid.o VerticalInterpolation.o ViIfc.o

tarball:  *.ftn90 *.cdk90 *.h  Makefile
	tar cfzv /data/armnraid1/www/ssm/sources/ez_interpv_$(TARRLS)_all.tgz *.ftn90 *.cdk90 *.h Makefile 

obj:	$(OBJECTS)

gen_ec_arch_dir:
#Creer le repertoire $EC_ARCH 
	mkdir -p ./$(EC_ARCH)

locallib: gen_ec_arch_dir \
        $(TEMPLIB)(VertInterpConstants.o)   $(TEMPLIB)(VerticalGrid.o) \
        $(TEMPLIB)(VerticalInterpolation.o)  $(TEMPLIB)(ViIfc.o)

genlib: $(OBJECTS)
#Creer ou mettre a jour la programmatheque 
	r.ar -arch $(EC_ARCH) rcv $(MYLIB) $(OBJECTS)

updlib: 
#mettre a jour la programmatheque 
	r.ar -arch $(EC_ARCH) rcv $(MYLIB) *.o
	if [ "$(UPDATEX)" = "1" ] ; \
	then \
	r.ar -arch $(EC_ARCH) rcv $(LIB_X) *.o ; \
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
	rm -f *.o *.f90 *.mod *.stb  VInterp_*.Abs

.SUFFIXES:

.SUFFIXES : .o .ftn90 .cdk .cdk90 .c .a

SHELL = /bin/sh

COMPILE = compile

FFLAGS =

CFLAGS =

OPTIMIZ = 2
#OPTIMIZ =  1
#OPTIMIZ =  0
#OPTIMIZ =  0 -debug

RMNLIB_BASE_PATH=$(ARMNLIB)

MYLIB = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmnbeta.a

LIB_X = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmn_x.a

TEMPLIB = ./templib_$(EC_ARCH)$(ABI).a

UPDATEX =

.PRECIOUS: $(RMNLIB) $(MALIB)

default: obj

include $(ARMNLIB)/include/makefile_suffix_rules.inc

vitypes.o: CreateViTypesMod.ftn90 ViTypes2_f90.h  ViTypes_f90.h
	r.compile -arch $(EC_ARCH) -abi $(ABI) -O $(OPTIMIZ) -opt "=$(FFLAGS)" -src CreateViTypesMod.ftn90
	mv CreateViTypesMod.o vitypes.o

#ViIfc.o: VertInterpConstants.cdk90 vitypes.o ViIfc.ftn90 VerticalInterpolation.ftn90 VerticalGrid.cdk90
ViIfc.o: ViIfc.ftn90 vitypes.o VertInterpConstants.o VerticalGrid.o VerticalInterpolation.o

VerticalGrid.o: VerticalGrid.cdk90 VertInterpConstants.cdk90

VerticalInterpolation.o: VerticalInterpolation.ftn90 VertInterpConstants.cdk90 \
	VerticalGrid.cdk90


OBJECTS= \
	VertInterpConstants.o VerticalGrid.o VerticalInterpolation.o ViIfc.o

tarball:  *.ftn90 *.cdk90 *.h  Makefile
	tar cfzv /data/armnraid1/www/ssm/sources/ez_interpv_1.0_all.tgz *.ftn90 *.cdk90 *.h Makefile 

obj:	$(OBJECTS)
	rm -f vitypes.o

locallib: vitypes.o \
        $(TEMPLIB)(VertInterpConstants.o)   $(TEMPLIB)(VerticalGrid.o) \
        $(TEMPLIB)(VerticalInterpolation.o)  $(TEMPLIB)(ViIfc.o)
	rm -f vitypes.o

genlib: $(OBJECTS)
#Creer ou mettre a jour la programmatheque 
	rm -f vitypes.o
	r.ar -arch $(EC_ARCH) rcv $(MYLIB) $(OBJECTS)

updlib: 
#mettre a jour la programmatheque 
	rm -f vitypes.o
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

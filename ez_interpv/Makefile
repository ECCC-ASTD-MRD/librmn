.SUFFIXES:

.SUFFIXES : .o .ftn90 .cdk .cdk90 .c

SHELL = /bin/sh

COMPILE = compile

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2
#OPTIMIZ = -O 1
#OPTIMIZ = -O 0
#OPTIMIZ = -O 0 -debug

MYLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmnbeta.a

LIB_X = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn_x.a

UPDATEX =

.PRECIOUS: $(RMNLIB) $(MALIB)

default: obj

.ftn90.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.cdk90.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.ftn.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

#.f.o:
#	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<



vitypes.mod: CreateViTypesMod.ftn90 ViTypes2_f90.h  ViTypes_f90.h
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src CreateViTypesMod.ftn90
	rm CreateViTypesMod.o

ViIfc.o: VertInterpConstants.cdk90 vitypes.mod ViIfc.ftn90 VerticalInterpolation.ftn90 VerticalGrid.cdk90

VerticalGrid.o: VerticalGrid.cdk90 VertInterpConstants.cdk90

VerticalInterpolation.o: VerticalInterpolation.ftn90 VertInterpConstants.cdk90 \
	VerticalGrid.cdk90


OBJECTS= \
	VertInterpConstants.o VerticalGrid.o VerticalInterpolation.o ViIfc.o

tarball:  *.ftn90 *.cdk90 *.h  Makefile
	tar cfzv /data/armnraid1/www/ssm/ez_interpv_1.0_all.tgz *.ftn90 *.cdk90 *.h Makefile 

obj:	$(OBJECTS)

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
	rm -f *.o *.f90 *.mod *.stb  VInterp_*.Abs

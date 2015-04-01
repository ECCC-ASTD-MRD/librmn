include /usr/local/env/armnlib/include/$(ARCH)$(ABI)/Makefile_addons

.SUFFIXES :

.SUFFIXES : .ftn .f .c .o

SHELL = /bin/sh

CPP = /lib/cpp

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmnbeta.a

LIB_X = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn_x.a

UPDATEX =

.PRECIOUS: $(MYLIB)

default: obj

.ftn.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.ftn.f:
	rm -f $*.f
	r.ftntof -P $(DEFINE) $< > $*.f
	chmod 444 $*.f

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

.f.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

FTNDECKS= \
         alpas2.ftn       alpas8.ftn       alpdl2.ftn       alpdl8.ftn \
         alpdr2.ftn       alpdr8.ftn       alpnm2.ftn       alpnm8.ftn \
         dimcal.ftn       epsil2.ftn       epsil8.ftn       fasp8.ftn \
         gauss.ftn        gauss8.ftn       ggasp0.ftn \
         ggasp8.ftn       gwaqd0.ftn       gwaqd8.ftn       ordleg8.ftn \
         perm.ftn         qqqfasp.ftn      scof2.ftn        spagg0.ftn \
         spagg8.ftn       fasp2.ftn        ewfdr2.ftn       spaf2.ftn \
         spaf8.ftn

OBJECTS= \
         alpas2.o       alpas8.o       alpdl2.o       alpdl8.o \
         alpdr2.o       alpdr8.o       alpnm2.o       alpnm8.o \
         dimcal.o       epsil2.o       epsil8.o       fasp8.o \
         gauss.o        gauss8.o       ggasp0.o \
         ggasp8.o       gwaqd0.o       gwaqd8.o       ordleg8.o \
         perm.o         qqqfasp.o      scof2.o        spagg0.o \
         spagg8.o       fasp2.o        ewfdr2.o       spaf2.o \
         spaf8.o


FICHIERS = $(FTNDECKS)

tarball:  $(FICHIERS) Makefile
	tar cfzv /data/armnraid1/www/ssm/spectral_1.0_all.tgz $(FICHIERS) Makefile 

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

copie:
	for i in $(FTNDECKS) ;\
	do \
	cp /users/dor/armn/lib/rmnlib/spmo/$$i $$i;\
	done

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
	rm *.o

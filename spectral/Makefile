.SUFFIXES :

.SUFFIXES : .ftn .f .c .o .a

SHELL = /bin/sh

CPP = /lib/cpp

FFLAGS =

CFLAGS =

SUPP_OPT = -openmp

OPTIMIZ = 2

RMNLIB_BASE_PATH=$(ARMNLIB)

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmnbeta.a

LIB_X = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmn_x.a

TEMPLIB = ./$(EC_ARCH)/lib_local.a

UPDATEX =

TARRLS = beta

.PRECIOUS: $(MYLIB)

default: obj

include $(ARMNLIB)/include/makefile_suffix_rules.inc

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
	tar cfzv /data/armnraid1/www/ssm/sources/spectral_$(TARRLS)_all.tgz $(FICHIERS) Makefile 

obj: $(OBJECTS) 
#Produire les fichiers objets (.o) pour tous les fichiers

gen_ec_arch_dir:
#Creer le repertoire $EC_ARCH 
	mkdir -p ./$(EC_ARCH)

locallib: gen_ec_arch_dir \
         $(TEMPLIB)(alpas2.o)       $(TEMPLIB)(alpas8.o)       $(TEMPLIB)(alpdl2.o)       $(TEMPLIB)(alpdl8.o) \
         $(TEMPLIB)(alpdr2.o)       $(TEMPLIB)(alpdr8.o)       $(TEMPLIB)(alpnm2.o)       $(TEMPLIB)(alpnm8.o) \
         $(TEMPLIB)(dimcal.o)       $(TEMPLIB)(epsil2.o)       $(TEMPLIB)(epsil8.o)       $(TEMPLIB)(fasp8.o) \
         $(TEMPLIB)(gauss.o)        $(TEMPLIB)(gauss8.o)       $(TEMPLIB)(ggasp0.o) \
         $(TEMPLIB)(ggasp8.o)       $(TEMPLIB)(gwaqd0.o)       $(TEMPLIB)(gwaqd8.o)       $(TEMPLIB)(ordleg8.o) \
         $(TEMPLIB)(perm.o)         $(TEMPLIB)(qqqfasp.o)      $(TEMPLIB)(scof2.o)        $(TEMPLIB)(spagg0.o) \
         $(TEMPLIB)(spagg8.o)       $(TEMPLIB)(fasp2.o)        $(TEMPLIB)(ewfdr2.o)       $(TEMPLIB)(spaf2.o) \
         $(TEMPLIB)(spaf8.o)

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
	rm -f *.o

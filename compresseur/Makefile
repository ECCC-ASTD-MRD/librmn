include /usr/local/env/armnlib/include/$(ARCH)$(ABI)/Makefile_addons

.SUFFIXES :

.SUFFIXES : .f90 .ftn .f .c .o

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
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $< -ignorempty

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -options_comp -src $<

.f.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.f90.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

OBJECTS= zfstlib.o c_zfstlib.o

tarball:  c_zfstlib.c  zfstlib.f90  zfstlib.h Makefile
	tar cfzv /data/armnraid1/www/ssm/compresseur_1.0_all.tgz c_zfstlib.c  zfstlib.f90  zfstlib.h Makefile 

obj: $(OBJECTS) 
#Produire les fichiers objets (.o) pour tous les fichiers

obj2: $(OBJECTS) 
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
	rm *.o

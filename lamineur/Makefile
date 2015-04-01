#NOTES on slab functions, Delamineur programs
#
#November 20, 2001
#read_slab.c - Delamineur_98 that produces diese grid records, needs bemol_2000
#              to produce Z grid records and does not allow any filtering
#              This program is usually linked to "Delamineur_2000" and used by
#              model GEMDM
#routines.f  - fortran functions that go with read_slab.c
#read_slab_old.c - Delamineur that produces Z grid records and allow filtering,
#                  currently used by operations for model GEM
#routines_old.f  - fortran functions that go with read_slab_old.c
#
#write_slab.c - Slab functions that correspond for read_slab_old.c
#write_slab2000.c-Slab functions that correspond for read_slab.c
#
#slab.h       -  include deck for read_slab.c and write_slab2000.c
#slab_old.h   -  include deck for read_slab_old.c and write_slab.c
#
#slabtest.f   -  Test program to "create" slabfiles for Delamineur
#
#slabtest2000.f- Test program to "create" diese slabfiles for Delamineur_98
#
# include Makefile_$(ARCH)$(ABI)
include /usr/local/env/armnlib/include/$(ARCH)$(ABI)/Makefile_addons

.SUFFIXES : .ftn .f .c .o

SHELL = /bin/sh

CPP = /lib/cpp

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2

CPPFLAGS = -I$(ARMNLIB)/include

RMNLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn.a

MYLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmnbeta.a

LIB_X = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn_x.a

UPDATEX =

.PRECIOUS: $(RMNLIB) $(MYLIB)

default: obj

.ftn.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

.f.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

OBJECT = write_slab.o

OBJECT2000 = write_slab2000.o

OBJ2 = read_slab.o routines.o

OBJ3 = read_slab_old.o routines_old.o

OBJTEST = slabtest.o

OBJTEST2000 = slabtest2000.o

FICHIERS = write_slab2000.c read_slab.c routines.f slab.h

tarball:  $(FICHIERS) Makefile
	tar cfzv /data/armnraid1/www/ssm/lamineur_1.0_all.tgz $(FICHIERS) Makefile 

obj: $(OBJECT2000) 
#Produire les fichiers objets (.o) pour tous les fichiers

genlib: $(OBJECT2000)
#Creer ou mettre a jour la programmatheque 
	r.ar -arch $(ARCH) rcv $(MYLIB) $(OBJECT2000)

updlib: 
#mettre a jour la programmatheque 
	r.ar -arch $(ARCH) rcv $(MYLIB) *.o
	if [ "$(UPDATEX)" = "1" ] ; \
	then \
	r.ar -arch $(ARCH) rcv $(LIB_X) *.o ; \
	fi

slabtest: ${OBJTEST}
	r.build -o $@ -obj $(OBJTEST) $(OBJECT) -arch $(ARCH) -abi $(ABI) -librmn
slabtest98: ${OBJTEST}
	r.build -o $@ -obj $(OBJTEST2000) $(OBJECT2000) -arch $(ARCH) -abi $(ABI) -librmn

#
#	if creating for LINUX, compile on GALOIS!

delamineur_98: ${OBJ2}
	r.build -bidon c -main delamineur -o $@ -obj ${OBJ2} -arch $(ARCH) -abi $(ABI) -librmn

delamineur_xx: ${OBJ3}
	r.build -o $@ -obj $(OBJ3) -arch $(ARCH) -abi $(ABI) -librmn

delamineur: ${OBJ3}
	r.build -o $@ -obj ${OBJ3} -arch $(ARCH) -abi $(ABI) -librmn -fstd89

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
	rm *.o slabtest delamineur*

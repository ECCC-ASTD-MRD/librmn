#include Makefile_$(ARCH)$(ABI)
include /usr/local/env/armnlib/include/$(ARCH)$(ABI)/Makefile_addons

SHELL = /bin/sh

CPP = /lib/cpp

MYLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmnbeta.a

LIB_X = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn_x.a

UPDATEX =

COMPILE = compile

FFLAGS =

CFLAGS =

OPTIMIZ = -O 3
#OPTIMIZ = -debug

CPPFLAGS = -I$(ARMNLIB)/include

.PRECIOUS: $(MYLIB)

default: obj

.f.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -options_comp -src $<


OBJECTS= \
         pak.o compact.o   compact_integer.o     compact_rle.o\
         unpackWrapper.o   compact_IEEEblock.o   f_pow.o\
	       float_packer.o


OBJ_Test= \
          cmainInteger.o   cmainFloat.o          cmainDouble.o\
          cmainRLE.o       cmainIEEE.o           cmainIEEEDouble.o\
          tpak.o           mins.o

OBJ_Test2= \
          cmainInteger.o   cmainFloat.o          cmainDouble.o\
          cmainRLE.o       cmainIEEE.o           cmainIEEEDouble.o\
          test_I.o           mins.o

FICHIERS = pak.c compact.c compact_h.c compact_integer.c compact_rle.c \
           unpackWrapper.c compact_IEEEblock.c compact_IEEEblock_h.c \
           f_pow.f float_packer.c
          
tarball:  $(FICHIERS) Makefile
	tar cfzv /data/armnraid1/www/ssm/packers_1.0_all.tgz $(FICHIERS) Makefile 
          
obj: $(OBJECTS) $(OBJ_NEC)
#Produire les fichiers objets (.o) pour tous les fichiers

genlib: $(OBJECTS) $(OBJ_NEC)
#Creer ou mettre a jour la programmatheque 
	r.ar -arch $(ARCH) rcv $(MYLIB) $(OBJECTS) $(OBJ_NEC)

updlib: 
#mettre a jour la programmatheque 
	r.ar -arch $(ARCH) rcv $(MYLIB) *.o
	if [ "$(UPDATEX)" = "1" ] ; \
	then \
	r.ar -arch $(ARCH) rcv $(LIB_X) *.o ; \
	fi

mytest: $(OBJECTS) $(OBJ_Test) $(LIB)
	r.build -o $@ -obj $(OBJECTS) $(OBJ_Test) -arch $(ARCH) -abi $(ABI) -librmn rmnbeta -debug

test_I: $(OBJECTS) $(OBJ_Test2) $(LIB)
	r.build -o $@ -obj $(OBJECTS) $(OBJ_Test2) -arch $(ARCH) -abi $(ABI) -librmn rmnbeta -debug

clean:
	rm *.o mytest

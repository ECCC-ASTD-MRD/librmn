.SUFFIXES:

.SUFFIXES : .s .o .ftn .f90 .c

SHELL = /bin/sh

RMNLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn.a

FFLAGS =

CFLAGS =

OPTIMIZ = -O 1

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librtools.a

LIB_X = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librtools.a

UPDATEX =

.PRECIOUS: $(RMNLIB) $(MALIB)

default: $(BASE_ARCH)_time_base.c obj

include $(ARMNLIB)/include/makefile_suffix_rules.inc

OBJECTS= \
        tmg.o rtools_wtime.o fort_hpm.o $(BASE_ARCH)_time_base.o

$(BASE_ARCH)_time_base.c:
	touch $(BASE_ARCH)_time_base.c

$(BASE_ARCH)_time_base.s:
	touch $(BASE_ARCH)_time_base.s

$(BASE_ARCH)_time_base.o: $(BASE_ARCH)_time_base.s
	as -o $(BASE_ARCH)_time_base.o $(BASE_ARCH)_time_base.s  || touch $(BASE_ARCH)_time_base.o

test: locallib
	r.compile -src rtools_wtime.c -conly -o a.out  -defines=-DTEST -obj $(BASE_ARCH)_time_base.o
	r.compile -src rtools_wtime.c 
	r.compile -o b.out -src tmg.ftn -defines=-DTEST -obj rtools_wtime.o -librmn -O 2
	time ./a.out
	time ./b.out

tstwtime: tstwtime.o $(OBJECTS)
	r.build -o tstwtime_$(ARCH) -obj tstwtime.o $(OBJECTS) -arch $(ARCH) \
	-abi$(ABI)

obj: $(OBJECTS)
#Produire les fichiers objets (.o) pour tous les fichiers

genlib: $(OBJECTS)
#Creer ou mettre a jour la programmatheque 
	r.ar -arch $(ARCH) rcv $(MYLIB) $(OBJECTS)

tarball:
	tar cfzv /data/armnraid1/www/ssm/sources/librtools_1.0_all.tgz tmg.ftn rtools_wtime.c fort_hpm.c Makefile

locallib: $(OBJECTS)
	ar rcv ./templib_$(EC_ARCH)$(ABI).a $(OBJECTS)

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
	rm -f *.o  *.out templib*.a
	rm -rf .fo

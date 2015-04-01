include /usr/local/env/armnlib/include/$(ARCH)$(ABI)/Makefile_addons

.SUFFIXES : .ftn .f .c .o

SHELL = /bin/sh

CPP = /lib/cpp

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn_x.a

RMNLIB = rmn

.PRECIOUS: $(MYLIB)

default: obj

.ftn.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

.f.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

OBJECT = cmcwlog.o 

OBJ2 = cmcrlog.o

OBJTEST = slabtest.o

default:	cmc_p_log cmc_r_log net_w_log cmc_w_log
	make clean

tarball: *.c *.h
	tar cfzv /data/armnraid1/www/ssm/cmc_log_1.0_all.tgz *.c *.h Makefile 
        
obj: $(OBJECT) $(OBJ2)
#Produire les fichiers objets (.o) pour tous les fichiers

genlib: $(OBJECT) $(OBJ2)
#Creer ou mettre a jour la programmatheque 
	$(AR) rcv $(MYLIB) $(OBJECT) $(OBJ2)

updlib: 
#mettre a jour la programmatheque 
	r.ar -arch $(ARCH) rcv $(MYLIB) *.o
	if [ "$(UPDATEX)" = "1" ] ; \
	then \
	r.ar -arch $(ARCH) rcv $(LIB_X) *.o ; \
	fi

cmc_p_log: ${OBJ2} cmc_p_log.o
	r.build -conly -o $@.$(ARCH) -obj $(OBJ2) cmc_p_log.o -arch $(ARCH) -abi $(ABI) -librmn $(RMNLIB)

cmc_r_log: ${OBJ2} cmc_r_log.o
	r.build -conly -o $@.$(ARCH) -obj $(OBJ2) cmc_r_log.o -arch $(ARCH) -abi $(ABI) -librmn $(RMNLIB)

net_w_log: ${OBJECT} net_w_log.o
	r.build -conly -o $@.$(ARCH) -obj $(OBJECT) net_w_log.o -arch $(ARCH) -abi $(ABI) -librmn $(RMNLIB)

cmc_w_log: ${OBJECT} cmc_w_log.o
	r.build -o $@.$(ARCH) -obj $(OBJECT) cmc_w_log.o -conly -arch $(ARCH) -abi $(ABI) -librmn  $(RMNLIB)

clean:
	rm -f *.o 

veryclean:
	rm -f *.o cmc_p_log cmc_r_log cmc_w_log net_w_log *log.???*

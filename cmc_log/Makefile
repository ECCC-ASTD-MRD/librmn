.SUFFIXES :

.SUFFIXES : .ftn .f .c .o .a

SHELL = /bin/sh

CPP = /lib/cpp

FFLAGS =

CFLAGS =

OPTIMIZ = 2

RMNLIB_BASE_PATH=$(ARMNLIB)

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmnbeta.a

TEMPLIB = ./templib_$(EC_ARCH)$(ABI).a

RMNLIB = rmn

.PRECIOUS: $(MYLIB)

default: obj

include $(ARMNLIB)/include/makefile_suffix_rules.inc

OBJECT = cmcwlog.o 

OBJ2 = cmcrlog.o

OBJTEST = slabtest.o

default:	cmc_p_log cmc_r_log net_w_log cmc_w_log
	make clean

tarball: *.c *.h
	tar cfzv /data/armnraid1/www/ssm/sources/cmc_log_1.0_all.tgz *.c *.h Makefile 
        
obj: $(OBJECT) $(OBJ2)
#Produire les fichiers objets (.o) pour tous les fichiers

locallib: $(TEMPLIB)(cmcwlog.o)

genlib: $(OBJECT) $(OBJ2)
#Creer ou mettre a jour la programmatheque 
	$(AR) rcv $(MYLIB) $(OBJECT) $(OBJ2)

updlib: 
#mettre a jour la programmatheque 
	r.ar -arch $(EC_ARCH) rcv $(MYLIB) *.o
	if [ "$(UPDATEX)" = "1" ] ; \
	then \
	r.ar -arch $(EC_ARCH) rcv $(LIB_X) *.o ; \
	fi

cmc_p_log: ${OBJ2} cmc_p_log.o
	r.build -conly -o $@.$(EC_ARCH) -obj $(OBJ2) cmc_p_log.o -arch $(EC_ARCH) -abi $(ABI) -librmn $(RMNLIB)

cmc_r_log: ${OBJ2} cmc_r_log.o
	r.build -conly -o $@.$(EC_ARCH) -obj $(OBJ2) cmc_r_log.o -arch $(EC_ARCH) -abi $(ABI) -librmn $(RMNLIB)

net_w_log: ${OBJECT} net_w_log.o
	r.build -conly -o $@.$(EC_ARCH) -obj $(OBJECT) net_w_log.o -arch $(EC_ARCH) -abi $(ABI) -librmn $(RMNLIB)

cmc_w_log: ${OBJECT} cmc_w_log.o
	r.build -o $@.$(EC_ARCH) -obj $(OBJECT) cmc_w_log.o -conly -arch $(EC_ARCH) -abi $(ABI) -librmn  $(RMNLIB)

clean:
	rm -f *.o 

veryclean:
	rm -f *.o cmc_p_log cmc_r_log cmc_w_log net_w_log *log.???*

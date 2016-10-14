.SUFFIXES :

.SUFFIXES : .ftn90 .c .o .a

SHELL = /bin/bash

CPP = /lib/cpp

RMNLIB_BASE_PATH=$(ARMNLIB)

RMNLIB = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmn.a

TEMPLIB = ./$(EC_ARCH)/lib_local.a

FFLAGS =

CFLAGS =

DEFINE = -DFICHSTD98

SUPP_OPT = -openmp

OPTIMIZ = 2 

#OPTIMIZ = -O 3 -fast
#C_OPTIMIZ = -O 2
#F_OPTIMIZ = -O 2

CPPFLAGS = -I$(RPN_TEMPLATE_LIBS)/include

MYLIB = $(ARMNLIB)/lib/$(EC_ARCH)$(ABI)/librmnbeta.a

LIB_X = $(ARMNLIB)/lib/$(EC_ARCH)$(ABI)/librmn_x.a

UPDATEX =

TARRLS = beta

.PRECIOUS: $(RMNLIB) $(MALIB)

default: obj

.DEFAULT:
	co $@

include $(RPN_TEMPLATE_LIBS)/include/makefile_suffix_rules.inc

OBJET = f_ezscint.o c_ezscint.o

FICHIERS_CDK = \
cubic.cdk   ez_gaussgd.cdk  ez_qqqxtrp.cdk   gaussgd.cdk  pi.cdk       qqqpar.cdk   zlin.cdk \
cubic8.cdk  ez_qqqmrot.cdk  ez_qqqxtrp0.cdk  inline.cdk   qqqmrot.cdk  qqqxtrp.cdk  zlin8.cdk

FICHIERS_FTN90 = \
f_ezscint.ftn90

FICHIERS_C = \
c_ezscint.c

FICHIERS_H = \
ez_funcdef.h   ezscint.h

FICHIERS = $(FICHIERS_FTN90) $(FICHIERS_C)

#libezscint: f_ezscint.o c_ezscint.o
#	ar rv libezscint-590.a.fl c_ezscint.o f_ezscint.o

#f_ezscint.o: f_ezscint.ftn90
##	s.compile -src f_ezscint.ftn90 $(F_OPTIMIZ)

#c_ezscint.o: c_ezscint.c
#	s.compile -src c_ezscint.c $(C_OPTIMIZ)

obj: $(OBJET)
#Produire les fichiers objets (.o) pour tous les fichiers
	rm -f f_ezscint.f90

gen_ec_arch_dir:
#Creer le repertoire $EC_ARCH 
	mkdir -p ./$(EC_ARCH)

locallib: gen_ec_arch_dir \
        $(TEMPLIB)(f_ezscint.o)   $(TEMPLIB)(c_ezscint.o)

genlib: $(OBJET)
#Creer ou mettre a jour la programmatheque
	r.ar -arch $(EC_ARCH) rcv $(MYLIB) $(OBJET)

updlib:
#mettre a jour la programmatheque
	r.ar -arch $(EC_ARCH) rcv $(MYLIB) *.o
	if [ "$(UPDATEX)" = "1" ] ; \
	then \
	r.ar -arch $(EC_ARCH) rcv $(LIB_X) *.o ; \
	fi

clean:
#Faire le grand menage. On enleve tous les fichiers sources\ninutiles et les .o
	-if [ "*.ftn90" != "`echo *.ftn90`" ] ; \
	then \
	for i in *.ftn90 ; \
	do \
	fn=`r.basename $$i '.ftn90'`; \
	rm -f $$fn.f; \
	done \
	fi
	rm -f *~ *.o f_ezscint.f90

fastclean:
	rm *.o f_ezscint.f90



.SUFFIXES :

.SUFFIXES : .ftn .f .o .a

SHELL = /bin/sh

CPP = /lib/cpp

RMNLIB_BASE_PATH=$(ARMNLIB)

MYLIB = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmnbeta.a

TEMPLIB = ./$(EC_ARCH)/lib_local.a

COMPILE = compile

FFLAGS = 

CFLAGS = 

SUPP_OPT = -openmp

OPTIMIZ = 2

CPPFLAGS = -I$(ARMNLIB)/include

LIB_X = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmn_x.a

UPDATEX =

TARRLS = beta

.PRECIOUS: $(MYLIB)

default: burp98_f.o

include $(ARMNLIB)/include/makefile_suffix_rules.inc

FTNDECKS= \
         bufrchr.ftn       mrbcol.ftn       mrbcov.ftn       mrbcvt.ftn \
         mrbdcv.ftn       mrbini.ftn       mrblocx.ftn       mrbprm.ftn \
         mrbprml.ftn       mrbrpt.ftn       mrbtyp.ftn       mrbupd.ftn \
         mrfcls.ftn       mrfdel.ftn       mrfgoc.ftn       mrfgor.ftn \
         mrfloc.ftn       mrfmxl.ftn       mrfnbr.ftn       mrfopc.ftn \
         mrfopn.ftn       mrfopr.ftn       mrfprm.ftn       mrfvoi.ftn \
         qbrptri.ftn       qrbnbdt.ftn       qrbsct.ftn     mrfbfl.ftn

FDECKS= \
         bufrchr.f       mrbcol.f       mrbcov.f       mrbcvt.f \
         mrbdcv.f       mrbini.f       mrblocx.f       mrbprm.f \
         mrbprml.f       mrbrpt.f       mrbtyp.f       mrbupd.f \
         mrfcls.f       mrfdel.f       mrfgoc.f       mrfgor.f \
         mrfloc.f       mrfmxl.f       mrfnbr.f       mrfopc.f \
         mrfopn.f       mrfopr.f       mrfprm.f       mrfvoi.f \
         qbrptri.f       qrbnbdt.f       qrbsct.f     mrfbfl.f

OBJECTS= \
         bufrchr.o       mrbcol.o       mrbcov.o       mrbcvt.o \
         mrbdcv.o       mrbini.o       mrblocx.o       mrbprm.o \
         mrbprml.o       mrbrpt.o       mrbtyp.o       mrbupd.o \
         mrfcls.o       mrfdel.o       mrfgoc.o       mrfgor.o \
         mrfloc.o       mrfmxl.o       mrfnbr.o       mrfopc.o \
         mrfopn.o       mrfopr.o       mrfprm.o       mrfvoi.o \
         qbrptri.o       qrbnbdt.o       qrbsct.o     mrfbfl.o


COMDECKS= \
         bpl.cdk       burpopt.cdk       codes.cdk       defi.cdk \
         enforc8.cdk   masques.cdk

FICHIERS = $(FDECKS) $(CDECKS) $(ASDECKS) $(HDECKS)

OBJET = burp98_f.o

bufrchr.f: bufrchr.ftn  mrbcol.ftn 	mrbcov.ftn 	mrbcvt.ftn \
	mrbdcv.ftn 	mrblocx.ftn 	mrbprm.ftn 	mrbprml.ftn \
	mrbrpt.ftn 	mrbtyp.ftn 	mrfmxl.ftn 	mrfprm.ftn \
	qrbsct.ftn 
mrbcol.f: mrbcol.ftn 	defi.cdk 	masques.cdk 
mrbcov.f: mrbcov.ftn 	defi.cdk 	masques.cdk 
mrbcvt.f: mrbcvt.ftn 	burpopt.cdk 	codes.cdk 	defi.cdk
mrbdcv.f: mrbdcv.ftn 	defi.cdk 	masques.cdk 
mrbini.f: mrbini.ftn 	codes.cdk 	defi.cdk 	enforc8.cdk 
mrblocx.f: mrblocx.ftn 	bpl.cdk 	burpopt.cdk 	codes.cdk \
	defi.cdk 	masques.cdk 
mrbprm.f: mrbprm.ftn 	bpl.cdk 	defi.cdk
mrbprml.f: mrbprml.ftn 	bpl.cdk 	codes.cdk 	defi.cdk
mrbrpt.f: mrbrpt.ftn 	burpopt.cdk 	codes.cdk 	defi.cdk
mrbtyp.f: mrbtyp.ftn 	bpl.cdk 	codes.cdk 	masques.cdk 
mrbupd.f: mrbupd.ftn 	codes.cdk 	defi.cdk 	enforc8.cdk 
mrfcls.f: mrfcls.ftn 	burpopt.cdk 	codes.cdk 	defi.cdk 
mrfdel.f: mrfdel.ftn 	burpopt.cdk 	codes.cdk 	defi.cdk 
mrfgoc.f: mrfgoc.ftn 
mrfgor.f: mrfgor.ftn 	burpopt.cdk 	codes.cdk 	defi.cdk 
mrfloc.f: mrfloc.ftn 	burpopt.cdk 	codes.cdk 	defi.cdk \
	enforc8.cdk 
mrfmxl.f: mrfmxl.ftn 	defi.cdk
mrfbfl.f: mrfbfl.ftn
mrfnbr.f: mrfnbr.ftn 	defi.cdk 
mrfopc.f: mrfopc.ftn 	burpopt.cdk 	codes.cdk 	defi.cdk 
mrfopn.f: mrfopn.ftn 	bpl.cdk 	burpopt.cdk 	codes.cdk \
	defi.cdk 	enforc8.cdk 
mrfopr.f: mrfopr.ftn 	burpopt.cdk 	codes.cdk 	defi.cdk 
mrfprm.f: mrfprm.ftn 	codes.cdk 	defi.cdk 	enforc8.cdk
mrfvoi.f: mrfvoi.ftn 	defi.cdk 	enforc8.cdk 
qbrptri.f: qbrptri.ftn 
qrbnbdt.f: qrbnbdt.ftn 	codes.cdk 
qrbsct.f: qrbsct.ftn 	burpopt.cdk 	codes.cdk 	defi.cdk 

burp98_f.f: $(FDECKS)
#Produire le source du progiciel burp et le regrouper\ndans le fichier burp98_f.f
	cat $(FDECKS) > burp98_f.f

tarball: $(FTNDECKS) $(COMDECKS)
	tar cfzv /data/armnraid1/www/ssm/sources/burp98_f_$(TARRLS)_all.tgz $(FTNDECKS) $(COMDECKS) Makefile 

obj: $(OBJET) 
#Produire les fichiers objets (.o) pour tous les fichiers

burptest.f: burptest.main
#Produire le source du programme test fortran
	r.ftntof -P $(DEFINE) burptest.main > burptest.f

burptest.o: burptest.f
#Produire un objet du programme test fortran

burptest: burptest.o $(OBJET) 
#creer un executable burptest
	r.build -o $@ -obj burptest.o -arch $(EC_ARCH) -abi $(ABI) -librmn

testc.o: testc.c

ctest: testc.o $(OBJET)
#Produire un executable pour tester l'interface C de burp
	r.build -o $@ -obj testc.o -arch $(EC_ARCH) -abi $(ABI) -librmn

gen_ec_arch_dir:
#Creer le repertoire $EC_ARCH 
	mkdir -p ./$(EC_ARCH)

locallib: gen_ec_arch_dir $(TEMPLIB)(burp98_f.o)
        
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
	-if [ "*.ftn" != "`echo *.ftn`" ] ; \
	then \
	for i in *.ftn ; \
	do \
	fn=`r.basename $$i '.ftn'`; \
	rm -f $$fn.f; \
	done \
	fi
	rm -f *.o burp98_f.f

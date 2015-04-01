include /usr/local/env/armnlib/include/$(ARCH)$(ABI)/Makefile_addons

.SUFFIXES :

.SUFFIXES : .f .o .rat .dck

SHELL = /bin/sh

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmnbeta.a

LIB_X = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn_x.a

UPDATEX =

OBJET = readlx.o

OBJET2 = tstrdlx.o 

#CFLAGS = -I$(ARMNLIB)/include/macros -DC910=c910 -g
#FFLAGS = -g

# FFLAGS = -O  (pour cidsv08)

# FFLAGS = -O1 (pour cidsv02)

# FFLAGS = -O1

# DEFINE = -DC910=c910 -DRATFOR=ratfor

FICHIERS = \
 bufftok.cdk   parmadr.cdk   qlxbuff.cdk   qlxfmt.cdk\
 aaaamac.dck   argdims.dck   argdope.dck   lexins.dck    qlxadi.dck\
 qlxadr.dck    qlxasg.dck    qlxbak.dck    qlxcall.dck\
 qlxchr.dck    qlxdbg.dck    qlxdtyp.dck   qlxerr.dck\
 qlxflsh.dck   qlxfnd.dck    qlxind.dck    qlxins.dck\
 qlxnum.dck    qlxnvar.dck   qlxopr.dck    qlxopt.dck\
 qlxpri.dck    qlxprnt.dck   qlxrpn.dck    qlxskp.dck\
 qlxtok.dck    qlxundf.dck   qlxval.dck    qlxxpr.dck\
 readlx.dck

FILES = readlx.f readlx.f_AIX

default: obj

.dck.rat:
	cat *.dck | r.ftnpp - -P $(CPPFLAGS) -I$(ARMNLIB)/include $(DEFINE) > $*.rat

.rat.f:
	rat77 -f $< -l $*.of -o $*.f

.f.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

#readlx.rat: $(FICHIERS)
#readlx.f: readlx.rat
readlx.o: readlx.f

tstrdlx.o: tstrdlx.f

#genrat: $(FICHIERS)
#	cat *.dck | /lib/cpp -P $(CPPFLAGS) -I$(ARMNLIB)/include $(DEFINE) > readlx.rat

tarball:  $(FILES) Makefile
	tar cfzv /data/armnraid1/www/ssm/readlx_1.0_all.tgz $(FILES) Makefile 

obj: $(OBJET) 
#Produire les fichiers objets (.o) pour tous les fichiers

tstrdlx: $(OBJET2) $(OBJET)
	r.build -o $@ -obj $(OBJET2) $(OBJET) -arch $(ARCH) -abi $(ABI) -librmn rmnbeta

tstc: tstc.o c_readlx.o readln.o
	r.build -o $@ -obj tstc.o c_readlx.o readln.o -arch $(ARCH) -abi $(ABI) -librmn

genlib: $(OBJET)
#Creer ou mettre a jour la programmatheque 
	r.ar -arch $(ARCH) rcv $(MYLIB) $(OBJET)

updlib: 
#mettre a jour la programmatheque 
	r.ar -arch $(ARCH) rcv $(MYLIB) *.o
	if [ "$(UPDATEX)" = "1" ] ; \
	then \
	r.ar -arch $(ARCH) rcv $(LIB_X) *.o ; \
	fi

clean:
	-rm readlx.f *.o *.rat *.of tstrdlx


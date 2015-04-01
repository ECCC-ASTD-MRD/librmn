include /usr/local/env/armnlib/include/$(ARCH)$(ABI)/Makefile_addons

.SUFFIXES :

.SUFFIXES : .ftn .f .c .o

SHELL = /bin/sh

CPP = /lib/cpp

RMNLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn.a

FFLAGS =

CFLAGS =

OPTIMIZ = -O 2

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmnbeta.a

LIB_X = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn_x.a

UPDATEX =

.PRECIOUS: $(RMNLIB) $(MALIB)

default: obj

.ftn.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

.f.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<


FTNDECKS= \
         dgauss.ftn       grgg.ftn       idxes.ftn       llfxy.ftn \
         mzonxst.ftn      npspin.ftn     ordleg.ftn      sglo.ftn \
         sglogg.ftn       sll.ftn        sps.ftn         vglo.ftn \
         vglogg.ftn       vll.ftn        vps.ftn         xyflcc.ftn \
         xyfll.ftn        igaxg95.ftn    legvar.ftn      mxma.ftn \
         grid_to_grid.ftn

CDECKS= \
         c_time.c         ipsort.c       movr8r4.c

FDECKS= \
         afix.f           afix8.f        amax.f          amean.f \
         amin.f           bilin.f        corcof.f        coriol.f \
         coupe.f          cvt_base40.f   d1.f            d1int1.f \
         d1int2.f         d1intr.f       datec.f         datmgp.f \
         defvec.f         fconw.f \
         fd1.f            fdm.f                          gdadcn.f \
         gdadgd.f         gdmpgd.f       gdsqrt.f        \
         grlcc.f          grll.f         grps.f          idatmg.f \
         igaxg.f          incdat.f        int1d1.f \
         inzono.f         isort.f        jdatec.f        jjulien.f \
         moduledate.f     movlev.f        movlev8.f \
         mscale.f         mwerms.f       \
         permut.f       polagr.f        rossr3.f \
         s1scor.f         \
         sort.f           spd.f          splat.f \
         sqadsq.f         valide.f \
         vpolagr.f \
         windv.f        xgaig.f \
         system_time.f  mxm.f\
	 cvmgm.f          cvmgn.f        cvmgp.f         cvmgt.f\
         cvmgz.f          nr_rand.f

OBJECTS= \
         afix.o           afix8.o        amax.o          amean.o \
         amin.o           bilin.o        c_time.o        corcof.o \
         coriol.o         coupe.o        cvt_base40.o    d1.o \
         d1int1.o         d1int2.o       d1intr.o        datec.o \
         datmgp.o         defvec.o       dgauss.o        \
         fconw.o          fd1.o          fdm.o           \
         gdadcn.o         gdadgd.o       gdmpgd.o        gdsqrt.o \
         grgg.o           grlcc.o        grll.o          grps.o \
         idatmg.o         idxes.o        igaxg.o         incdat.o \
         int1d1.o         inzono.o       ipsort.o        isort.o \
         jdatec.o         jjulien.o      llfxy.o         moduledate.o \
         movlev.o         movlev8.o      mscale.o        mwerms.o \
         mzonxst.o        npspin.o       ordleg.o        permut.o \
         polagr.o         rossr3.o       s1scor.o        sglo.o \
         sglogg.o         sll.o          sort.o          spd.o \
         splat.o          sps.o          sqadsq.o        valide.o \
         vglo.o           vglogg.o       vll.o           \
                          vpolagr.o      vps.o           windv.o \
         xgaig.o          xyflcc.o       xyfll.o         system_time.o\
         mxma.o           mxm.o          igaxg95.o       legvar.o\
         movr8r4.o        nr_rand.o      grid_to_grid.o

COMDECKS= \
         cubic.cdk       kases.cdk       lagrange.cdk       ordr.cdk \
         pi.cdk       piprm.cdk  qqqcgb1.cdk  qqqgb2.cdk

FICHIERS = $(FDECKS) $(FTNDECKS) $(CDECKS) $(COMDECKS)

dgauss.o:	dgauss.ftn 	kases.cdk 	pi.cdk 
grgg.o:		grgg.ftn 	kases.cdk 	pi.cdk 
idxes.o:	idxes.ftn 	kases.cdk 
llfxy.o:	llfxy.ftn 	pi.cdk 
mxma.o:         mxma.ftn
mzonxst.o:	mzonxst.ftn 
npspin.o:       npspin.ftn 	kases.cdk 	pi.cdk 
ordleg.o:       ordleg.ftn 	pi.cdk 
sglo.o:		sglo.ftn 	cubic.cdk 	kases.cdk 	ordr.cdk 
sglogg.o:	sglogg.ftn 	cubic.cdk 	kases.cdk 	lagrange.cdk \
		ordr.cdk 	pi.cdk 
sll.o:		sll.ftn 	cubic.cdk 	lagrange.cdk 	ordr.cdk 
sps.o: 		sps.ftn 	cubic.cdk 	lagrange.cdk 	ordr.cdk 
vglo.o:		vglo.ftn 	cubic.cdk 	kases.cdk 	pi.cdk 
vglogg.o:	vglogg.ftn 	cubic.cdk 	kases.cdk 	lagrange.cdk \
		pi.cdk 
vll.o:		vll.ftn 	pi.cdk 
vps.o:		vps.ftn 	pi.cdk 
xyflcc.o: 	xyflcc.ftn 	piprm.cdk 
xyfll.o: 	xyfll.ftn 	pi.cdk 
legvar.o:       legvar.ftn
grid_to_grid.o: grid_to_grid.ftn

tarball: $(FICHIERS)
	tar cfzv /data/armnraid1/www/ssm/base_1.0_all.tgz $(FICHIERS) Makefile 

obj: $(OBJECTS) $(OBJ_FRONT)
#Produire les fichiers objets (.o) pour tous les fichiers

genlib: $(OBJECTS) $(OBJ_FRONT)
#Creer ou mettre a jour la programmatheque 
	r.ar -arch $(ARCH) rcv $(MYLIB) $(OBJECTS) $(OBJ_FRONT)

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

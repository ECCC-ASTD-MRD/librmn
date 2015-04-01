include /usr/local/env/armnlib/include/$(ARCH)$(ABI)/Makefile_addons
.SUFFIXES :

.SUFFIXES : .ftn .c .o

SHELL = /bin/sh

CPP = /lib/cpp

RMNLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn.a

FFLAGS = 

CFLAGS = 

DEFINE = -defines =-DFICHSTD98

OPTIMIZ = -O 2
#OPTIMIZ = -O 0 -debug

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmnbeta.a

LIB_X = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn_x.a

UPDATEX =

.PRECIOUS: $(RMNLIB) $(MALIB)

default: obj

.DEFAULT:
	co $@


.ftn.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(DEFINE) $(OPTIMIZ) -opt "=$(CFLAGS)" -src $<

.f.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.f90.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

OBJET = f_ezscint.o c_ezscint.o

FICHIERS_CDK = \
cubic.cdk   ez_gaussgd.cdk  ez_qqqxtrp.cdk   gaussgd.cdk  pi.cdk       qqqpar.cdk   zlin.cdk \
cubic8.cdk  ez_qqqmrot.cdk  ez_qqqxtrp0.cdk  inline.cdk   qqqmrot.cdk  qqqxtrp.cdk  zlin8.cdk

FICHIERS_FTN = \
ez_aminmax.ftn\
ez_cal.ftn\
ez_calcpolemodule.ftn\
ez_calcpoleval.ftn\
ez_cartauv.ftn\
ez_cartauv_turbo.ftn\
ez_cherche.ftn\
ez_corrbgd.ftn\
ez_crot.ftn\
ez_fillaxzx.ftn\
ez_fillayzs.ftn\
ez_fillnpole.ftn\
ez_fillspole.ftn\
ez_gausslat.ftn\
ez_gdwfllw.ftn\
ez_genpole.ftn\
ez_gfllfxy.ftn\
ez_gfxyfll.ftn\
ez_gggdint_nw.ftn\
ez_gggdint_w.ftn\
ez_ggll2gd.ftn\
ez_glat.ftn\
ez_iaxint.ftn\
ez_irgdint_1_nw.ftn\
ez_irgdint_1_w.ftn\
ez_irgdint_3_nw.ftn\
ez_irgdint_3_w.ftn\
ez_irgdint_3_wnnc.ftn\
ez_lac.ftn\
ez_lamb_gdwfllw.ftn\
ez_lamb_llwfgdw.ftn\
ez_lambfll.ftn\
ez_lambllfxy99.ftn\
ez_lambsecfll.ftn\
ez_lambxyfll99.ftn\
ez_ll2ergd.ftn\
ez_ll2igd.ftn\
ez_ll2rgd.ftn\
ez_llflamb.ftn\
ez_llflambsec.ftn\
ez_llll2gd.ftn\
ez_llwfgdw.ftn\
ez_mdup.ftn\
ez_module.ftn\
ez_mtxinv8.ftn\
ez_mxm.ftn\
ez_nwtncof.ftn\
ez_nwtncof2.ftn\
ez_polrint.ftn\
ez_rgdint_0.ftn\
ez_rgdint_1_nw.ftn\
ez_rgdint_1_w.ftn\
ez_rgdint_3_nw.ftn\
ez_rgdint_3_w.ftn\
ez_rgdint_3_wnnc.ftn\
ez_sincoslatlon.ftn\
ez_uvacart.ftn\
ez_uvacart_turbo.ftn\
ez_vllfxy.ftn\
ez_vrotf.ftn\
ez_vrotf2.ftn\
ez_vtxyfll.ftn\
ez_vxyfll.ftn\
ez_xpngdag2.ftn\
ez_xpngdb2.ftn\
ez_xtrap.ftn\
ez_rgopt.ftn\
llwfgfw.ftn\
rgll2gd.ftn

FICHIERS_C = \
ez_calclatlon.c \
ez_calcnpolarwind.c \
ez_calcntncof.c \
ez_calcspolarwind.c \
ez_calcxy.c \
ez_corrval.c \
ez_corrval_aunord.c \
ez_corrval_ausud.c \
ez_corrvec.c \
ez_corrvec_aunord.c \
ez_corrvec_ausud.c \
ez_defaxes.c \
ez_defxg.c \
ez_defzone_dehors.c \
ez_defzone_est.c \
ez_defzone_nord.c \
ez_defzone_ouest.c \
ez_defzone_polenord.c \
ez_defzone_polesud.c \
ez_defzone_sud.c \
ez_defzones.c \
ez_eliminer_grille.c \
ez_freegridset.c \
ez_ftnstrclean.c \
ez_gdgxpndaxes.c \
ez_gduvfwd_turbo.c \
ez_gdwdfuv_turbo.c \
ez_generate_gem_cache.c \
ez_genpole.c \
ez_gfllfxy.c \
ez_gfwfllw.c \
ez_gfwfllw_turbo.c \
ez_gfxyfll.c \
ez_identifygrid.c \
ez_igscint.c \
ez_interp.c \
ez_lireEnrPositionnels.c \
ez_llfgr.c \
ez_llwfgfw.c \
ez_llwfgfw_turbo.c \
ez_rgll2gd.c \
ez_sincoslatlon.c \
ez_util.c \
ez_xpncof.c \
ez_xpnsrcgd.c \
ezdefset.c \
ezgdef.c \
ezgdef_ffile.c \
ezgdef_fmem.c \
ezgetopt.c \
ezgetval.c \
ezgfstp.c \
ezgprm.c \
ezgxprm.c \
ezqkdef.c \
ezsetopt.c \
ezsetval.c \
ezsint.c \
ezuvint.c \
ezwdint.c \
gdgaxes.c \
gdinterp.c \
gdll.c \
gdllfxy.c \
gdllsval.c \
gdllvval.c \
gdllwdval.c \
gdrls.c \
gduvfwd.c \
gdwdfuv.c \
gdxpncf.c \
gdxysint.c \
gdxyfll.c \
gdxysval.c \
gdxyvval.c \
gdxywdval.c \
gdxyzfll.c \
gdllfxyz.c

FICHIERS_H = \
ez_funcdef.h   ezscint.h

FICHIERS_CDK = \
cubic.cdk   ez_gaussgd.cdk  ez_qqqxtrp.cdk   gaussgd.cdk  pi.cdk       qqqpar.cdk   zlin.cdk \
cubic8.cdk  ez_qqqmrot.cdk  ez_qqqxtrp0.cdk  inline.cdk   qqqmrot.cdk  qqqxtrp.cdk  zlin8.cdk


FICHIERS = $(FICHIERS_FTN) $(FICHIERS_C) $(FICHIERS_H) $(FICHIERS_CDK)

f_ezscint90.o: f_ezscint90.f90

f_ezscint90.f90: $(FICHIERS_F90)
	cat $(FICHIERS_F90) > f_ezscint90.f90

f_ezscint.o: f_ezscint.ftn

f_ezscint.ftn: $(FICHIERS_FTN) $(FICHIERS_CDK)
	cat $(FICHIERS_FTN) > f_ezscint.ftn

c_ezscint.o: c_ezscint.c

c_ezscint.c: $(FICHIERS_C) $(FICHIERS_H)
	cat $(FICHIERS_C) > c_ezscint.c

tarball:  $(FICHIERS) Makefile
	tar cfzv /data/armnraid1/www/ssm/interp_1.0_all.tgz $(FICHIERS) Makefile 
        
obj: $(OBJET)
#Produire les fichiers objets (.o) pour tous les fichiers

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
#Faire le grand menage. On enleve tous les fichiers sources\ninutiles et les .o 
	-if [ "*.ftn" != "`echo *.ftn`" ] ; \
	then \
	for i in *.ftn ; \
	do \
	fn=`r.basename $$i '.ftn'`; \
	rm -f $$fn.f; \
	done \
	fi
	rm *.o c_ezscint.c f_ezscint.ftn

fastclean:
	rm *.o c_ezscint.c



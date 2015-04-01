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

#OPTIMIZ = -O 2
#OPTIMIZ = -O 3 -fast
C_OPTIMIZ = -O 2
F_OPTIMIZ = -O 2

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB = $(ARMNLIB)/lib/$(EC_ARCH)$(ABI)/librmnbeta.a

LIB_X = $(ARMNLIB)/lib/$(EC_ARCH)$(ABI)/librmn_x.a

UPDATEX =

TARRLS = beta

.PRECIOUS: $(RMNLIB) $(MALIB)

default: obj

.DEFAULT:
	co $@

include $(ARMNLIB)/include/makefile_suffix_rules.inc

OBJET = f_ezscint.o c_ezscint.o

FICHIERS_CDK = \
cubic.cdk   ez_gaussgd.cdk  ez_qqqxtrp.cdk   gaussgd.cdk  pi.cdk       qqqpar.cdk   zlin.cdk \
cubic8.cdk  ez_qqqmrot.cdk  ez_qqqxtrp0.cdk  inline.cdk   qqqmrot.cdk  qqqxtrp.cdk  zlin8.cdk

#FICHIERS_FTN90 = \
#isi4.cdk \
#f_isi_gd_def_fst_gnl.ftn90

FICHIERS_FTN90 = \
ez_aminmax.ftn90\
ez_applywgts.ftn90\
ez_avg.ftn90\
ez_avg_sph.ftn90\
ez_cal.ftn90\
ez_calcpolemodule.ftn90\
ez_calcpoleval.ftn90\
ez_calcxy_y.ftn90\
ez_calcxy_y_m.ftn90\
ez_cartauv.ftn90\
ez_cherche.ftn90\
ez_corrbgd.ftn90\
ez_crot.ftn90\
ez_crot_2010.ftn90\
ez_uvfllc2d.ftn90\
ez_fillaxzx.ftn90\
ez_fillayzs.ftn90\
ez_fillnpole.ftn90\
ez_fillspole.ftn90\
ez_gausslat.ftn90\
ez_gdwfllw.ftn90\
ez_genpole.ftn90\
ez_gfllfxy.ftn90\
ez_gfxyfll.ftn90\
ez_gggdint_nw.ftn90\
ez_gggdint_w.ftn90\
ez_ggll2gd.ftn90\
ezget_mask_zones.ftn90\
ez_glat.ftn90\
ez_iaxint.ftn90\
ez_irgdint_1_nw.ftn90\
ez_irgdint_1_w.ftn90\
ez_irgdint_3_nw.ftn90\
ez_irgdint_3_w.ftn90\
ez_irgdint_3_wnnc.ftn90\
ez_lac.ftn90\
ez_lac_8.ftn90\
ez_lamb_gdwfllw.ftn90\
ez_lamb_llwfgdw.ftn90\
ez_lambfll.ftn90\
ez_lambllfxy99.ftn90\
ez_lambxyfll99.ftn90\
ez_ll2ergd.ftn90\
ez_ll2igd.ftn90\
ez_ll2rgd.ftn90\
ez_llflamb.ftn90\
ez_llll2gd.ftn90\
ez_llwfgdw.ftn90\
ez_mdup.ftn90\
ez_module.ftn90\
ez_mtxinv8.ftn90\
ez_mxm.ftn90\
ez_nwtncof.ftn90\
ez_nwtncof2.ftn90\
ez_polrint.ftn90\
ez_rgdint_0.ftn90\
ez_rgdint_1_nw.ftn90\
ez_rgdint_1_w.ftn90\
ez_rgdint_3_nw.ftn90\
ez_rgdint_3_w.ftn90\
ez_rgdint_3_wnnc.ftn90\
ez_sincoslatlon.ftn90\
ez_uvacart.ftn90\
ez_vllfxy.ftn90\
ez_vrotf.ftn90\
ez_vrotf2.ftn90\
ez_vtxyfll.ftn90\
ez_vxyfll.ftn90\
ez_xpngdag2.ftn90\
ez_xpngdb2.ftn90\
ez_xtrap.ftn90\
ez_rgopt.ftn90\
lorenzo_mask_fill.ftn90\
llwfgfw.ftn90 \
inside_or_outside.ftn90 pt_in_quad.ftn90 \
rgll2gd.ftn90

FICHIERS_C = \
ez_addgrid.c \
ez_calcdist.c \
ez_calclatlon.c \
ez_calcnpolarwind.c \
ez_calcntncof.c \
ez_calcspolarwind.c \
ez_calc_crc.c \
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
ez_findgrid.c \
ez_find_gdin.c \
ez_find_gdin_in_gset.c \
ez_freegridset.c \
ez_ftnstrclean.c \
ez_gdgxpndaxes.c \
ez_generate_gem_cache.c \
ez_genpole.c \
ez_gfllfxy.c \
ez_gfwfllw.c \
ez_gfxyfll.c \
ez_identifygrid.c \
ez_igscint.c \
ez_interp.c \
ez_lireEnrPositionnels.c \
ez_llfgr.c \
ez_llwfgfw.c \
ez_mask.c \
ez_rgll2gd.c \
ez_sincoslatlon.c \
ez_util.c \
ez_xpncof.c \
ez_xpnsrcgd.c \
ezdefset.c \
ezgdef.c \
ezgdef_ffile.c \
ezgdef_fll.c \
ezgdef_fmem.c \
ezgdef_supergrid.c \
ezgdef_yymask.c \
ezget_nsubgrids.c \
ezget_subgridids.c \
ezget_subgridmem.c \
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
ezyy_use1subgrid.c \
ezyy_calcxy.c \
ezyymint.c \
ezyysint.c \
ezyyuvint.c \
ezyywdint.c \
gd_key2rowcol.c \
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

FICHIERS = $(FICHIERS_FTN90) $(FICHIERS_C)

libezscint: f_ezscint.o c_ezscint.o
	ar rv libezscint-590.a.fl c_ezscint.o f_ezscint.o

f_ezscint.o: f_ezscint.ftn90
	s.compile -src f_ezscint.ftn90 $(F_OPTIMIZ)

f_ezscint.ftn90: $(FICHIERS_FTN90) $(FICHIERS_CDK)
	cat $(FICHIERS_FTN90) > f_ezscint.ftn90

c_ezscint.o: c_ezscint.c
	s.compile -src c_ezscint.c $(C_OPTIMIZ)

c_ezscint.c: $(FICHIERS_C) $(FICHIERS_H)
	cat $(FICHIERS_C) > c_ezscint.c

tarball:  $(FICHIERS) Makefile
	tar cfzv /data/armnraid1/www/ssm/sources/interp_$(TARRLS)_all.tgz $(FICHIERS) Makefile 

obj: $(OBJET)
#Produire les fichiers objets (.o) pour tous les fichiers
	rm -f c_ezscint.c f_ezscint.f90 f_ezscint.ftn90

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
	rm -f *~ *.o c_ezscint.c f_ezscint.ftn90

fastclean:
	rm *.o c_ezscint.c



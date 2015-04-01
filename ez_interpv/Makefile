.SUFFIXES:

.SUFFIXES : .o .ftn90 .cdk .cdk90 .c .a

SHELL = /bin/sh

COMPILE = compile

FFLAGS =

CFLAGS =

SUPP_OPT = -openmp

OPTIMIZ = 2
#OPTIMIZ =  1
#OPTIMIZ =  0
#OPTIMIZ =  0 -debug

RMNLIB_BASE_PATH=$(ARMNLIB)

MYLIB = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmnbeta.a

LIB_X = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmn_x.a

TEMPLIB = ./$(EC_ARCH)/lib_local.a
SSMLIB = ./$(EC_ARCH)/lib/libezinterpv.a
SSMLIBDIR = ./$(EC_ARCH)/lib
SSMINC = ./$(EC_ARCH)/include

UPDATEX =

TARRLS = beta

.PRECIOUS: $(RMNLIB) $(MALIB)

default: obj

include $(ARMNLIB)/include/makefile_suffix_rules.inc

VertInterpConstants.o: VertInterpConstants.cdk90 ViConstants_f90.h

ViIfc.o: ViIfc.ftn90 ViIfc_Body.ftn90 VertInterpConstants.o VerticalGrid.o VerticalInterpolation.o

VerticalGrid.o: VerticalGrid.cdk90 VerticalGrid_Body.cdk90 VertInterpConstants.o

VerticalInterpolation.o: VerticalInterpolation.ftn90 VerticalInterpolation_Body.ftn90 \
	VertInterpConstants.o VerticalGrid.o


OBJECTS= \
	VertInterpConstants.o VerticalGrid.o VerticalInterpolation.o ViIfc.o

tarball:  *.ftn90 *.cdk90 *.h  Makefile
	tar cfzv /data/armnraid1/www/ssm/sources/ez_interpv_$(TARRLS)_all.tgz *.ftn90 *.cdk90 *.h Makefile 

obj:	$(OBJECTS)

gen_ec_arch_dir:
#Creer le repertoire $EC_ARCH 
	mkdir -p ./$(EC_ARCH)

locallib: gen_ec_arch_dir \
        $(TEMPLIB)(VertInterpConstants.o)   $(TEMPLIB)(VerticalGrid.o) \
        $(TEMPLIB)(VerticalInterpolation.o)  $(TEMPLIB)(ViIfc.o)

make_lib_dir:
	mkdir -p $(SSMINC)
	mkdir -p $(SSMLIBDIR)

ssm_pkg: make_lib_dir gen_ec_arch_dir \
        $(SSMLIB)(VertInterpConstants.o)   $(SSMLIB)(VerticalGrid.o) \
        $(SSMLIB)(VerticalInterpolation.o)  $(SSMLIB)(ViIfc.o)
	cp *.mod VertInterp_f90.h ViConstants_f90.h $(SSMINC)
	 # IDENTIFY THE VGRID PATH (i.e. REVISION)
	                                      # sed removes colons from PATH
	                                      # The length lost gives the no. of paths
	                                      # Scan each path for 'vgrid'
	path=$$LIBPATH;\
	short_path=`echo $$path | sed -n 's/://g;p'`;\
	num_paths=$$(($${#path}-$${#short_path}));\
	let num_paths=$$num_paths+1;\
	let n=1;\
	while [ $$n -le $$num_paths ]; do\
	  vgrid_path=`echo $$LIBPATH | cut -d ':' -f $$n `;\
	  let found=`echo $$vgrid_path | grep -c vgrid`;\
	  if [ $$found -eq 1 ]; then\
	    break;\
	  fi;\
	  let n=$$n+1;\
	done;\
	if [ $$n -gt $$num_paths ]; then\
	  vgrid_path=' ';\
	fi;\
	echo "vgrid_path="$$vgrid_path;\
	echo "This library was created with vgrid_descriptor revision "$$vgrid_path > $(SSMLIBDIR)/ReadMe.txt

genlib: $(OBJECTS)
#Creer ou mettre a jour la programmatheque 
	r.ar -arch $(EC_ARCH) rcv $(MYLIB) $(OBJECTS)

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
	rm -f *.o *.f90 *.mod *.stb  VInterp_*.Abs

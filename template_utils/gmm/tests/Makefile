
.SUFFIXES :

.SUFFIXES : .tmpl90 .cdk90 .ftn .ftn90 .ptn .f .f90 .c .s .o

SHELL = /bin/sh

.ftn90.o:
	s.compile -src $< -debug -O 0 -defines =-DDEBUG_MODE -includes .

.ftn.o:
	s.compile -src $< -debug -O 0 -defines =-DDEBUG_MODE -includes .

.c.o:
	s.compile -src $< -debug -O 0 -defines =-DDEBUG_MODE -includes $(HOME)/data0/src/xrec/include

gmm-test: gmm-test.o
	s.compile -o gmm-test -debug -O 0 -src gmm-test.ftn90 -includes . -librmn rmnbeta

lgmm-test: gmm-test.o
	s.compile -o gmm-test -debug -O 0 -src gmm-test.ftn90 -obj ../gmm.o -includes .. -librmn rmnbeta

1d_schemes: 1d_schemes.o pdcreatepngimage.o
	s.compile -src 1d_schemes.f -obj pdcreatepngimage.o -debug -O 0  -o 1d_schemes -libpath $(HOME)/data0/src/xrec/$(ARCH)/lib /usr/X11R6/lib -libappl wgl x_wgl wgl gd_wgl rmngd png z Xt X11 m


advect-2008: advect-2008.o pdcreatepngimage.o
	s.compile -src advect-2008.ftn90 -obj pdcreatepngimage.o -debug -O 0  -o advect-2008 -libpath $(HOME)/data0/src/xrec/$(ARCH)/lib /usr/X11R6/lib -libappl wgl x_wgl wgl gd_wgl rmngd png z Xt X11 m

test-create: gmm-test-create.o gmm_build_meta.o
	s.compile -obj gmm-test-create.o gmm_build_meta.o -debug -o test-create -librmn rmn_010 -codebeta gmm

test-shuffle: gmm-test-shuffle.o gmm_build_meta.o
	s.compile -obj gmm-test-shuffle.o gmm_build_meta.o ../gmm.o -debug -o test-shuffle -librmn rmn_010 

clean:	
	/bin/rm -f *.mod *.o gmm-test

.SUFFIXES :

.SUFFIXES : .ftn .f .c .o .a .ftn90 .f90 .tmpl90

SUPP_OPT = 

RMNLIB = rmn_014

RMNLIB_BASE_PATH=$(ARMNLIB)

MYLIB = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmnbeta.a

TEMPLIB = ./$(EC_ARCH)/lib_local.a

TARRLS = beta

FICHIERS = whiteboard_interface_st.cdk whiteboard_interface_omp.cdk whiteboard.tmpl90 \
	WhiteBoard.h WhiteBoard.hf WhiteBoard.c whiteboard_st.f90 whiteboard_omp.f90

OBJECTS = whiteboard_omp.o whiteboard_st.o WhiteBoard.o

include $(RPN_TEMPLATE_LIBS)/include/makefile_suffix_rules.inc

all:	whiteboard_st.o whiteboard_omp.o WhiteBoard.o whiteboard_interface_st.cdk whiteboard_interface_omp.cdk

dist:	whiteboard_interface_st.cdk whiteboard_interface_omp.cdk

WhiteBoard.o: WhiteBoard.c
	s.compile -src WhiteBoard.c -O

whiteboard.ftn90: whiteboard.tmpl90
	r.tmpl90.ftn90 whiteboard

whiteboard_interface.cdk90: whiteboard.tmpl90
	r.tmpl90.ftn90 whiteboard

whiteboard_st.f90: whiteboard.ftn90
	s.compile -src whiteboard.ftn90 -O -defines =-DWITHOUT_OpenMP -P
	mv whiteboard.f90 whiteboard_st.f90

whiteboard_st.o: whiteboard_st.f90
	s.compile -src whiteboard_st.f90 -O

whiteboard_interface_st.cdk: whiteboard_interface.cdk90
	s.compile -P -src whiteboard_interface.cdk90 -defines =-DWITHOUT_OpenMP
	mv whiteboard_interface.f90 whiteboard_interface_st.cdk

whiteboard_omp.f90: whiteboard.ftn90
	s.compile -src whiteboard.ftn90 -O -defines =-DWITH_OpenMP -openmp -P
	mv whiteboard.f90 whiteboard_omp.f90

whiteboard_omp.o : whiteboard_omp.f90
	s.compile -src whiteboard_omp.f90 -O -openmp

whiteboard_interface_omp.cdk: whiteboard_interface.cdk90
	s.compile -P -src whiteboard_interface.cdk90 -defines =-DWITH_OpenMP -openmp
	mv whiteboard_interface.f90 whiteboard_interface_omp.cdk

check: whiteboard_interface_st.cdk whiteboard_interface_omp.cdk WhiteBoard.h
	-diff whiteboard_interface_st.cdk $(RPN_TEMPLATE_LIBS)/include/whiteboard_interface_st.cdk
	-diff whiteboard_interface_omp.cdk $(RPN_TEMPLATE_LIBS)/include/whiteboard_interface_omp.cdk
	-diff WhiteBoard.h $(RPN_TEMPLATE_LIBS)/include/WhiteBoard.h

dist: $(FICHIERS)
	ls -al $(FICHIERS) Makefile

tarball: $(FICHIERS) Makefile
	ls -al $(FICHIERS) Makefile tests
	tar cfzv /data/armnraid1/www/ssm/sources/whiteboard_$(TARRLS)_all.tgz $(FICHIERS) Makefile

obj: $(OBJECTS)

gen_ec_arch_dir:
#Creer le repertoire $EC_ARCH
	mkdir -p ./$(EC_ARCH)

locallib: gen_ec_arch_dir \
	$(TEMPLIB)(whiteboard_st.o)	$(TEMPLIB)(whiteboard_omp.o)	$(TEMPLIB)(WhiteBoard.o)
	rm -f $(OBJECTS)

updlib:
#mettre a jour la programmatheque
	r.ar -arch $(EC_ARCH) rcv $(MYLIB) *.o
	if [ "$(UPDATEX)" = "1" ] ; \
	then \
	r.ar -arch $(EC_ARCH) rcv $(LIB_X) *.o ; \
	fi

genlib: $(OBJECTS)
	r.ar  rcv $(MYLIB) $(OBJECTS)
	rm -f $(OBJECTS)

tests: test1 test2 test3 test4 test5

test1:  whiteboard_st.f90 WhiteBoard.c tests/f_logical_move.ftn90
	s.compile -librmn $(RMNLIB) -src whiteboard_st.f90 WhiteBoard.c tests/f_logical_move.ftn90 -o tests/a.out
	rm -f f_logical_move.f90 *.o
	(cd tests ; ./a.out)
	rm -f tests/a.out

test2:  whiteboard_st.f90 WhiteBoard.c tests/whiteboard_test.ftn90
	s.compile -librmn $(RMNLIB) -src whiteboard_st.f90 WhiteBoard.c tests/whiteboard_test.ftn90 -o tests/a.out
	rm -f whiteboard_test.f90 *.o
	(cd tests ; ./a.out)
	rm -f tests/a.out

test3:  whiteboard_st.f90 WhiteBoard.c tests/whiteboard_test2.ftn90
	s.compile -librmn $(RMNLIB) -src whiteboard_st.f90 WhiteBoard.c tests/whiteboard_test2.ftn90 -o tests/a.out
	rm -f whiteboard_test2.f90 *.o
	(cd tests ; ./a.out)
	rm -f tests/a.out

test4:  whiteboard_st.f90 WhiteBoard.c tests/whiteboard_test4.ftn90
	s.compile -librmn $(RMNLIB) -src whiteboard_st.f90 WhiteBoard.c tests/whiteboard_test4.ftn90 -o tests/a.out
	rm -f whiteboard_test4.f90 *.o
	(cd tests ; ./a.out)
	rm -f tests/a.out

test5:  whiteboard_st.f90 WhiteBoard.c tests/whiteboard_test5.ftn90
	s.compile -librmn $(RMNLIB) -src whiteboard_st.f90 WhiteBoard.c tests/whiteboard_test5.ftn90 -o tests/a.out
	rm -f whiteboard_test5.f90 *.o
	(cd tests ; ./a.out)
	rm -f tests/a.out

test6:  whiteboard_st.f90 WhiteBoard.c tests/whiteboard_test6.ftn90
	s.compile -librmn $(RMNLIB) -src whiteboard_st.f90 WhiteBoard.c tests/whiteboard_test6.ftn90 -o tests/a.out
	rm -f whiteboard_test6.f90 *.o
	(cd tests ; ./a.out)
	rm -f tests/a.out

partclean:
	rm -f whiteboard.o whiteboard_st.f90 whiteboard_omp.f90
	rm -f whiteboard.ftn90 whiteboard.f90 whiteboard_interface.cdk90 whiteboard_interface.f90
	rm -rf .fo

distclean:
	rm -f WhiteBoard.o whiteboard_st.o whiteboard_omp.o
	rm -f whiteboard.ftn90 whiteboard.f90 whiteboard_interface.cdk90 whiteboard_interface.f90
	rm -rf .fo

clean:
	rm -f WhiteBoard.o whiteboard_st.o whiteboard_omp.o whiteboard_interface_st.cdk whiteboard_interface_omp.cdk
	rm -f whiteboard.ftn90 whiteboard.f90 whiteboard_interface.cdk90 whiteboard_interface.f90
	rm -f whiteboard_st.f90 whiteboard_omp.f90
	rm -rf .fo

veryclean:
	rm -f WhiteBoard.o whiteboard_st.o whiteboard_omp.o whiteboard_interface_st.cdk whiteboard_interface_omp.cdk
	rm -f whiteboard.ftn90 whiteboard.f90 whiteboard_interface.cdk90 whiteboard_interface.f90
	rm -f $(TEMPLIB) whiteboard_st.f90 whiteboard_omp.f90
	rm -rf .fo

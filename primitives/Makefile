include /usr/local/env/armnlib/include/$(ARCH)$(ABI)/Makefile_addons
# include Makefile_$(ARCH)$(ABI)

.SUFFIXES :

.SUFFIXES : .f90 .ftn .f .c .o

SHELL = /bin/sh

CPP = /lib/cpp

FFLAGS =

CFLAGS = 

OPTIMIZ = -O 2

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmnbeta.a

LIB_X = $(ARMNLIB)/lib/$(ARCH)$(ABI)/librmn_x.a

UPDATEX =

.PRECIOUS: $(MYLIB)

default: obj

rmnlib_v:
	-rm rmnlib_version.f
	./rmnlib_version


#rmnlib_version.o:
#	-rm rmnlib_version.f
#	./rmnlib_version
#	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $*.f

.ftn.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $< -ignorempty

.c.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(CFLAGS)" -options_comp -src $<

.f.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

.f90.o:
	r.compile -arch $(ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $<

FTNDECKS= \
         ieeepak.ftn \
         memoir8.ftn     second.ftn      stkmem.ftn       tracebck.ftn\
         ccard.ftn       sx_timing_stubs.ftn              f_baseio.ftn

CDECKS= \
         c_baseio.c      c_ccard.c       dmms.c           fread32.c \
         getenvc.c       loc_sub.c       mgilib2.c         nice19.c \
         peek.c          poke.c          qqexit.c         read_direct.c \
         resetenv.c      rmtcall.c       set_run_dir.c    vmm.c \
         wkoffit.c       rah2char.c      fmain2cmain.c    calc_crc.c\
         ibm32_ieee.c    second2.c       status_file.c    stkmem2.c\
         prog_filename.c rename_c.c      micro_sleep.c    remove_c.c\
	 gossip_sock.c   gossip_timeout.c f90_threads.c   resident_time.c\
         get_max_rss.c   appl_var.c       get_ccard_arg.c crc32.c\
         f_gettimeofday.c f_bit_slicer.c  gossip_thread.c flush_stdout.c \
         c_env_var_cracker.c

FDECKS= \
         constnt.f       exdb.f \
         iopdatm.f \
         qqqmima.f       small_alloc.f \
         up2low.f\
         qqqcall.f       qqqr8sz.f          qvmcks.f      gnthread.f  ccard_arg_m.f90

OBJECTS= \
         c_baseio.o        c_ccard.o        constnt.o         dmms.o \
         exdb.o            fread32.o \
         getenvc.o        ieeepak.o         iopdatm.o \
         loc_sub.o         memoir8.o        mgilib2.o          nice19.o \
         peek.o            poke.o           qqexit.o          qqqmima.o \
         read_direct.o     resetenv.o       rmnlib_version.o  rmtcall.o \
         second.o          set_run_dir.o    small_alloc.o     stkmem.o \
         sx_timing_stubs.o tracebck.o       up2low.o          vmm.o \
         wkoffit.o         f_baseio.o       qqqcall.o         qqqr8sz.o\
         ccard.o           qvmcks.o         rah2char.o        fmain2cmain.o\
	 calc_crc.o        ibm32_ieee.o     second2.o         status_file.o\
         stkmem2.o         prog_filename.o  rename_c.o        micro_sleep.o\
         remove_c.o        gossip_sock.o    gossip_timeout.o  gnthread.o\
         f90_threads.o     c_env_var_cracker.o get_max_rss.o  resident_time.o\
         appl_var.o        get_ccard_arg.o  ccard_arg_m.o     crc32.o\
         f_gettimeofday.o  f_bit_slicer.o   gossip_thread.o   flush_stdout.o
         
COMDECKS = xxpakc.cdk mgi.h  wafile.h
         
FICHIERS = $(FTNDECKS) $(FDECKS) $(CDECKS) $(COMDECKS)


c_baseio.o:     c_baseio.c  wafile.h  wafile.h 
dmms.o:     	dmms.c
f_baseio.o:     f_baseio.ftn
getenvc.o: 	getenvc.c
ieeepak.o: 	ieeepak.ftn   xxpakc.cdk 
memoir8.o: 	memoir8.ftn 
mgilib.o:	mgilib.c  mgi.h 
peek.o: 	peek.c  
poke.o: 	poke.c  
qqexit.o: 	qqexit.c
rmtcall.o: 	rmtcall.c
second.o: 	second.ftn 
stkmem.o: 	stkmem.ftn 
tracebck.o: 	tracebck.ftn 
ccard.o:        ccard.ftn
sx_timing_stubs.o:sx_timing_stubs.ftn

tarball: $(FICHIERS) rmnlib_v
	tar cfzv /data/armnraid1/www/ssm/primitives_1.0_all.tgz $(FICHIERS) rmnlib_version.f Makefile 
  
obj: $(OBJECTS) 
#Produire les fichiers objets (.o) pour tous les fichiers

genlib: $(OBJECTS)
#Creer ou mettre a jour la programmatheque 
	r.ar -arch $(ARCH) rcv $(MYLIB) $(OBJECTS)

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

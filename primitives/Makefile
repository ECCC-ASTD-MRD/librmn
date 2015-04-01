.SUFFIXES :

.SUFFIXES : .f90 .ftn .f .c .o .a

SHELL = /bin/sh

CPP = /lib/cpp

FFLAGS =

CFLAGS = 

OPTIMIZ = 2

C_SUPP_OPT = -options_comp

FTN_SUPP_OPT = -ignorempty

RMNLIB_BASE_PATH=$(ARMNLIB)

CPPFLAGS = -I$(ARMNLIB)/include

MYLIB = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmnbeta.a

LIB_X = $(RMNLIB_BASE_PATH)/lib/$(EC_ARCH)$(ABI)/librmn_x.a

TEMPLIB = ./templib_$(EC_ARCH)$(ABI).a

UPDATEX =

.PRECIOUS: $(MYLIB)

default: obj

rmnlib_v:
	-rm rmnlib_version.f
	./rmnlib_version


#rmnlib_version.o:
#	-rm rmnlib_version.f
#	./rmnlib_version
#	r.compile -arch $(EC_ARCH) -abi $(ABI) $(OPTIMIZ) -opt "=$(FFLAGS)" -src $*.f

include $(ARMNLIB)/include/makefile_suffix_rules.inc

FTNDECKS= \
         ieeepak.ftn \
         memoir8.ftn     second.ftn      stkmem.ftn       tracebck.ftn\
         ccard.ftn       sx_timing_stubs.ftn              f_baseio.ftn \
         vpow_ibm.ftn

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
         c_env_var_cracker.c array_file.c logical_smt_cpus.c  pointers_and_addresses.c \
         mkdir_c.c       client_timeout.c

FDECKS= \
         constnt.f       exdb.f \
         iopdatm.f \
         qqqmima.f       small_alloc.f \
         up2low.f\
         qqqcall.f       qqqr8sz.f          qvmcks.f      gnthread.f  ccard_arg_m.f90 \
         non_preempt_clock.f 

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
         f_gettimeofday.o  f_bit_slicer.o   gossip_thread.o   flush_stdout.o\
         array_file.o      logical_smt_cpus.o  pointers_and_addresses.o \
         mkdir_c.o         non_preempt_clock.o client_timeout.o vpow_ibm.o
         
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
	tar cfzv /data/armnraid1/www/ssm/sources/primitives_1.0_all.tgz $(FICHIERS) rmnlib_version.f Makefile 
  
obj: $(OBJECTS) 
#Produire les fichiers objets (.o) pour tous les fichiers

locallib: \
         $(TEMPLIB)(c_baseio.o)        $(TEMPLIB)(c_ccard.o)        $(TEMPLIB)(constnt.o)         $(TEMPLIB)(dmms.o) \
         $(TEMPLIB)(exdb.o)            $(TEMPLIB)(fread32.o) \
         $(TEMPLIB)(getenvc.o)         $(TEMPLIB)(ieeepak.o)        $(TEMPLIB)(iopdatm.o) \
         $(TEMPLIB)(loc_sub.o)         $(TEMPLIB)(memoir8.o)        $(TEMPLIB)(mgilib2.o)         $(TEMPLIB)(nice19.o) \
         $(TEMPLIB)(peek.o)            $(TEMPLIB)(poke.o)           $(TEMPLIB)(qqexit.o)          $(TEMPLIB)(qqqmima.o) \
         $(TEMPLIB)(read_direct.o)     $(TEMPLIB)(resetenv.o)       $(TEMPLIB)(rmnlib_version.o)  $(TEMPLIB)(rmtcall.o) \
         $(TEMPLIB)(second.o)          $(TEMPLIB)(set_run_dir.o)    $(TEMPLIB)(small_alloc.o)     $(TEMPLIB)(stkmem.o) \
         $(TEMPLIB)(sx_timing_stubs.o) $(TEMPLIB)(tracebck.o)       $(TEMPLIB)(up2low.o)          $(TEMPLIB)(vmm.o) \
         $(TEMPLIB)(wkoffit.o)         $(TEMPLIB)(f_baseio.o)       $(TEMPLIB)(qqqcall.o)         $(TEMPLIB)(qqqr8sz.o)\
         $(TEMPLIB)(ccard.o)           $(TEMPLIB)(qvmcks.o)         $(TEMPLIB)(rah2char.o)        $(TEMPLIB)(fmain2cmain.o)\
         $(TEMPLIB)(calc_crc.o)        $(TEMPLIB)(ibm32_ieee.o)     $(TEMPLIB)(second2.o)         $(TEMPLIB)(status_file.o)\
         $(TEMPLIB)(stkmem2.o)         $(TEMPLIB)(prog_filename.o)  $(TEMPLIB)(rename_c.o)        $(TEMPLIB)(micro_sleep.o)\
         $(TEMPLIB)(remove_c.o)        $(TEMPLIB)(gossip_sock.o)    $(TEMPLIB)(gossip_timeout.o)  $(TEMPLIB)(gnthread.o)\
         $(TEMPLIB)(f90_threads.o)     $(TEMPLIB)(c_env_var_cracker.o) $(TEMPLIB)(get_max_rss.o)  $(TEMPLIB)(resident_time.o)\
         $(TEMPLIB)(appl_var.o)        $(TEMPLIB)(get_ccard_arg.o)  $(TEMPLIB)(ccard_arg_m.o)     $(TEMPLIB)(crc32.o)\
         $(TEMPLIB)(f_gettimeofday.o)  $(TEMPLIB)(f_bit_slicer.o)   $(TEMPLIB)(gossip_thread.o)   $(TEMPLIB)(flush_stdout.o)\
         $(TEMPLIB)(array_file.o)      $(TEMPLIB)(logical_smt_cpus.o)  $(TEMPLIB)(pointers_and_addresses.o)\
         $(TEMPLIB)(mkdir_c.o)         $(TEMPLIB)(non_preempt_clock.o) $(TEMPLIB)(client_timeout.o) $(TEMPLIB)(vpow_ibm.o)

genlib: rmnlib_v $(OBJECTS)
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
	rm *.o

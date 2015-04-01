#!/bin/ksh
r.compile -O 2 -src RPN_COMM.cdk90 RPN_COMM*.c -mpi -openmp
r.compile -O 2 -src RPN_COMM*.f -mpi -openmp
ar rcv ${1:-librpn_commbeta_${EC_ARCH}}.a *.o
rm -f *.o *.mod rii_files .fo


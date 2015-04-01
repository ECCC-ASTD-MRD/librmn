#!/bin/ksh
# used by the ssm distribution to update library from source
r.compile -O 2 -src bmf*.cdk90 bmf*.f90 bmf*.c -mpi -openmp
ar rcv ${1:-libbmfbeta_${EC_ARCH}}.a *.o
rm -f *.o *.mod rii_files .fo


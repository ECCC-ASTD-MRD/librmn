all:
	icc -DLittle_Endian -I.. -I../../PUBLIC_INCLUDES -fp-model precise -mkl -msse3 -traceback -Wtrigraphs -qopenmp -O2 -g test.c -lrmn -o test_c
	ifort -align array32byte -assume byterecl -convert big_endian -fp-model source -fpe0 -mkl -msse3 -traceback -stand f08 -diag-disable 5140 -diag-disable 7713 -diag-disable 10212 -qopenmp -O2 -g -I.. -I../../PUBLIC_INCLUDES test.F90 -lrmn -o test_f
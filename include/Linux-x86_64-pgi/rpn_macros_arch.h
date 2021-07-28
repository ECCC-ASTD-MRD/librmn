//! Name mangling for Fortran functions without underscores in their name
#define f77name(a) a##_
//! Name mangling for Fortran functions with underscores in their name
#define f77_name(a) a##_
#ifndef Little_Endian
#define Little_Endian
#endif
#define PTR_AS_INT long long
#define INT_32 int
#define INT_64 long long
#define tell(fdesc) lseek(fdesc,0,1)
#define FORTRAN_loc_delta            4
#define wordint INT_32
#define ftnword INT_32
#define ftnfloat float
#define wordfloat float
#define bytesperword 4
#define D77MULT             4
// F2Cl is an hidden parameter of Fortran compilers for string length.
// The type of the argument is implementation specific and can be different
// according to the compiler
#define F2Cl INT_32

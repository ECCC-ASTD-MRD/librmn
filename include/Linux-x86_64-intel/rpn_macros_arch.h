//! Name mangling for Fortran functions without underscores in their name
#define f77name(a) a##_
//! Name mangling for Fortran functions with underscores in their name
#define f77_name(a) a##_

#ifndef Little_Endian
#define Little_Endian
#endif

#define tell(fdesc) lseek(fdesc,0,1)

#define wordint int32_t
#define ftnword int32_t
#define ftnfloat float
#define wordfloat float
#define bytesperword 4

// Used only in c_fnom defined in c_baseio.c and passed to the qqqf7op function
#define D77MULT 4

// F2Cl is an hidden parameter of Fortran compilers for string length.
// The type of the argument is implementation specific and can be different
// according to the compiler
#define F2Cl long long

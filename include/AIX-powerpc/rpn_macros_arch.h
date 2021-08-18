//! Name mangling for Fortran functions without underscores in their name
#define f77name(a) a##_
//! Name mangling for Fortran functions with underscores in their name
#define f77_name(a) a##_

#ifndef Big_Endian
#define Big_Endian
#endif

#define wordint int32_t
#define ftnword int32_t
#define ftnfloat float
#define wordfloat float
#define bytesperword 4

// Used only in c_fnom defined in c_baseio.c and passed to the qqqf7op function
#define D77MULT 4

#define F2Cl int

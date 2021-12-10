//! \file rpn_macros_arch.h Architecture and compiler specific definitions.  This file should never be included directly; use rpnmacros.h

#include <stdint.h>

//! Name mangling for Fortran functions without underscores in their name
#define f77name(a) a##_
//! Name mangling for Fortran functions with underscores in their name
#define f77_name(a) a##_

#ifndef Little_Endian
#define Little_Endian
#endif

#define tell(fdesc) lseek(fdesc,0,1)

// F2Cl is an hidden parameter of Fortran compilers for string length.
// The type of the argument is implementation specific and can be different
// according to the compiler
#define F2Cl int64_t

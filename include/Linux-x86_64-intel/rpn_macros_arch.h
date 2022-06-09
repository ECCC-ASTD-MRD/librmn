//! \file rpn_macros_arch.h Architecture and compiler specific definitions.  This file should never be included directly; use rpnmacros.h

#include <stdint.h>

//! Name mangling for Fortran functions without underscores in their name
#define f77name(a) a##_
//! Name mangling for Fortran functions with underscores in their name
#define f77_name(a) a##_

// F2Cl is an hidden parameter of Fortran compilers for string length.
// The type of the argument is implementation specific and can be different
// according to the compiler
#define F2Cl const int64_t

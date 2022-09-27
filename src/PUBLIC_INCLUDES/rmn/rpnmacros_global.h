//! \file rpnmacros_global.h Shared definitions.  This file should never be included directly; use rpnmacros.h

#ifndef _RPN_MACROS_GLOBAL_
#define _RPN_MACROS_GLOBAL_

#include <stdint.h>

//! \deprecated Use the type from stdint.h directly and iso_c_bindings instead
#define F77_INTEGER int32_t
//! \deprecated Use the float type directly and iso_c_bindings instead
#define F77_REAL float
//! \deprecated Use the type from stdint.h directly and iso_c_bindings instead
#define F77_INTEGER8 int64_t
//! \deprecated Use the double type directly and iso_c_bindings instead
#define F77_REAL8 double
//! \deprecated Use the char type directly and iso_c_bindings instead
#define F77_CHARACTER char

//! \deprecated Use the iso_c_bindings instead
#define F77_LOGICAL_ARG(X) void *X

#endif

//! \file rpnmacros_global.h Shared definitions.  This file should never be included directly; use rpnmacros.h

#ifndef _RPN_MACROS_GLOBAL_
#define _RPN_MACROS_GLOBAL_

#include <stdint.h>

#define F77_INTEGER   int32_t
#define F77_REAL      float
#define F77_INTEGER8  int64_t
#define F77_REAL8     double
#define F77_CHARACTER char

#define F77_LOGICAL_ARG(X)   void *X

#endif

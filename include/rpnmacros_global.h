//! \file rpnmacros_global.h Shared definitions.  This file should never be included directly; use rpnmacros.h

#if !defined (_RPN_MACROS_GLOBAL_)
#define _RPN_MACROS_GLOBAL_ _rpn_macros_global_

#include <stdint.h>

#define F77_INTEGER   int32_t
#define F77_REAL      float
#define F77_INTEGER8  int64_t
#define F77_REAL8     double
#define F77_CHARACTER char

#define F77_LOGICAL_ARG(X)   void *X

#define CONCAT(A,B) A##B
#define F77STRLEN(X) CONCAT(X,_length)
#define HIDDENLEN(X) , F2Cl F77STRLEN(X)

#if defined(_AIX)
#define __AIX__ 
#define AIX IBM_AIX
#endif

#endif

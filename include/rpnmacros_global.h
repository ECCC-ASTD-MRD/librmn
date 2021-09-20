//! \file rpnmacros_global.h Shared definitions.  This file should never be included directly; use rpnmacros.h

#if !defined (_RPN_MACROS_GLOBAL_)
#define _RPN_MACROS_GLOBAL_ _rpn_macros_global_

//! \deprecated This macro will be removed from future releases.  Please use the types defined stdint.h instead.
#define wordint int32_t

//! \deprecated This macro will be removed from future releases.  Please use the types defined stdint.h instead.
#define ftnword int32_t

//! \deprecated This macro will be removed from future releases.  Please use the types defined stdint.h instead.
#define ftnfloat float

//! \deprecated This macro will be removed from future releases.  Please use the types defined stdint.h instead.
#define wordfloat float

//! \deprecated This macro will be removed from future releases.  Please use the types defined stdint.h instead.
#define word uint32_t

//! \deprecated This macro will be removed from future releases.  Please use the types defined stdint.h instead.
#define bytesperword 4

// Used only in c_fnom defined in c_baseio.c and passed to the qqqf7op function
#define D77MULT 4

#define F77_INTEGER   int32_t
#define F77_REAL      float
#define F77_INTEGER8  int64_t
#define F77_REAL8     double
#define F77_CHARACTER char

#define F77_LOGICAL_ARG(X)   void *X

#define CONCAT(A,B)          A##B
#define F77STRLEN(X)       CONCAT(X,_length)
#define HIDDENLEN(X)       , F2Cl F77STRLEN(X)

#if defined(_AIX)
#define __AIX__ 
#define AIX IBM_AIX
#endif

#endif

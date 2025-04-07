#ifndef _RPN_MACROS_
#define _RPN_MACROS_ _rpn_macros_

/*! \file rpnmacros.h Platform and cmpiler specific definitions */

#include "rpnmacros_global.h"

/*
All the comments in this file need to be old-style blocs comments, since
it's also included by Fortran code
*/


/*
For now, all supported platforms and compilers mangle names the same
way.  This might change and the future and have to be moved to be
arch/compiler specific
*/

/*! Name mangling for Fortran functions without underscores in their name */
#define f77name(a) a##_
/*! Name mangling for Fortran functions with underscores in their name */
#define f77_name(a) a##_


/*
F2Cl is an hidden parameter of Fortran compilers for string length.
The type of the argument is implementation specific and can be different
according to the compiler
*/


#ifdef __x86_64__
#   ifndef Little_Endian
#       define Little_Endian
#   endif

#if defined (__INTEL_COMPILER) || defined (__INTEL_LLVM_COMPILER)
        typedef const int64_t F2Cl;
#   elif __clang__
        typedef const int32_t F2Cl;
#   elif __PGI
        typedef const int32_t F2Cl;
#   elif __GNUC__
        /* Since we tested for the other compilers first, this is probably really GCC */
        typedef const int32_t F2Cl;
#   else
#       error "Unknown arch/compiler combo!"
#   endif
#endif


#ifdef __aarch64__
#   ifdef __PGI
        typedef const int64_t F2Cl;
#   else
        typedef const int32_t F2Cl;
#   endif
#endif


#ifdef __PPC64__
#   ifdef __GNUC__
        typedef const int32_t F2Cl;
#       ifdef __LITTLE_ENDIAN__
#           define Little_Endian
#       endif
#   endif
#endif


static F2Cl unknown_arch_compiler_combo = 0; // Will fail if F2Cl has not been defined

#endif

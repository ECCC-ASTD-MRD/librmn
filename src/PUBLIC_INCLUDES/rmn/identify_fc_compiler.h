/*
 * Copyright (C) 2025  Environnement et Changement climatique Canada
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * Author:
 *     M. Valin,   Recherche en Prevision Numerique, 2023
 *
 * attempt to identify the C or Fortran compiler used to compile code including this file
*/
#undef ADDRESS_SIZE
#if defined(__x86_64__) || defined(__LP64__)
#define ADDRESS_SIZE 64
#elif defined(__i386__)
#define ADDRESS_SIZE 32
#else
#define ADDRESS_SIZE -1
#endif

#undef FORTRAN_COMPILER_ID
#undef C_COMPILER_ID
#undef COMPILER_IS_INTEL_LLVM
#undef COMPILER_IS_INTEL_OLD
#undef COMPILER_IS_PGI
#undef COMPILER_IS_CLANG
#undef COMPILER_IS_FLANG_AOCC
#undef COMPILER_IS_FLANG
#undef COMPILER_IS_GNU
#undef COMPILER_IS_UNKNOWN

#if defined(__STDC_VERSION__)

#if defined(__INTEL_LLVM_COMPILER) && defined(__STDC_VERSION__)
#define COMPILER_IS_INTEL_LLVM
#define C_COMPILER_ID "INTEL_ICX"

#elif defined(__INTEL_COMPILER) && defined(__STDC_VERSION__)
#define COMPILER_IS_INTEL_OLD
#define C_COMPILER_ID "INTEL_ICC"

#elif defined(__PGI)
#define COMPILER_IS_PGI
#define C_COMPILER_ID "PGI/Nvidia"

#elif defined(__clang__)
#define COMPILER_IS_CLANG
#define C_COMPILER_ID "CLANG"

#elif defined(__GNUC__)
#define COMPILER_IS_GNU
#define C_COMPILER_ID "GNU"

#else
#define COMPILER_IS_UNKNOWN
#define C_COMPILER_ID "UNKNOWN"

#endif


#else

#undef C_COMPILER_ID

#if defined(__GFORTRAN__)
#define COMPILER_IS_GNU
#define FORTRAN_COMPILER_ID 'GNU'

#elif defined(__INTEL_LLVM_COMPILER) && ! defined(__STDC_VERSION__)
#define COMPILER_IS_INTEL_LLVM
#define FORTRAN_COMPILER_ID 'INTEL_IFX'

#elif defined(__INTEL_COMPILER) && ! defined(__STDC_VERSION__)
#define COMPILER_IS_INTEL_OLD
#define FORTRAN_COMPILER_ID 'INTEL_IFORT'

#elif defined(__PGIF90__)
#define COMPILER_IS_PGI
#define FORTRAN_COMPILER_ID 'PGI/Nvidia'

#elif defined(__FLANG)
#define COMPILER_IS_FLANG_AOCC
#define FORTRAN_COMPILER_ID 'FLANG_AOCC'

#elif defined(__flang__)
#define COMPILER_IS_FLANG
#define FORTRAN_COMPILER_ID 'FLANG'

#else
#define COMPILER_IS_UNKNOWN
#define FORTRAN_COMPILER_ID 'UNKNOWN'

#endif

#endif

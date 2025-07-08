/*
 * Copyright (C) 2023  Environnement et Changement climatique Canada
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
 * attempt to identify the C compiler used to compile code including this file
*/

char *identify_c_compiler();
int identify_address_mode();

#if ! defined(IDENTIFY_C_COMPILER)
#define IDENTIFY_C_COMPILER

#if defined(__x86_64__)
// static int ADDRESS_MODE = 64 ;
#define ADDRESS_MODE 64

#elif defined(__i386__)
static int ADDRESS_MODE = 32 ;
#define ADDRESS_MODE 32

#else
// static int ADDRESS_MODE = -1 ;
#define ADDRESS_MODE -1

#endif

#if defined(__INTEL_LLVM_COMPILER)
// #warning "ICX detected"
#define COMPILER_IS_ICX
#define C_COMPILER_NAME "INTEL_ICX"
//   static char C_COMPILER_NAME[] = "INTEL_ICX" ;  // icx

#elif defined(__INTEL_COMPILER)
// #warning "ICC detected"
#define COMPILER_IS_ICC
#define C_COMPILER_NAME "INTEL_ICC"
//   static char C_COMPILER_NAME[] = "INTEL_ICC" ;  // icc

#elif defined(__PGI)
// #warning "PGI detected"
#define COMPILER_IS_PGI
#define C_COMPILER_NAME "PGI/Nvidia"
//   static char C_COMPILER_NAME[] = "PGI/Nvidia" ;   // pgcc nvcc

#elif defined(__clang__)
// #warning "CLANG detected"
#define COMPILER_IS_CLANG
#define C_COMPILER_NAME "CLANG"
//   static char C_COMPILER_NAME[] = "CLANG" ;        // llvm/aocc clang

#elif defined(__GNUC__)
// #warning "GNU detected"
#define COMPILER_IS_GCC
#define C_COMPILER_NAME "GCC"
//   static char C_COMPILER_NAME[] = "GNU" ;          // gcc or lookalike

#else
// #warning "UNKNOWN detected"
#define COMPILER_IS_UNKNOWN
#define C_COMPILER_NAME "UNKNOWN"
//   static char C_COMPILER_NAME[] = "UNKNOWN" ;      // unknown

#endif

#endif

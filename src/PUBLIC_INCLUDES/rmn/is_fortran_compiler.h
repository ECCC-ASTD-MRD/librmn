#if 0
  Copyright (C) 2023  Environnement et Changement climatique Canada

  This is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation,
  version 2.1 of the License.

  This software is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  Author:
      M. Valin,   Recherche en Prevision Numerique, 2023

  attempt to automatically set IN_FORTRAN_CODE
  if a Fortran compiler is positively identified as such,  macro IN_FORTRAN_CODE will be set

  usage :
     #include <rmn/is_fortran_compiler.h>
  N.B.
    macro FORTRAN_COMPILER_KIND consistent with FORTRAN_COMPILER from identify_compiler_f.hf
#endif

#if ! defined(IN_FORTRAN_CODE)

#if defined(__GFORTRAN__)
/* GNU gfortran */
#define IN_FORTRAN_CODE 1
#define FORTRAN_COMPILER_KIND "GNU"

#elif defined(__INTEL_LLVM_COMPILER)
/* macro unfortunately also set for the C compiler */

#elif defined(__INTEL_COMPILER)
/* macro unfortunately also set for the C compiler */

#elif defined(__PGIF90__)
/* Nvidia/PGI compiler */
#define IN_FORTRAN_CODE 1
#define FORTRAN_COMPILER_KIND "PGI/Nvidia"

#elif defined(__FLANG)
/* aocc flang compiler */
#define IN_FORTRAN_CODE 1
#define FORTRAN_COMPILER_KIND "FLANG-TRADITIONAL"

#elif defined(__flang__)
/* llvm flang-new (f18) compiler */
#define IN_FORTRAN_CODE 1
#define FORTRAN_COMPILER_KIND "FLANG-LLVM"

#endif

#endif

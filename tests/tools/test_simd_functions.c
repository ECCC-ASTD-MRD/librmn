//
// Copyright (C) 2024  Environnement Canada
//
// This is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation,
// version 2.1 of the License.
//
// This software is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details .
//
// Author:
//     M. Valin,   Recherche en Prevision Numerique, 2024
//
// test the SIMD functions
//
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <rmn/test_helpers.h>

// use emulated intrinsics
#define NO_SIMD
#define ALIAS_INTEL_SIMD_INTRINSICS

// verbose mode for include file rmn/simd_functions.h
#define VERBOSE_SIMD
#include <rmn/simd_functions.h>
#include <rmn/simd_functions.h>   // deliberate double inclusion

#undef REFERENCE

#include "test_simd_functions_body.h"

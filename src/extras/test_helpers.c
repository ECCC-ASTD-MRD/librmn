/*
 * Hopefully useful code for C
 * Copyright (C) 2023-2024  Recherche en Prevision Numerique
 *
 * This code is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This code is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include <rmn/test_helpers.h>

// Fortran interface
void StartOfTest(char *name){ start_of_test(name) ; }

// common Fortran manglings
void start_of_test_(char *name){ start_of_test(name) ; }
void start_of_test__(char *name){ start_of_test(name) ; }

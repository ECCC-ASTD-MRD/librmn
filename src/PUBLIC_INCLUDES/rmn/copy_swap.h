// Hopefully useful functions for C and FORTRAN
// Copyright (C) 2024  Recherche en Prevision Numerique
//
// This code is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation,
// version 2.1 of the License.
//
// This code is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Library General Public License for more details.
//
#if ! defined(COPY_SWAP_H)
#define COPY_SWAP_H
#include <stdint.h>

static uint32_t word32 = 1 ;
static uint8_t *le = (uint8_t *) &word32 ;

#if defined(__AVX2__) && defined(__x86_64__)
void FetchShuffleStore(void *src, void *dst, uint8_t *indx, int nbytes);
#endif

void Swap_08_16(void *src, void *dst, int n16);
void Swap_08_32(void *src, void *dst, int n32);
void Swap_08_64(void *src, void *dst, int n64);
void Swap_16_32(void *src, void *dst, int n32);
void Swap_16_64(void *src, void *dst, int n64);
void Swap_32_64(void *src, void *dst, int n64);

int Copy_items_r2l(void *src, uint32_t srclen, void *dst, uint32_t dstlen, uint32_t ns);
int Copy_items_l2r(void *src, uint32_t srclen, void *dst, uint32_t dstlen, uint32_t ns);

#endif

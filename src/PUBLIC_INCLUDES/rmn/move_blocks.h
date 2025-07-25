//
// Copyright (C) 2023  Environnement Canada
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
//     M. Valin,   Recherche en Prevision Numerique, 2023
//

// 2 D data block movers/analyzers
#if ! defined(move_w32_block)

#include <stdlib.h>
#include <stdint.h>

#include <rmn/data_kind.h>
#include <rmn/data_properties.h>
#include <rmn/ieee_extras.h>

// move_w32_block(void *restrict src, int lnis, void *restrict dst, int lnid, int ni, int nj [, block_properties *bp] );
// generic interface to block movers. bp MUST BE ABSENT if src is not a pointer to int/unsigned int/float
#define move_w32_block(src,...) _Generic((src), \
                                                   int32_t  *: move_int32_block,  \
                                                   uint32_t *: move_uint32_block, \
                                                   float    *: move_float_block,  \
                                                   void     *: move_mem32_block,  \
                                                   default   : move_mem32_block   \
                                                   ) (src,__VA_ARGS__)

int move_uint32_block(uint32_t *src, int lnis, void *dst, int lnid, int ni, int nj, block_properties *bp);
int move_int32_block(int32_t   *src, int lnis, void *dst, int lnid, int ni, int nj, block_properties *bp);
int move_float_block(float     *src, int lnis, void *dst, int lnid, int ni, int nj, block_properties *bp);
int move_data32_block(void     *src, int lnis, void *dst, int lnid, int ni, int nj, block_properties *bp);
int move_mem32_block(void      *src, int lnis, void *dst, int lnid, int ni, int nj);

void print_float_props(block_properties bp);
void print_int_props(block_properties bp);

int analyze_data32_block(void *src, int lnis, int ni, int nj, block_properties *bp);
void adjust_block_properties(block_properties *bp, data_kind datatype);
void add_block_properties(block_properties *bp, block_properties *bp_extra);

#endif

// Hopefully useful code for C
// Copyright (C) 2024-2025  Recherche en Prevision Numerique
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
// Author:
//     M. Valin,   Recherche en Prevision Numerique, 2025

#if ! defined(NULL_BLOCK_PROPERTIES)

#include <rmn/data_kind.h>

// basic block block properties
typedef struct{
  iuf32_t  maxs ;      // max signed value in block (meaningless for unsigned data)
  iuf32_t  mins ;      // min signed value in block (meaningless for unsigned data)
  iuf32_t  minu ;      // min unsigned value in block
  iuf32_t  maxu ;      // max unsigned value in block
  int32_t  zeros ;     // number of ZERO values in block (-1 if unknown)
  data_kind kind ;     // data type (signed / unsigned / float / unknown / ... )
} block_properties ;

#define NULL_BLOCK_PROPERTIES (block_properties) {.maxs = {0}, .mins = {0}, .minu = {0}, .maxu = {0}, .zeros = -1 , .kind = bad_data }

static inline int int_max_abs(block_properties bp){
  uint32_t max1, max2 ;
  if(bp.maxs.i <= 0) return -bp.mins.i ;  // all negative or 0
  if(bp.mins.i >= 0) return bp.maxs.i ;   // all positive or 0
  max1 = bp.maxs.i ;                      // largest positive value
  max2 = -bp.mins.i ;                     // negative value with largest absolute value
  return (max1 > max2) ? max1 : max2 ;
}

static inline int int_min_abs(block_properties bp){
  uint32_t min1, min2 ;
  if(bp.maxs.i <= 0) return -bp.maxs.i ;  // all negative or 0
  if(bp.mins.i >= 0) return bp.mins.i ;   // all positive or 0
  min1 = bp.maxu.i ;                      // smallest positive value
  min2 = -bp.maxu.i ;                     // negative value closest to zero
  return (min1 < min2) ? min1 : min2 ;
}

#define FLOAT_MAX_VALUE(BP) ( ((BP).kind == float_data) ? (BP).maxs.f : fp32_nan(0) )
#define FLOAT_MIN_VALUE(BP) ( ((BP).kind == float_data) ? (BP).mins.f : fp32_nan(0) )
#define FLOAT_MAX_ABS(BP)   ( ((BP).kind == float_data) ? (BP).maxu.f : fp32_nan(0) )
#define FLOAT_MIN_ABS(BP)   ( ((BP).kind == float_data) ? (BP).minu.f : fp32_nan(0) )

#define INT_MAX_VALUE(BP)   ( ((BP).kind == int_data) ? (BP).maxs.i     : 0x80000000 )
#define INT_MIN_VALUE(BP)   ( ((BP).kind == int_data) ? (BP).mins.i     : 0x7FFFFFFF )
#define INT_MAX_ABS(BP)     ( ((BP).kind == int_data) ? int_max_abs(BP) : 0x00000000 )
#define INT_MIN_ABS(BP)     ( ((BP).kind == int_data) ? int_min_abs(BP) : 0xFFFFFFFF )

#define UINT_MAX_VALUE(BP)   ( ((BP).kind == uint_data) ? (BP).maxu.u : 0x00000000u )
#define UINT_MIN_VALUE(BP)   ( ((BP).kind == uint_data) ? (BP).minu.u : 0xFFFFFFFFu )
#define UINT_MAX_ABS(BP)     ( ((BP).kind == uint_data) ? (BP).maxu.u : 0x00000000u )
#define UINT_MIN_ABS(BP)     ( ((BP).kind == uint_data) ? (BP).minu.u : 0xFFFFFFFFu )

#endif

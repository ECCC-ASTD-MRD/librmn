/*
 * Hopefully useful code for C and Fortran
 * Copyright (C) 2022-2025  Recherche en Prevision Numerique
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
#include <stdint.h>
#include <string.h>

// uncomment de following line ti use extra fetch rather than permute
// #define WITH_FETCH

// uncomment the following line to only use PLAIN C code
// #define USE_PLAIN_C

// uncomment the following line to use emulated Intel SIMD intrinsics
// #define ALIAS_INTEL_SIMD_INTRINSICS

// comment the following line to activate SIMD code
// #define EMULATE_SIMD

// use SIMD intrinsics by default
#include <rmn/simd_functions.h>
#include <rmn/lorenzo.h>

// SIMD target is AVX2 (256 bit)
// predict the bottom row (1D prediction)
// row  : bottom row
// diff : prediction for bottom row
// n    : number of values in row
// NOTE : this version DOES NOT WORK IN PLACE
static inline void LorenzoPredictRow0(int32_t * restrict row, int32_t * restrict diff, int n){
#if ! defined(USE_PLAIN_C)
    __v256i vi, vi1 ;
    int i0, ii0 ;
#endif
  int i ;
  diff[0] = row[0] ;
#if ! defined(USE_PLAIN_C)
  if(n < 9){   // the SIMD version will not work for n < 9
    for(i=1 ; i<n ; i++) diff[i] = row[i] - row[i-1] ;
    return ;
  }
  for(ii0 = 1 ; ii0 < n ; ii0 += 8) {
    i0 = (ii0 > n-8) ? (n-8) : ii0 ;
    vi   = loadu_v256((__v256i *) (row+i0)  ) ;
    vi1  = loadu_v256((__v256i *) (row+i0-1)) ;
    storeu_v256( (__v256i *) (diff+i0), sub_v8i( vi, vi1 ) ) ;
  }
#else    // ! defined(USE_PLAIN_C)
  for(i=1 ; i<n ; i++) diff[i] = row[i] - row[i-1] ;
#endif   // ! defined(USE_PLAIN_C)
}

// bottom row, n < 8, in place prediction
// row  : bottom row
// n    : number of values in row
static inline void LorenzoPredictRow0_inplace_07(int32_t * restrict row, int n){
  int i, n7 = (n & 7) ;
  int32_t r[7] ;
  r[0] = row[0] ;
  for(i=1 ; i<n7 ; i++) r[i] = row[i] - row[i-1] ;
  for(i=0 ; i<n7 ; i++) row[i] = r[i] ;
}

// bottom row, in place prediction
#if ! defined(USE_PLAIN_C)
// SIMD target is AVX2 (256 bit)
// row  : bottom row
// n    : number of values in row
static inline void LorenzoPredictRow0_inplace(int32_t * restrict row, int n){
  int i0, j0, n7 = (n & 7) ;
  __v256i vi, vi1, vt, vr, v0, vs ;

  if(n < 8) {
    LorenzoPredictRow0_inplace_07(row, n) ;
    return ;
  }

  n7 = n7 ? n7 : 8 ;
  v0   = zero_v256() ;                         // 0s
  vs   = cvt_v8c_v8i( _mm_set1_epi64x(0x0605040302010000lu) ) ;  // shuffle patterm
  // first 8 elements
  vi   = loadu_v256((__v256i *) (row)  ) ;        // row[0:7]
  vi1  = permutev_v8i(vi, vs) ;                   // 0, 0, 1, 2, 3, 4, 5, 6  permutation
  vi1  = blend_v8i(vi1, v0, 1) ;                  // first element set to 0 (row[-1])
  // row[i] - row[i-1]
  vt   = sub_v8i( vi, vi1 ) ;
  vr   = vt ;
  j0 = 0 ;
  // chunks of 8 elements (second chunk may overlap first chunk)
  for(i0=n7 ; i0<n ; i0+=8){
    vi   = loadu_v256((__v256i *) (row+i0)  ) ;   // row[i0:i0+7]
#if defined(WITH_FETCH)
    vi1  = loadu_v256((__v256i *) (row+i0-1)) ;   // row[i0-1:i0+6]
#else
    v0   = set1_v8i(row[i0-1]) ;                  // row[i0-1]
    vi1  = permutev_v8i(vi, vs) ;                 // 0, 0, 1, 2, 3, 4, 5, 6  permutation
    vi1  = blend_v8i(vi1, v0, 1) ;                // first element set to row[i0-1]
#endif
    // row[i0+i] - row[i0+i-1]
    vt   = sub_v8i( vi, vi1 ) ;
    storeu_v256( (__v256i *) (row+j0), vr) ;      // delayed store, result of previous pass
    vr = vt ;
    j0 = i0 ;
  }
  storeu_v256( (__v256i *) (row+j0), vr) ;        // delayed store, result of previous pass
}

#else    // ! defined(USE_PLAIN_C)
// plain C version
// row  : bottom row
// n    : number of values in row
static inline void LorenzoPredictRow0_inplace(int32_t * restrict row, int n){
  int i, i0, j0, n7 = (n & 7) ;
  int32_t t[8], r[8], r0 ;

  if(n < 8) {
    LorenzoPredictRow0_inplace_07(row, n) ;
    return ;
  }
  r0 = row[0] ;
  n7 = n7 ? n7 : 8 ;
  // first 8 elements
  for(i=0 ; i<8 ; i++) r[i] = row[i] - row[i-1] ;
  j0 = 0 ;
  // chunks of 8 elements (second chunk may overlap first chunk)
  for(i0=n7 ; i0<n ; i0+=8){
    for(i=0 ; i<8 ; i++) t[i] = row[i0+i] - row[i0+i-1] ;
    for(i=0 ; i<8 ; i++) row[j0+i] = r[i] ;
    for(i=0 ; i<8 ; i++) r[i] = t[i] ;
    j0 = i0 ;
  }
  for(i=0 ; i<8 ; i++) row[j0+i] = r[i] ;
  row[0] = r0 ;
}

#endif   // ! defined(USE_PLAIN_C)

// all rows but bottom row, not in place prediction
// predict row j where j > 0 (2D prediction)
// top  : row j
// bot  : row (j - 1)
// diff : prediction error for row j
// n    : number of values in row
// this function WILL NOT WORK IN-PLACE (i.e. if diff == top)
static inline void LorenzoPredictRowJ(int32_t * restrict top, int32_t * restrict bot, int32_t * restrict diff, int n){
#if ! defined(USE_PLAIN_C)
    __v256i vi, vi1, vj1, vij1 ;
    int i0, ii0 ;
#endif   // ! defined(USE_PLAIN_C)
  int i ;
  diff[0] = top[0] - bot[0] ;         // first point in row, 1D prediction using row below

#if ! defined(USE_PLAIN_C)
  if(n < 9){   // the SIMD version will not work for n < 9
    for(i=1 ; i<n ; i++) diff[i] = top[i] - ( top[i-1] + bot[i] - bot[i-1] ) ;
    return ;
  }
  for(ii0 = 1 ; ii0 < n ; ii0 += 8) {
    i0 = (ii0 > n-8) ? (n-8) : ii0 ;
    vi   = loadu_v256((__v256i *) (top+i0)  ) ;   // top[i]
    vi1  = loadu_v256((__v256i *) (top+i0-1)) ;   // top[i-1]
    vj1  = loadu_v256((__v256i *) (bot+i0)  ) ;   // bot[i]
    vij1 = loadu_v256((__v256i *) (bot+i0-1)) ;   // bot[i-1]
    // predicted[i,j] = z[i-1,j] + z[i,j-1] - z[i-1] = top[i-1] + bot[i] - bot[i-1]
    // diff[i,j] = orig[i,j] - predicted[i,j]
    storeu_v256( (__v256i *) (diff+i0), sub_v8i( vi, sub_v8i( add_v8i(vi1, vj1) , vij1 ) ) );
  }
#else    // ! defined(USE_PLAIN_C)
  for(i=1 ; i<n ; i++) diff[i] = top[i] - ( top[i-1] + bot[i] - bot[i-1] ) ;
#endif   // ! defined(USE_PLAIN_C)
}

// all rows but bottom row, n < 8, in place prediction
// predict row j where j > 0 (2D prediction)
// top  : row j
// bot  : row (j - 1)
// n    : number of values in row
static void LorenzoPredictRowJ_inplace_07(int32_t * restrict top, int32_t * restrict bot, int n){
  int i, n7 = (n & 7) ;
  int32_t r[7] ;
  r[0] = top[0] - bot[0] ;
  for(i=1 ; i<n7 ; i++) r[i] = top[i] - ( top[i-1] + bot[i] - bot[i-1] ) ;
  for(i=0 ; i<n7 ; i++) top[i] = r[i] ;
}

// all rows but bottom row, in place prediction, n >= 8
// predict row j where j > 0 (2D prediction)
// top  : row j
// bot  : row (j - 1)
// n    : number of values in row
#if ! defined(USE_PLAIN_C)

static inline void LorenzoPredictRowJ_inplace(int32_t * restrict top, int32_t * restrict bot, int n){
  int i0, j0, n7 = (n & 7) ;
  __v256i vi, vi1, vj1, vij1, vt, vr, v0, vs ;

  if(n < 8) {
    LorenzoPredictRowJ_inplace_07(top, bot, n) ;
    return ;
  }
  n7 = n7 ? n7 : 8 ;
  v0   = zero_v256() ;                         // 0s
  vs   = cvt_v8c_v8i( _mm_set1_epi64x(0x0605040302010000lu) ) ;  // shuffle patterm
  // first 8 elements
  vi   = loadu_v256((__v256i *) (top)  ) ;     // top[0:7]
  vj1  = loadu_v256((__v256i *) (bot)  ) ;     // bot[0:7]
  vi1  = permutev_v8i(vi, vs) ;                // 0, 0, 1, 2, 3, 4, 5, 6  permutation ( top[-1:6] )
  vi1  = blend_v8i(vi1, v0, 1) ;               // first element set to 0 (top[-1])
  vij1 = permutev_v8i(vj1, vs) ;               // 0, 0, 1, 2, 3, 4, 5, 6  permutation ( bot[-1:6] )
  vij1 = blend_v8i(vij1, v0, 1) ;              // first element set to 0 (bot[-1])
  // top[i] - ( top[i-1] + bot[i] - bot[i-1] )
  vt   = sub_v8i( vi, sub_v8i( add_v8i(vi1, vj1) , vij1 ) ) ;
  vr   = vt ;
  j0 = 0 ;
  // chunks of 8 elements (second chunk may overlap first chunk)
  for(i0=n7 ; i0<n ; i0+=8){
    vi   = loadu_v256((__v256i *) (top+i0)  ) ;   // top[i0:i0+7]
    vj1  = loadu_v256((__v256i *) (bot+i0)  ) ;   // bot[i0:i0+7]
#if defined(WITH_FETCH)
    vi1  = loadu_v256((__v256i *) (top+i0-1)) ;   // top[i0-1:i0+6]
    vij1 = loadu_v256((__v256i *) (bot+i0-1)) ;   // bot[i0-1:i0+6]
#else
    v0   = set1_v8i(top[i0-1]) ;                  // top[i0-1]
    vi1  = permutev_v8i(vi, vs) ;                 // 0, 0, 1, 2, 3, 4, 5, 6  permutation
    vi1  = blend_v8i(vi1, v0, 1) ;                // first element set to top[i0-1]
    v0   = set1_v8i(bot[i0-1]) ;                  // bot[i0-1]
    vij1 = permutev_v8i(vj1, vs) ;                // 0, 0, 1, 2, 3, 4, 5, 6  permutation
    vij1 = blend_v8i(vij1, v0, 1) ;               // first element set to bot[i0-1]
#endif
    // top[i0+i] - ( top[i0+i-1] + bot[i0+i] - bot[i0+i-1] )
    vt   = sub_v8i( vi, sub_v8i( add_v8i(vi1, vj1) , vij1 ) ) ;
    storeu_v256( (__v256i *) (top+j0), vr) ;      // delayed store, result of previous pass
    vr = vt ;
    j0 = i0 ;
  }
  storeu_v256( (__v256i *) (top+j0), vr) ;        // delayed store, result of last pass
}
#else
// plain C version
// all rows but bottom row, in place prediction, n >= 8
// predict row j where j > 0 (2D prediction)
// top  : row j
// bot  : row (j - 1)
// n    : number of values in row
static inline void LorenzoPredictRowJ_inplace(int32_t * restrict top, int32_t * restrict bot, int n){
  int i, i0, j0, n7 = (n & 7) ;
  int32_t t[8], r[8], r0 ;

  if(n < 8) {
    LorenzoPredictRowJ_inplace_07(top, bot, n) ;
    return ;
  }
  r0 = top[0] - bot[0] ;
  n7 = n7 ? n7 : 8 ;
  // first 8 elements
  for(i=0 ; i<8 ; i++) r[i] = top[i] - ( top[i-1] + bot[i] - bot[i-1] ) ;
  j0 = 0 ;
  // chunks of 8 elements (second chunk may overlap first chunk)
  for(i0=n7 ; i0<n ; i0+=8){
    for(i=0 ; i<8 ; i++) t[i] = top[i0+i] - ( top[i0+i-1] + bot[i0+i] - bot[i0+i-1] ) ;
    for(i=0 ; i<8 ; i++) top[j0+i] = r[i] ;
    for(i=0 ; i<8 ; i++) r[i] = t[i] ;
    j0 = i0 ;
  }
  for(i=0 ; i<8 ; i++) top[j0+i] = r[i] ;
  top[0] = r0 ;
}
#endif

// in place version of function LorenzoPredict (normally called by LorenzoPredict)
// in order to operate in place, prediction is done backwards from top row to bottom row
// 2D lorenzo prediction (32 bit signed integers)
// orig : input  : original values (32 bit signed integers)
//        output : original value - predicted value (using 2D Lorenzo predictor) (32 bit signed integers)
// ni   : number of useful values in row
// lnio : row storage dimension for orig
// nj   : number of rows
// the SIMD version tends to be 1.5-4 times faster than the non SIMD version
// this version WILL NOT WORK FOR ni < 9
static void Lorenzo_predict_inplace(int32_t * restrict orig, int ni, int lnio, int nj){
  orig += (lnio * (nj - 1)) ;
  while(--nj > 0){                                    // all rows other than bottom row
    LorenzoPredictRowJ_inplace(orig, orig-lnio, ni) ; // predict upper row in row pair -> diff
    orig -= lnio ;                                    // next row
  }
  LorenzoPredictRow0_inplace(orig, ni) ;              // bottom row
}

// plain C version for cases where ni < 9
// DOES NOT WORK IN PLACE
static void LorenzoPredictShort(int32_t * restrict orig, int32_t * restrict diff, int ni, int lnio, int lnid, int nj){
  int i ;
  diff[0] = orig[0] ;
  for(i=1 ; i<ni ; i++) diff[i] = orig[i] - orig[i-1] ;
  while(--nj > 0){
    diff += lnid ; 
    orig += lnio ;
    diff[0] = orig[0] - orig[0-lnio] ;
    for(i=1 ; i<ni ; i++) diff[i] = orig[i] - (orig[i-1] + orig[i-lnio] - orig[i-1-lnio]) ;
  }
}

// 2D lorenzo prediction (32 bit signed integers)
// orig : input : original values (32 bit signed integers)
// diff : output : original value - predicted value (using 2D Lorenzo predictor) (32 bit signed integers)
// ni   : number of useful values in row
// lnio : row storage dimension for orig
// lnid : row storage dimension for diff
// nj   : number of rows
// the SIMD version tends to be 1.5-4 times faster than the non SIMD version
void LorenzoPredict(int32_t * restrict orig, int32_t * restrict diff, int ni, int lnio, int lnid, int nj){

  if(orig == diff){             // called in place
    Lorenzo_predict_inplace(orig, ni, lnio, nj) ;
    return ;
  }
  if(ni < 9){             // less than 9 values, the SIMD version will not give correct results
    LorenzoPredictShort(orig, diff, ni, lnio, lnid, nj) ;
    return ;
  }
  LorenzoPredictRow0(orig, diff, ni) ;                // bottom row
  while(--nj > 0){
    diff += lnid ; 
    orig += lnio ;
    LorenzoPredictRowJ(orig, orig-lnio, diff, ni) ;   // all other rows
  }
}

// in place function normally called by not in place function
// restore ogiginal from 2D lorenzo prediction (32 bit signed integers)
// orig : input  : original value - predicted value (32 bit signed integers)
//        output : restored original values from predicted differences
// ni   : number of useful values in row
// lnio : row storage dimension
// nj   : number of rows
// NOTE : no SIMD version exists, as the process is fully recursive
// NOTE : nj == 1 may be used for 1D prediction restore
static void LorenzoUnpredictInplace_(int32_t *orig, int ni, int lnio, int nj){
  int i ;
  int32_t *top, *bot ;
  int32_t d00, d01, d10, d11 ;
//   d01 d11   unpredict : d11 = d11 + d01 + d10 - d00
//   d00 d10
// fprintf(stderr, "LorenzoUnpredictInplace_ : ni = %d, lni = %d, nj = %d\n", ni, lnio, nj) ;

  d00 = orig[0] ;                                    // first point in bottom row
  for(i=1 ; i<ni ; i++) {                            // remainder of bottom row
    d00 = orig[i] = orig[i] + d00 ; 
  }

  while(--nj > 0){
    bot   = orig ;
    orig += lnio ; 
    top   = orig ;
    d01 = orig[0] = top[0] + bot[0] ;                // first point in row (1D prediction)
    d00 = bot[0] ;

    for(i=1 ; i<ni ; i++) {
      d10 = bot[i] ;
      d11 = top[i] ;
      d01 = orig[i] = (d11 + d01) + (d10 - d00) ;    // (original - predicted) + predicted
      d00 = d10 ;
    }
  }
}

// restore ogiginal from 2D lorenzo prediction (32 bit signed integers)
// diff : input : original value - predicted value (32 bit signed integers)
// orig : output : restored original values from predicted differences
// ni   : number of useful values in row
// lnio : row storage dimension for orig
// lnid : row storage dimension for diff (ignored if in place)
// nj   : number of rows
// NOTE : no SIMD version exists, as the process is fully recursive
// NOTE : nj == 1 may be used for 1D prediction restore
void LorenzoUnpredict(int32_t * restrict orig, int32_t * restrict diff, int ni, int lnio, int lnid, int nj){
  int i ;
  int32_t *top, *bot ;
  int32_t d00, d01, d10, d11 ;
//   d01 d11   unpredict : d11 = d11 + d01 + d10 - d00
//   d00 d10
  if(orig == diff){         // in place
    LorenzoUnpredictInplace_(orig, ni, lnio, nj) ;
    return ;
  }

  d00 = orig[0] = diff[0] ;         // restore first point of bottom row
  for(i=1 ; i<ni ; i++) {           // restore rest of bottom row
    d00 = orig[i] = diff[i] + d00 ;
  }

  while(--nj > 0){
    bot = orig ;
    orig += lnio ; 
    diff += lnid ;
    top = diff ;
    d01 = orig[0] = top[0] + bot[0] ;                // first point in row (1D prediction)
    d00 = bot[0] ;
    for(i=1 ; i<ni ; i++) {
      d10 = bot[i] ;
      d11 = top[i] ;
      d01 = orig[i] = (d11 + d01) + (d10 - d00) ;    // (original - predicted) + predicted
      d00 = d10 ;
    }
  }
}


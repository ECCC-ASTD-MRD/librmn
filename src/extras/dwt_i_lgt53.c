/* 
 * Copyright (C) 2025  Recherche en Prevision Numerique
 *                     Environnement Canada
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 */
// see
//
// Cohen-Daubechies-Feauveau 5/3 wavelets
// Le GAll / Tabatabai transform
// (reversible, using integers, similar to JPEG 2000)
//
// https://en.wikipedia.org/wiki/Cohen%E2%80%93Daubechies%E2%80%93Feauveau_wavelet
// https://en.wikipedia.org/wiki/Discrete_wavelet_transform
//
// this code is using a lifting implementation
// https://en.wikipedia.org/wiki/Lifting_scheme
//
// 1 dimensional transform, "in place", "even/odd split", or "in place with even/odd split"
//   original data
//   +--------------------------------------------------------+
//   |                  n data                                |
//   +--------------------------------------------------------+
//
//   void fwd_1d_lgt53_asis(int *x, int n);
//   void inv_1d_lgt53_asis(int *x, int n);
//
//   transformed data (in place, no split, even number of data)
//   +--------------------------------------------------------+
//   |   n data, even/odd, even/odd, ..... , even/odd         +
//   +--------------------------------------------------------+
//
//   transformed data (in place, no split, odd number of data)
//   +--------------------------------------------------------+
//   |   n data, even/odd, even/odd, ..... , even/odd, even   +
//   +--------------------------------------------------------+
//
//   void fwd_1d_lgt53(int *x, int n);
//   void inv_1d_lgt53(int *x, int n);
//
//   transformed data, in place with even/odd split
//   +--------------------------------------------------------+
//   | (n+1)/2 even data            |    (n/2) odd data       |
//   +--------------------------------------------------------+
//
//   the process can be applied again to the even transformed part to achieve a multi level transform
//   void fwd_1d_lgt53_n(int *x, int n, int levels);
//   void inv_1d_lgt53_n(int *x, int n, int levels);
//
//   void fwd_1d_lgt53_split(int *x, int *e, int *o, int n);
//   void inv_1d_lgt53_split(int *x, int *e, int *o, int n);
//
//   original data                     transformed data (2 output arrays)
//   +------------------------------+  +-------------------+  +------------------+
//   |             n data           |  | (n+1)/2 even data |  |   n/2 odd data   |
//   +------------------------------+  +-------------------+  +------------------+
//
//   even data are the "approximation" terms ("low frequency" terms)
//   odd data are the "detail" terms         ("high frequency" terms)
//
//   void fwd_2d_lgt53(int *x, int lni, int ni, int nj);
//   void inv_2d_lgt53(int *x, int lni, int ni, int nj);
//
// 2 dimensional in place with 2 D split
//   original data                               transformed data (in same array)
//   +------------------------------------+--+      +-------------------+----------------+--+
//   |                  ^                 |  |      +                   |                |  |
//   |                  |                 |  |      +   even i/odd j    |  odd i/odd j   |  |
//   |                  |                 |  |      +                   |                |  |
//   |                  |                 |  |      +                   |                |  |
//   |                  |                 |  +      +-------------------+----------------+  +
//   |               NJ data              |  |      +                   |                |  |
//   |                  |                 |  |      +                   |                |  |
//   |                  |                 |  |      +   even i/even j   |  odd i/even j  |  |
//   |<----- NI data ---|---------------->|  |      +                   |                |  |
//   |                  v                 |  |      +                   |                |  |
//   +------------------------------------+--+      +-------------------+----------------+--+
//   <---------------- LNI data ------------->      <---------------- LNI data ------------->
//
//   the process can be applied again to the even/even transformed part to achieve a multi level transform
//   void fwd_2d_lgt53_n(int *x, int lni, int ni, int nj, int levels);
//   void inv_2d_lgt53_n(int *x, int lni, int ni, int nj, int levels);
//
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

// #include <immintrin.h>
#define USE_SIMD_INTRINSICS
// #define NO_SIMD
#include <rmn/simd_functions.h>

#include <rmn/dwt_i_lgt53.h>

// utility functions used by main functions
static inline int is_odd(int n) { return (n & 1) ; }

#define ROUND 1

// predict odd terms using even terms
static inline int predict(int o0, int e0, int e1){ return o0 - ((e0 + e1 + ROUND) >> 1) ; }
static inline int predict_edge(int o0, int e0   ){ return o0 - e0 ; }   // predict(o0, e0, e0)

// update even terms using odd terms
static inline int update(int e1, int o0, int o1){ return e1 + ((o0 + o1 + 2) >> 2) ; }
static inline int update_edge(int e1, int o0   ){ return e1 + ((o0 + 1) >> 1) ; }  // update(e1, o0, o0) ;

// inverse predict odd terms using even terms
static inline int un_predict(int o0, int e0, int e1){ return o0 + ((e0 + e1 + ROUND) >> 1) ; }
static inline int un_predict_edge(int o0, int e0   ){ return o0 + e0 ; }   // un_predict(o0, e0, e0)

// inverse update even terms using odd terms
static inline int un_update(int e1, int o0, int o1){ return e1 - ((o0 + o1 + 2) >> 2) ; }
static inline int un_update_edge(int e1, int o0   ){ return e1 - ((o0 + 1) >> 1) ; } // un_update(e1, o0, o0) ;

#if defined(__AVX2__)
// even/odd merge 4 even terms + 4 odd terms into 8 terms
static inline __v256i merge_2v128(__v128i ve, __v128i vo){
  return setr_2v128( unpacklo_v4i(ve, vo) , unpackhi_v4i(ve, vo) ) ;
}
// even/odd merge the low 4 even terms and the low 4 odd terms into 8 terms
static inline __v256i merge_lo_2v128(__v256i ve, __v256i vo){
  __v256i v0 = unpacklo_v8i(ve, vo) ;
  __v256i v1 = unpackhi_v8i(ve, vo) ;
  return permute2_v256(v0, v1, 0x20) ;
}
// even/odd merge the high 4 even terms and the high 4 odd terms into 8 terms
static inline __v256i merge_hi_2v128(__v256i ve, __v256i vo){
  __v256i v0 = unpacklo_v8i(ve, vo) ;
  __v256i v1 = unpackhi_v8i(ve, vo) ;
  return permute2_v256(v0, v1, 0x31) ;
}
// merge 8 even terms + 8 odd terms and store 16 terms
static inline void merge_store_2v256(uint32_t *s, __v256i ve, __v256i vo){
  storeu_v256((__v256i *)(s  ), merge_lo_2v128(ve, vo)) ;
  storeu_v256((__v256i *)(s+8), merge_hi_2v128(ve, vo)) ;
}
// merge 4 even terms + 4 odd terms and store 8 terms
static inline void merge_store_2v128(uint32_t *s, __v128i ve, __v128i vo){
  storeu_v256((__v256i *)(s), merge_2v128(ve, vo) ) ;
}
// merge (n+1)/2 even terms and n/2 odd terms into s[n]
void merge_even_odd(uint32_t *s, uint32_t *e, uint32_t *o, int n){
  while(n>15){
    merge_store_2v256(s, loadu_v256((__v256i *)e), loadu_v256((__v256i *)o)) ;
    n-=16 ; s+=16 ; e+=8 ; o+=8 ;             // update pointers and count
  }
  if(n>7){
    merge_store_2v128(s, loadu_v128((__v128i *)e), loadu_v128((__v128i *)o)) ;
    n-=8 ; s+=8 ; e+=4 ; o+=4 ;               // update pointers and count
  }
  if(n>0){                                    // any leftovers ?
    __v128i ve, vo, v0 ;
    ve = loadu_v128((__v128i *)e) ;      // 4 even terms (3 possibly irrelevant)
    vo = loadu_v128((__v128i *)o) ;      // 4 odd terms (4 possibly irrelevant)
    v0 = unpacklo_v4i(ve, vo) ;         // merge first 2 pairs
    if(n > 3){
      storeu_v128((__v128i *)(s  ), v0) ; // store 2 even terms ,  2 odd terms
      v0 = unpackhi_v4i(ve, vo) ;       // merge next 2 pairs
      n-=4 ; s+=4 ; e+=2 ; o+=2 ;             // update pointers and count
    }
    if(n > 1){
      storeu_si64((void *)s, v0) ;        // store 1 even term, 1 odd term
      v0 = bsrli_v128(v0, 8) ;           // shift right by 64 bits
      n-=2 ; s+=2 ; e+=1 ; o+=1 ;             // update pointers and count
    }
    if(n > 0){
      storeu_si32((void *)s, v0) ;        // store last even term
    }
  }
}
#endif

// ============================ FORWARD TRANSFORMS ============================

// forward Le Gall Tabatabai transform, in place, split layout
// x [INOUT] : 1D array to transform
// n    [IN] : dimension of x
void fwd_1d_lgt53(int *x, int n){
  if(n < 2) return ;       // 1 item only, nothing to do

  int i, neven = (n+1) >> 1, nodd = n >> 1 ;
  int o[nodd], e[neven] ;
  fwd_1d_lgt53_split(x, e, o, n) ;     // use local arrays e and o
  for(i=0 ; i<nodd ; i++){ x[i] = e[i] ; x[neven+i] = o[i] ; }   // copy into x
  x[neven-1] = e[neven-1] ;
}

// forward Le Gall Tabatabai transform, in place, split layout, multiple successive transforms
// x [INOUT] : 1D array to transform
// n    [IN] : dimension of x
// nl   [IN} : number of successive transforms
void fwd_1d_lgt53_n(int *x, int n, int nl){
  fwd_1d_lgt53(x, n) ;
  if(nl > 0){
    fwd_1d_lgt53_n(x, (n+1)/2, nl -1) ;
  }
}

// forward Le Gall Tabatabai transform, in place, even/odd pairs layout
// x [INOUT] : 1D array to transform
// n    [IN] : dimension of x
void fwd_1d_lgt53_asis(int *x, int n){
  if(n < 2) return ;       // 1 item only, nothing to do

  for(int i=1; i<n-2+(n&1); i+=2){     // predict odd
    x[i] = predict(x[i], x[i-1], x[i+1]) ;
  }

  if(is_odd(n))
    x[n-1] = update_edge(x[n-1], x[n-2]) ;   // last is even, update
  else
    x[n-1] = predict_edge(x[n-1], x[n-2]) ;  // last is odd, predict

  x[0] = update_edge(x[0], x[1]) ;           // update first even
  for(int i=2; i<n-(n&1); i+=2){
    x[i] = update(x[i], x[i-1], x[i+1]) ;  // update even
  }
}

// 1 dimensional forward Le Gall Tabatabai transform, not in place, even/odd arrays
// x    [IN] : 1D array to transform
// e   [OUT] : 1D array of even terms
// o   [OUT] : 1D array of odd terms
// n    [IN] : dimension of x (assumed even)
// this SIMD version is used only if n is a multiple of 16
void fwd_1d_lgt53_split_simd(int *x_, int *e_, int *o_, int n){
#if ! defined(__AVX2__)
  fwd_1d_lgt53_split_c(x_, e_, o_, n) ;
#else
  if(n & 15){          // n is not a multiple of 16, use the C version
    fwd_1d_lgt53_split_c(x_, e_, o_, n) ;
    return ;
  }
  int *x = x_, *e = e_, *o = o_ ;
  int i = 0, neven = n >> 1 ;
  __v256i vc1, vc2 ;
  __v256  ve0, ve1,vd0, vd1, vo0, vo1 ;
  vc1 = set1_v8i(1) ;      // vector of 1
  vc2 = set1_v8i(2) ;      // vector of 2

  for( ; i<neven-15 ; i+=16, o+=16, e+=16, x+=32){            // by 32 elements (16 odd/even pairs)
    // separate even and odd terms
    vd0 = loadu_v8f((float *)(x   )) ;
    vd1 = loadu_v8f((float *)(x+ 8)) ;
    ve0 = shuffle_2v8f(vd0, vd1, 136) ;                  // 0b10001000 [ 0 2 8 A 4 6 C E ]
    ve0 = __V256f permutei_v4d(__V256d ve0, 216) ;  // 0b11011000 [ 0 2 4 6 8 A C E ]  e[i]
    vd0 = loadu_v8f((float *)(x+ 1)) ;
    vd1 = loadu_v8f((float *)(x+ 9)) ;
    vo0 = shuffle_2v8f(vd0, vd1, 136) ;                  // 0b10001000 [ 1 3 9 B 5 7 D F ]
    vo0 = __V256f permutei_v4d(__V256d vo0, 216) ;  // 0b11011000 [ 1 3 5 7 9 B D F ]  o[i]
    ve1 = shuffle_2v8f(vd0, vd1, 221) ;                  // 0b10001000 [ 2 4 A C 6 8 E 10]
    ve1 = __V256f permutei_v4d(__V256d ve1, 216) ;  // 0b11011000 [ 2 4 6 8 A C E 10]  e[i+1]
    storeu_v8f((float *)(e   ), ve0) ;                  // store even terms

    // predict odd terms
    ve0 = __V256f add_v8i(__V256i ve0, vc1) ;           // e[i] + 1
    ve0 = __V256f add_v8i(__V256i ve0, __V256i ve1) ;  // e[i] + 1 + e[i+1]
    ve0 = __V256f srai_v8i(__V256i ve0, 1) ;            // (e[i] + 1 + e[i+1]) >> 1
    vo0 = __V256f sub_v8i(__V256i vo0, __V256i ve0) ;  // o[i] - ( (e[i] + 1 + e[i+1]) >> 1 )
    storeu_v8f((float *)(o   ), vo0) ;                      // store predicted odd terms

    // separate even and odd terms
    vd0 = loadu_v8f((float *)(x+16)) ;
    vd1 = loadu_v8f((float *)(x+24)) ;
    ve0 = shuffle_2v8f(vd0, vd1, 136) ;                  // 0b10001000 [ 0 2 8 A 4 6 C E ]
    ve0 = __V256f permutei_v4d(__V256d ve0, 216) ;  // 0b11011000 [ 0 2 4 6 8 A C E ]  e[i]
    vd0 = loadu_v8f((float *)(x+17)) ;
    vd1 = loadu_v8f((float *)(x+25)) ;
    vo0 = shuffle_2v8f(vd0, vd1, 136) ;                  // 0b10001000 [ 1 3 9 B 5 7 D F ]
    vo0 = __V256f permutei_v4d(__V256d vo0, 216) ;  // 0b11011000 [ 1 3 5 7 9 B D F ]  o[i]
    ve1 = shuffle_2v8f(vd0, vd1, 221) ;                  // 0b10001000 [ 2 4 A C 6 8 E 10]
    ve1 = __V256f permutei_v4d(__V256d ve1, 216) ;  // 0b11011000 [ 2 4 6 8 A C E 10]  e[i+1]
    storeu_v8f((float *)(e+ 8), ve0) ;                  // store even terms

    // predict odd terms
    ve0 = __V256f add_v8i(__V256i ve0, vc1) ;           // e[i] + 1
    ve0 = __V256f add_v8i(__V256i ve0, __V256i ve1) ;  // e[i] + 1 + e[i+1]
    ve0 = __V256f srai_v8i(__V256i ve0, 1) ;            // (e[i] + 1 + e[i+1]) >> 1
    vo0 = __V256f sub_v8i(__V256i vo0, __V256i ve0) ;  // o[i] - ( (e[i] + 1 + e[i+1]) >> 1 )
    storeu_v8f((float *)(o+ 8), vo0) ;                      // store predicted odd terms
  }
  for( ; i<neven-7 ; i+=8, o+=8, e+=8, x+=16){                // by 16 elements
    // separate even and odd terms
    vd0 = loadu_v8f((float *)(x   )) ;
    vd1 = loadu_v8f((float *)(x+ 8)) ;
    ve0 = shuffle_2v8f(vd0, vd1, 136) ;                  // 0b10001000 [ 0 2 8 A 4 6 C E ]
    ve0 = __V256f permutei_v4d(__V256d ve0, 216) ;  // 0b11011000 [ 0 2 4 6 8 A C E ]  e[i]
    vd0 = loadu_v8f((float *)(x+ 1)) ;
    vd1 = loadu_v8f((float *)(x+ 9)) ;
    vo0 = shuffle_2v8f(vd0, vd1, 136) ;                  // 0b10001000 [ 1 3 9 B 5 7 D F ]
    vo0 = __V256f permutei_v4d(__V256d vo0, 216) ;  // 0b11011000 [ 1 3 5 7 9 B D F ]  o[i]
    ve1 = shuffle_2v8f(vd0, vd1, 221) ;                  // 0b10001000 [ 2 4 A C 6 8 E 10]
    ve1 = __V256f permutei_v4d(__V256d ve1, 216) ;  // 0b11011000 [ 2 4 6 8 A C E 10]  e[i+1]
    storeu_v8f((float *)(e   ), ve0) ;                  // store even terms

    // predict odd terms
    ve0 = __V256f add_v8i(__V256i ve0, vc1) ;           // e[i] + 1
    ve0 = __V256f add_v8i(__V256i ve0, __V256i ve1) ;  // e[i] + 1 + e[i+1]
    ve0 = __V256f srai_v8i(__V256i ve0, 1) ;            // (e[i] + 1 + e[i+1]) >> 1
    vo0 = __V256f sub_v8i(__V256i vo0, __V256i ve0) ;  // o[i] - ( (e[i] + 1 + e[i+1]) >> 1 )
    storeu_v8f((float *)(o   ), vo0) ;                      // store predicted odd terms
  }
  e = e_ ; o = o_ ; x = x_ ;
  o[neven-1] = x[n-1] - x[n-2] ;                                   // fix predicted last odd term

  int e00 = x[0] + ((o[0] + 1) >> 1) ;                             // updated first even term

  i = 0 ;
  for( ; i<neven-15 ; i+=16, o+=16, e+=16){                        // by 16 even elements
     ve1 = loadu_v8f((float *)(e   )) ;
     vo0 = loadu_v8f((float *)(o- 1)) ;
     vo1 = loadu_v8f((float *)(o   )) ;
     vo0 = __V256f add_v8i(__V256i vo0, vc2) ;           // o[i-1] + 2
     vo0 = __V256f add_v8i(__V256i vo0, __V256i vo1) ;  // o[i] + o[i-1] + 2
     vo0 = __V256f srai_v8i(__V256i vo0, 2) ;            // (o[i] + o[i-1] + 2) >> 2
     ve1 = __V256f add_v8i(__V256i ve1, __V256i vo0) ;  // e[i] + ( (o[i] + o[i-1] + 2) >> 2 )
     storeu_v8f((float *)(e   ), ve1) ;                      // store updated even terms

     ve1 = loadu_v8f((float *)(e+ 8)) ;
     vo0 = loadu_v8f((float *)(o+ 7)) ;
     vo1 = loadu_v8f((float *)(o+ 8)) ;
     vo0 = __V256f add_v8i(__V256i vo0, vc2) ;           // o[i-1] + 2
     vo0 = __V256f add_v8i(__V256i vo0, __V256i vo1) ;  // o[i] + o[i-1] + 2
     vo0 = __V256f srai_v8i(__V256i vo0, 2) ;            // (o[i] + o[i-1] + 2) >> 2
     ve1 = __V256f add_v8i(__V256i ve1, __V256i vo0) ;  // e[i] + ( (o[i] + o[i-1] + 2) >> 2 )
     storeu_v8f((float *)(e+ 8), ve1) ;                      // store updated even terms
  }
  for( ; i<neven-7 ; i+=8, o+=8, e+=8){                            // by 8 even elements
     ve1 = loadu_v8f((float *)(e   )) ;
     vo0 = loadu_v8f((float *)(o- 1)) ;
     vo1 = loadu_v8f((float *)(o   )) ;
     vo0 = __V256f add_v8i(__V256i vo0, vc2) ;           // o[i-1] + 2
     vo0 = __V256f add_v8i(__V256i vo0, __V256i vo1) ;  // o[i] + o[i-1] + 2
     vo0 = __V256f srai_v8i(__V256i vo0, 2) ;            // (o[i] + o[i-1] + 2) >> 2
     ve1 = __V256f add_v8i(__V256i ve1, __V256i vo0) ;  // e[i] + ( (o[i] + o[i-1] + 2) >> 2 )
     storeu_v8f((float *)(e   ), ve1) ;                      // store updated even terms
  }
  e_[0] = e00 ;                                                    // fix updated first even term
#endif
}

// 1 dimensional forward Le Gall Tabatabai transform, not in place, even/odd arrays
// x    [IN] : 1D array to transform
// e   [OUT] : 1D array of even terms
// o   [OUT] : 1D array of odd terms
// n    [IN] : dimension of x (assumed even)
// plain C version
void fwd_1d_lgt53_split_c(int *x, int *e, int *o, int n){
  int i ;
  int neven = (n + 1) >> 1, nodd  = n >> 1 ;

  if(n < 3){       // 1 or 2 items, special case
    e[0] = x[0];
    if(n == 2){
      o[0] = predict_edge(x[1], x[0]) ;
      e[0] = update_edge(x[0], o[0]) ;
    }
    return;
  }

  for(i = 0 ; i < nodd ; i++) o[i] = predict(x[i+i+1], x[i+i], x[i+i+2]) ;  // predict odd terms
  if(neven == nodd) o[nodd-1] = predict_edge(x[n-1], x[n-2]) ;              // last term is odd

  e[0 ] = update_edge(x[0], o[0]) ;
  for(i = 1; i < neven ; i++) e[i] = update(x[i+i], o[i], o[i-1]) ;         // update even terms
  if(neven != nodd) e[neven-1] = update_edge(x[n-1], o[nodd-1]) ;           // last term is even
}

// 1 dimensional forward Le Gall Tabatabai transform, not in place, even/odd arrays
// x    [IN] : 1D array to transform
// e   [OUT] : 1D array of even terms
// o   [OUT] : 1D array of odd terms
// n    [IN] : dimension of x (assumed even)
void fwd_1d_lgt53_split(int *x, int *e, int *o, int n){
#if defined(__AVX2__)
  fwd_1d_lgt53_split_simd(x, e, o, n) ;
#else
  fwd_1d_lgt53_split_c(x, e, o, n) ;
#endif
}

// internal functions used by 2D transform in the j direction
// predict row o0 using even rows e0 and e1, store in row o
// o  [OUT] : 1D array of predicted odd terms
// o0  [IN] : 1D array of odd terms
// e0  [IN] : 1D array of even terms used to predict odd terms
// e1  [IN] : 1D array of even terms used to predict odd terms
static inline void row_predict(int *o, int *o0, int *e0, int *e1, int ni){
  int i = 0 ;
#if defined(__AVX2__)
  __v256i vc1 = set1_v8i(1) ;
  while(i < ni-7){
    __v256i vro, vo0, ve0, ve1 ;
    vo0 = loadu_v256((__v256i *)(o0+i)) ;
    ve0 = loadu_v256((__v256i *)(e0+i)) ;
    ve1 = loadu_v256((__v256i *)(e1+i)) ;
    ve0 = add_v8i(ve0, vc1) ;      // e0[i] + 1
    ve0 = add_v8i(ve0, ve1) ;      // e1[i] + e0[i] + 1
    ve0 = srai_v8i(ve0, 1) ;       // (e1[i] + e0[i] + 1) >> 1
    vro = sub_v8i(vo0, ve0) ;      // (o[i] - (e1[i] + e0[i] + 1) >> 1)
    storeu_v256((__v256i *)(o+i), vro) ;
    i += 8 ;
  }
#endif
  for( ; i<ni ; i++){ o[i] = o0[i] - ((e0[i] + e1[i] + 1) >> 1) ; }
  if(i != ni){
    fprintf(stderr,"ni = %d, i= %d\n", ni, i);
    exit(1) ;
  }
}
static inline void row_predict_edge(int *o, int *o0, int *e0, int ni){
  int i ;
  for(i=0 ; i<ni ; i++){ o[i] = o0[i] - e0[i] ; }
}

// update row e0 using odd rows o0 and o1, store in row e
// e  [OUT] : 1D array of updated even terms
// e0  [IN] : 1D array of even terms
// o0  [IN] : 1D array of edd terms used to update even terms
// o1  [IN] : 1D array of odd terms used to update even terms
static inline void row_update(int *e, int *e0, int *o0, int *o1, int ni){
  int i = 0 ;
#if defined(__AVX2__)
  __v256i vc2 = set1_v8i(2) ;
  while(i < ni-7){
    __v256i vre, ve0, vo0, vo1 ;
    ve0 = loadu_v256((__v256i *)(e0+i)) ;
    vo0 = loadu_v256((__v256i *)(o0+i)) ;
    vo1 = loadu_v256((__v256i *)(o1+i)) ;
    vo0 = add_v8i(vo0, vc2) ;      // o0[i] + 2
    vo0 = add_v8i(vo0, vo1) ;      // o1[i] + o0[i] + 2
    vo0 = srai_v8i(vo0, 2) ;       // (o1[i] + o0[i] + 2) >> 2
    vre = add_v8i(ve0, vo0) ;      // (e[i] + (o1[i] + o0[i] + 2) >> 1)
    storeu_v256((__v256i *)(e+i), vre) ;
    i += 8 ;
  }
#endif
  for( ; i<ni ; i++){ e[i] = e0[i] + ((o0[i] + o1[i] + 2) >> 2) ; }
  if(i != ni){
    fprintf(stderr,"ni = %d, i= %d\n", ni, i);
    exit(1) ;
  }
}
static inline void row_update_edge(int *e, int *e0, int *o0, int ni){
  int i ;
  for(i=0 ; i<ni ; i++){ e[i] = e0[i] + ((o0[i] + 1) >> 1) ; }
}

// used by fwd_2d_lgt53 (VLA form)
static void fwd_2d_lgt53_(int lni, int ni, int nj, int x[nj][lni]){
  int j, nie = (ni+1)/2 , njo = nj/2, nje = (nj+1)/2 ;
  int o[njo][ni] ;   // local temporary copy of odd terms

  if(nj == 1){   // 1 row only, perform 1d transform
    fwd_1d_lgt53(&x[0][0], ni) ;
    return ;
  }

  // 1d transform in the i direction, move to temporary array o (x[j+j+1][] : odd rows)
  for(j=0 ; j<njo ; j++){ fwd_1d_lgt53_split(&x[j+j+1][0], &o[j][0], &o[j][nie], ni) ; }

  // 1d transform in the i direction, move to bottom part of array x (x[j+j][] : even rows)
  fwd_1d_lgt53(&x[0][0], ni) ;   // first even row has to be done in place
  for(j=1 ; j<nje ; j++){ fwd_1d_lgt53_split(&x[j+j][0], &x[j][0], &x[j][nie], ni) ; }

  if(is_odd(nj)){       // last row is even

    // predict odd rows
    for(j=0 ; j<njo   ; j++){ row_predict(&x[nje+j][0], &o[j][0], &x[j][0], &x[j+1][0], ni) ; }
    // update even rows
    row_update_edge(&x[0][0], &x[0][0], &x[nje][0], ni) ;          // first even row
    for(j=1 ; j<nje-1 ; j++){ row_update(&x[j][0], &x[j][0], &x[nje+j-1][0], &x[nje+j][0], ni) ; }
    row_update_edge(&x[j][0], &x[j][0], &x[nje+njo-1][0], ni) ;   // last even row

  }else{                // last row is odd

    // predict odd rows
    for(j=0 ; j<njo-1 ; j++){ row_predict(&x[nje+j][0], &o[j][0], &x[j][0], &x[j+1][0], ni) ; }
    row_predict_edge(&x[nje+j][0], &o[j][0], &x[j][0], ni) ;      // last odd row
    // update even rows
    row_update_edge(&x[0][0], &x[0][0], &x[nje][0], ni) ;          // first even row
    for(j=1 ; j<nje ; j++){ row_update(&x[j][0], &x[j][0], &x[nje+j-1][0], &x[nje+j][0], ni) ; }

  }
}

// in place 2D forward Le Gall Tabatabai transform
// initially array : even/odd terms even/odd rows
// transformed array in quadrant form
// x  [INOUT] : 2D array to transform
// lni   [IN] : storage length of x rows
// ni    [IN] : length of x rows
// nj    [IN] : number of x rows
void fwd_2d_lgt53(int *x, int lni, int ni, int nj){
  fwd_2d_lgt53_(lni, ni, nj, (void *)x) ;  // call VLA prototype function
}

// in place 2D forward Le Gall Tabatabai multiple successive transform
// initially array : even/odd terms even/odd rows
// transformed array in quadrant form
// x  [INOUT] : 2D array to transform
// lni   [IN] : storage length of x rows
// ni    [IN] : length of x rows
// nj    [IN] : number of x rows
// nl    [IN] : number of successive transforms
void fwd_2d_lgt53_n(int *x, int lni, int ni, int nj, int nl){
  fwd_2d_lgt53_(lni, ni, nj, (void *)x) ;
  if(nl > 0){
    fwd_2d_lgt53_n(x, lni, (ni + 1) / 2, (nj + 1) / 2, nl - 1) ;
  }
}

// ============================ INVERSE TRANSFORMS ============================

// inverse Le Gall Tabatabai transform, in place, split layout
// x [INOUT] : 1D array to transform
// n    [IN] : dimension of x
void inv_1d_lgt53(int *x, int n){
  if(n < 2) return ;    // 1 item only, nothing to do

  int i, neven = (n+1) >> 1, nodd = n >> 1 ;
  int *o = x+neven, e[neven] ;
  for(i=0 ; i<neven ; i++) e[i] = 777 ;

  e[0] = un_update_edge(x[0], o[0]) ;
  for(i=1 ; i<neven-1 ; i++) e[i] = un_update(x[i], o[i], o[i-1]) ;
  if(is_odd(n))
    e[neven-1] = un_update_edge(x[neven-1], o[nodd-1]) ;
  else
    e[neven-1] = un_update(x[neven-1], o[nodd-1], o[nodd-2]) ;

  for(i=0 ; i<nodd-1 ; i++) x[i+i+1] = un_predict(o[i], e[i], e[i+1]) ;
  if(is_odd(n))
    x[n-2] = un_predict(o[nodd-1], e[neven-1], e[neven-2]) ;
  else
    x[n-1] = un_predict_edge(o[nodd-1], e[neven-1]) ;

  for(i=0 ; i<neven ; i++) x[i+i] = e[i] ;       // copy o back into x
}

// inverse Le Gall Tabatabai transform, in place, split layout, multiple successive transforms
// x [INOUT] : 1D array to transform
// n    [IN] : dimension of x
// nl   [IN} : number of successive transforms
void inv_1d_lgt53_n(int *x, int n, int nl){
  if(nl > 0){
    inv_1d_lgt53_n(x, (n+1)/2, nl-1) ;
  }
  inv_1d_lgt53(x, n) ;
}

// inverse Le Gall Tabatabai transform, in place, evn/odd pairs
// x [INOUT] : 1D array to transform
// n    [IN] : dimension of x
void inv_1d_lgt53_asis(int *x, int n){
  if(n < 2) return ;    // 1 item only, nothing to do

  int i ;

  for(i=2; i<n-(n&1); i+=2){                      // unupdate even
    x[i] = un_update(x[i], x[i-1], x[i+1]) ;
  }
  x[0] = un_update_edge(x[0], x[1]) ;           // unupdate first even

  if(is_odd(n))
    x[n-1] = un_update_edge(x[n-1], x[n-2]) ;   // last is even, unupdate
  else
    x[n-1] = un_predict_edge(x[n-1], x[n-2]) ;  // last is odd, unpredict

  for(i=1; i<n-2+(n&1); i+=2){                    // unpredict odd
    x[i] = un_predict(x[i], x[i-1], x[i+1]) ;
  }
}

// forward Le Gall Tabatabai transform, not in place, even/odd arrays
// x   [OUT] : 1D array to receive transform
// e    [IN] : 1D array of even terms
// o    [IN] : 1D array of odd terms
// n    [IN] : dimension of x (even or odd)
// this SIMD version is used only if n is a multiple of 16
void inv_1d_lgt53_split_simd(int *x_, int *e_, int *o_, int n){
  int *x = x_, *e = e_, *o = o_ ;
#if ! defined(__AVX2__)
  inv_1d_lgt53_split_c(x, e, o, n) ;
#else
  int i, neven = n >> 1, nodd = neven ;
//   int teven[neven], *te_ = &teven[0], *te = te_ ;
  int *te_ = x_ + neven, *te = te_ ;    // use upper part of x_ as temporary storage for te

  if(n & 15) {   // not a multiple of 16
    inv_1d_lgt53_split_c(x, e, o, n) ;
    return;
  }
  __v256i vc1, vc2 ;
  __v256i  ve0, ve1, vo0, vo1 ;
  vc1 = set1_v8i(1) ;      // vector of 1
  vc2 = set1_v8i(2) ;      // vector of 2
// e[i] = e[i] - ((o[i] + o[i-1] + 2) >> 2)
  e = e_ ; o = o_ ; x = x_ ; te = te_ ;
  for(i=0 ; i<neven ; i+=8, o+=8, e+=8, te+=8){     // by 16 elements (8 odd/even pairs)
    ve0 = loadu_v256((__v256i *)(e  )) ;
    vo0 = loadu_v256((__v256i *)(o  )) ;
    vo1 = loadu_v256((__v256i *)(o-1)) ;
    vo0 = add_v8i(vo0, vc2) ;              // o[i] + 2
    vo0 = add_v8i(vo0, vo1) ;              // o[i] + o[i-1] + 2
    vo0 = srai_v8i(vo0, 2) ;               // (o[i] + o[i-1] + 2) >> 2
    ve0 = sub_v8i(ve0, vo0) ;              // e[i] - ((o[i] + o[i-1] + 2) >> 2)
    storeu_v256((__v256i *)te, ve0) ;       // te[i] = e[i] - ((o[i] + o[i-1] + 2) >> 2)
  }

  e = e_ ; o = o_ ; te = te_ ;
  te[0] = e[0] - ((o[0] + 1) >> 1) ;                // fix first even term

  int onn = o[nodd-1] ;                            // save last unpredicted odd term
// o[i] = o[i] + ((e[i] + e[i+1] + 1) >> 1)
  for(i=0 ; i<nodd ; i+=8, o+=8, x+=16, te+=8){    // by 16 elements (8 even/odd pairs)
    vo0 = loadu_v256((__v256i *)(o   )) ;
    ve0 = loadu_v256((__v256i *)(te  )) ;  // e[i]
    ve1 = loadu_v256((__v256i *)(te+1)) ;  // e[i+1
    ve1 = add_v8i(ve1, vc1) ;             // e[i+1] + 1
    ve1 = add_v8i(ve1, ve0) ;             // e[i] + e[i+1] + 1
    ve1 = srai_v8i(ve1, 1) ;              // (e[i] + e[i+1] + 1) >> 1
    vo0 =  add_v8i(vo0, ve1) ;            // o[i] + (e[i] + e[i+1] + 1) >> 1
    merge_store_2v256((uint32_t *)x, ve0, vo0) ;     // store e[i]/o[i] pairs
  }
  x_[n-1] = onn + x_[n-2] ;                         // fix last odd value
#endif
}

// inverse Le Gall Tabatabai transform, not in place, even/odd arrays
// x   [OUT] : 1D array to receive transform
// e    [IN] : 1D array of even terms
// o    [IN] : 1D array of odd terms
// n    [IN] : dimension of x (even or odd)
// if n < 3 explicit action is taken
void inv_1d_lgt53_split_c(int *x, int *e, int *o, int n){
  int i, nodd, neven ;

  if(n < 3) {   // 2 points minimum
    x[0] = e[0] ;
    if(n == 2){
      x[0] = un_update_edge(e[0], o[0]) ;
      x[1] = un_predict_edge(o[0], x[0]) ;
    }
    return;
  }
  nodd = n >> 1, neven = (n+1) >> 1 ;

  x[0] = un_update_edge(e[0], o[0]) ;                                          // un update first even term
  for (i = 1; i < nodd; i ++) x[i+i] = un_update(e[i], o[i], o[i-1]) ;         // un update nodd - 1 even terms
  if(neven > nodd) x[n-1] = un_update_edge(e[neven-1], o[nodd-1]) ;            // un update last even term if last is even

  for (i = 0; i < nodd-1; i++) x[i+i+1] = un_predict(o[i], x[i+i], x[i+i+2]) ; // unpredict nodd - 1 odd terms
  if(neven == nodd){
    x[n-1] = un_predict_edge(o[nodd-1], x[n-2]) ;                              // last term is odd
  }else{
    x[n-2] = un_predict(o[nodd-1], x[n-3], x[n-1]) ;                           // last term is even
  }
}

// inverse Le Gall Tabatabai transform, not in place, even/odd arrays
// x   [OUT] : 1D array to receive transform
// e    [IN] : 1D array of even terms
// o    [IN] : 1D array of odd terms
// n    [IN] : dimension of x (even or odd)
void inv_1d_lgt53_split(int *x, int *e, int *o, int n){
#if defined(__AVX2__)
  inv_1d_lgt53_split_simd(x, e, o, n) ;
#else
  inv_1d_lgt53_split_c(x, e, o, n) ;
#endif
}
// internal functions used by 2D inverse transform in the j direction
// unpredict row o0 using even rows e0 and e1, store in row o
// o  [OUT] : 1D array of unpredicted odd terms
// o0  [IN] : 1D array of odd terms
// e0  [IN] : 1D array of even terms used to predict odd termss
// e1  [IN] : 1D array of even terms used to predict odd termss
// ni  [IN] : row length
static void row_un_predict(int *o, int *o0, int *e0, int *e1, int ni){
  int i ;
  for(i=0 ; i<ni ; i++){ o[i] = un_predict(o0[i], e0[i], e1[i]) ; }
}
static void row_un_predict_edge(int *o, int *o0, int *e0, int ni){
  int i ;
  for(i=0 ; i<ni ; i++){ o[i] = un_predict_edge(o0[i], e0[i]) ; }
}

// unupdate row e0 using odd rows o0 and o1, store in row e
// e  [OUT] : 1D array of unupdated even terms
// e0  [IN] : 1D array of even terms
// o0  [IN] : 1D array of edd terms used to update even terms
// o1  [IN] : 1D array of odd terms used to update even terms
// ni  [IN] : row length
static void row_un_update(int *e, int *e0, int *o0, int *o1, int ni){
  int i ;
  for(i=0 ; i<ni ; i++){ e[i] = un_update(e0[i], o0[i], o1[i]) ; }
}
static void row_un_update_edge(int *e, int *e0, int *o0, int ni){
  int i ;
  for(i=0 ; i<ni ; i++){ e[i] = un_update_edge(e0[i], o0[i]) ; }
}

// in place 2D inverse Le Gall Tabatabai transform, in place, quadrant layout
// initial array in quadrant form
// transformed array : even/odd terms even/odd rows
// x  [INOUT] : 2D array to transform
// lni   [IN] : storage length of x rows
// ni    [IN] : length of x rows
// nj    [IN] : number of x rows
// used by inv_2d_lgt53 (VLA form)
static void inv_2d_lgt53_(int lni, int ni, int nj, int x[nj][lni]){
  int j, nie = (ni+1)/2 , njo = nj/2, nje = (nj+1)/2 ;
  int e[nje][ni] ;   // local temporary copy of even terms

  if(nj == 1){   // 1 row only, perform 1d inverse transform
    inv_1d_lgt53(&x[0][0], ni) ;
    return ;
  }
  // unupdate even rows, move to temporary array e
  row_un_update_edge(&e[0][0], &x[0][0], &x[nje][0], ni) ;  // first even row
  if(is_odd(nj)){   // last row is even, nje == njo+1
    // unupdate even rows
    for(j=1 ; j<nje-1 ; j++){ row_un_update(&e[j][0], &x[j][0], &x[nje+j-1][0], &x[nje+j][0], ni) ; }
    row_un_update_edge(&e[j][0], &x[j][0], &x[nje+njo-1][0], ni) ;   // last even row
    // unpredict odd rows
    for(j=0 ; j<njo ; j++) { row_un_predict(&x[nje+j][0], &x[nje+j][0], &e[j+1][0], &e[j][0], ni) ; }
  }else{            // last row is odd, nje == njo
    // unupdate even rows
    for(j=1 ; j<nje ; j++){ row_un_update(&e[j][0], &x[j][0], &x[nje+j-1][0], &x[nje+j][0], ni) ; }
    // unpredict odd rows
    for(j=0 ; j<njo-1 ; j++) { row_un_predict(&x[nje+j][0], &x[nje+j][0], &e[j+1][0], &e[j][0], ni) ; }
    row_un_predict_edge(&x[nje+j][0], &x[nje+j][0], &e[j][0], ni) ;
  }

  // 1d transform in the i direction, move to proper place
  if(is_odd(nj)){    // last row is even
    // odd rows
    for(j=0 ; j<njo ; j++){ inv_1d_lgt53_split(&x[j+j+1][0], &x[nje+j][0], &x[nje+j][nie], ni) ; }
    // even rows
    for(j=0 ; j<nje ; j++) { inv_1d_lgt53_split(&x[j+j][0], &e[j][0],  &e[j][nie], ni) ; }
  }else{             // last row is odd
    // odd rows
    for(j=0 ; j<njo-1 ; j++){ inv_1d_lgt53_split(&x[j+j+1][0], &x[nje+j][0], &x[nje+j][nie], ni) ; }
    inv_1d_lgt53(&x[nj-1][0], ni) ;    // last odd row
    // even rows
    for(j=0 ; j<nje ; j++) { inv_1d_lgt53_split(&x[j+j][0], &e[j][0],  &e[j][nie], ni) ; }
  }
}

// in place 2D inverse Le Gall Tabatabai transform, in place, quadrant layout
// initial array in quadrant form
// transformed array : even/odd terms even/odd rows
// x  [INOUT] : 2D array to transform
// lni   [IN] : storage length of x rows
// ni    [IN] : length of x rows
// nj    [IN] : number of x rows
void inv_2d_lgt53(int *x, int lni, int ni, int nj){
  inv_2d_lgt53_(lni, ni, nj, (void *)x) ;    // call VLA prototype function
}

// in place 2D inverse Le Gall Tabatabai transform, in place, quadrant layout,
// multiple successive transforms
// initial array in quadrant form
// transformed array : even/odd terms even/odd rows
// x  [INOUT] : 2D array to transform
// lni   [IN] : storage length of x rows
// ni    [IN] : length of x rows
// nj    [IN] : number of x rows
// nl    [IN] : number of successive transforms
void inv_2d_lgt53_n(int *x, int lni, int ni, int nj, int levels){
  if(levels > 0){
    inv_2d_lgt53_n(x, lni, (ni + 1) / 2, (nj + 1) / 2, levels-1) ;
  }
    inv_2d_lgt53_(lni, ni, nj, (void *)x) ;
}

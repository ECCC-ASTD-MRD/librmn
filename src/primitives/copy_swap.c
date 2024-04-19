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
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#if defined(__x86_64__)
#include <immintrin.h>
#endif

#include <rmn/copy_swap.h>

static uint8_t indx_08_32[] = { 3, 2, 1, 0, 7, 6, 5, 4,11,10, 9, 8,15,14,13,12, 3, 2, 1, 0, 7, 6, 5, 4,11,10, 9, 8,15,14,13,12};
static uint8_t indx_08_16[] = { 1, 0, 3, 2, 5, 4, 7, 6, 9, 8,11,10,13,12,15,14, 1, 0, 3, 2, 5, 4, 7, 6, 9, 8,11,10,13,12,15,14};
static uint8_t indx_08_64[] = { 7, 6, 5, 4, 3, 2, 1, 0,15,14,13,12,11,10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0,15,14,13,12,11,10, 9, 8};
static uint8_t indx_16_32[] = { 2, 3, 0, 1, 6, 7, 4, 5,10,11, 8, 9,14,15,12,13, 2, 3, 0, 1, 6, 7, 4, 5,10,11, 8, 9,14,15,12,13};
static uint8_t indx_16_64[] = { 6, 7, 4, 5, 2, 3, 0, 1,14,15,12,13,10,11, 8, 9, 6, 7, 4, 5, 2, 3, 0, 1,14,15,12,13,10,11, 8, 9};
static uint8_t indx_32_64[] = { 4, 5, 6, 7, 0, 1, 2, 3,12,13,14,15, 8, 9,10,11, 4, 5, 6, 7, 0, 1, 2, 3,12,13,14,15, 8, 9,10,11};

// ================================ Swap_n_m family ===============================
#if defined(__AVX2__) && defined(__x86_64__)
// shuffle nbytes bytes from src to dst using shuffle table indx
// universal shuffler used by the Swap_n_m family
// src    [IN] : source array of bytes
// dst   [OUT] : destination array of bytes
// indx   [IN] : 32 byte table, first 16 bytes == last 16 bytes, used to shuffle groups of 16 bytes
// nbytes [IN] : number of bytes to shuffle (ideally a multiple of 16)
// N.B. if nbytes is not a multiple of 16, extra bytes will be fetched from src,
//      but no extra bytes will be stored into dst
void FetchShuffleStore(void *src, void *dst, uint8_t *indx, int nbytes){
  uint8_t *s8 = (uint8_t *)src, *d8 = (uint8_t *)dst;
  __m256i vswap = _mm256_loadu_si256((__m256i const *) indx);
  while(nbytes >= 64){            // 2 x 256 bits = 64 bytes
    _mm256_storeu_si256((__m256i *)d8, _mm256_shuffle_epi8 (_mm256_loadu_si256((__m256i const *)s8), vswap));
    s8 += 32 ; d8 += 32 ;
    _mm256_storeu_si256((__m256i *)d8, _mm256_shuffle_epi8 (_mm256_loadu_si256((__m256i const *)s8), vswap));
    s8 += 32 ; d8 += 32 ;
    nbytes -= 64 ;
  }
  if(nbytes >= 32){            // 1 x 256 bits = 32 bytes
    _mm256_storeu_si256((__m256i *)d8, _mm256_shuffle_epi8 (_mm256_loadu_si256((__m256i const *)s8), vswap));
    s8 += 32 ; d8 += 32 ; nbytes -= 32 ;
  }
  if(nbytes >= 16){            // 1 x 128 bits = 16 bytes
    _mm_storeu_si128((__m128i *)d8, _mm_shuffle_epi8 (_mm_loadu_si128((__m128i const *)s8), _mm256_extractf128_si256(vswap, 0)));
    s8 += 16 ; d8 += 16 ; nbytes -= 16 ;
  }
  if(nbytes > 0){          // process the leftovers, operate on 16 bytes, only store nbytes bytes into destination
    int i ;
    uint8_t t[16] ;        // t used because of the "in place" case
    _mm_storeu_si128((__m128i *)t, _mm_shuffle_epi8 (_mm_loadu_si128((__m128i const *)s8), _mm256_extractf128_si256(vswap, 0)));
    for(i=0 ; i<nbytes && i<16 ; i++) d8[i] = t[i] ;
  }
}
#endif

void Swap_08_16(void *src, void *dst, int n16){            //  8<->16 bit endian swap
  uint16_t *s16 = (uint16_t *)src, *d16 = (uint16_t *)dst ;
#if defined(__AVX2__) && defined(__x86_64__)
  FetchShuffleStore(s16, d16, indx_08_16, n16*2) ;
#else
  int i, i0 ;
  for(i=0 ; i<n16 ; i++){
    uint16_t t16 = s16[i];
    t16 = (t16 >> 8) | (t16 << 8);
    d16[i] = t16;
  }
#endif
}

void Swap_08_32(void *src, void *dst, int n32){           //  8<->32 bit endian swap
  uint32_t *s32 = (uint32_t *)src, *d32 = (uint32_t *)dst ;
#if defined(__AVX2__) && defined(__x86_64__)
  FetchShuffleStore(s32, d32, indx_08_32, n32*4) ;
#else
  int i, i0 ;
  for(i=0 ; i<n32 ; i++){
    uint32_t t32 = s32[i];
    t32 = (t32 << 24) | ((t32 & 0xFF00) << 8) | ((t32 >> 8) & 0xFF00) | (t32 >> 24);
    d32[i] = t32;
  }
#endif
}

void Swap_08_64(void *src, void *dst, int n64){           //  8<->64 bit endian swap
  uint64_t *s64 = (uint64_t *)src, *d64 = (uint64_t *)dst;
#if defined(__AVX2__) && defined(__x86_64__)
  FetchShuffleStore(s64, d64, indx_08_64, n64*8) ;
#else
  int i, i0 ;
  for(i=0 ; i<n64 ; i++){
    uint64_t t64 = s64[i];
    t64 = (t64<<32) | (t64>>32);                                                   // swap words in doubleword
    t64 = ((t64 & 0x0000FFFF0000FFFF) << 16) | ((t64 >> 16) & 0x0000FFFF0000FFFF); // swap halfwords in words
    t64 = ((t64 & 0x00FF00FF00FF00FF) << 8)  | ((t64 >>  8) & 0x00FF00FF00FF00FF); // swap bytes in halfwords
    d64[i] = t64;
  }
#endif
}

void Swap_16_32(void *src, void *dst, int n32){           // 16<->32 bit endian swap
  uint32_t *s32 = (uint32_t *)src, *d32 = (uint32_t *)dst ;
#if defined(__AVX2__) && defined(__x86_64__)
  FetchShuffleStore(s32, d32, indx_16_32, n32*4) ;
#else
  int i, i0 ;
  for(i=0 ; i<n32 ; i++){
    uint32_t t32 = s32[i];
    t32 = (t32 << 16) | (t32 >> 16);
    d32[i] = t32;
  }
#endif
}

void Swap_16_64(void *src, void *dst, int n64){           // 16<->64 bit endian swap
  uint64_t *s64 = (uint64_t *)src, *d64 = (uint64_t *)dst ;
#if defined(__AVX2__) && defined(__x86_64__)
  FetchShuffleStore(s64, d64, indx_16_64, n64*8) ;
#else
  int i, i0 ;
  for(i=0 ; i < n64 ; i++){
    uint64_t t64 = s64[i];
    t64 = (t64 >> 32) | (t64 << 32);                                               // swap words in doubleword
    t64 = ((t64 & 0x00FF00FF00FF00FF) << 16) | ((t64 >> 16) & 0x00FF00FF00FF00FF); // swap halfwords in words
    d64[i] = t64;
  }
#endif
}

void Swap_32_64(void *src, void *dst, int n64){            // 32<->64 bit endian swap
  uint64_t *s64 = (uint64_t *)src, *d64 = (uint64_t *)dst ;
#if defined(__AVX2__) && defined(__x86_64__)
  FetchShuffleStore(s64, d64, indx_32_64, n64*8) ;
//   __m256i vs0, vs1 ;
//   for(i0=0 ; i0 < n64 - 7 ; i0 +=8){                       // 32/64 endian swap of 8 64 bit elements
//     vs0 = _mm256_loadu_si256((__m256i const *)(s64  ));    // load 4 64 bit elements
//     vs1 = _mm256_loadu_si256((__m256i const *)(s64+4));    // load 4 64 bit elements
//     vs0 = _mm256_shuffle_epi32(vs0 , 0b10110001);          // 1, 0, 3, 2
//     vs1 = _mm256_shuffle_epi32(vs1 , 0b10110001);          // 1, 0, 3, 2
//     _mm256_storeu_si256((__m256i *)(d64  ), vs0);          // store 4 64 bit elements
//     _mm256_storeu_si256((__m256i *)(d64+4), vs1);          // store 4 64 bit elements
//     s64 += 8;
//     d64 += 8;
//   }
//   for(i=0 ; i<7 && (i0+i)<n64 ; i++){                      // up to 7 tokens
//     uint64_t t64 = s64[i];
//     t64 = (t64 >> 32) | (t64 << 32);
//     d64[i] = t64;
//   }
#else
  int i, i0 ;
  for(i=0 ; i < n64 ; i++){
    uint64_t t64 = s64[i];
    t64 = (t64 >> 32) | (t64 << 32);
    d64[i] = t64;
  }
#endif
}

// ================================ Copy_items family ===============================
static uint32_t magic = 0b011101001 ;   // bits 0, 3, 5, 6, 7 are ON, bits 1, 2, 4, 8 are OFF

// copy variable length (8/16/32/64 bit) items into 8/16/32/64 bit containers
// larger item filled from right to left _r2l functions)
// larger item filled from left to right _l2r functions)
// BIG ENDIAN CODE IS UNTESTED
//
// src     [IN] : pointer to source data
// srclen  [IN] : length in bytes of src elements (1/2/4/8)
// dst    [OUT] : pointer to destination
// dstlen  [IN] : length in bytes of dst elements (1/2/4/8)
// ns      [IN] : number of src elements to cpy into dst
// the function returns the number of destination elements
// right to left fill/extract to/from larger item
int Copy_items_r2l(void *src, uint32_t srclen, void *dst, uint32_t dstlen, uint32_t ns){
  uint32_t nd = (ns*srclen + dstlen - 1) / dstlen ;

  if( (srclen > 8) || (dstlen > 8) ) return -1 ;                       // src/dst length > 8
  if( (1 & (magic >> srclen)) || (1 & (magic >> dstlen)) ) return 1 ;  // src/dst length not 1/2/4/8
  if(*le){                                   // little endian host
    if(src != dst){                          // in place is a NO-OP
//       memcpy(dst, src, ns*srclen) ;          // straight copy on a little endian host
      memcpy(dst, src, nd*dstlen) ;          // straight copy on a little endian host
    }
  }else{                                     // big endian host
    if(srclen == dstlen){                    // same length for source and destination elements
//       if(src != dst) memcpy(dst, src, ns*srclen) ;
      if(src != dst) memcpy(dst, src, nd*dstlen) ;
      return nd ;
    }
    return -1 ;                              // BIG ENDIAN VERSION NOT IMPLEMENTED YET
  }
  return nd ;
}
// left to right fill/extract to/from larger item
int Copy_items_l2r(void *src, uint32_t srclen, void *dst, uint32_t dstlen, uint32_t ns){
  uint32_t nd ;
  uint8_t  *s8  = (uint8_t  *)src, *d8  = (uint8_t  *)dst, t8 ;
  uint16_t *s16 = (uint16_t *)src, *d16 = (uint16_t *)dst, t16 ;
  uint32_t *s32 = (uint32_t *)src, *d32 = (uint32_t *)dst, t32 ;
  uint64_t *s64 = (uint64_t *)src, *d64 = (uint64_t *)dst, t64 ;
  int i, j ;

  if( (srclen > 8) || (dstlen > 8) ) return -1 ;                       // src/dst length > 8
  if( (1 & (magic >> srclen)) || (1 & (magic >> dstlen)) ) return 1 ;  // src/dst length not 1/2/4/8

  if(srclen == dstlen){                      // same length for source and destination elements
    if(src != dst) {                         // in place is a NO-OP
      uint32_t nd = (ns*srclen + dstlen - 1) / dstlen ;
      memcpy(dst, src, nd*dstlen) ;          // straight copy if not in place
    }
    return nd = ns ;
  }

  if(*le){                                   // little endian host
    switch(dstlen) {
      case 1 :           // destination is 8 bits wide
        nd = ns * srclen ;
        switch (srclen){
          case 2 :       // 16 bits to 8 bits
            Swap_08_16(s8, d16, ns) ;
//             for(i=0 ; i<ns ; i++) { t16 = *s16 ; d8[0] = t16 >> 8 ; d8[1] = t16 & 0xFF ;
//                                     s16++ ; d8 += 2 ; } ;
            break ;
          case 4 :       // 32 bits to 8 bits
            Swap_08_32(s8, d32, ns) ;
//             for(i=0 ; i<ns ; i++) { t32 = *s32 ; d8[0] = (t32>>24) ; d8[1] = (t32>>16) & 0xFF ; 
//                                     d8[2] = (t32>>8) & 0xFF ; d8[3] = t32 & 0xFF ;
//                                     s32++ ; d8 += 4 ; } ;
            break ;
          case 8 :       // 64 bits to 8 bits
            Swap_08_64(s8, d64, ns) ;
//             for(i=0 ; i<ns ; i++) { t64 = *s64 ; d8[0] = t64>>56 ; d8[1] = (t64>>48) & 0xFF ; d8[2] = (t64>>40) & 0xFF ;
//                                     d8[3] = (t64>>32) & 0xFF ; d8[4] = (t64>>24) & 0xFF ; d8[5] = (t64>>16) & 0xFF ;
//                                     d8[6] = (t64>>8) & 0xFF  ; d8[7] = t64 & 0xFF ;
//                                     s64++ ; d8 += 8 ; } ;
            break ;
        }
        break ;

      case 2 :           // destination is 16 bits wide
        switch (srclen){
          case 1 :       // 8 bits to 16 bits
            nd = (ns+1) / 2 ;
            Swap_08_16(s8, d16, nd) ;
//             for(i=0 ; i<nd ; i++) { t16 = s8[0] ; t16 = (t16 << 8) | s8[1] ; d16[0] = t16 ;
//                                     s8 += 2 ; d16++ ; }
            break ;
          case 4 :       // 32 bits to 16 bits
            nd = ns * 2 ;
            Swap_16_32(s32, d16, ns) ;
//             for(i=0 ; i<ns ; i++) { t32 = *s32 ; d16[0] = t32 >> 16 ; d16[1] = t32 & 0xFFFF ;
//                                     s32++ ; d16 += 2 ; }
            break ;
          case 8 :       // 64 bits to 16 bits
            nd = ns * 4 ;
            Swap_16_64(s64, d16, ns) ;
//             for(i=0 ; i<ns ; i++) { t64 = *s64 ; d16[0] = t64>>48 ; d16[1] = (t64>>32) & 0xFFFF ;
//                                     d16[2] = (t64>>16) & 0xFFFF ; d16[3] = t64 & 0xFFFF ;
//                                     s64++ ; d16 += 4 ; }
            break ;
        }
        break ;

      case 4 :           // destination is 32 bits wide
        switch (srclen){
          case 1 :       // 8 bits to 32 bits
            nd = (ns+3) / 4 ;
            Swap_08_32(s8, d32, nd) ;
//             for(i=0 ; i<nd ; i++) { t32 = (s8[0] << 24) | (s8[1] << 16) | (s8[2] << 8) | s8[3] ; d32[0] = t32 ; 
//                                     d32++ ; s8 += 4 ; }
            break ;
          case 2 :       // 16 bits to 32 bits
            nd = (ns+1) / 2 ;
            Swap_16_32(s16, d32, nd) ;
//             for(i=0 ; i<nd ; i++) { t32 = (s16[0] << 16) | s16[1] ; d32[0] = t32 ; 
//                                     d32++ ; s16 += 2 ; }
            break ;
          case 8 :       // 64 bits to 32 bits
            nd = ns * 2 ;
            Swap_32_64(s64, d32, ns) ;
//             for(i=0 ; i<ns ; i++) { t64 = *s64 ; d32[0] = t64 >> 32 ; d32[1] = t64 & 0xFFFFFFFF ; 
//                                     d32 += 2  ; s64++; }
            break ;
        }
        break ;

      case 8 :           // destination is 64 bits wide
        switch (srclen){
          case 1 :       // 8 bits to 64 bits
            nd = (ns+7) / 8 ;
            Swap_08_64(s8, d64, nd) ;
//             for(i=0 ; i<nd ; i++) { t64 = s8[0] ;          t64 = (t64<<8)|s8[1] ; t64 = (t64<<8)|s8[2] ; t64 = (t64<<8)|s8[3] ;
//                                     t64 = (t64<<8)|s8[4] ; t64 = (t64<<8)|s8[5] ; t64 = (t64<<8)|s8[6] ; t64 = (t64<<8)|s8[7] ;
//                                     d64[0] = t64 ; d64++ ; s8 += 8 ; }
            break ;
          case 2 :       // 16 bits to 64 bits
            nd = (ns+3) / 4 ;
            Swap_16_64(s16, d64, nd) ;
//             for(i=0 ; i<nd ; i++) { t64 = s16[0] ; t64 = (t64<<16)|s16[1] ; t64 = (t64<<16)|s16[2] ; t64 = (t64<<16)|s16[3] ;
//                                     d64[0] = t64 ; d64++ ; s16 += 4 ; }
            break ;
          case 4 :       // 32 bits to 64 bits
            nd = (ns+1) / 2 ;
            Swap_32_64(s32, d64, nd) ;
//             for(i=0 ; i<nd ; i++) { t64 = s32[0] ; t64 = (t64<<32)|s32[1] ;
//                                     d64[0] = t64 ; d64++ ; s32 += 2 ; }
            break ;
        }
        break ;
    }
  }else{                                     // big endian host
    if(src != dst){                          // in place is a NO-OP
      memcpy(dst, src, ns*srclen) ;           // straight copy on a big endian host
    }
  }
  return nd ;
}

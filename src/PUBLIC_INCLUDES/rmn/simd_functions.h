// Hopefully useful code for C (memory block movers)
// Copyright (C) 2022-2025  Recherche en Prevision Numerique
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

// protect the whole file againt double inclusion
#if ! defined(SIMD_LOOP)

// use SIMD intrinsics by default
#if ! defined(NO_SIMD) && ! defined(EMULATE_SIMD)
#define USE_SIMD_INTRINSICS
#endif

#if defined(USE_SIMD_INTRINSICS)

#if defined(__x86_64__)          // for now only X86_64 intrinsics are supported
#define USE_INTEL_SIMD_INTRINSICS
#define WITH_SIMD
#undef NO_SIMD
#undef EMULATE_SIMD

#else                            // other platforms
#define EMULATE_SIMD
#undef USE_SIMD_INTRINSICS
#undef USE_INTEL_SIMD_INTRINSICS
#define ALIAS_INTEL_SIMD_INTRINSICS

#endif                           // defined(__x86_64__)

#endif   // defined(USE_SIMD_INTRINSICS)

#if defined(NO_SIMD) || defined(EMULATE_SIMD)

// do not attempt to use the Intel or other SIMD intrincics
#undef USE_INTEL_SIMD_INTRINSICS
#undef USE_SIMD_INTRINSICS
// emulate Intel intrinsics if found in code
#define ALIAS_INTEL_SIMD_INTRINSICS

#else // neither NO_SIMD nor EMULATE_SIMD
#define WITH_SIMD

#if defined(__x86_64__)                // for now only X86_64 intrinsics are supported
#define USE_INTEL_SIMD_INTRINSICS
#else
#define ALIAS_INTEL_SIMD_INTRINSICS    // other platforms
#endif

#endif // NO_SIMD EMULATE_SIMD

#if defined(NO_SIMD)
#undef WITH_SIMD     // EMULATE_SIMD does not force NO_SIMD
#endif
// =================================================================================================================
#include <stdio.h>
#include <stdint.h>
#include <rmn/ct_assert.h>

// 128 bit struct (allows 128 bit assignments) (2 x 64 bits)
typedef  struct{ union {int64_t i64[2] ; uint64_t u64[2] ;                 } ; } u128_t ;
// 256 bit struct (allows 256 bit assignments) (2 x 128 bits or 4 x 64 bits)
typedef  struct{ union {int64_t i64[4] ; uint64_t u64[4] ; u128_t u128[2] ;} ; } u256_t ;

// plain C code uses these unions of arrays for SIMD types
typedef struct{ union{ double d[2]; int64_t i64[2]; uint64_t u64[2]; float f[4]; int32_t i32[4]; uint32_t u32[4];
                       int16_t i16[ 8]; uint16_t u16[ 8]; int8_t i8[16]; uint8_t u8[16] ; u128_t u128 ; } ; } vec_128 ;
CT_ASSERT(sizeof(vec_128) == 16, "ERROR: sizeof(vec_128) MUST BE 16")

typedef struct{ union{ double d[4]; int64_t i64[4]; uint64_t u64[2]; float f[8]; int32_t i32[8]; uint32_t u32[8]; u256_t u256 ;
                       int16_t i16[16]; uint16_t u16[16]; int8_t i8[32]; uint8_t u8[32] ; u128_t u128[2] ; } ; } vec_256 ;
CT_ASSERT(sizeof(vec_256) == 32, "ERROR: sizeof(vec_256) MUST BE 32")

#if defined(USE_INTEL_SIMD_INTRINSICS)

#if defined(__x86_64__)
#undef ALIAS_INTEL_SIMD_INTRINSICS
#include <immintrin.h>
#endif

#if defined(VERBOSE_SIMD)
#warning "simd_functions : using Intel SIMD intrinsics"
#endif

#else     // not defined(USE_INTEL_SIMD_INTRINSICS)

#define ALIAS_INTEL_SIMD_INTRINSICS
#if defined(VERBOSE_SIMD)
#warning "simd_functions : using EMULATED Intel SIMD intrinsics"
#endif

#endif    // defined(USE_INTEL_SIMD_INTRINSICS)

#if defined(ALIAS_INTEL_SIMD_INTRINSICS) && defined(VERBOSE_SIMD)   // not true if defined(USE_INTEL_SIMD_INTRINSICS)
#warning "simd_functions : ALIASING Intel SIMD intrinsics"
#endif   // defined(ALIAS_INTEL_SIMD_INTRINSICS)

// =================================================================================================================

#define SIMD_STATIC static inline
#define SIMD_LOOP(N, OPER) { int i ;  for(i=0 ; i<N ; i++) { OPER ; } ; }
// functions with pure vector loop
#define SIMD_FN(SCOPE, KIND, N, FN, OPER) SCOPE KIND FN { KIND R ; SIMD_LOOP(N, OPER) ; return R ; }
#define VOID_FN(SCOPE, N, FN, OPER)       SCOPE void FN {          SIMD_LOOP(N, OPER) ;            }
// SIMD_FN/VOID_FN with loop preamble and postamble
#define SIMD_FN_P(SCOPE, KIND, N, FN, OPER, PRE, POST) SCOPE KIND FN { KIND R ; PRE ; SIMD_LOOP(N, OPER) ; POST ; return R ; }
#define VOID_FN_P(SCOPE, N, FN, OPER, PRE, POST)       SCOPE void FN {        ; PRE ; SIMD_LOOP(N, OPER) ; POST ;            }

// #if defined(__x86_64__) && defined(USE_INTEL_SIMD_INTRINSICS)
#if defined(USE_INTEL_SIMD_INTRINSICS)

// use Intel X86_64 SIMD intrinsics

// casts (true casts when using native Intel SIMD intrinsics)
#define __V256   (__m256)
#define __V256f  (__m256)
#define __V256i  (__m256i)
#define __V256d  (__m256d)
#define __V128   (__m128)
#define __V128f  (__m128)
#define __V128i  (__m128i)
#define __V128d  (__m128d)

// define __vxxx types as the corresponding __mxxx
typedef __m256  __v256  ;
typedef __m256  __v256f ;
typedef __m256i __v256i ;
typedef __m256d __v256d ;
typedef __m128  __v128  ;
typedef __m128  __v128f ;
typedef __m128i __v128i ;
typedef __m128d __v128d ;

#if defined(__PGIC__)
#undef _mm256_alignr_epi8
#define _mm256_alignr_epi8(a, b, imm) \
_mm256_set_m128i(_mm_alignr_epi8(_mm256_extracti128_si256(a, 1), _mm256_extracti128_si256(b, 1), imm) , \
                 _mm_alignr_epi8(_mm256_extracti128_si256(a, 0), _mm256_extracti128_si256(b, 0), imm) )
#endif

// implementation of vector shifts
// _mm256_alignr_epi32 and _mm_alignr_epi32 for AVX2
// shift 64 byte vh|vl right by (imm8 & 7) * 32 bits
#if ! defined(__AVX512F__) || ! defined(__AVX512VL__)
#if defined(__AVX2__)

#undef _mm_alignr_epi32
#define _mm_alignr_epi32(VH, VL, IMM8) _mm_alignr_epi8(VH, VL, IMM8*4)

#undef _mm256_alignr_epi32
#define _mm256_alignr_epi32(VH, VL, IMM8)  \
  (IMM8 > 4) ? \
  _mm256_alignr_epi8( VH , _mm256_permute2x128_si256(VL, VH, 0x21) , (IMM8-4)*4 ) : \
  _mm256_alignr_epi8( _mm256_permute2x128_si256(VL, VH, 0x21) , VL , (IMM8  )*4 ) ;

#endif  // defined(__AVX2__)
#endif  // ! defined(__AVX512F__) || ! defined(__AVX512VL__)

// shift 32/64 byte vh|vl right by imm8 bytes
#if defined(__AVX2__)
#define _mm_bsrli2_epi128(VH, VL, IMM8) _mm_alignr_epi8(VH, VL, IMM8)
#define _mm256_bsrli2_epi256(VH, VL, IMM8)  \
  (IMM8 > 16) ? \
  _mm256_alignr_epi8( VH , _mm256_permute2x128_si256(VL, VH, 0x21) , (IMM8-16) ) : \
  _mm256_alignr_epi8( _mm256_permute2x128_si256(VL, VH, 0x21) , VL , (IMM8   ) ) ;
#endif

// =================================================================================================================

#else    // defined(USE_INTEL_SIMD_INTRINSICS)

// use C version of SIMD functions

// casts (fake casts with simulated SIMD intrinsics, all typedefs are the same)
#define __V256
#define __V256f
#define __V256i
#define __V256d
#define __V128
#define __V128f
#define __V128i
#define __V128d

// define all _m256x and _v256x types as the universal vec_256 type
// define all _m128x and _v128x types as the universal vec_128 type
typedef vec_256 __m256  ;
typedef vec_256 __m256i ;
typedef vec_256 __m256d ;
typedef vec_128 __m128  ;
typedef vec_128 __m128i ;
typedef vec_128 __m128d ;

typedef vec_256 __v256  ;
typedef vec_256 __v256f ;
typedef vec_256 __v256i ;
typedef vec_256 __v256d ;
typedef vec_128 __v128  ;
typedef vec_128 __v128f ;
typedef vec_128 __v128i ;
typedef vec_128 __v128d ;

#endif    // defined(USE_INTEL_SIMD_INTRINSICS)

// =================================================================================================================

// some functions are always "aliased"
// print functions
#define _mm_print_pd         print_v2d
#define _mm_print_ps         print_v4f
#define  _mm_print_epu64     print_v2l
#define _mm_print_epu32      print_v4i
#define _mm_print_epu16      print_v8h
#define _mm_print_epu8       print_v16c
#define _mm256_print_pd      print_v4d
#define _mm256_print_ps      print_v8f
#define _mm256_print_epu64   print_v4l
#define _mm256_print_epu32   print_v8i
#define _mm256_print_epu16   print_v16h
#define _mm256_print_epu8    print_v32c
// vector mask functions
#define _mm256_memmask_epi32 mask_v8i
#define _mm_memmask_epi32    mask_v4i

// =================================================================================================================
#if defined(ALIAS_INTEL_SIMD_INTRINSICS)

#define _mm256_set1_ps         set1_v8f
#define _mm_set1_ps            set1_v4f
#define _mm256_set1_pd         set1_v4d
#define _mm_set1_pd            set1_v2d
#define _mm256_set1_epi64x     set1_v4l
#define _mm_set1_epi64x        set1_v2l
#define _mm256_set1_epi32      set1_v8i
#define _mm_set1_epi32         set1_v4i
#define _mm256_setzero_si256   zero_v256
#define _mm_setzero_si128      zero_v128
#define _mm256_setones_si256   ones_v256
#define _mm_setones_si128      ones_v128
#define _mm256_setr_m128i      setr_2v128

#define _mm_cvtsi32_si128      cvt_i32_v4i
#define _mm256_cvtepi8_epi32   cvt_v8c_v8i
#define _mm_cvtepi8_epi32      cvt_v4c_v4i

#define _mm256_loadu_si256     loadu_v256
#define _mm_loadu_si128        loadu_v128
#define _mm256_loadu_ps        loadu_v8f
#define _mm_loadu_ps           loadu_v4f
#define _mm256_maskload_epi32  maskload_v8i 
#define _mm_maskload_epi32     maskload_v4i 

#define _mm_storeu_si64        storeu_si64
#define _mm_storeu_si32        storeu_si32
#define _mm256_storeu_si256    storeu_v256
#define _mm_storeu_si128       storeu_v128
#define _mm256_storeu_ps       storeu_v8f
#define _mm_storeu_ps          storeu_v4f
#define _mm256_maskstore_epi32 maskstore_v8i
#define _mm_maskstore_epi32    maskstore_v4i

#define _mm256_slli_epi32      slli_v8i
#define _mm_slli_epi32         slli_v4i
#define _mm256_srli_epi32      srli_v8i
#define _mm_srli_epi32         srli_v4i
#define _mm256_srai_epi32      srai_v8i
#define _mm_srai_epi32         srai_v4i
#define _mm256_bsrli_epi128    bsrli_v256
#define _mm_bsrli_si128        bsrli_v128
#define _mm_alignr_epi8        alignr_v16c
#define _mm_bsrli2_epi128      bsrli2_v128
#define _mm256_bsrli2_epi256   bsrli2_v256
#define _mm256_alignr_epi32    alignr_v8i
#define _mm_alignr_epi32       alignr_v4i

#define _mm256_max_epi32       max_v8i
#define _mm_max_epi32          max_v4i
#define _mm256_min_epi32       min_v8i
#define _mm_min_epi32          min_v4i
#define _mm256_max_epu32       max_v8u
#define _mm_max_epu32          max_v4u
#define _mm256_min_epu32       min_v8u
#define _mm_min_epu32          min_v4u
#define _mm256_abs_epi32       abs_v8i
#define _mm_abs_epi32          abs_v4i

#define _mm256_cmpeq_epi32     cmpeq_v8i
#define _mm256_cmpgt_epi32     cmpgt_v8i
#define _mm_cmpeq_epi32        cmpeq_v4i
#define _mm_cmpgt_epi32        cmpgt_v4i

#define _mm256_castps_si256    v8f_2_v8i
#define _mm256_castsi256_ps    v8i_2_v8f
#define _mm256_castsi256_si128 v256_v128
#define _mm256_castsi128_si256 v128_v256

#define  _mm256_xor_si256      xor_v256
#define  _mm256_and_si256      and_v256
#define  _mm256_andnot_si256   andnot_v256
#define  _mm256_or_si256       or_v256
#define  _mm_xor_si128         xor_v128
#define  _mm_and_si128         and_v128
#define  _mm_andnot_si128      andnot_v128
#define  _mm_or_si128          or_v128

#define _mm256_add_ps          add_v8f
#define _mm256_sub_ps          sub_v8f
#define _mm256_add_epi32       add_v8i
#define _mm256_sub_epi32       sub_v8i
#define _mm_add_ps             add_v4f
#define _mm_sub_ps             sub_v4f
#define _mm_add_epi32          add_v4i
#define _mm_sub_epi32          sub_v4i

#define _mm256_permutevar8x32_epi32 permutev_v8i
#define _mm256_permute2x128_si256   permute2_v256
#define  _mm256_permute4x64_pd      permutei_v4d
#define  _mm256_permute4x64_epi64   permutei_v4l
#define _mm256_extracti128_si256 extracti_128
#define _mm256_inserti128_si256  inserti_128

#define _mm256_blend_epi32     blend_v8i
#define _mm256_blendv_ps       blendv_v8f
#define _mm_blendv_ps          blendv_v4f
#define _mm256_blendv_epi8     blendv_v32c
#define _mm_blendv_epi8        blendv_v16c

#define _mm256_shuffle_epi32   shuffle_v8i
#define _mm_shuffle_epi32      shuffle_v4i
#define _mm256_shuffle_ps      shuffle_2v8f
#define _mm_shuffle_ps         shuffle_2v4f

#define _mm256_unpacklo_epi32  unpacklo_v8i  
#define _mm_unpacklo_epi32     unpacklo_v4i  
#define _mm256_unpackhi_epi32  unpackhi_v8i  
#define _mm_unpackhi_epi32     unpackhi_v4i  

#endif   // defined(ALIAS_INTEL_SIMD_INTRINSICS)
// =================================================================================================================
#if defined(USE_INTEL_SIMD_INTRINSICS)

#define set1_v8f      _mm256_set1_ps
#define set1_v4f      _mm_set1_ps
#define set1_v4d      _mm256_set1_pd
#define set1_v2d      _mm_set1_pd
#define set1_v4l      _mm256_set1_epi64x
#define set1_v2l      _mm_set1_epi64x
#define set1_v8i      _mm256_set1_epi32
#define set1_v4i      _mm_set1_epi32
#define zero_v256     _mm256_setzero_si256
#define zero_v128     _mm_setzero_si128
#define ones_v256     _mm256_setones_si256
#define ones_v128     _mm_setones_si128
#define setr_2v128    _mm256_setr_m128i
// setones is not in the official Intel intrinsics
static inline __m256i _mm256_setones_si256(void){  __m256i t = _mm256_setzero_si256() ; return _mm256_cmpeq_epi32(t, t) ; }
static inline __m128i _mm_setones_si128(void){ __m128i t = _mm_setzero_si128() ; return _mm_cmpeq_epi32(t, t) ; }

#define cvt_i32_v4i   _mm_cvtsi32_si128
#define cvt_v8c_v8i   _mm256_cvtepi8_epi32
#define cvt_v4c_v4i   _mm_cvtepi8_epi32

#define loadu_v256    _mm256_loadu_si256
#define loadu_v128    _mm_loadu_si128
#define loadu_v8f     _mm256_loadu_ps
#define loadu_v4f     _mm_loadu_ps
#define maskload_v8i  _mm256_maskload_epi32
#define maskload_v4i  _mm_maskload_epi32

#define storeu_si64   _mm_storeu_si64
#define storeu_si32   _mm_storeu_si32
#define storeu_v256   _mm256_storeu_si256
#define storeu_v128   _mm_storeu_si128
#define storeu_v8f    _mm256_storeu_ps
#define storeu_v4f    _mm_storeu_ps
#define maskstore_v8i _mm256_maskstore_epi32
#define maskstore_v4i _mm_maskstore_epi32

#define slli_v8i      _mm256_slli_epi32
#define slli_v4i      _mm_slli_epi32
#define srli_v8i      _mm256_srli_epi32
#define srli_v4i      _mm_srli_epi32
#define srai_v8i      _mm256_srai_epi32
#define srai_v4i      _mm_srai_epi32
#define alignr_v16c   _mm_alignr_epi8
#define bsrli_v256    _mm256_bsrli_epi128
#define bsrli_v128    _mm_bsrli_si128
#define bsrli2_v128   _mm_bsrli2_epi128
#define bsrli2_v256   _mm256_bsrli2_epi256
#define alignr_v8i    _mm256_alignr_epi32
#define alignr_v4i    _mm_alignr_epi32

#define max_v8i       _mm256_max_epi32
#define max_v4i       _mm_max_epi32
#define min_v8i       _mm256_min_epi32
#define min_v4i       _mm_min_epi32
#define max_v8u       _mm256_max_epu32
#define max_v4u       _mm_max_epu32
#define min_v8u       _mm256_min_epu32
#define min_v4u       _mm_min_epu32
#define abs_v8i       _mm256_abs_epi32
#define abs_v4i       _mm_abs_epi32

#define cmpeq_v8i     _mm256_cmpeq_epi32
#define cmpgt_v8i     _mm256_cmpgt_epi32
#define cmpeq_v4i     _mm_cmpeq_epi32
#define cmpgt_v4i     _mm_cmpgt_epi32

#define v8f_2_v8i     _mm256_castps_si256
#define v8i_2_v8f     _mm256_castsi256_ps
#define v256_v128     _mm256_castsi256_si128
#define v128_v256     _mm256_castsi128_si256

#define xor_v256      _mm256_xor_si256
#define and_v256      _mm256_and_si256
#define andnot_v256   _mm256_andnot_si256
#define or_v256       _mm256_or_si256
#define xor_v128      _mm_xor_si128
#define and_v128      _mm_and_si128
#define andnot_v128   _mm_andnot_si128
#define or_v128       _mm_or_si128

#define add_v8f       _mm256_add_ps
#define sub_v8f       _mm256_sub_ps
#define add_v8i       _mm256_add_epi32
#define sub_v8i       _mm256_sub_epi32
#define add_v4f       _mm_add_ps
#define sub_v4f       _mm_sub_ps
#define add_v4i       _mm_add_epi32
#define sub_v4i       _mm_sub_epi32

#define permutev_v8i   _mm256_permutevar8x32_epi32
#define permute2_v256  _mm256_permute2x128_si256
#define permutei_v4d   _mm256_permute4x64_pd
#define permutei_v4l   _mm256_permute4x64_epi64
#define extracti_128   _mm256_extracti128_si256
#define inserti_128    _mm256_inserti128_si256

#define blend_v8i      _mm256_blend_epi32
#define blendv_v8f     _mm256_blendv_ps
#define blendv_v4f     _mm_blendv_ps
#define blendv_v32c    _mm256_blendv_epi8
#define blendv_v16c    _mm_blendv_epi8

#define shuffle_v8i    _mm256_shuffle_epi32
#define shuffle_v4i    _mm_shuffle_epi32
#define shuffle_2v8f   _mm256_shuffle_ps
#define shuffle_2v4f   _mm_shuffle_ps

#define unpacklo_v8i   _mm256_unpacklo_epi32
#define unpacklo_v4i   _mm_unpacklo_epi32
#define unpackhi_v8i   _mm256_unpackhi_epi32
#define unpackhi_v4i   _mm_unpackhi_epi32

// =================================================================================================================
#else    // defined(USE_INTEL_SIMD_INTRINSICS)
SIMD_FN(SIMD_STATIC, __m256,  8, set1_v8f( float    f32 ) , R.f[i] = f32 )
SIMD_FN(SIMD_STATIC, __m128,  4, set1_v4f( float    f32 ) , R.f[i] = f32 )
SIMD_FN(SIMD_STATIC, __m256,  4, set1_v4d( double   f64 ) , R.d[i] = f64 )
SIMD_FN(SIMD_STATIC, __m128,  2, set1_v2d( double   f64 ) , R.d[i] = f64 )
SIMD_FN(SIMD_STATIC, __m256i, 4, set1_v4l( uint64_t i64 ) , R.i64[i] = i64 )
SIMD_FN(SIMD_STATIC, __m128i, 2, set1_v2l( uint64_t i64 ) , R.i64[i] = i64 )
SIMD_FN(SIMD_STATIC, __m256i, 8, set1_v8i( int32_t  i32 ) , R.i32[i] = i32 )
SIMD_FN(SIMD_STATIC, __m128i, 4, set1_v4i( int32_t  i32 ) , R.i32[i] = i32 )
SIMD_FN(SIMD_STATIC, __m256i, 8, zero_v256( void ) , R.u32[i] = 0 )
SIMD_FN(SIMD_STATIC, __m128i, 4, zero_v128( void ) , R.u32[i] = 0 )
SIMD_FN(SIMD_STATIC, __m256i, 1, setr_2v128(__m128i LO, __m128i HI) , R.u128[0] = LO.u128 ; R.u128[1] = HI.u128 ; )
SIMD_FN(SIMD_STATIC, __m256i, 8, ones_v256( void ) , R.u32[i] = 0xFFFFFFFFu )        // not part of official Intel intrinsics
SIMD_FN(SIMD_STATIC, __m128i, 4, ones_v128( void ) , R.u32[i] = 0xFFFFFFFFu )        // not part of official Intel intrinsics

SIMD_FN(SIMD_STATIC, __m128,  4, cvt_i32_v4i(int32_t  i32) , R.i32[i] = (i==0) ? i32 : 0 )  // integer to lower 32 bits of vector
SIMD_FN(SIMD_STATIC, __m256i, 8, cvt_v8c_v8i( __m128i A ) , R.i32[i] = A.i8[i] )     // convert signed 8 bit to 32 bit (8 values)
SIMD_FN(SIMD_STATIC, __m128i, 4, cvt_v4c_v4i( __m128i A ) , R.i32[i] = A.i8[i] )     // convert signed 8 bit to 32 bit (4 values)

SIMD_FN(SIMD_STATIC, __m256i, 8, loadu_v256( __m256i *mem ) , R.i32[i] = mem->i32[i] )
SIMD_FN(SIMD_STATIC, __m128i, 4, loadu_v128( __m128i *mem ) , R.i32[i] = mem->i32[i] )
SIMD_FN(SIMD_STATIC, __m256i, 8, loadu_v8f( float *mem ) ,    R.f[i] = mem[i] )
SIMD_FN(SIMD_STATIC, __m128i, 4, loadu_v4f( float *mem ) ,    R.f[i] = mem[i] )
SIMD_FN(SIMD_STATIC, __m256i, 8, maskload_v8i( int *mem, __m256i mask ) , R.i32[i] = (mask.i32[i] < 0) ? mem[i] : 0 )
SIMD_FN(SIMD_STATIC, __m128i, 4, maskload_v4i( int *mem, __m128i mask ) , R.i32[i] = (mask.i32[i]>>31) ? mem[i] : 0 )

VOID_FN(SIMD_STATIC, 1, storeu_si64(void *mem, __m128i V) , ((uint64_t *)mem)[0] =  V.u64[0] )
VOID_FN(SIMD_STATIC, 1, storeu_si32(void *mem, __m128i V) , ((uint32_t *)mem)[0] =  V.u64[0] )
VOID_FN(SIMD_STATIC, 8, storeu_v256( __m256i *mem, __m256i V ) , mem->i32[i] = V.i32[i] )
VOID_FN(SIMD_STATIC, 4, storeu_v128( __m128i *mem, __m128i V ) , mem->i32[i] = V.i32[i] )
VOID_FN(SIMD_STATIC, 8, storeu_v8f( float *mem, __m256 V ) , mem[i] = V.f[i] )
VOID_FN(SIMD_STATIC, 4, storeu_v4f( float *mem, __m128 V ) , mem[i] = V.f[i] )
VOID_FN(SIMD_STATIC, 8, maskstore_v8i( int *mem, __m256i mask, __m256 V ) , if(mask.i32[i] < 0) { mem[i] = V.i32[i] ; } )
VOID_FN(SIMD_STATIC, 4, maskstore_v4i( int *mem, __m128i mask, __m128 V ) , if(mask.i32[i] < 0) { mem[i] = V.i32[i] ; } )

SIMD_FN(SIMD_STATIC, __m256i, 8, slli_v8i( __m256i A, int count ) , R.u32[i] = (A.u32[i] << count) )
SIMD_FN(SIMD_STATIC, __m128i, 4, slli_v4i( __m128i A, int count ) , R.u32[i] = (A.u32[i] << count) )
SIMD_FN(SIMD_STATIC, __m256i, 8, srli_v8i( __m256i A, int count ) , R.u32[i] = (A.u32[i] >> count) )
SIMD_FN(SIMD_STATIC, __m128i, 4, srli_v4i( __m128i A, int count ) , R.u32[i] = (A.u32[i] >> count) )
SIMD_FN(SIMD_STATIC, __m256i, 8, srai_v8i( __m256i A, int count ) , R.i32[i] = (A.i32[i] >> count) )
SIMD_FN(SIMD_STATIC, __m128i, 4, srai_v4i( __m128i A, int count ) , R.i32[i] = (A.i32[i] >> count) )
SIMD_FN(SIMD_STATIC, __m256i,32, bsrli_v256(__m256i A, int imm8), R.u8[i] = ((i+imm8)>31) ? 0 : A.u8[i+imm8] )
SIMD_FN(SIMD_STATIC, __m128i,16, bsrli_v128(__m128i A, int imm8), R.u8[i] = ((i+imm8)>15) ? 0 : A.u8[i+imm8] )
// shift a pair of concatenated registers by a number of 8 bit or 32 bit elements
SIMD_FN(SIMD_STATIC, __m128i,16, alignr_v16c(__m128i A, __m128i B, int imm8) , R.u8[i] = ((i+imm8)>31) ? 0 : (((i+imm8)<16) ? B.u8[i+imm8] : A.u8[i+imm8-16]) )
SIMD_FN(SIMD_STATIC, __m128i,16, bsrli2_v128(__m128i A, __m128i B, int imm8) , R.u8[i] = ((i+imm8)>31) ? 0 : (((i+imm8)<16) ? B.u8[i+imm8] : A.u8[i+imm8-16]) )
SIMD_FN(SIMD_STATIC, __m256i,32, bsrli2_v256(__m256i A, __m256i B, int imm8) , R.u8[i] = (i+imm8)<32 ? B.u8[i+imm8] : A.u8[i+imm8-32] )
SIMD_FN(SIMD_STATIC, __m256i, 8, alignr_v8i(__m256i A, __m256i B, int imm8) , imm8 &= 7 ; R.i32[i] = (i+imm8)<8 ? B.i32[i+imm8] : A.i32[i+imm8-8] )
SIMD_FN(SIMD_STATIC, __m128i, 4, alignr_v4i(__m128i A, __m128i B, int imm8) , imm8 &= 7 ; R.i32[i] = (i+imm8)<4 ? B.i32[i+imm8] : A.i32[i+imm8-4] )

SIMD_FN(SIMD_STATIC, __m256i, 8, max_v8i( __m256i A, __m256i B ) , R.i32[i] = (A.i32[i] > B.i32[i]) ? A.i32[i] : B.i32[i] )
SIMD_FN(SIMD_STATIC, __m128i, 4, max_v4i( __m128i A, __m128i B ) , R.i32[i] = (A.i32[i] > B.i32[i]) ? A.i32[i] : B.i32[i] )
SIMD_FN(SIMD_STATIC, __m256i, 8, min_v8i( __m256i A, __m256i B ) , R.i32[i] = (A.i32[i] < B.i32[i]) ? A.i32[i] : B.i32[i] )
SIMD_FN(SIMD_STATIC, __m128i, 4, min_v4i( __m128i A, __m128i B ) , R.i32[i] = (A.i32[i] < B.i32[i]) ? A.i32[i] : B.i32[i] )
SIMD_FN(SIMD_STATIC, __m256i, 8, max_v8u( __m256i A, __m256i B ) , R.u32[i] = (A.u32[i] > B.u32[i]) ? A.u32[i] : B.u32[i] )
SIMD_FN(SIMD_STATIC, __m128i, 4, max_v4u( __m128i A, __m128i B ) , R.u32[i] = (A.u32[i] > B.u32[i]) ? A.u32[i] : B.u32[i] )
SIMD_FN(SIMD_STATIC, __m256i, 8, min_v8u( __m256i A, __m256i B ) , R.u32[i] = (A.u32[i] < B.u32[i]) ? A.u32[i] : B.u32[i] )
SIMD_FN(SIMD_STATIC, __m128i, 4, min_v4u( __m128i A, __m128i B ) , R.u32[i] = (A.u32[i] < B.u32[i]) ? A.u32[i] : B.u32[i] )
SIMD_FN(SIMD_STATIC, __m256i, 8, abs_v8i(__m256i A) , R.i32[i] = (A.i32[i] < 0) ? -A.i32[i] : A.i32[i] )
SIMD_FN(SIMD_STATIC, __m128i, 4, abs_v4i(__m128i A) , R.i32[i] = (A.i32[i] < 0) ? -A.i32[i] : A.i32[i] )

SIMD_FN(SIMD_STATIC, __m256i, 8, cmpeq_v8i(__m256i A, __m256i B), R.i32[i] = (A.i32[i] == B.i32[i]) ? -1 : 0)
SIMD_FN(SIMD_STATIC, __m128i, 4, cmpeq_v4i(__m128i A, __m128i B), R.i32[i] = (A.i32[i] == B.i32[i]) ? -1 : 0)
SIMD_FN(SIMD_STATIC, __m256i, 8, cmpgt_v8i(__m256i A, __m256i B), R.i32[i] = (A.i32[i] >  B.i32[i]) ? -1 : 0)
SIMD_FN(SIMD_STATIC, __m128i, 4, cmpgt_v4i(__m128i A, __m128i B), R.i32[i] = (A.i32[i] >  B.i32[i]) ? -1 : 0)

SIMD_FN(SIMD_STATIC, __m256i, 8, v8f_2_v8i(__m256  A) , R.i32[i] = A.i32[i] )     // float to integer
SIMD_FN(SIMD_STATIC, __m256i, 8, v8i_2_v8f(__m256i A) , R.i32[i] = A.i32[i] )     // integer to float
SIMD_FN(SIMD_STATIC, __m128i, 4, v256_v128(__m256i A) , R.i32[i] = A.i32[i] )     // 256 to 128 (upper part of 256 ignored)
SIMD_FN(SIMD_STATIC, __m256i, 4, v128_v256(__m128i A) , R.i32[i] = A.i32[i] )     // 128 to 256 (upper part of 256 undefined)

SIMD_FN(SIMD_STATIC, __m256i, 8, xor_v256( __m256i A,  __m256i B), R.i32[i] = A.i32[i] ^ B.i32[i])
SIMD_FN(SIMD_STATIC, __m128i, 4, xor_v128( __m128i A,  __m128i B),    R.i32[i] = A.i32[i] ^ B.i32[i])
SIMD_FN(SIMD_STATIC, __m256i, 8, and_v256( __m256i A,  __m256i B), R.i32[i] = A.i32[i] & B.i32[i])
SIMD_FN(SIMD_STATIC, __m128i, 4, and_v128( __m128i A,  __m128i B),    R.i32[i] = A.i32[i] & B.i32[i])
SIMD_FN(SIMD_STATIC, __m256i, 8, andnot_v256( __m256i A,  __m256i B), R.i32[i] = A.i32[i] & (~B.i32[i]))
SIMD_FN(SIMD_STATIC, __m128i, 4, andnot_v128( __m128i A,  __m128i B),    R.i32[i] = A.i32[i] & (~B.i32[i]))
SIMD_FN(SIMD_STATIC, __m256i, 8, or_v256( __m256i A,  __m256i B), R.i32[i] = A.i32[i] | B.i32[i])
SIMD_FN(SIMD_STATIC, __m128i, 4, or_v128( __m128i A,  __m128i B),    R.i32[i] = A.i32[i] | B.i32[i])

SIMD_FN(SIMD_STATIC, __m256 , 8, add_v8f(__m256  A, __m256 B)  , R.f[i]   = A.f[i]   + B.f[i] )
SIMD_FN(SIMD_STATIC, __m256 , 8, sub_v8f(__m256  A, __m256 B)  , R.f[i]   = A.f[i]   - B.f[i] )
SIMD_FN(SIMD_STATIC, __m128 , 4, add_v4f(__m128  A, __m128  B) , R.f[i]   = A.f[i]   + B.f[i] )
SIMD_FN(SIMD_STATIC, __m128 , 4, sub_v4f(__m128  A, __m128  B) , R.f[i]   = A.f[i]   - B.f[i] )
SIMD_FN(SIMD_STATIC, __m256i, 8, add_v8i(__m256i A, __m256i B) , R.i32[i] = A.i32[i] + B.i32[i] )
SIMD_FN(SIMD_STATIC, __m256i, 8, sub_v8i(__m256i A, __m256i B) , R.i32[i] = A.i32[i] - B.i32[i] )
SIMD_FN(SIMD_STATIC, __m128i, 4, add_v4i(__m128i A, __m128i B) , R.i32[i] = A.i32[i] + B.i32[i] )
SIMD_FN(SIMD_STATIC, __m128i, 4, sub_v4i(__m128i A, __m128i B) , R.i32[i] = A.i32[i] - B.i32[i] )

SIMD_FN(SIMD_STATIC, __m256i, 8, permutev_v8i(__m256i A, __m256i IDX), R.i32[i] = A.i32[IDX.i32[i]] )
SIMD_STATIC __m256i permute2_v256(__m256i A, __m256i B, int imm){
  __m256i R ;
  switch(imm & 3){
    case 0: R.u128[0] = A.u128[0] ; break ;
    case 1: R.u128[0] = A.u128[1] ; break ;
    case 2: R.u128[0] = B.u128[0] ; break ;
    case 3: R.u128[0] = B.u128[1] ; break ;
  }
  if(imm & 0x80) R.u128[0] = (u128_t) {0lu, 0lu} ;
  imm >>=4 ;
  switch(imm & 3){
    case 0: R.u128[1] = A.u128[0] ; break ;
    case 1: R.u128[1] = A.u128[1] ; break ;
    case 2: R.u128[1] = B.u128[0] ; break ;
    case 3: R.u128[1] = B.u128[1] ; break ;
  }
  if(imm & 0x80) R.u128[1] = (u128_t) {0lu, 0lu} ;
  return R ;
}
SIMD_FN(SIMD_STATIC, __m256d, 4, permutei_v4d(__m256d A, int imm) , R.u64[i] = A.u64[imm&3] ; imm >>= 2 )
SIMD_FN(SIMD_STATIC, __m256i, 4, permutei_v4l(__m256i A, int imm) , R.u64[i] = A.u64[imm&3] ; imm >>= 2 )
SIMD_FN(SIMD_STATIC, __m128i, 4, extracti_128(__m256i A, int upper) , R.i32[i] = A.i32[i + (upper ? 4 : 0)] )
// SIMD_FN(SIMD_STATIC, __m256i, 4, inserti_128(__m256i A, __m128i B, int upper) , R.i32[i] = upper ? A.i32[i] : B.i32[i] ; R.i32[i+4] = upper ? B.i32[i] : A.i32[i+4] )
SIMD_FN(SIMD_STATIC, __m256i, 1, inserti_128(__m256i A, __m128i B, int upper) , R.u128[1-upper] = A.u128[1-upper] ; R.u128[upper] = B.u128 )

SIMD_FN(SIMD_STATIC, __m256,   8, blend_v8i(__m256i A, __m256i B, const int imm8), R.i32[i] = ((imm8>>i) & 1) ? B.i32[i] : A.i32[i] )
SIMD_FN(SIMD_STATIC, __m256,   8, blendv_v8f(__m256 A, __m256 B, __m256 MASK),     R.i32[i] = ((MASK.i32[i] >> 31) & (B.i32[i] ^ A.i32[i])) ^  A.i32[i] )
SIMD_FN(SIMD_STATIC, __m128,   4, blendv_v4f(__m128 A, __m128 B, __m128 MASK),     R.i32[i] = ((MASK.i32[i] >> 31) & (B.i32[i] ^ A.i32[i])) ^  A.i32[i] )
SIMD_FN(SIMD_STATIC, __m256i, 32, blendv_v32c(__m256i A, __m256i B, __m256i MASK), R.u8[i] = ((MASK.i8[i] >> 7) & (B.u8[i] ^ A.u8[i])) ^  A.u8[i] )
SIMD_FN(SIMD_STATIC, __m128i, 16, blendv_v16c(__m128i A, __m128i B, __m128i MASK), R.u8[i] = ((MASK.i8[i] >> 7) & (B.u8[i] ^ A.u8[i])) ^  A.u8[i] )

SIMD_FN(SIMD_STATIC, __m256i,  4, shuffle_v8i(__v256i A, int imm) , R.u32[i] = A.u32[imm&3] ; R.u32[i+4] = A.u32[4+(imm&3)] ; imm >>= 2 )
SIMD_FN(SIMD_STATIC, __m128i,  4, shuffle_v4i(__v128i A, int imm) , R.u32[i] = A.u32[imm&3] ;                                 imm >>= 2 )

SIMD_FN(SIMD_STATIC, __m256,   1, shuffle_2v8f(__v256 A, __m256 B, int imm) , R.u32[0] = A.u32[imm&3] ; R.u32[4] = A.u32[4+(imm&3)] ; imm >>= 2 ; \
                                                                              R.u32[1] = A.u32[imm&3] ; R.u32[5] = A.u32[4+(imm&3)] ; imm >>= 2 ; \
                                                                              R.u32[2] = B.u32[imm&3] ; R.u32[6] = B.u32[4+(imm&3)] ; imm >>= 2 ; \
                                                                              R.u32[3] = B.u32[imm&3] ; R.u32[7] = B.u32[4+(imm&3)] ; )
SIMD_FN(SIMD_STATIC, __m128,   1, shuffle_2v4f(__v128 A, __m128 B, int imm) , R.u32[0] = A.u32[imm&3] ; imm >>= 2 ; \
                                                                              R.u32[1] = A.u32[imm&3] ; imm >>= 2 ; \
                                                                              R.u32[2] = B.u32[imm&3] ; imm >>= 2 ; \
                                                                              R.u32[3] = B.u32[imm&3] ; )

SIMD_FN(SIMD_STATIC, __m256i,  1, unpacklo_v8i(__v256i A, __v256i B) , R.u32[0] = A.u32[0] ; R.u32[2]= A.u32[1] ; \
                                                                       R.u32[1] = B.u32[0] ; R.u32[3]= B.u32[1] ; \
                                                                       R.u32[4] = A.u32[4] ; R.u32[6]= A.u32[5] ; \
                                                                       R.u32[5] = B.u32[4] ; R.u32[7]= B.u32[5] ; )
SIMD_FN(SIMD_STATIC, __m128i,  1, unpacklo_v4i(__v128i A, __v128i B) , R.u32[0] = A.u32[0] ; R.u32[2]= A.u32[1] ; \
                                                                       R.u32[1] = B.u32[0] ; R.u32[3]= B.u32[1] ; )
SIMD_FN(SIMD_STATIC, __m256i,  1, unpackhi_v8i(__v256i A, __v256i B) , R.u32[0] = A.u32[2] ; R.u32[2]= A.u32[3] ; \
                                                                       R.u32[1] = B.u32[2] ; R.u32[3]= B.u32[3] ; \
                                                                       R.u32[4] = A.u32[6] ; R.u32[6]= A.u32[7] ; \
                                                                       R.u32[5] = B.u32[6] ; R.u32[7]= B.u32[7] ; )
SIMD_FN(SIMD_STATIC, __m128i,  1, unpackhi_v4i(__v128i A, __v128i B) , R.u32[0] = A.u32[2] ; R.u32[2]= A.u32[3] ; \
                                                                       R.u32[1] = B.u32[2] ; R.u32[3]= B.u32[3] ; )

#endif    // defined(USE_INTEL_SIMD_INTRINSICS)
// =================================================================================================================

#if defined(ALIAS_INTEL_SIMD_INTRINSICS)
// #define _mm256_set1_ps  set1_v8f
#endif
#if defined(USE_INTEL_SIMD_INTRINSICS)
// #define  set1_v8f _mm256_set1_ps
#else
#endif

#if ! defined(USE_INTEL_SIMD_INTRINSICS)

#endif

// ================================== derived functions ==================================

// integer blend, defined using float blend with type cast
#define _mm256_blendv_epi32  blendv_v8i
#define _mm_blendv_epi32     blendv_v4i
SIMD_FN(SIMD_STATIC, __m256i, 8, blendv_v8i(__m256i A, __m256i B, __m256i MASK), R = __V256i blendv_v8f(__V256 A, __V256 B, __V256 MASK) )
SIMD_FN(SIMD_STATIC, __m128i, 4, blendv_v4i(__m128i A, __m128i B, __m128i MASK), R = __V128i blendv_v4f(__V128 A, __V128 B, __V128 MASK) )

// ================================== print vectors ==================================

// print 128 bit vectors
SIMD_STATIC void print_v2d(char *msg, __v128d vm){
  int i ; vec_128 v ;
  storeu_v128((__v128i *) &v, __V128i vm) ;
  fprintf(stderr, "%s : ", msg) ;
  for(i=0 ; i<2 ; i++) fprintf(stderr, "%23.16E ", v.d[i]);
  fprintf(stderr, "\n") ;
}
SIMD_STATIC void print_v4f(char *msg, __v128 vm){
  int i ; vec_128 v ;
  storeu_v128((__v128i *) &v, __V128i vm) ;
  fprintf(stderr, "%s : ", msg) ;
//   for(i=0 ; i<4 ; i++) fprintf(stderr, "%11.4F ", v.f[i]);
  for(i=0 ; i<4 ; i++) fprintf(stderr, (v.f[i]>.01 || v.f[i]<99999.0f) ? "%11.4F" : "%11.4E ", v.f[i]);
  fprintf(stderr, "\n") ;
}
SIMD_STATIC void print_v2l(char *msg, __v128i vm){
  int i ; vec_128 v ;
  storeu_v128((__v128i *) &v, vm) ;
  fprintf(stderr, "%s : ", msg) ;
  for(i=0 ; i<2 ; i++) fprintf(stderr, "%23.16lx ", v.i64[i]);
  fprintf(stderr, "\n") ;
}
SIMD_STATIC void print_v4i(char *msg, __v128i vm){
  int i ; vec_128 v ;
  storeu_v128((__v128i *) &v, vm) ;
  fprintf(stderr, "%s : ", msg) ;
  for(i=0 ; i<4 ; i++) fprintf(stderr, "%11.8x ", v.u32[i]);
  fprintf(stderr, "\n") ;
}
SIMD_STATIC void print_v8h(char *msg, __v128i vm){
  int i ; vec_128 v ;
  storeu_v128((__v128i *) &v, vm) ;
  fprintf(stderr, "%s : ", msg) ;
  for(i=0 ; i<8 ; i++) fprintf(stderr, "%5.4x ", v.u16[i]);
  fprintf(stderr, "\n") ;
}
SIMD_STATIC void print_v16c(char *msg, __v128i vm){
  int i ; vec_128 v ;
  storeu_v128((__v128i *) &v, vm) ;
  fprintf(stderr, "%s : ", msg) ;
  for(i=0 ; i<16 ; i++) fprintf(stderr, "%2.2x ", v.u8[i]);
  fprintf(stderr, "\n") ;
}

// print 256 bit vectors
SIMD_STATIC void print_v4d(char *msg, __v256d vm){
  int i ; vec_256 v ;
  storeu_v256((__v256i *) &v, __V256i vm) ;
  fprintf(stderr, "%s : ", msg) ;
  for(i=0 ; i<4 ; i++) fprintf(stderr, "%23.16E ", v.d[i]);
  fprintf(stderr, "\n") ;
}
SIMD_STATIC void print_v8f(char *msg, __v256 vm){
  int i ; vec_256 v ;
  storeu_v256((__v256i *) &v, __V256i vm) ;
  fprintf(stderr, "%s : ", msg) ;
//   for(i=0 ; i<8 ; i++) fprintf(stderr, "%11.4F ", v.f[i]);
  for(i=0 ; i<8 ; i++) fprintf(stderr, (v.f[i]>.01 || v.f[i]<99999.0f) ? "%11.4F" : "%11.4E ", v.f[i]);
  fprintf(stderr, "\n") ;
}
SIMD_STATIC void print_v4l(char *msg, __v256i vm){
  int i ; vec_256 v ;
  storeu_v256((__v256i *) &v, vm) ;
  fprintf(stderr, "%s : ", msg) ;
  for(i=0 ; i<4 ; i++) fprintf(stderr, "%23.16lx ", v.i64[i]);
  fprintf(stderr, "\n") ;
}
SIMD_STATIC void print_v8i(char *msg, __v256i vm){
  int i ; vec_256 v ;
  storeu_v256((__v256i *) &v, vm) ;
  fprintf(stderr, "%s : ", msg) ;
  for(i=0 ; i<8 ; i++) fprintf(stderr, "%11.8x ", v.u32[i]);
  fprintf(stderr, "\n") ;
}
SIMD_STATIC void print_v16h(char *msg, __v256i vm){
  int i ; vec_256 v ;
  storeu_v256((__v256i *) &v, vm) ;
  fprintf(stderr, "%s : ", msg) ;
  for(i=0 ; i<16 ; i++) fprintf(stderr, "%5.4x ", v.u16[i]);
  fprintf(stderr, "\n") ;
}
SIMD_STATIC void _mm256_print_epu8(char *msg, __v256i vm){
  int i ; vec_256 v ;
  storeu_v256((__v256i *) &v, vm) ;
  fprintf(stderr, "%s : ", msg) ;
  for(i=0 ; i<32 ; i++) fprintf(stderr, "%2.2x ", v.u8[i]);
  fprintf(stderr, "\n") ;
}

// ================================== vector masks ==================================

// build a 128 bit mask (4 x 32 bits) to keep n (0-4) elements in masked operations
// mask is built as a 32 bit mask (4x8bit), then expanded to 128 bits (4x32bit)
static inline __m128i mask_v4i(int n){
  __m128i vm ;
  if(n == 0) return zero_v128() ;
  uint32_t i32 = ~0u ;                           // all 1s
  i32 = (n&3) ? i32 >> ( 8 * (4 - (n&3)) ) : i32 ;     // shift right to eliminate unneeded elements
  vm = set1_v4i(i32) ;                           // load 4 32 bit items into 128 bit register
  return cvt_v4c_v4i(vm) ;                       // convert from 8 bit to 32 bit mask (4 elements)
}

// build a 256 bit mask (8 x 32 bits) to keep n (0-8) elements in masked operations
// mask is built as a 64 bit mask (8x8bit), then expanded to 256 bits (8x32bit)
static inline __m256i mask_v8i(int n){
  __m128i vm ;
  if(n == 0) return zero_v256() ;
  uint64_t i64 = ~0lu ;                           // all 1s (64 bits)
  i64 = (n&7) ? i64 >> ( 8 * (8 - (n&7)) ) : i64 ;  // shift right to eliminate unneeded elements
  vm = set1_v2l(i64) ;                             // load 2 64 bit items into 128 bit register
  return cvt_v8c_v8i(vm) ;                        // convert from 8 bit to 32 bit mask (8 elements)
}

#if 0
// #define F4ADDI _mm_add_epi32
// #define F8ADDI _mm256_add_epi32
// #define F4MAXI _mm_max_epi32
// #define F8MAXI _mm256_max_epi32
// #define F4MINI _mm_min_epi32
// #define F8MINI _mm256_min_epi32
// #define F4MINU _mm_min_epu32
// #define F8MINU _mm256_min_epu32

#define ZERO8I(V)              V = _mm256_xor_si256(V, V)
#define ZERO8F(V)              V = (__m256)_mm256_xor_si256((__m256i)V, (__m256i)V)
#define ZERO8(V,T)             V = T _mm256_xor_si256((__m256i)V, (__m256i)V)

#define SET8I(V,VALUE)         V = _mm256_set1_epi32(VALUE)
#define SET4I(V,VALUE)         V = _mm_set1_epi32(VALUE)
#define SET8F(V,VALUE)         V = _mm256_set1_ps(VALUE)
#define SET4F(V,VALUE)         V = _mm_set1_ps(VALUE)

#define FETCH8I(V,A256)       _mm256_loadu_si256((__m256i *)(A256))
#define FETCH8F(V,A256)       _mm256_loadu_ps((float *)(A256))
#define FETCH8(V,T,A256)      T _mm256_loadu_si256((__m256i *)(A256))

#define STORE8(V,A256)        _mm256_storeu_si256((__m256i *)(A256), (__m256i)V)
#define MOVE8I(V,S256,D256)   V = FETCH8I(V, S256) ; STORE8(V, D256)
#define MOVE8F(V,S256,D256)   V = FETCH8F(V, S256) ; STORE8(V, D256)
// #define MOVE8(V,T,S256,D256)  V = (T)FETCH8I(V, S256) ; STORE8(V, D256)

#define MAXS8(V,VMAX)         _mm256_max_epi32(VMAX, V)
#define MINS8(V,VMIN)         _mm256_min_epi32(VMIN, V)
#define MINU8(V,VMI0)         _mm256_min_epu32(VMI0, V)
#define MIMA8I(V,VMIN,VMAX)   VMAX = MAXS8(VMAX, V) ; VMIN = MINS8(VMIN, V)

#define AND8I(A,B)            _mm256_and_si256((__m256i)A, (__m256i)B)
#define ADD8I(A,B)            _mm256_add_epi32((__m256i)A, (__m256i)B)
#define ABS8I(V)              _mm256_abs_epi32(V)
#define VEQ8(A,B)             _mm256_cmpeq_epi32((__m256i)A, (__m256i)B)
#define BLEND8(A,B,MASK)      (__m256i)_mm256_blendv_ps((__m256)A, (__m256)B, (__m256)MASK)

#define UPPER4(V8)            _mm256_extracti128_si256((__m256i)V8, 1)
#define LOWER4(V8)            _mm256_extracti128_si256((__m256i)V8, 0)

// fold 8 elements into 4 using 4 element function FN4
#define FOLD8_I4(FN4, V8, V4) V4 = FN4( (__m128i)UPPER4(V8) , (__m128i)LOWER4(V8) )
#define FOLD8_F4(FN4, V8, V4) V4 = FN4( (__m128)UPPER4(V8) , (__m128)LOWER4(V8) )

// fold 4 elements into 1 using 4 element function FN4
#define FOLD4_I1(FN4, V4)     V4 = FN4( (__m128i)_mm_bsrli_si128((__m128i)V4, 8), (__m128i)V4) ; V4 = FN4( (__m128i)_mm_bsrli_si128((__m128i)V4, 4), (__m128i)V4) ;
#define FOLD4_F1(FN4, V4)     V4 = FN4( (__m128)_mm_bsrli_si128((__m128i)V4, 8), (__m128)V4) ; V4 = FN4( (__m128)_mm_bsrli_si128((__m128i)V4, 4), (__m128)V4) ;

// fold 4 elements into 1 using 4 element function FN4 and store result
#define FOLD4_IS(FN4, V4, R)  FOLD4_I1(FN4, V4) ; _mm_storeu_si32(R , (__m128i)V4) ;
#define FOLD4_FS(FN4, V4, R)  FOLD4_F1(FN4, V4) ; _mm_storeu_si32(R , (__m128i)V4) ;

// fold 8 elements into 1 using 4 element function FN4 and store result
#define FOLD8_IS(FN4, V8, R) { __m128i v = FOLD8_I4(FN4, V8, v) ; FOLD4_IS(FN4, v, R) ; }
#define FOLD8_FS(FN4, V8, R) { __m128 v = FOLD8_F4(FN4, V8, v) ; FOLD4_FS(FN4, v, R) ; }

// lowest non zero absolute value
//                            -1 where V == 0    ABS value    bump zeros count  blend VMI0 where zero  unsigned minimum
#define MIN08(V,VMI0,V0,VZ) { __m256i z=VEQ8(V,V0) ; V=ABS8I(V) ; VZ=ADD8I(VZ,z) ;  V=BLEND8(V,VMI0,z) ;   VMI0=MINU8(V,VMI0) ; }
#endif

#endif // protect the whole file againt double inclusion with SIMD_LOOP

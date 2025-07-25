//
// Copyright (C) 2025  Environnement Canada
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
//     M. Valin,   Recherche en Prevision Numerique, 2025
//

#if ! defined(IEEE_FUNCTIONS_DEFINED)
#define IEEE_FUNCTIONS_DEFINED

#include <stdint.h>

// get cpu float control register
static inline int32_t get_cpu_csr(void){
#if defined(__x86_64__)
  int32_t csr ;
  __asm__ volatile ( "stmxcsr %[csr]" : [csr]  "=m" (csr) : ) ;
  return csr ;
#elif defined(__aarch64__)
  int64_t csr ;
  asm volatile ("mrs %[csr], fpcr" : [csr] "=r" (csr) : );
  return csr ;
#else
  return 0 ;
#endif
}

// set cpu float control register
static inline void set_cpu_csr(int32_t t){
#if defined(__x86_64__)
  __asm__ volatile ( "ldmxcsr %[t]" : : [t]  "m" (t) : ) ;
#elif defined(__aarch64__)
  asm volatile ("msr fpcr, %[t]" : : [t] "r" (t) : ) ;
#endif
}

// disallow processing of denormalized numbers (process as zero values)
static inline int32_t fp32_disallow_denorm(void){
  int32_t csr = get_cpu_csr() ;
#if defined(__x86_64__)
  int32_t newcsr = (csr | (0x0040) | (0x8000)) & (~0x003F) ;
  set_cpu_csr(newcsr) ;
//   set_cpu_csr(csr | (0x0040) | (0x8000)) ;
  return csr ;
#elif defined(__aarch64__)
  // flush to zero is FIZ (bit 24), 1 to flush to zero
  set_cpu_csr(csr | (1 << 24)) ;
  return csr ;
#else
  return 0 ;
#endif
}

// allow full processing of denormalized numbers
static inline int32_t fp32_allow_denorm(void){
  int32_t csr = get_cpu_csr() ;
#if defined(__x86_64__)
  int32_t newcsr = csr & (~0x0040) & (~0x8000) & (~0x003F) ;
  set_cpu_csr(newcsr) ;
  return csr ;
#elif defined(__aarch64__)
  // flush to zero is FIZ (bit 24), 1 to flush to zero
  set_cpu_csr(csr & (~(1 << 24)) ) ;
  return csr ;
#else
  return 0 ;
#endif
}

// transform a float into a fake signed integer (comparison order preserving)
// sign magnitude float to signed integer, order preserving
// both 0.0 and -0.0 come back as 0
static inline int32_t fp32_to_fi32(float f){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.f = f ;
  int32_t t = (r.i >> 31) ;   // 0 or 0xFFFFFFFF
  r.i &= 0x7FFFFFFF ;         // get rid of sign
  r.i ^= t ;                  // no-op if t == 0, negate if t == 0xFFFFFFFF
  r.i -= t ;                  // complement and add 1 is 2's complement negate
  return r.i ;                // float represented as an integer
}

// sign magnitude float to rounded and scaled signed integer, order preserving
// both 0.0 and -0.0 come back as 0
// nbits MUST NOT BE > 23
static inline int32_t fp32_to_fsi32(float f, int nbits){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.f = f ;
  int32_t round = ((1 << nbits) >> 1) ;
  int32_t t = (r.i >> 31) ;   // 0 or 0xFFFFFFFF
  r.i &= 0x7FFFFFFF ;         // get rid of sign
  r.i = (r.i + round) ;       // apply rounding term to absolute value
  r.i >>= nbits ;             // scale the absolute value, then apply sign
  r.i ^= t ;                  // no-op if t == 0, negate if t == 0xFFFFFFFF
  r.i -= t ;                  // complement and add 1 is 2's complement negate
  return r.i ;                // float represented as an integer
}

// restore float from fake integer representing float
// signed integer to sign magnitude float, order preserving
// 0 comes back as 0.0f, -0.0f is never produced
static inline float fi32_to_fp32(int32_t i){
  union{ int32_t i ; float f ; } r ;
  r.i = i ;
  int32_t t = (r.i >> 31) ;     // 0 or 0xFFFFFFFF
  r.i ^= t ;                    // no-op if t == 0, negate if t == 0xFFFFFFFF
  r.i -= t ;                    // complement and add 1 is 2's complement negate
  r.i |= (t << 31) ;            // restore sign bit
  return r.f ;                  // restored float
}

// rounded and scaled signed integer to sign magnitude float, order preserving
// nbits MUST NOT BE > 23 (and MUST be the same value previously used by fp32_to_fsi32)
// 0 comes back as 0.0f, -0.0f is never produced
static inline float fsi32_to_fp32(int32_t i, int nbits){
  union{ int32_t i ; float f ; } r ;
  int32_t t = (i >> 31) ;       // 0 or 0xFFFFFFFF
  r.i = i ;                     // will need absolute value of i
  r.i ^= t ;                    // no-op if t == 0, negate if t == 0xFFFFFFFF
  r.i -= t ;                    // complement and add 1 is 2's complement negate
  r.i <<= nbits ;               // unscale the absolute value
  r.i |= (t << 31) ;            // restore the sign bit
  return r.f ;                  // restored float
}

// sign, exponent, mantissa from 32 bit float
typedef struct{
  int16_t  s ;
  int16_t  e ;
  uint32_t m ;
}fp32_sem ;

// get sign of float value z
static inline int32_t fp32_sign(float z){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.f = z ;
  return (r.i >> 31) ;  // will return 0 or -1
}

// get biased exponent from z
static inline uint32_t fp32_exp_raw(float z){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.f = z ;
  return ((r.i >> 23) & 0xFF) ;
}

// get unbiased exponent from z
static inline int32_t fp32_exp(float z){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.f = z ;
  return ((r.i >> 23) & 0xFF) -127 ;
}

// get mantissa from float value z
static inline uint32_t fp32_mant(float z){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.f = z ;
  return (r.u & 0x7FFFFF) ;
}

// is z a NaN (not a number) ?
static inline int fp32_isnan(float z){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.f = z ;
  if((r.i & 0x7FFFFF) == 0) return 0 ;       // infinity or valid float
  return (((r.i >> 23) & 0xFF) == 0xFF) ;    // mantissa != 0 and exponent == 0xFF
}

// is z equal to infinity ?
static inline int fp32_isinf(float z){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.f = z ;
  if((r.i & 0x7FFFFF) != 0) return 0 ;       // NaN or valid float
  return (((r.i >> 23) & 0xFF) == 0xFF) ;    // mantissa == 0 and exponent == 0xFF
}

// generate a "quiet" or a "signaling" NaN
// signaling LSB == 1 : generate a "signaling" NaN
// signaling LSB == 0 : generate a "quiet" NaN
static inline float fp32_nan(int signaling){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.u = 0X7F800001 | ((signaling & 1) << 22) ;
  r.u |= (signaling  << 31) ;
  return r.f ;
}

// generate a positive or negative infinite value
// sign LSB == 0 : generate a "positive" InF
// sign LSB == 1 : generate a "negative" InF
static inline float fp32_inf(int sign){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.u = 0xFF << 23 ;
  r.u |= (sign << 31) ;
  return r.f ;
}

// positive power of 2 >= |z| if z >= 1.0
// positive power of 2 <= |z| if z < 1.0
static inline float fp32_pow2_factor(float z){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.f = z ;
  if( ((r.u >> 23) & 0xFF) >= 127 ){    // |z| >= 1.0 (exponent >= 127)
    r.u += 0x7FFFFF ;                   // bump up
  }
  r.u &= 0x7F800000 ;                   // only keep exponent
  return r.f ;
}

// bump |z| to the power of 2 >= |z|, keep sign intact
static inline float fp32_pow2_ceil(float z){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.f = z ;
  r.u += 0x007FFFFFu ;   // round up to ceiling
  r.u &= 0xFF800000u ;   // get rid of mantissa
  return r.f ;
}

// truncate |z| to the power of 2 <= |z|, keep sign intact
static inline float fp32_pow2_trunc(float z){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.f = z ;
  r.u &= 0xFF800000u ;   // get rid of mantissa
  return r.f ;
}

// round |z| to the nearest power of 2, keep sign intact
static inline float fp32_pow2_round(float z){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.f = z ;
  r.u += 0x00400000 ;   // round up in absolute value
  r.u &= 0xFF800000 ;   // get rid of mantissa
  return r.f ;
}

// return +2.0 to the power p
// if p is too large or too small, return "quiet" NaN
// p == -127 will return 0.0f
static inline float fp32_pow2(int p){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  r.u = (p <=127 && p >= -127) ? ((p + 127) << 23) : 0X7F800001u ;
  return r.f ;
}

// re-create 32 bit float from 3 integer values (sign, unbiased exponent, mantissa)
static inline float fp32_from_i3(int32_t s, int32_t e, uint32_t m){
  union{ int32_t i ; uint32_t u ; float f ; } r ;
  if(e > 128 || e < -127) return fp32_nan(0) ;  // bad exponent (e + 127 outside of 0->255)
  if(m & 0xF8000000u)     return fp32_nan(0) ;  // bad mantissa ( > 0x7FFFFFu) ;
  r.i = s ; r.i <<= 31 ;      // MSB is sign, from LSB of s
  e = (e + 127) ;             // add bias to exponent
  r.i |= (e << 23) ;          // install exponent
  r.i |= m ;                  // install mantissa
  return r.f ;
}

// split 32 bit float z into sign, exponent (unbiased), and mantissa
static inline fp32_sem fp32_to_sem(float z){
  fp32_sem r ;
  r.s = fp32_sign(z) ;
  r.e = fp32_exp(z) ;
  r.m = fp32_mant(z) ;
  return r ;
}

// re-create 32 bit float from sign/exponent/mantissa struct
static inline float fp32_from_sem(fp32_sem sem){
  return fp32_from_i3(sem.s, sem.e, sem.m) ;
}

#endif

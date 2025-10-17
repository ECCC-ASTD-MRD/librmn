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

static uint8_t cm[32], ca[32], cb[32] ;
static uint32_t ia[8] = { 0x01,  0x02, 0x03,  0x04,  0x05,  0x06, 0x07,  0x08 } ;
static uint32_t ib[8] = { 0x11,  0x12, 0x13,  0x14,  0x15,  0x16, 0x17,  0x18 } ;
static uint32_t im[8] = {    0,1u<<31,    0,1u<<31,     0,1u<<31,    0,1u<<31 } ;
// static uint32_t i0[8] = {    0,     0,    0,     0,     0,     0,    0,     0 } ;
static uint32_t ra[8] ;
static uint32_t vs[] = {  0,  1,  2,  3,  4,  5,  6,  7,   8,  9,  10, 11, 12, 13, 14, 15,
                         16, 17, 18, 19, 20, 21, 22, 23 , 24 , 25, 26, 27, 28, 29, 30, 31,
                         32 } ;
static uint8_t  cs[] = {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
                         16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                         32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
                         48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,
                         64 } ;
static float    vf[] = { 1.0f, 1.11f, 1.22f, 1.33f, 1.44f, 1.55f, 1.66f, 1.77f, 1.88f } ;

#if defined(USE_SIMD_INTRINSICS)
static char  *outfile = "./test_simd.ref" ;
#else
static char  *outfile = "./test_simd.txt" ;
#endif

int main(int argc, char **argv){
  __v128i v4ia, v4ib, v4ca, v4cb, v4cm, v4cr, v400, v411, v4ra ;
  __v256i v8ia,       v8ca, v8cb, v8cm, v8cr, v800, v811, v8ra ;
  __v128  v4fa ;
  __v128d v4da ;
  __v256  v8fa ;
  __v256d v8da ;
  u128_t a128, b128 ;
  u256_t a256, b256 ;
  vec_128 v128 ;
  vec_256 v256 ;
  int i ;

  freopen(outfile, "w", stderr) ;

  if(argc >= 2){
    start_of_test_notime(argv[0]);
  }else{
    start_of_test_notime("test_simd") ;
  }

  fprintf(stderr, "- test of 128/256 bit types\n");
  // 128 bit assignments
  a128 = (u128_t) { {{ 0LU, 1LU }} } ;    // 128 bit type cast
  b128 = (u128_t) { {{ 2LU, 3LU }} } ;
  v128.u128 = a128 ;
  for(i=0 ; i<2 ; i++) if(v128.i64[i] != i  ) exit(1) ;
  v128.u128 = b128 ;
  for(i=0 ; i<2 ; i++) if(v128.i64[i] != i+2) exit(1) ;
  a256.u128[0] = a128 ;
  a256.u128[1] = b128 ;
  v256.u128[0] = a128 ;
  v256.u128[1] = b128 ;
  // 256 bit assignments
  b256 = a256 ;
  for(i=0 ; i<4 ; i++) if(b256.i64[i] != i) exit(1) ;
  v256.u256 = b256 ;
  for(i=0 ; i<4 ; i++) if(v256.i64[i] != i) exit(1) ;
  fprintf(stderr, "SUCCESS\n") ;

  fprintf(stderr, "- constants\n");
  v400 = set1_v4i(1) ;
  v400 = _mm_xor_si128(v400, v400)    ; v411 = _mm_cmpeq_epi32(v400, v400) ;
  _mm_print_epu32("v400", v400) ; _mm_print_epu32("v411", v411) ;
  v800 = set1_v8i(1) ;
  v800 = _mm256_xor_si256(v800, v800) ; v811 = _mm256_cmpeq_epi32(v800, v800) ;
  _mm256_print_epu32("v800", v800) ; _mm256_print_epu32("v811", v811) ;

  fprintf(stderr, " set1_v4l(8) set1_v2l(8/64/8/64)\n");
  v8ia = set1_v4l(0x0807060504030201lu) ; print_v32c("v8ia", v8ia) ;
  v4ia = set1_v2l(0x0807060504030201lu) ; print_v16c("v4ia", v4ia) ;
  v4ia = set1_v2l(0x0807060504030201lu) ; print_v2l("v4ia", v4ia) ;
  v4ib = set1_v2l(0xF8F7F6F5F4F3F2F1lu) ; print_v16c("v4ib", v4ib) ;
  v4ib = set1_v2l(0xF8F7F6F5F4F3F2F1lu) ; _mm_print_epu64("v4ib", v4ib) ;

  fprintf(stderr, "- cvt_v8c_v8i cvt_v4c_v4i cvt_v4c_v4i\n");
  v8ia = cvt_v8c_v8i(v4ia) ; print_v8i("v8ia", v8ia) ;
  v4ia = cvt_v4c_v4i(v4ia) ; print_v4i("v4ia", v4ia) ;
  v4ib = cvt_v4c_v4i(v4ib) ; print_v4i("v4ib", v4ib) ;

  fprintf(stderr, "- set1_v4f set1_v2d set1_v8f set1_v4d\n");
  v4fa = set1_v4f(-1.23456f) ;   _mm_print_ps("v4fa", v4fa) ;
  v4da = set1_v2d(-2.3456789) ;  print_v2d("v4da", v4da) ;
  v8fa = set1_v8f(1.23456f) ;    print_v8f("v8fa", v8fa) ;
  v8da = set1_v4d(2.3456789) ;   _mm256_print_pd("v8da", v8da) ;

  fprintf(stderr, "- maskload_v4i(3) maskload_v8i(5/3) maskstore_v4i(3) maskstore_v8i(5/3) maskstore_v4i(3/2)\n");
  v4ra = maskload_v4i((int *)ia, mask_v4i(3)) ; print_v4i("v4ra", v4ra) ;
  v8ra = maskload_v8i((int *)ia, mask_v8i(5)) ; print_v8i("v8ra", v8ra) ;
  v8ra = maskload_v8i((int *)ia, mask_v8i(3)) ; print_v8i("v8ra", v8ra) ;
  storeu_v128((__v128i *)ra, v4ia) ; v4ra = loadu_v128((__v128i *)ra) ; _mm_print_epu32("v4ra0", v4ra) ;
  maskstore_v4i((int *)ra, mask_v4i(3), v411) ; v4ra = loadu_v128((__v128i *)ra) ; _mm_print_epu32("v4ra1", v4ra) ;
  maskstore_v4i((int *)ra, mask_v4i(2), v400) ; v4ra = loadu_v128((__v128i *)ra) ; _mm_print_epu32("v4ra2", v4ra) ;
  storeu_v256((__v256i *)ra, v8ia) ; v8ra = loadu_v256((__v256i *)ra) ; _mm256_print_epu32("v8ra0", v8ra) ;
  maskstore_v8i((int *)ra, mask_v8i(5), v811) ; v8ra = loadu_v256((__v256i *)ra) ; _mm256_print_epu32("v8ra1", v8ra) ;
  maskstore_v8i((int *)ra, mask_v8i(3), v800) ; v8ra = loadu_v256((__v256i *)ra) ; _mm256_print_epu32("v8ra2", v8ra) ;

  fprintf(stderr, "- insert_128 (v400 -> v811 upper)\n");
  _mm256_print_epu32("v811", v811) ; _mm_print_epu32("v400", v400) ;
  v8ra = inserti_128(v811, v400, 1) ;
  _mm256_print_epu32("v8ra", v8ra) ;

  fprintf(stderr, "- insert_128 (v400 -> v811 lower)\n");
  _mm256_print_epu32("v811", v811) ; _mm_print_epu32("v400", v400) ;
  v8ra = inserti_128(v811, v400, 0) ;
  _mm256_print_epu32("v8ra", v8ra) ;

  fprintf(stderr, "- alignr_v8i/alignr_v4i\n") ;
  __v256i v8l = loadu_v256((__v256i *) &(vs[0])) ;
  print_v8i("v8l ", v8l) ;
  __v256i v8h = loadu_v256((__v256i *) &(vs[8])) ;
  print_v8i("v8h ", v8h) ;
#if ! defined(__PGIC__xx)
  v8ra = alignr_v8i(v8h, v8l, 0) ;
  print_v8i(">> 0", v8ra) ;
  v8ra = alignr_v8i(v8h, v8l, 1) ;
  print_v8i(">> 1", v8ra) ;
  v8ra = alignr_v8i(v8h, v8l, 4) ;
  print_v8i(">> 4", v8ra) ;
  v8ra = alignr_v8i(v8h, v8l, 5) ;
  print_v8i(">> 5", v8ra) ;
  v8ra = alignr_v8i(v8h, v8l, 7) ;
  print_v8i(">> 7", v8ra) ;
#endif
  __v128i v4l = loadu_v128((__v128i *) &(vs[0])) ;
  print_v4i("v4l ", v4l) ;
  __v128i v4h = loadu_v128((__v128i *) &(vs[4])) ;
  print_v4i("v4h ", v4h) ;
  v4ra = alignr_v4i(v4h, v4l, 0) ;
  print_v4i(">> 0", v4ra) ;
  v4ra = alignr_v4i(v4h, v4l, 1) ;
  print_v4i(">> 1", v4ra) ;
  v4ra = alignr_v4i(v4h, v4l, 2) ;
  print_v4i(">> 2", v4ra) ;
  v4ra = alignr_v4i(v4h, v4l, 4) ;
  print_v4i(">> 4", v4ra) ;

  fprintf(stderr, "- alignr_v16c/bsrli2_v128\n") ;
  __v128i v16l = loadu_v128((__v128i *) &(cs[ 0])) ;
  __v128i v16h = loadu_v128((__v128i *) &(cs[ 16])) ;
  print_v16c("v16l", v16l) ;
  print_v16c("v16h", v16h) ;
  __v128i v16r ;
  v16r = _mm_alignr_epi8(v16h, v16l, 7) ;
  print_v16c(">> 7", v16r) ;
  v16r = _mm_alignr_epi8(v16h, v16l,15) ;
  print_v16c(">>15", v16r) ;
  v16r = _mm_alignr_epi8(v16h, v16l,16) ;
  print_v16c(">>16", v16r) ;
  v16r = _mm_alignr_epi8(v16h, v16l,17) ;
  print_v16c(">>17", v16r) ;
  v16r = _mm_alignr_epi8(v16h, v16l,31) ;
  print_v16c(">>31", v16r) ;
  v16r = _mm_alignr_epi8(v16h, v16l,32) ;
  print_v16c(">>32", v16r) ;

#if ! defined(__PGIC__xx)
  fprintf(stderr, "- bsrli2_v256\n") ;
  __v256i v32l = loadu_v256((__v256i *) &(cs[ 0])) ;
  __v256i v32h = loadu_v256((__v256i *) &(cs[32])) ;
  print_v32c("v32l", v32l) ;
  print_v32c("v32h", v32h) ;
  __v256i v32r ;
  v32r = bsrli2_v256(v32h, v32l, 7) ;
  print_v32c(">> 7", v32r) ;
  v32r = bsrli2_v256(v32h, v32l, 15) ;
  print_v32c(">>15", v32r) ;
  v32r = bsrli2_v256(v32h, v32l, 16) ;
  print_v32c(">>16", v32r) ;
  v32r = bsrli2_v256(v32h, v32l, 24) ;
  print_v32c(">>24", v32r) ;
  v32r = bsrli2_v256(v32h, v32l, 31) ;
  print_v32c(">>31", v32r) ;
  v32r = bsrli2_v256(v32h, v32l, 32) ;
  print_v32c(">>32", v32r) ;
#endif
  fprintf(stderr, "- add\n") ;
  v8ra = add_v8i(v8h, v8l) ;
  print_v8i("h+l ", v8ra) ;
  v4ra = add_v4i(v4h, v4l) ;
  print_v4i("h+l ", v4ra) ;
  __v256 v8f1 = loadu_v8f( &(vf[0]) ) ;
  __v256 v8f2 = loadu_v8f( &(vf[1]) ) ;
  __v128 v4f1 = loadu_v4f( &(vf[4]) ) ;
  __v128 v4f2 = loadu_v4f( &(vf[5]) ) ;
  print_v8f("v8f1", v8f1) ;
  print_v8f("v8f2", v8f2) ;
  v8fa = add_v8f(v8f1, v8f2) ;
  print_v8f("v8fa", v8fa) ;
  print_v4f("v4f1", v4f1) ;
  print_v4f("v4f2", v4f2) ;
  v4fa = add_v4f(v4f1, v4f2) ;
  print_v4f("v4fa", v4fa) ;

  fprintf(stderr, "- sub\n") ;
  v8ra = sub_v8i(v8h, v8l) ;
  print_v8i("h-l ", v8ra) ;
  v4ra = sub_v4i(v4h, v4l) ;
  print_v4i("h-l ", v4ra) ;
  v8fa = sub_v8f(v8f1, v8f2) ;
  print_v8f("v8fa", v8fa) ;
  v4fa = sub_v4f(v4f1, v4f2) ;
  print_v4f("v4fa", v4fa) ;

//   for(i=0 ; i<16 ; i++) { ca[i] = i ; cb[i] = ca[i] + 0x10 ; cm[i] = (i & 1) ? 0xFF : 0x00 ; }
  for(i=0 ; i<32 ; i++) { ca[i] = i ; cb[i] = ca[i] + 0x40 ; cm[i] = (i & 1) ? 0xFF : 0x00 ; }
  v4ca = loadu_v128( (__v128i *) ca) ; v4cb = loadu_v128( (__v128i *) cb) ; v4cm = loadu_v128( (__v128i *) cm) ;
  fprintf(stderr, "- _mm_blendv_epi8\n");
  v4cr = _mm_blendv_epi8(v4ca, v4cb, v4cm) ;
  _mm_print_epu8("v4ca", v4ca) ; _mm_print_epu8("v4cb", v4cb) ; _mm_print_epu8("v4cm", v4cm) ; _mm_print_epu8("v4cr", v4cr) ;
  v4ca = loadu_v128( (__v128i *) ca) ;    v4cb = loadu_v128( (__v128i *) cb) ;
  fprintf(stderr, "- _mm_blendv_ps\n");
  v4cm = loadu_v128( (__v128i *) im) ;
  v4cr = __V128i _mm_blendv_ps(__V128 v4ca, __V128 v4cb, __V128 v4cm) ;
  _mm_print_epu32("v4ca", v4ca) ; _mm_print_epu32("v4cb", v4cb) ; _mm_print_epu32("v4cm", v4cm) ; _mm_print_epu32("v4cr", v4cr) ;
  fprintf(stderr, "- _mm_blendv_epi32\n");
  v4cm = loadu_v128( (__v128i *) im) ;
  v4cr = _mm_blendv_epi32(v4ca, v4cb, v4cm) ;
  _mm_print_epu32("v4ca", v4ca) ; _mm_print_epu32("v4cb", v4cb) ; _mm_print_epu32("v4cm", v4cm) ; _mm_print_epu32("v4cr", v4cr) ;

  v8ca = loadu_v256( (__v256i *) ia) ; v8cb = loadu_v256( (__v256i *) ib) ; v8cm = loadu_v256( (__v256i *) im) ;
  fprintf(stderr, "- _mm256_blendv_ps\n");
  v8cr = __V256i blendv_v8f(__V256 v8ca, __V256 v8cb, __V256 v8cm) ;
  _mm256_print_epu32("v8ca", v8ca) ; _mm256_print_epu32("v8cb", v8cb) ; _mm256_print_epu32("v8cm", v8cm) ; _mm256_print_epu32("v8cr", v8cr) ;
  fprintf(stderr, "- _mm256_blendv_epi32\n");
  v8cr = blendv_v8i(v8ca, v8cb, v8cm) ;
  _mm256_print_epu32("v8ca", v8ca) ; _mm256_print_epu32("v8cb", v8cb) ; _mm256_print_epu32("v8cm", v8cm) ; _mm256_print_epu32("v8cr", v8cr) ;

  fprintf(stderr, " _mm256_cmpeq_epi32 (v800 == v8cm)\n");
  _mm256_print_epu32("v8cm", v8cm) ;
  v8ra = cmpeq_v8i(v800, v8cm) ; _mm256_print_epu32("v8ra", v8ra) ;

  fprintf(stderr, " _mm256_cmpgt_epi32 signed (v800 > v8cm)\n");
  v8ra = cmpgt_v8i(v800, v8cm) ; _mm256_print_epu32("v8ra", v8ra) ;

  fprintf(stderr, "- _mm_cmpeq_epi32 (v400 == v4cm)\n");
  _mm_print_epu32("v4cm", v4cm) ;
  v4ra = cmpeq_v4i(v400, v4cm) ; _mm_print_epu32("v4ra", v4ra) ;

  fprintf(stderr, "- _mm_cmpgt_epi32 signed (v400 > v4cm)\n");
  v4ra = cmpgt_v4i(v400, v4cm) ; _mm_print_epu32("v4ra", v4ra) ;

  fprintf(stderr, "- _mm256_add_epi32 (v8ca + v8cb)\n");
  v8ra = add_v8i(v8ca, v8cb) ; _mm256_print_epu32("v8ra", v8ra) ;

  fprintf(stderr, "- _mm_sub_epi32 (v4cb - v4ca) (v4cb - v4ca + v4ca)\n");
  v4ra = sub_v4i(v4cb, v4ca) ; _mm_print_epu32("v4ra", v4ra) ;
  v4ra = add_v4i(v4ra, v4ca) ; _mm_print_epu32("v4ra", v4ra) ;

  fprintf(stderr, " max_v4u (v4cb , v4ca)\n");
  v4ra = max_v4u(v4ca, v4cb) ;
  _mm_print_epu32("v4ra", v4ra) ;

  fprintf(stderr, "- min_v4i (v4cb , v4ca)\n");
  v4ra = min_v4i(v4ca, v4cb) ;
  _mm_print_epu32("v4ra", v4ra) ;

  fprintf(stderr, "- abs_v8i (v811)\n");
  v8ra = abs_v8i(v811) ; _mm256_print_epu32("v8ra", v8ra) ;
  fprintf(stderr, "- abs_v4i(-v4cr)\n");
  v4ra = sub_v4i(v400, v4cr) ; _mm_print_epu32("v4cr", v4cr) ; _mm_print_epu32("    ", v4ra) ;
  v4ra = abs_v4i(v4ra) ; _mm_print_epu32("v4ra", v4ra) ;  _mm_print_epu32("O.K.", _mm_cmpeq_epi32(v4ra, v4cr)) ;

  fprintf(stderr, "- _mm256_castsi128_si256 (v4cm -> v811)\n");  // upper 4 undefined values different with aocc 4.2
  v8ra = v811 ;
  _mm256_print_epu32("v8ra", v8ra) ;
  v8ra = _mm256_castsi128_si256(v4cm) ;
  v8ra = inserti_128(v8ra, v400, 1) ;  // zero upper part to avoid annoying difference (upper part is undefined)
  _mm256_print_epu32("v8ra", v8ra) ;
  fprintf(stderr, "- _mm256_castsi256_si128 (v8cm -> v4ra)\n");
  _mm256_print_epu32("v8cm", v8cm) ;
  v4ra = _mm256_castsi256_si128(v8cm) ;
  _mm_print_epu32("v4ra", v4ra) ;

  fprintf(stderr, "- cvt_i32_v4i\n") ;
  v4ra = cvt_i32_v4i(0x12345678) ;
  _mm_print_epu32("v4ra", v4ra) ;

  uint32_t even[8] = {0, 2, 4, 6, 8, 10, 12, 14 } ;
  uint32_t odd[8]  = {1, 3, 5, 7, 9, 11, 13, 15 } ;
  __v128i ve4, vo4 ;
  __v256i ve8, vo8 ;
  ve4 = loadu_v128((__v128i *) even) ; vo4 = loadu_v128((__v128i *) odd) ;
  ve8 = loadu_v256((__v256i *) even) ; vo8 = loadu_v256((__v256i *) odd) ;
  fprintf(stderr, "- unpacklo\n") ;
  v4ra = unpacklo_v4i(ve4, vo4) ; _mm_print_epu32("v4ra", v4ra) ;
  v8ra = unpacklo_v8i(ve8, vo8) ; _mm256_print_epu32("v8ra", v8ra) ;
  fprintf(stderr, "- unpackhi\n") ;
  v4ra = unpackhi_v4i(ve4, vo4) ; _mm_print_epu32("v4ra", v4ra) ;
  v8ra = unpackhi_v8i(ve8, vo8) ; _mm256_print_epu32("v8ra", v8ra) ;

  fprintf(stderr, "- shuffle\n") ;
  v4ra = shuffle_v4i(v4ra, 0b11011000) ; _mm_print_epu32("v4ra", v4ra) ;
  v8ra = shuffle_v8i(v8ra, 0b11011000) ; _mm256_print_epu32("v8ra", v8ra) ;

  v4ra = (__v128i) shuffle_2v4f((__v128) v4ra, (__v128) v4ra, 0b01000100) ; _mm_print_epu32("v4ra", v4ra) ;
  v8ra = (__v256i) shuffle_2v8f((__v256) v8ra, (__v256) v8ra, 0b01000100) ; _mm256_print_epu32("v8ra", v8ra) ;

  fprintf(stderr, "- setr\n") ;
  v8ra = setr_2v128(v4ra, v4ra) ; _mm256_print_epu32("v8ra", v8ra) ;

  return 0 ;

}

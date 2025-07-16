#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

// #include <rmn/ieee_functions.h>
#include <rmn/ieee_extras.h>
#include <rmn/test_helpers.h>

// recursively scale floating point values for some exponent range tests
void scale_floats_recurrent(float *f, int n , float fact);    // must be in another file to fool optimizer for subnormal test

int main(int argc, char **argv){
  (void) (argc) ;
  int mant, exp, sign, i, i0, incr = 1 ;
  int64_t i64 = 0 ;
  float fact = 0.5000f ;

  start_of_test(argv[0]);
  if(argc > 1) incr = atoi(argv[1]) ;
  if(argc > 2) fact = atof(argv[2]) ;

  fprintf(stderr, "============================== IEEE subnormal test ==============================\n") ;

  int32_t csr ;
  float vd[32] ;

  csr = fp32_disallow_denorm() ;
  fprintf(stderr, "disallow_denorm, old csr = %8.8x, new csr = %8.8x\n", csr, get_cpu_csr()) ;
  union{float f ; int32_t i ; } fi ;
  fi.i = 1 << 23 ;
  fi.i |= (1 << 22) ;
  vd[0] = fi.f ;
  scale_floats_recurrent(vd, 32, fact) ;
  fprintf(stderr, "fact = %f, f = %12G", fact, vd[0]) ;
  for(i=1 ; i<32 ; i+=5){
    fprintf(stderr, ", %12G ", vd[i]) ;
  }
  fprintf(stderr, "\n");
  if(vd[2] != 0.0f) goto fail ;   // vd[2] should be subnormal

  csr = fp32_allow_denorm() ;
  fprintf(stderr, "   allow_denorm, old csr = %8.8x, new csr = %8.8x\n", csr, get_cpu_csr()) ;
  scale_floats_recurrent(vd, 32, fact) ;
  fprintf(stderr, "fact = %f, f = %12G", fact, vd[0]) ;
  for(i=1 ; i<32 ; i+=5){
    fprintf(stderr, ", %12G ", vd[i]) ;
  }
  fprintf(stderr, "\n");
  if(vd[ 6] == 0.0f) goto fail ;
  if(vd[31] != 0.0f) goto fail ;

  fprintf(stderr, "============================== IEEE manipulation functions ==============================\n") ;

  fp32_allow_denorm() ;     // do not consider denormalized numbers as 0

  i0 = -0x7FFFFFFF - 1 ;
  fprintf(stderr, "fp32_to_fi32(%4.0f) = %8.8x\n",-0.0, fp32_to_fi32(-0.0)) ;
  fprintf(stderr, "fp32_to_fi32(%4.0f) = %8.8x\n",0.0, fp32_to_fi32(0.0)) ;
  for(i64 = i0 ; i64 <= 0x7FFFFFFF ; i64+=incr){
    i = i64 ;
    float r   = fi32_to_fp32(i) ;
    int32_t t = fp32_to_fi32(r) ;
    if(i == i0 && t == 0) continue ;    // 0x80000000 -> -0.0, -0.0 -> 0
    if(i != t) {
      fprintf(stderr, "ERROR: expecting %d from %f, got %d\n", i, r, t) ;
      goto fail ;
    }
  }
  fprintf(stderr, "SUCCESS : fake integer to/from real test %ld values (%8.8x to %8.0x by %d)\n", i64, i0, i, incr) ;

  i64 = 0 ;
  for(sign=0 ; sign <2 ; sign++){
    for(exp=0 ; exp<255 ; exp++){
      for(mant=0 ; mant < 0x800000 ; mant+=incr, i64++){
        union{ int32_t i ; uint32_t u ; float f ; } iuf ;
        int32_t t ;
        iuf.i = mant | (exp << 23) | (sign << 31) ;   // build float from sign/exponent/mantissa
        t = fp32_to_fi32(iuf.f) ;
        if(iuf.f != fi32_to_fp32(t)) goto fail ;
      }
    }
  }
  fprintf(stderr, "SUCCESS : 'float' -> 'fp32_to_fi32(float)' -> 'float' test (%8.8lx values every %d)\n", i64, incr) ;

  fprintf(stderr, "NaN test : fp32_nan(0/1) = %f (%f), isnan(0/1) = %d, %d, isinf(0/1) = %d, %d\n",
                  fp32_nan(0), fp32_nan(1), fp32_isnan(fp32_nan(0)), fp32_isnan(fp32_nan(1)), fp32_isinf(fp32_nan(0)), fp32_isinf(fp32_nan(1))) ;
  if(fp32_isnan(fp32_nan(0)) == 0 || fp32_isnan(fp32_nan(1)) == 0 || fp32_isinf(fp32_nan(0)) || fp32_isinf(fp32_nan(1))) goto fail ;

  union{uint32_t u ; float f ; } uf1, uf2 ;
  uf1.f = fp32_inf(0) ; uf2.f = fp32_inf(1) ;
  fprintf(stderr, "InF test : fp32_inf(0/1) = %f (%f), isnan(0/1) = %d, %d, isinf(0/1) = %d, %d\n",
                   uf1.f, uf2.f, fp32_isnan(uf1.f), fp32_isnan(uf2.f), fp32_isinf(uf1.f), fp32_isinf(uf2.f)) ;
  if(fp32_isnan(uf1.f) || fp32_isnan(uf2.f) || fp32_isinf(uf1.f) == 0 || fp32_isinf(uf2.f) == 0) goto fail ;

  fprintf(stderr, "truncate and round +1.5 to a power of 2 trunc = %5.1f, round = %5.1f\n", fp32_pow2_trunc(+1.5f), fp32_pow2_round(+1.5f)) ;
  if(fp32_pow2_trunc(+1.5f) != 1.0f || fp32_pow2_round(+1.5f) != 2.0) goto fail;

  fprintf(stderr, "truncate and round -1.5 to a power of 2 trunc = %5.1f, round = %5.1f\n", fp32_pow2_trunc(-1.5f), fp32_pow2_round(-1.5f)) ;
  if(fp32_pow2_trunc(-1.5f) != -1.0f || fp32_pow2_round(-1.5f) != -2.0) goto fail;

  fprintf(stderr, "2.0 to the power 5 = %f\n", fp32_pow2(5)) ;
  if(fp32_pow2(5) != 32.0f) goto fail ;

  for(exp=0 ; exp<127 ; exp++){
    if(fp32_pow2(-exp)*fp32_pow2(exp) != 1.0f) {
      fprintf(stderr, "ERROR : fp32_pow2(-%d)*fp32_pow2(%d) != 1.0f, got %G \n", exp, exp, fp32_pow2(-exp)*fp32_pow2(exp)) ;
      goto fail ;
    }
  }

  fprintf(stderr, "SUCCESS\n") ;
  return 0 ;

fail:
  fprintf(stderr, "FAILED\n") ;
  return 1 ;
}

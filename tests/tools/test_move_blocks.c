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
// test the memory block movers
//
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <rmn/test_helpers.h>
#include <rmn/move_blocks.h>

#define NITER 100
#define WITH_TIMING

#if defined(WITH_TIMING)
#include <rmn/timers.h>
#else
#define TIME_LOOP_DATA ;
#define TIME_LOOP_EZ(niter, npts, code) ;
  char *timer_msg = "" ;
  float NaNoSeC = 0.0f ;
#endif

#define NI  127
#define NJ  128
#define LNI 129

int main(int argc, char **argv){
  (void) (argc) ;
  (void) (argv) ;
  uint32_t z[NJ*2][LNI], r[NJ*2][LNI] ;
  float f1[NJ*2][LNI] ;
  float f2[NJ*2][LNI] ;
  float f3[NJ*2][LNI] ;
  int i, j, ni, nj, errors ;
  block_properties bp ;
  float t0 ;
  TIME_LOOP_DATA ;


  if(cycles_overhead == 0) cycles_overhead = 1;
  start_of_test("block move functions") ;

  for(j=0 ; j<NJ ; j++){
    for(i=0 ; i<LNI ; i++){
      z[j][i] = (i - NI/2) + (j - NJ/2) ;
      f2[j][i] = -(i*j+1) ;
      f3[j][i] = (i*j) ;
      if(z[j][i] == 0) z[j][i] = 1 ;
      f1[j][i] = (j * LNI) + i ;
      f1[j][i] = -f1[j][i] ;
    }
  }
  f1[0][0] = 0.5f ;

  ni = 127 ; nj = 125 ;
  {
    uint32_t blk[nj][ni] ;

    move_w32_block(&f1[0][0],   LNI, blk, ni, 2*ni/3, 2*nj/3, &bp) ;  // same operation, different syntax
    move_w32_block((float *)f1, LNI, blk, ni, 2*ni/3, 2*nj/3, &bp) ;

    print_float_props(bp) ;
    fprintf(stderr, "FLOAT_MAX_VALUE = %f, FLOAT_MIN_VALUE = %f, FLOAT_MAX_ABS = %f, FLOAT_MIN_ABS = %f\n",
                     FLOAT_MAX_VALUE(bp),  FLOAT_MIN_VALUE(bp),  FLOAT_MAX_ABS(bp),  FLOAT_MIN_ABS(bp)) ;
    fprintf(stderr, "UINT_MAX_VALUE = %8.8x, UINT_MIN_VALUE = %8.8x, UINT_MAX_ABS = %8.8x, UINT_MIN_ABS = %8.8x\n",
                     UINT_MAX_VALUE(bp),     UINT_MIN_VALUE(bp),     UINT_MAX_ABS(bp),     UINT_MIN_ABS(bp)) ;
    fprintf(stderr, "INT_MAX_VALUE = %d, INT_MIN_VALUE = %d, INT_MAX_ABS = %u, INT_MIN_ABS = %u\n",
                     INT_MAX_VALUE(bp),  INT_MIN_VALUE(bp),  INT_MAX_ABS(bp),  INT_MIN_ABS(bp)) ;
    fprintf(stderr, "\n") ;

    move_w32_block((float *)f2, LNI, blk, ni, 2*ni/3, 2*nj/3, &bp) ;
    print_float_props(bp) ;
    fprintf(stderr, "FLOAT_MAX_VALUE = %f, FLOAT_MIN_VALUE = %f, FLOAT_MAX_ABS = %f, FLOAT_MIN_ABS = %f\n",
                     FLOAT_MAX_VALUE(bp),  FLOAT_MIN_VALUE(bp),  FLOAT_MAX_ABS(bp),  FLOAT_MIN_ABS(bp)) ;
    fprintf(stderr, "UINT_MAX_VALUE = %8.8x, UINT_MIN_VALUE = %8.8x, UINT_MAX_ABS = %8.8x, UINT_MIN_ABS = %8.8x\n",
                     UINT_MAX_VALUE(bp),     UINT_MIN_VALUE(bp),     UINT_MAX_ABS(bp),     UINT_MIN_ABS(bp)) ;
    fprintf(stderr, "INT_MAX_VALUE = %d, INT_MIN_VALUE = %d, INT_MAX_ABS = %u, INT_MIN_ABS = %u\n",
                     INT_MAX_VALUE(bp),  INT_MIN_VALUE(bp),  INT_MAX_ABS(bp),  INT_MIN_ABS(bp)) ;
    fprintf(stderr, "\n") ;

    move_w32_block((float *)f3, LNI, blk, ni, 2*ni/3, 2*nj/3, &bp) ;
    print_float_props(bp) ;
    fprintf(stderr, "FLOAT_MAX_VALUE = %f, FLOAT_MIN_VALUE = %f, FLOAT_MAX_ABS = %f, FLOAT_MIN_ABS = %f\n",
                     FLOAT_MAX_VALUE(bp),  FLOAT_MIN_VALUE(bp),  FLOAT_MAX_ABS(bp),  FLOAT_MIN_ABS(bp)) ;
    fprintf(stderr, "UINT_MAX_VALUE = %8.8x, UINT_MIN_VALUE = %8.8x, UINT_MAX_ABS = %8.8x, UINT_MIN_ABS = %8.8x\n",
                     UINT_MAX_VALUE(bp),     UINT_MIN_VALUE(bp),     UINT_MAX_ABS(bp),     UINT_MIN_ABS(bp)) ;
    fprintf(stderr, "INT_MAX_VALUE = %d, INT_MIN_VALUE = %d, INT_MAX_ABS = %u, INT_MIN_ABS = %u\n",
                     INT_MAX_VALUE(bp),  INT_MIN_VALUE(bp),  INT_MAX_ABS(bp),  INT_MIN_ABS(bp)) ;
    fprintf(stderr, "\n") ;

    for(j=0 ; j<1 ; j++){
      for(i=0 ; i<ni ; i++){
        blk[j][i] = 0xFFFF ;
      }
    }
    move_w32_block((uint32_t *) &z[0][0], LNI, blk, ni, ni,     nj,     &bp) ;
    print_int_props(bp) ;
    fprintf(stderr, "FLOAT_MAX_VALUE = %f, FLOAT_MIN_VALUE = %f, FLOAT_MAX_ABS = %f, FLOAT_MIN_ABS = %f\n",
                     FLOAT_MAX_VALUE(bp),  FLOAT_MIN_VALUE(bp),  FLOAT_MAX_ABS(bp),  FLOAT_MIN_ABS(bp)) ;
    fprintf(stderr, "UINT_MAX_VALUE = %8.8x, UINT_MIN_VALUE = %8.8x, UINT_MAX_ABS = %8.8x, UINT_MIN_ABS = %8.8x\n",
                     UINT_MAX_VALUE(bp),     UINT_MIN_VALUE(bp),     UINT_MAX_ABS(bp),     UINT_MIN_ABS(bp)) ;
    fprintf(stderr, "INT_MAX_VALUE = %d, INT_MIN_VALUE = %d, INT_MAX_ABS = %u, INT_MIN_ABS = %u\n",
                     INT_MAX_VALUE(bp),  INT_MIN_VALUE(bp),  INT_MAX_ABS(bp),  INT_MIN_ABS(bp)) ;
    fprintf(stderr, "\n") ;

    move_w32_block((int32_t *)  &z[0][0], LNI, blk, ni, ni,     nj,     &bp) ;
    print_int_props(bp) ;
    fprintf(stderr, "FLOAT_MAX_VALUE = %f, FLOAT_MIN_VALUE = %f, FLOAT_MAX_ABS = %f, FLOAT_MIN_ABS = %f\n",
                     FLOAT_MAX_VALUE(bp),  FLOAT_MIN_VALUE(bp),  FLOAT_MAX_ABS(bp),  FLOAT_MIN_ABS(bp)) ;
    fprintf(stderr, "UINT_MAX_VALUE = %8.8x, UINT_MIN_VALUE = %8.8x, UINT_MAX_ABS = %8.8x, UINT_MIN_ABS = %8.8x\n",
                     UINT_MAX_VALUE(bp),     UINT_MIN_VALUE(bp),     UINT_MAX_ABS(bp),     UINT_MIN_ABS(bp)) ;
    fprintf(stderr, "INT_MAX_VALUE = %d, INT_MIN_VALUE = %d, INT_MAX_ABS = %u, INT_MIN_ABS = %u\n",
                     INT_MAX_VALUE(bp),  INT_MIN_VALUE(bp),  INT_MAX_ABS(bp),  INT_MIN_ABS(bp)) ;
    fprintf(stderr, "\n") ;

    errors = 0 ;
    for(j=0 ; j<nj ; j++){
      for(i=0 ; i<ni ; i++){
        if(blk[j][i] != z[j][i]) {
          if(errors == 0) fprintf(stderr, "(%d,%d) expected %4.4x, got %4.4x\n", i, j, z[j][i], blk[j][i]) ;
          errors++ ;
        }
      }
    }
    fprintf(stderr, "get block errors = %d [%dx%d]\n", errors, ni, nj) ;
    if(errors > 0) goto fail ;

    move_w32_block(&blk[0][0], ni, r, LNI, ni, nj, NULL) ;
    errors = 0 ;
    for(j=0 ; j<nj ; j++){
      for(i=0 ; i<ni ; i++){
        if(blk[j][i] != r[j][i]) {
          if(errors == 0) fprintf(stderr, "(%d,%d) expected %4.4x, got %4.4x\n", i, j, z[j][i], blk[j][i]) ;
          errors++ ;
        }
      }
    }
    fprintf(stderr, "put block errors = %d [%dx%d]\n", errors, ni, nj) ;
    if(errors > 0) goto fail ;

    TIME_LOOP_EZ(NITER, ni*nj, move_w32_block((int32_t *)z, LNI, blk, ni, ni, nj, &bp) ) ;
    if(timer_min == timer_max) timer_avg = timer_max ;
    t0 = timer_min * NaNoSeC / (ni*nj) ;
    fprintf(stderr, "move int + prop : %4.2f ns/word\n", t0) ;

    TIME_LOOP_EZ(NITER, ni*nj, move_w32_block((int32_t *)z, LNI, NULL, LNI, ni, nj, &bp) ) ;
    if(timer_min == timer_max) timer_avg = timer_max ;
    t0 = timer_min * NaNoSeC / (ni*nj) ;
    fprintf(stderr, "nomove int prop : %4.2f ns/word\n", t0) ;

    TIME_LOOP_EZ(NITER, ni*nj, move_w32_block((float *)z, LNI, blk, ni, ni, nj, &bp) ) ;
    if(timer_min == timer_max) timer_avg = timer_max ;
    t0 = timer_min * NaNoSeC / (ni*nj) ;
    fprintf(stderr, "move flt + prop : %4.2f ns/word\n", t0) ;

    TIME_LOOP_EZ(NITER, ni*nj, move_w32_block((float *)z, LNI, NULL, LNI, ni, nj, &bp) ) ;
    if(timer_min == timer_max) timer_avg = timer_max ;
    t0 = timer_min * NaNoSeC / (ni*nj) ;
    fprintf(stderr, "nomove flt prop : %4.2f ns/word\n", t0) ;

    TIME_LOOP_EZ(NITER, ni*nj, move_data32_block(z, LNI, blk, ni, ni, nj, NULL) ) ;
    if(timer_min == timer_max) timer_avg = timer_max ;
    t0 = timer_min * NaNoSeC / (ni*nj) ;
    fprintf(stderr, "move datanoprop : %4.2f ns/word\n", t0) ;

    TIME_LOOP_EZ(NITER, ni*nj, move_mem32_block(z, LNI, blk, ni, ni, nj) ) ;
    if(timer_min == timer_max) timer_avg = timer_max ;
    t0 = timer_min * NaNoSeC / (ni*nj) ;
    fprintf(stderr, "move mem32      : %4.2f ns/word\n", t0) ;

    TIME_LOOP_EZ(NITER, ni*nj, move_w32_block((void *)z, LNI, blk, ni, ni, nj) ) ;
    if(timer_min == timer_max) timer_avg = timer_max ;
    t0 = timer_min * NaNoSeC / (ni*nj) ;
    fprintf(stderr, "move in         : %4.2f ns/word\n", t0) ;

    TIME_LOOP_EZ(NITER, ni*nj, move_w32_block((void *)blk, ni, r, LNI, ni, nj) ) ;
    t0 = timer_min * NaNoSeC / (ni*nj) ;
    fprintf(stderr, "move back       : %4.2f ns/word\n", t0) ;

  }

  fprintf(stderr, "SUCCESS\n") ;
  return 0 ;

fail:
  fprintf(stderr, "FAIL\n") ;
  return 1 ;
}

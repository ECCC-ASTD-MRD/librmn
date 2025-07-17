#include <stdio.h>
#include <stdlib.h>

#include <rmn/dwt_i_lgt53.h>
#include <rmn/timers.h>

#define NTIMES 100

static int nerr0 = 0 ;

static int not_same(int *a, int *b, int n){
  int i, nerr = 0 ;
  for(i=0; i<n ; i++) {
    if(a[i] != b[i]){
      nerr++ ;
//       if(nerr < 3) fprintf(stderr, "error at i = %d, expected %d, got %d\n", i, a[i], b[i]) ;
// exit(1) ;
    }
  }
  nerr0 = nerr ;
  return nerr ;
}

int main(int argc, char **argv){
  int nerr ;
  (void) argc ;
  (void) argv ;
  int i, j ;
  int tmp[16] = {-1, 11, 2, -23, 3, 32, 1, 0, 10, 1, -22, -3, 44, 5, -66, 8 } ;
  int ref[16] ;
  int sin[16] = {0, 1, 2, 1, 0, -1, -2, -1, 0, 1, 2, 1, 0, -1, -2, -1 } ;
  int cos[16] = {2, 1, 0, -1, -2, -1, 0, 1, 2, 1, 0, -1, -2, -1, 0, 1 } ;
  int t2d[16][16] ;
  int r2d[16][16] ;
//   int e[16], o[16] ;
  int npts, nrows, ncols, xe[256], xo[256] ;

  int xo1[64], xe1[64], xo2[64], xe2[64] ;
  int a64_[66], b64_[66], *a64 = &a64_[1], *b64 = &b64_[1] ;
  int a32_[34], b32_[34], *a32 = &a32_[1], *b32 = &b32_[1] ;

  if(argc > 1){
    if( *argv[1] == 't') goto timings ;
    if( *argv[1] == 'x') goto experiments ;
  }

  for(i=0; i<16 ; i++){ ref[i] = tmp[i] ; }
  for(j=0 ; j<16 ; j++){
    for(i=0; i<16 ; i++){
      t2d[j][i] = r2d[j][i] = sin[i] * cos[j] + 1 ;
//       t2d[j][i] = r2d[j][i] = tmp[j] + i ;
    }
  }

//   fprintf(stderr, "original 1D data\n") ;
//   for(i=0; i<16 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; } fprintf(stderr, "\n\n");

  fprintf(stderr, "1D, in place, no split, 1 -> 256 point(s)") ;
  for(npts=1 ; npts<257 ; npts++){
    fwd_1d_lgt53_asis((void *)t2d, npts) ;
    inv_1d_lgt53_asis((void *)t2d, npts) ;
    nerr = not_same((void *)r2d, (void *)t2d, 256) ;
    if(nerr){
      fprintf(stderr, ", %3d point(s), errors = %d\n", npts, nerr);
      goto fail ;
    }
  }
  fprintf(stderr, ", errors = 0\n\n") ;
#if 1
  fprintf(stderr, "1D, in place, 1 point") ;
  fwd_1d_lgt53_asis(tmp, 1) ;
//   for(i=0; i<1 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; } fprintf(stderr, "\n");
  inv_1d_lgt53_asis(tmp, 1) ;
//   for(i=0; i<1 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; }
  fprintf(stderr, ", errors = %d\n\n", nerr=not_same(ref, tmp, 1));
  if(nerr) goto fail ;

  fprintf(stderr, "1D, in place, 2 points") ;
  fwd_1d_lgt53_asis(tmp, 2) ;
//   for(i=0; i<2 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; } fprintf(stderr, "\n");
  inv_1d_lgt53_asis(tmp, 2) ;
//   for(i=0; i<2 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; }
  fprintf(stderr, ", errors = %d\n\n", nerr=not_same(ref, tmp, 2));
  if(nerr) goto fail ;

  fprintf(stderr, "1D, in place, 3 points") ;
  fwd_1d_lgt53_asis(tmp, 3) ;
//   for(i=0; i<3 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; } fprintf(stderr, "\n");
  inv_1d_lgt53_asis(tmp, 3) ;
//   for(i=0; i<3 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; }
  fprintf(stderr, ", errors = %d\n\n", nerr=not_same(ref, tmp, 3));
  if(nerr) goto fail ;

  fprintf(stderr, "1D, in place, 4 points") ;
  fwd_1d_lgt53_asis(tmp, 4) ;
//   for(i=0; i<4 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; } fprintf(stderr, "\n");
  inv_1d_lgt53_asis(tmp, 4) ;
//   for(i=0; i<4 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; }
  fprintf(stderr, ", errors = %d\n\n", nerr=not_same(ref, tmp, 4));
  if(nerr) goto fail ;

  fprintf(stderr, "1D, in place, even number of points") ;
  fwd_1d_lgt53_asis(tmp, 16) ;
//   for(i=0; i<16 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; } fprintf(stderr, "\n");
  inv_1d_lgt53_asis(tmp, 16) ;
//   for(i=0; i<16 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; }
  fprintf(stderr, ", errors = %d\n\n", nerr=not_same(ref, tmp, 16));
  if(nerr) goto fail ;

//   for(i=0; i<15 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; } fprintf(stderr, "\n");
  fprintf(stderr, "1D, in place, odd number of points") ;
  fwd_1d_lgt53_asis(tmp, 15) ;
//   for(i=0; i<17 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; } fprintf(stderr, "\n");
// //   for(i=0; i<7 ; i++){ fprintf(stderr, "%4d ", tmp[i+i+1]) ; } fprintf(stderr, "\n");
// //   for(i=0; i<8 ; i++){ fprintf(stderr, "%4d ", tmp[i+i]) ; } fprintf(stderr, "\n");
  inv_1d_lgt53_asis(tmp, 15) ;
//   for(i=0; i<15 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; }
  fprintf(stderr, ", errors = %d\n\n", nerr=not_same(ref, tmp, 16));
  if(nerr) goto fail ;
#endif
  fprintf(stderr, "1D, split, not in place, 1 -> 256 points") ;
  for(npts=1 ; npts<257 ; npts++){
    fwd_1d_lgt53_split((void *)t2d, xe, xo, npts) ;
    inv_1d_lgt53_split((void *)t2d, xe, xo, npts) ;
    nerr = not_same((void *)r2d, (void *)t2d, 256) ;
    if(nerr){
      fprintf(stderr, ", %3d point(s), errors = %d\n", npts, nerr);
      goto fail ;
    }
  }
  fprintf(stderr, ", errors = 0\n\n") ;

  fprintf(stderr, "1D, split in place, 1 -> 256 points") ;
  for(npts=1 ; npts<257 ; npts++){
    fwd_1d_lgt53((void *)t2d, npts);
    nerr = not_same((void *)r2d, (void *)t2d, 256) ;
    if(nerr == 0 && npts > 1){
      fprintf(stderr, ", %3d point(s), unexpected errors = 0\n", npts);
    }
    inv_1d_lgt53((void *)t2d, npts);
    nerr = not_same((void *)r2d, (void *)t2d, 256) ;
    if(nerr){
      fprintf(stderr, ", %3d point(s), errors = %d\n", npts, nerr);
      goto fail ;
    }
  }
  fprintf(stderr, ", errors = 0\n\n") ;

  fprintf(stderr, "2D, split in place, 1 -> 256 points, ni = 1") ;
  for(npts=1 ; npts<257 ; npts++){
    fwd_2d_lgt53((void *)t2d, 1, 1, npts);
    nerr = not_same((void *)r2d, (void *)t2d, 256) ;
    if(nerr == 0 && npts > 1){
      fprintf(stderr, ", %3d point(s), unexpected errors = 0\n", npts);
    }
    inv_2d_lgt53((void *)t2d, 1, 1, npts);
    nerr = not_same((void *)r2d, (void *)t2d, 256) ;
    if(nerr){
      fprintf(stderr, ", %3d point(s), errors = %d\n", npts, nerr);
      goto fail ;
    }
  }
  fprintf(stderr, ", errors = 0\n\n") ;

  fprintf(stderr, "2D, split in place, 1 -> 256 points, nj = 1") ;
  for(npts=1 ; npts<257 ; npts++){
    fwd_2d_lgt53((void *)t2d, 256, npts, 1);  // 2D transform with nj == 1
    nerr = not_same((void *)r2d, (void *)t2d, 256) ;
    if(nerr == 0 && npts > 1){
      fprintf(stderr, ", %3d point(s), unexpected errors = 0\n", npts);
    }
    inv_2d_lgt53((void *)t2d, 256, npts, 1);  // 2D transform with nj == 1
    nerr = not_same((void *)r2d, (void *)t2d, 256) ;
    if(nerr){
      fprintf(stderr, ", %3d point(s), errors = %d\n", npts, nerr);
      goto fail ;
    }
  }
  fprintf(stderr, ", errors = 0\n\n") ;

  fprintf(stderr, "2D, in place, 3 -> 16 diagonal, lni == ni == nj") ;
  for(npts=3 ; npts<17 ; npts++){
    fwd_2d_lgt53((void *)t2d, npts, npts, npts);
    nerr = not_same((void *)r2d, (void *)t2d, 256) ;
    if(nerr == 0 && npts > 1){
      fprintf(stderr, ", %3d point(s), unexpected errors = 0\n", npts);
    }
    inv_2d_lgt53((void *)t2d, npts, npts, npts);
    nerr = not_same((void *)r2d, (void *)t2d, 256) ;
    if(nerr){
      fprintf(stderr, ", [%3d,%3d], errors = %d\n", npts, npts, nerr);
      goto fail ;
    }
  }
  fprintf(stderr, ", errors = 0\n\n") ;

  fprintf(stderr, "2D, split, in place, even number of points, 2 levels") ;
//   fwd_1d_lgt53_n(tmp, 16, 2);
  fwd_2d_lgt53_n(tmp, 16, 16, 1, 2);  // 2D transform with nj == 1
//   inv_1d_lgt53_n(tmp, 16, 2);
  inv_2d_lgt53_n(tmp, 16, 16, 1, 2);  // 2D transform with nj == 1
//   for(i=0; i<16 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; }
  fprintf(stderr, ", errors = %d\n\n", nerr=not_same(ref, tmp, 16));
  if(nerr) goto fail ;

  fprintf(stderr, "2D, split, in place, odd number of points, 2 levels") ;
//   fwd_1d_lgt53_n(tmp, 15, 2);
  fwd_2d_lgt53_n(tmp, 15, 15, 1, 2);  // 2D transform with nj == 1
//   inv_1d_lgt53_n(tmp, 15, 2);
  inv_2d_lgt53_n(tmp, 15, 15, 1, 2);  // 2D transform with nj == 1
//   for(i=0; i<15 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; }
  fprintf(stderr, ", errors = %d\n\n", nerr=not_same(ref, tmp, 16));
  if(nerr) goto fail ;

  fprintf(stderr, "2D, even number of points along j, ni == 1, 3 levels") ;
  fwd_2d_lgt53_n(tmp, 1, 1, 16, 3);
  inv_2d_lgt53_n(tmp, 1, 1, 16, 3);
//   for(i=0; i<16 ; i++){ fprintf(stderr, "%4d ", tmp[i]) ; }
  fprintf(stderr, ", errors = %d\n\n", nerr=not_same(ref, tmp, 16));
  if(nerr) goto fail ;

  fprintf(stderr, "2D, even number of points along j, ni == 3") ;
  fwd_2d_lgt53((void *)t2d, 16, 3, 16);
  inv_2d_lgt53((void *)t2d, 16, 3, 16);
  fprintf(stderr, " , errors = %d\n\n", nerr=not_same((void *)r2d, (void *)t2d, 16*16));
  if(nerr) goto fail ;

  nrows = 2 ; ncols = 2 ;
  fprintf(stderr, "2D, %d points along j, ni == %d", nrows, ncols) ;
  fwd_2d_lgt53((void *)t2d, 16, ncols, nrows);
  inv_2d_lgt53((void *)t2d, 16, ncols, nrows);
  fprintf(stderr, " , errors = %d\n\n", nerr=not_same((void *)r2d, (void *)t2d, 16*16));
  if(nerr) goto fail ;

  fprintf(stderr, "2D, even number of points along i and j") ;
  fwd_2d_lgt53((void *)t2d, 16, 16, 16) ;
  inv_2d_lgt53((void *)t2d, 16, 16, 16) ;
  fprintf(stderr, " , errors = %d\n\n", nerr=not_same((void *)r2d, (void *)t2d, 16*16));
  if(nerr) goto fail ;

  fprintf(stderr, "2D, odd number of points along j, ni == 2") ;
  fwd_2d_lgt53((void *)t2d, 16, 2, 15);
  inv_2d_lgt53((void *)t2d, 16, 2, 15);
  fprintf(stderr, " , errors = %d\n\n", nerr=not_same((void *)r2d, (void *)t2d, 16*16));
  if(nerr) goto fail ;

  fprintf(stderr, "2D, odd number of points along i and j") ;
  fwd_2d_lgt53((void *)t2d, 16, 15, 15) ;
  inv_2d_lgt53((void *)t2d, 16, 15, 15) ;
  fprintf(stderr, " , errors = %d\n\n", nerr=not_same((void *)r2d, (void *)t2d, 16*16));
  if(nerr) goto fail ;

//   goto success ;

timings:
  fprintf(stderr, "========== timing tests ==========\n");
  int bench[64][64], orig[64][64], iter, levels ;
  uint64_t t0[NTIMES], t1[NTIMES], t2[NTIMES], tmin1, tmin2 ;
  float tp1, tp2 ;
  int src[64], odd[64], even[64], sref[64] ;

  for(j=0 ; j<64 ; j++){
    sref[j] = src[j] = cos[j&15] + 1 ;
    for(i=0; i<64 ; i++){
      orig[j][i] = bench[j][i] = sin[i&15] * cos[j&15] + 1 ;
    }
  }

  tmin1 = tmin2 = 999999999 ;
  for(iter=0 ; iter<NTIMES ; iter++){
    t0[iter] = elapsed_cycles() ;
    for(i=0 ; i<64 ; i++) fwd_1d_lgt53_split((void *)src, even, odd, 64) ;
    t1[iter] = elapsed_cycles() ;
    for(i=0 ; i<64 ; i++) inv_1d_lgt53_split((void *)src, even, odd, 64) ;
    t2[iter] = elapsed_cycles() ;
    tmin1 = ((t1[iter] - t0[iter]) < tmin1) ? (t1[iter] - t0[iter]) : tmin1 ;
    tmin2 = ((t2[iter] - t1[iter]) < tmin2) ? (t2[iter] - t1[iter]) : tmin2 ;
  }
  fprintf(stderr, "1D transform (split) SIMD , errors = %d\n", not_same((void *)src, (void *)sref, 64));
  tp1 = cycles_to_ns(tmin1)/4096 ;
  tp2 = cycles_to_ns(tmin2)/4096 ;
  fprintf(stderr, "fwd transform : %6ld cycles (%5.2f ns/value), inv transform : %6ld cycles (%5.2f ns/value)\n", tmin1, tp1, tmin2, tp2) ;

  tmin1 = tmin2 = 999999999 ;
  for(iter=0 ; iter<NTIMES ; iter++){
    t0[iter] = elapsed_cycles() ;
    for(i=0 ; i<64 ; i++) fwd_1d_lgt53_split_c((void *)src, even, odd, 64) ;
    t1[iter] = elapsed_cycles() ;
    for(i=0 ; i<64 ; i++) inv_1d_lgt53_split_c((void *)src, even, odd, 64) ;
    t2[iter] = elapsed_cycles() ;
    tmin1 = ((t1[iter] - t0[iter]) < tmin1) ? (t1[iter] - t0[iter]) : tmin1 ;
    tmin2 = ((t2[iter] - t1[iter]) < tmin2) ? (t2[iter] - t1[iter]) : tmin2 ;
  }
  fprintf(stderr, "1D transform (split) C    , errors = %d\n", not_same((void *)src, (void *)sref, 64));
  tp1 = cycles_to_ns(tmin1)/4096 ;
  tp2 = cycles_to_ns(tmin2)/4096 ;
  fprintf(stderr, "fwd transform : %6ld cycles (%5.2f ns/value), inv transform : %6ld cycles (%5.2f ns/value)\n\n", tmin1, tp1, tmin2, tp2) ;

  tmin1 = tmin2 = 999999999 ;
  for(iter=0 ; iter<NTIMES ; iter++){
    t0[iter] = elapsed_cycles() ;
    for(i=0 ; i<64 ; i++) fwd_1d_lgt53((void *)src, 64) ;
    t1[iter] = elapsed_cycles() ;
    for(i=0 ; i<64 ; i++) inv_1d_lgt53((void *)src, 64) ;
    t2[iter] = elapsed_cycles() ;
    tmin1 = ((t1[iter] - t0[iter]) < tmin1) ? (t1[iter] - t0[iter]) : tmin1 ;
    tmin2 = ((t2[iter] - t1[iter]) < tmin2) ? (t2[iter] - t1[iter]) : tmin2 ;
  }
  fprintf(stderr, "1D transform (in place) , errors = %d\n", not_same((void *)src, (void *)sref, 64));
  tp1 = cycles_to_ns(tmin1)/4096 ;
  tp2 = cycles_to_ns(tmin2)/4096 ;
  fprintf(stderr, "fwd transform : %6ld cycles (%5.2f ns/value), inv transform : %6ld cycles (%5.2f ns/value)\n\n", tmin1, tp1, tmin2, tp2) ;

  tmin1 = tmin2 = 999999999 ;
  for(iter=0 ; iter<NTIMES ; iter++){
    t0[iter] = elapsed_cycles() ;
    for(i=0 ; i<64 ; i++) fwd_1d_lgt53_asis((void *)src, 64) ;
    t1[iter] = elapsed_cycles() ;
    for(i=0 ; i<64 ; i++) inv_1d_lgt53_asis((void *)src, 64) ;
    t2[iter] = elapsed_cycles() ;
    tmin1 = ((t1[iter] - t0[iter]) < tmin1) ? (t1[iter] - t0[iter]) : tmin1 ;
    tmin2 = ((t2[iter] - t1[iter]) < tmin2) ? (t2[iter] - t1[iter]) : tmin2 ;
  }
  fprintf(stderr, "1D transform (in place, as is) , errors = %d\n", not_same((void *)src, (void *)sref, 64));
  tp1 = cycles_to_ns(tmin1)/4096 ;
  tp2 = cycles_to_ns(tmin2)/4096 ;
  fprintf(stderr, "fwd transform : %6ld cycles (%5.2f ns/value), inv transform : %6ld cycles (%5.2f ns/value)\n\n", tmin1, tp1, tmin2, tp2) ;

  fprintf(stderr, "1 transform pair\n") ;
  for(levels=1 ; levels<4 ; levels++){
    uint64_t tu0, tu1, tu2 ;
    float tf1, tf2 ;
    tu0 = elapsed_cycles() ;
    fwd_2d_lgt53_n((void *)bench, 64, 32, 32, levels) ;
    tu1 = elapsed_cycles() ;
    inv_2d_lgt53_n((void *)bench, 64, 32, 32, levels) ;
    tu2 = elapsed_cycles() ;
    fprintf(stderr, "2D transform (32 x 32) in [64,64] (%d level transforms) , errors = %d\n", levels, nerr = not_same((void *)bench, (void *)orig, 4096));
    if(nerr) goto fail ;
    tp1 = cycles_to_ns(tu1-tu0)/1024 ;
    tp2 = cycles_to_ns(tu2-tu1)/1024 ;
    fprintf(stderr, "fwd transform : %6ld cycles (%5.2f ns/value), inv transform : %6ld cycles (%5.2f ns/value)\n", tu1-tu0, tu2-tu1, tp1, tp2) ;
  }
  fprintf(stderr, "\n") ;

  fprintf(stderr, "1 transform pair\n") ;
  for(levels=1 ; levels<5 ; levels++){
    uint64_t tu0, tu1, tu2 ;
    float tf1, tf2 ;
    tu0 = elapsed_cycles() ;
    fwd_2d_lgt53_n((void *)bench, 64, 64, 64, levels) ;
    tu1 = elapsed_cycles() ;
    inv_2d_lgt53_n((void *)bench, 64, 64, 64, levels) ;
    tu2 = elapsed_cycles() ;
    fprintf(stderr, "2D transform (64 x 64) in [64,64] (%d level transforms) , errors = %d\n", levels, nerr = not_same((void *)bench, (void *)orig, 4096));
    if(nerr) goto fail ;
    tp1 = cycles_to_ns(tu1-tu0)/4096 ;
    tp2 = cycles_to_ns(tu2-tu1)/4096 ;
    fprintf(stderr, "fwd transform : %6ld cycles (%5.2f ns/value), inv transform : %6ld cycles (%5.2f ns/value)\n", tu1-tu0, tu2-tu1, tp1, tp2) ;
  }
  fprintf(stderr, "\n") ;

  fprintf(stderr, "best of %d transform pairs\n", NTIMES) ;
  for(levels=1 ; levels<6 ; levels++){
    tmin1 = tmin2 = 999999999 ;
    for(iter=0 ; iter<NTIMES ; iter++){
      t0[iter] = elapsed_cycles() ;
      fwd_2d_lgt53_n((void *)bench, 64, 64, 64, levels) ;
      t1[iter] = elapsed_cycles() ;
      inv_2d_lgt53_n((void *)bench, 64, 64, 64, levels) ;
      t2[iter] = elapsed_cycles() ;
      tmin1 = ((t1[iter] - t0[iter]) < tmin1) ? (t1[iter] - t0[iter]) : tmin1 ;
      tmin2 = ((t2[iter] - t1[iter]) < tmin2) ? (t2[iter] - t1[iter]) : tmin2 ;
    }

    fprintf(stderr, "2D transform (64 x 64) in [64,64] (%d level transforms) , errors = %d\n", levels, not_same((void *)bench, (void *)orig, 4096));
    tp1 = cycles_to_ns(tmin1)/4096 ;
    tp2 = cycles_to_ns(tmin2)/4096 ;
    fprintf(stderr, "fwd transform : %6ld cycles (%5.2f ns/value), inv transform : %6ld cycles (%5.2f ns/value)\n", tmin1, tp1, tmin2, tp2) ;
//     for(i=0 ; i<64 ; i++) fprintf(stderr, "%d ",bench[i][i]-orig[i][i]);
    fprintf(stderr, "\n") ;
  }

//   goto success ;

experiments :

  for(i=0 ; i<64 ; i++){
    a64[i] = sin[i&15] + cos[i&15] + i ;
//     a64[i] = i ;
//     xo1[i] = xe1[i] = -1 ;
//     xo2[i] = xe2[i] = -1 ;
  }
  for(i=0 ; i<32 ; i++){ a32[i] = sin[i&15] + cos[i&15] + i ; }
  a64_[0]  = a32_[0]  = 999 ;
  a64_[65] = a32_[33] = 999 ;
  b64_[0]  = b32_[0]  = 999 ;
  b64_[65] = b32_[33] = 999 ;

  fprintf(stderr, "a32 and guards :\n") ;
  for(i=-1  ; i<33 ; i++) fprintf(stderr, "%3d ", a32[i]) ;
  fprintf(stderr, "\n") ;

  for(i=0 ; i<64 ; i++){ xo1[i] = xe1[i] = 999 ; } ;
  fwd_1d_lgt53_split_c(a32, xe1+1, xo1+1, 32) ;
//   for(i=0 ; i<18 ; i++) fprintf(stderr, "%3d ", xe1[i]) ;
//   fprintf(stderr, "\n") ;
//   for(i=0 ; i<18 ; i++) fprintf(stderr, "%3d ", xo1[i]) ;
//   fprintf(stderr, "\n") ;
  inv_1d_lgt53_split_c(b32, xe1+1, xo1+1, 32) ;
//   for(i=-1  ; i<33 ; i++) fprintf(stderr, "%3d ", b32[i]) ;
//   fprintf(stderr, "\n") ;
  fprintf(stderr, "a32-b32 differences (1D split) (c   ) (32) = %d\n", nerr = not_same((void *)a32_, (void *)b32_, 34)) ;
//   fprintf(stderr, "\n") ;
  if(nerr > 0) goto fail ;

  for(i=0 ; i<64 ; i++){ xo2[i] = xe2[i] = 999 ; } ;
  fwd_1d_lgt53_split_simd(a32, xe2+1, xo2+1, 32) ;
//   for(i=0 ; i<18 ; i++) fprintf(stderr, "%3d ", xe2[i]) ;
//   fprintf(stderr, "\n") ;
//   for(i=0 ; i<18 ; i++) fprintf(stderr, "%3d ", xo2[i]) ;
//   fprintf(stderr, "\n") ;
  inv_1d_lgt53_split_simd(b32, xe2+1, xo2+1, 32) ;
//   for(i=-1  ; i<33 ; i++) fprintf(stderr, "%3d ", b32[i]) ;
//   fprintf(stderr, "\n") ;
  fprintf(stderr, "a32-b32 differences (1D split) (simd) (32) = %d\n", nerr = not_same((void *)a32_, (void *)b32_, 34)) ;
//   fprintf(stderr, "\n") ;
  if(nerr > 0) goto fail ;
  fprintf(stderr, "xe differences = %d", nerr = not_same((void *)xe1, (void *)xe2, 18)) ;
  fprintf(stderr, ", xo differences = %d\n", nerr = not_same((void *)xo1, (void *)xo2, 18)) ;
//   fprintf(stderr, "\n") ;
  if(nerr > 0 || not_same((void *)xe1, (void *)xe2, 18) > 0) goto fail ;

  for(i=0 ; i<64 ; i++){ xo1[i] = xe1[i] = 999 ; } ;
  fwd_1d_lgt53_split_c(a32, xe1+1, xo1+1, 31) ;
//   for(i=0 ; i<18 ; i++) fprintf(stderr, "%3d ", xe1[i]) ;
//   fprintf(stderr, "\n") ;
//   for(i=0 ; i<18 ; i++) fprintf(stderr, "%3d ", xo1[i]) ;
//   fprintf(stderr, "\n") ;
  inv_1d_lgt53_split_c(b32, xe1+1, xo1+1, 31) ;
//   for(i=-1  ; i<33 ; i++) fprintf(stderr, "%3d ", b32[i]) ;
//   fprintf(stderr, "\n") ;
  fprintf(stderr, "a32-b32 differences (1D split) (c   ) (31) = %d\n", nerr = not_same((void *)a32_, (void *)b32_, 34)) ;
  fprintf(stderr, "\n") ;
  if(nerr > 0) goto fail ;

  fprintf(stderr, "a64 and guards :\n") ;
  for(i=-1  ; i<32 ; i++) fprintf(stderr, "%3d ", a64[i]) ;
  fprintf(stderr, "\n") ;
  for(i=32 ; i<65 ; i++) fprintf(stderr, "%3d ", a64[i]) ;
  fprintf(stderr, "\n") ;

  for(i=0 ; i<64 ; i++){ xo1[i] = xe1[i] = 999 ; } ;
  fwd_1d_lgt53_split_c(a64, xe1+1, xo1+1, 64) ;
//   for(i=0 ; i<34 ; i++) fprintf(stderr, "%3d ", xe1[i]) ;
//   fprintf(stderr, "\n") ;
//   for(i=0 ; i<34 ; i++) fprintf(stderr, "%3d ", xo1[i]) ;
//   fprintf(stderr, "\n") ;

//   fprintf(stderr, "\n") ;
  for(i=0 ; i<64 ; i++){ xo2[i] = xe2[i] = 999 ; } ;
  fwd_1d_lgt53_split_simd(a64, xe2+1, xo2+1, 64) ;
//   for(i=0 ; i<34 ; i++) fprintf(stderr, "%3d ", xe2[i]) ;
//   fprintf(stderr, "\n") ;
//   for(i=0 ; i<34 ; i++) fprintf(stderr, "%3d ", xo2[i]) ;
//   fprintf(stderr, "\n") ;

//   fprintf(stderr, "\n") ;
  fprintf(stderr, "xe differences = %d", nerr = not_same((void *)xe1, (void *)xe2, 64)) ;
  fprintf(stderr, ", xo differences = %d\n", nerr = not_same((void *)xo1, (void *)xo2, 64)) ;
  if(nerr > 0 || not_same((void *)xe1, (void *)xe2, 64) > 0) goto fail ;

//   fprintf(stderr, "\n") ;
  inv_1d_lgt53_split_c(b64, xe1+1, xo1+1, 64) ;
//   for(i=-1  ; i<32 ; i++) fprintf(stderr, "%3d ", b64[i]) ;
//   fprintf(stderr, "\n") ;
//   for(i=32 ; i<65 ; i++) fprintf(stderr, "%3d ", b64[i]) ;
//   fprintf(stderr, "\n") ;
  fprintf(stderr, "a64-b64 differences (1D split) (c   ) (64) = %d\n", nerr = not_same((void *)a64_, (void *)b64_, 66)) ;
  if(nerr > 0) goto fail ;
  inv_1d_lgt53_split_simd(b64, xe1+1, xo1+1, 64) ;
//   for(i=-1  ; i<32 ; i++) fprintf(stderr, "%3d ", b64[i]) ;
//   fprintf(stderr, "\n") ;
//   for(i=32 ; i<65 ; i++) fprintf(stderr, "%3d ", b64[i]) ;
//   fprintf(stderr, "\n") ;
  fprintf(stderr, "a64-b64 differences (1D split) (simd) (64) = %d\n", nerr = not_same((void *)a64_, (void *)b64_, 66)) ;
  if(nerr > 0) goto fail ;

success:
  fprintf(stderr, "\nSUCCESS\n") ;
  return 0 ;

fail:
  fprintf(stderr, "\nFAILED\n");
  return 1 ;
}

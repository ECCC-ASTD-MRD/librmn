// Hopefully useful code for C (memory block movers)
// Copyright (C) 2022  Recherche en Prevision Numerique
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

#define USE_INTEL_SIMD_INTRINSICS    // use Intel intrinsics
#define WITH_SIMD                    // re-activate SIMD intrinsics everywhere

#include <rmn/simd_functions.h>
#include <rmn/move_blocks.h>

#undef MIN
#define MIN(OLD,NEW) OLD = (NEW < OLD) ? NEW : OLD
#undef MAX
#define MAX(OLD,NEW) OLD = (NEW > OLD) ? NEW : OLD

// bp  [IN] : block properties struct (min / max / min abs)
void print_int_props(block_properties bp){
  int absmax, absmin, amax, amin ;
  if(bp.kind != int_data && bp.kind != uint_data){
    fprintf(stderr, "print_int_props : ERROR, data kind is not integer\n") ;
  }
  if(bp.kind == int_data){
    amax = (bp.maxs.i) < 0 ? -bp.maxs.i : bp.maxs.i ;
    amin = (bp.mins.i) < 0 ? -bp.mins.i : bp.mins.i ;
    absmax = (amax > amin) ? amax : amin ;
    absmin = (amax < amin) ? amax : amin ;
    fprintf(stderr, "int   props : mins = %12d, maxs = %12d, minu = %12d, maxu = %12d, mina = %12d, maxa = %12d\n",
            bp.mins.i, bp.maxs.i, bp.minu.i, bp.maxu.i, absmin, absmax) ;
  }else{
    fprintf(stderr, "uint  props : mins = %12.8x, maxs = %12.8x, minu = %12.8x, maxu = %12.8x\n",
            bp.mins.u, bp.maxs.u, bp.minu.u, bp.maxu.u) ;
  }
}

// bp  [IN] : block properties struct (min / max / min abs)
void print_float_props(block_properties bp){
  if(bp.kind == float_data){
    fprintf(stderr, "float props : mins = %12.3f, maxs = %12f, minu = %12f, maxu = %12f\n", bp.mins.f, bp.maxs.f, bp.minu.f, bp.maxu.f) ;
  }else{
    fprintf(stderr, "print_int_props : ERROR, data kind is not float\n") ;
  }
}

// fold 8 value vectors for max / min / max_abs / min_abs into scalars and store into bp
// this works whether int or float data was analyzed
// bp  [OUT] : pointer to block properties struct (max / min / max_abs / min_abs) (IGNORED if NULL)

// signed part
static inline void fold_properties_s(__m256i vmaxs, __m256i vmins, block_properties *bp){
  int32_t ti[8], i, *p = ti ;

  // storeu_v256( (__v256i *tu , vminu ) style code used to cause an internal error with nvc compiler
  storeu_v256( (__m256i *)p , vmaxs ) ;
  for(i=0 ; i<8 ; i++) ti[0] = (ti[i] > ti[0]) ? ti[i] : ti[0] ;
  bp->maxs.i = ti[0] ;
  storeu_v256( (__m256i *)p , vmins ) ;
  for(i=0 ; i<8 ; i++) ti[0] = (ti[i] < ti[0]) ? ti[i] : ti[0] ;
  bp->mins.i = ti[0] ;
}

// unsigned part
static inline void fold_properties_u(__m256i vmaxu, __m256i vminu, block_properties *bp){
  int32_t i ;
  uint32_t tu[8], *p=tu ;

  // storeu_v256( (__v256i *tu , vminu ) style code used to cause an internal error with nvc compiler
  storeu_v256( (__m256i *)p , vmaxu ) ;
  for(i=0 ; i<8 ; i++) tu[0] = (tu[i] > tu[0]) ? tu[i] : tu[0] ;
  bp->maxu.u = tu[0] ;
  storeu_v256( (__m256i *)p , vminu ) ;
  for(i=0 ; i<8 ; i++) tu[0] = (tu[i] < tu[0]) ? tu[i] : tu[0] ;
  bp->minu.u = tu[0] ;
}

// signed and unsigned parts
static inline void fold_properties(__v256i vmaxs, __v256i vmins, __v256i vmaxu, __v256i vminu, block_properties *bp){
  fold_properties_s(vmaxs, vmins, bp) ;   // signed part
  fold_properties_u(vmaxu, vminu, bp) ;   // unsigned part
}

// move a block (ni x nj) of 32 bit integers from src and store it into blk
// no block properties are computed
// src  [IN] : integer array to extract data from (NON CONTIGUOUS storage)
// lnis [IN] : row storage size in src
// dst [OUT] : array to put extracted data into (NON CONTIGUOUS storage)
// lnid [IN] : row storage size in dst
// ni   [IN] : row size (row storage size in blk)
// nj   [IN] : number of rows
// bp   [IN] : pointer to block properties struct (min / max / min abs) (IGNORED if NULL)
// return number of values processed
// bp is really expected to be NULL, as no properties are computed
// kind is set ro raw data, all properties are set to 0 if bp is not NULL
// if dst is NULL or equal to src, no move will be performed
int move_mem32_block(void *src, int lnis, void *dst, int lnid, int ni, int nj){
  uint32_t *d = (uint32_t *) dst ;
  uint32_t *s = (uint32_t *) src ;
  int32_t ninj = ni * nj ;

  if(dst == NULL || dst == src) goto end ;      // nothing to move

  if(lnis <= 0 || lnid <= 0 || ni <= 0 || nj <= 0){
    fprintf(stderr, "ERROR move_mem32_block : lnis = %d, lnid = %d, ni = %d, nj = %d\n", lnis, lnid, ni, nj);
    return -1 ;
  }

  if(ni < 8){
    while(nj--){
      switch(ni & 7){   // switch on row length, fall through
        //       copy value
        case 7 : d[6] = s[6] ;
        case 6 : d[5] = s[5] ;
        case 5 : d[4] = s[4] ;
        case 4 : d[3] = s[3] ;
        case 3 : d[2] = s[2] ;
        case 2 : d[1] = s[1] ;
        case 1 : d[0] = s[0] ;
        case 0 : d += lnid ; s += lnis ;   // pointers to next row
      }
    }
  }else{
    __v256i vdata ;
    int ni7, n ;
    ni7 = (ni & 7) ;               // modulo(ni , 8)
    while(nj--){
      uint32_t *s0, *d0 ;
      n = ni ; s0 = s ; d0 = d ;
      if(ni7){                                   // first slice has less than 8 elements
        vdata = loadu_v256((__v256i *)s0) ;      // load data from source array
        storeu_v256((__v256i *)d0, vdata) ;      // store into destination array (CONTIGUOUS)
        n -= ni7 ; s0 += ni7 ; d0 += ni7 ;       // bump count and pointers
      }
      while(n > 7){                              // following slice(s) have 8 elements
        vdata = loadu_v256((__v256i *)s0) ;      // load data from source array
        storeu_v256((__v256i *)d0, vdata) ;      // store into destination array (CONTIGUOUS)
        n -= 8 ; s0 += 8 ; d0 += 8 ;
      }
      s += lnis ; d += lnid ;                       // pointers to next row
    }
  }
end:
  return ninj ;
}

// analyze a block (ni x nj) of 32 bit elements from src
// compute moved block min/max properties if bp is not NULL
// src  [IN] : array to extract data from (NON CONTIGUOUS storage)
// lnis [IN] : row storage size in src
// ni   [IN] : row size (row storage size in blk)
// nj   [IN] : number of rows
// bp  [OUT] : pointer to block properties struct (min / max / min abs) (IGNORED if NULL)
// return number of values processed, -1 in case of error
int analyze_data32_block(void *src, int lnis, int ni, int nj, block_properties *bp){
  int32_t *s = (int32_t *) src ;
  int32_t ninj = ni * nj ;

  if(lnis <= 0 || ni <= 0 || nj <= 0 || src == NULL){
    fprintf(stderr, "ERROR analyze_data32_block : lnis = %d, ni = %d, nj = %d, src = %p\n", lnis, ni, nj, s);
    return -1 ;
  }

  if(bp == NULL) return 0 ;  // no analysis requested

  bp->zeros  = -1 ;          // initialize for failure
  bp->kind   = bad_data ;

  if(ni  <  8) {
//     int32_t maxs = 0x80000000, mins = 0x7FFFFFFF, t ;
//     uint32_t minu = 0xFFFFFFFFu, maxu = 0 ;
    int32_t maxs, mins, t ;
    uint32_t minu, maxu ;
    maxs = mins = s[0] ;               // set min/max to first value
    maxu = minu = (uint32_t)s[0] ;     // set min/max to first value
    while(nj--){
      switch(ni & 7){   // switch on row length
        //       copy value        signed min    signed max    unsigned min             unsigned max
        case 7 : t = s[6] ; MIN(mins,t) ; MAX(maxs,t) ; MIN(minu, (uint32_t)t) ; MAX(maxu, ((uint32_t)t)) ;
        case 6 : t = s[5] ; MIN(mins,t) ; MAX(maxs,t) ; MIN(minu, (uint32_t)t) ; MAX(maxu, ((uint32_t)t)) ;
        case 5 : t = s[4] ; MIN(mins,t) ; MAX(maxs,t) ; MIN(minu, (uint32_t)t) ; MAX(maxu, ((uint32_t)t)) ;
        case 4 : t = s[3] ; MIN(mins,t) ; MAX(maxs,t) ; MIN(minu, (uint32_t)t) ; MAX(maxu, ((uint32_t)t)) ;
        case 3 : t = s[2] ; MIN(mins,t) ; MAX(maxs,t) ; MIN(minu, (uint32_t)t) ; MAX(maxu, ((uint32_t)t)) ;
        case 2 : t = s[1] ; MIN(mins,t) ; MAX(maxs,t) ; MIN(minu, (uint32_t)t) ; MAX(maxu, ((uint32_t)t)) ;
        case 1 : t = s[0] ; MIN(mins,t) ; MAX(maxs,t) ; MIN(minu, (uint32_t)t) ; MAX(maxu, ((uint32_t)t)) ;
        case 0 : s += lnis ;   // pointers to next row
      }
    }
    bp->maxs.i = maxs ; bp->mins.i = mins ; bp->minu.u = minu ; bp->maxu.u = maxu ;
  }else{      // (ni  <  8)
    __v256i vmaxs, vmins, vmaxu, vminu, vdata ;
    int32_t *s0 ;
    int ni7, n ;

    vmins = vmaxs = vminu = vmaxu = loadu_v256((__v256i *)s) ;  // set min/max to first 8 values
    ni7 = (ni & 7) ;               // modulo(ni , 8)
    while(nj--){                                 // loop over rows
      n = ni ; s0 = s ;
      if(ni7){                                   // first slice has less than 8 elements
        vdata = loadu_v256((__v256i *)s0) ;      // load data from source array (CONTIGUOUS)
        vminu = min_v8u(vminu, vdata) ;          // minimum absolute value
        vmaxu = max_v8u(vmaxu, vdata) ;          // max value with data treated as UNSIGNED
        vmaxs = max_v8i(vmaxs, vdata) ;          // maximum signed value
        vmins = min_v8i(vmins, vdata) ;          // minimum signed value
        n -= ni7 ; s0 += ni7 ;                   // bump count and pointer
      }
      while(n > 7){                              // following slice(s) have 8 elements
        vdata = loadu_v256((__v256i *)s0) ;      // load data from source array (CONTIGUOUS)
        vminu = min_v8u(vminu, vdata) ;          // min value with data treated as UNSIGNED
        vmaxu = max_v8u(vmaxu, vdata) ;          // max value with data treated as UNSIGNED
        vmaxs = max_v8i(vmaxs, vdata) ;          // maximum signed value
        vmins = min_v8i(vmins, vdata) ;          // minimum signed value
        n -= 8 ; s0 += 8 ;
      }
      s += lnis ;                                // pointer to next row (can be NON CONTIGUOUS)
    }
    fold_properties(vmaxs, vmins, vmaxu, vminu, bp) ; // fold results into a single scalar
  }      // (ni  <  8)
  bp->kind   = raw_data ;
// fprintf(stderr,"move_data32_block : mins = %8.8x, maxs = %8.8x, minu = %8.8x, maxu = %8.8x\n",bp->mins.u, bp->maxs.u, bp->minu.u, bp->maxu.u);
  return ninj ;
}

// move a block (ni x nj) of 32 bit elements from src and store it into blk,
// compute moved block min/max properties if bp is not NULL
// src  [IN] : array to extract data from (NON CONTIGUOUS storage)
// lnis [IN] : row storage size in src
// dst [OUT] : array to put extracted data into (NON CONTIGUOUS storage)
// lnid [IN] : row storage size in dst
// ni   [IN] : row size (row storage size in blk)
// nj   [IN] : number of rows
// bp  [OUT] : pointer to block properties struct (min/max, signed/unsigned) (IGNORED if NULL)
// return number of values processed
// if dst is NULL or equal to src, no move will be performed, block_properties will be computed
int move_data32_block(void *src, int lnis, void *dst, int lnid, int ni, int nj, block_properties *bp){
  int32_t *s = (int32_t *) src ;
  int32_t *d = (int32_t *) dst ;
  int32_t ninj = ni * nj ;

  if(src == dst || dst == NULL){   // no data copy, lnid will be ignored
    return (lnis == lnid) ? analyze_data32_block(src, lnis, ni, nj, bp) : 0 ;
  }

  if(bp == NULL) return move_mem32_block(src, lnis, dst, lnid, ni, nj) ;  // no data analysis, just move data

  if(lnis <= 0 || lnid <= 0 || ni <= 0 || nj <= 0){
    fprintf(stderr, "ERROR move_data32_block : lnis = %d, lnid = %d, ni = %d, nj = %d\n", lnis, lnid, ni, nj);
    return -1 ;
  }

  bp->zeros  = -1 ;          // initialize for failure
  bp->kind   = bad_data ;

  if(ni  <  8) {             // row is shorter than 8, pure scalar code
    int32_t maxs, mins, t ;
    uint32_t minu, maxu ;
    maxs = mins = s[0] ;               // set min/max to first value
    maxu = minu = (uint32_t)s[0] ;     // set min/max to first value
    while(nj--){
      switch(ni & 7){   // switch on row length
        //       copy value        signed min    signed max    unsigned min             unsigned max
        case 7 : d[6] = t = s[6] ; MIN(mins,t) ; MAX(maxs,t) ; MIN(minu, (uint32_t)t) ; MAX(maxu, ((uint32_t)t)) ;
        case 6 : d[5] = t = s[5] ; MIN(mins,t) ; MAX(maxs,t) ; MIN(minu, (uint32_t)t) ; MAX(maxu, ((uint32_t)t)) ;
        case 5 : d[4] = t = s[4] ; MIN(mins,t) ; MAX(maxs,t) ; MIN(minu, (uint32_t)t) ; MAX(maxu, ((uint32_t)t)) ;
        case 4 : d[3] = t = s[3] ; MIN(mins,t) ; MAX(maxs,t) ; MIN(minu, (uint32_t)t) ; MAX(maxu, ((uint32_t)t)) ;
        case 3 : d[2] = t = s[2] ; MIN(mins,t) ; MAX(maxs,t) ; MIN(minu, (uint32_t)t) ; MAX(maxu, ((uint32_t)t)) ;
        case 2 : d[1] = t = s[1] ; MIN(mins,t) ; MAX(maxs,t) ; MIN(minu, (uint32_t)t) ; MAX(maxu, ((uint32_t)t)) ;
        case 1 : d[0] = t = s[0] ; MIN(mins,t) ; MAX(maxs,t) ; MIN(minu, (uint32_t)t) ; MAX(maxu, ((uint32_t)t)) ;
        case 0 : d += lnid ; s += lnis ;   // pointers to next row
      }
    }
    bp->maxs.i = maxs ; bp->mins.i = mins ; bp->minu.u = minu ; bp->maxu.u = maxu ;
  }else{      // (ni  <  8)  row length is 8 or more, SIMD code
    __v256i vmaxs, vmins, vmaxu, vminu, vdata ;
    int32_t *s0, *d0 ;
    int ni7, n ;

    vmins = vmaxs = vminu = vmaxu = loadu_v256((__v256i *)s) ;  // set min/max to first 8 values

    ni7 = (ni & 7) ;                             // modulo(ni , 8)
    while(nj--){                                 // loop over rows
      n = ni ; s0 = s ; d0 = d ;
      if(ni7){                                   // first slice has less than 8 elements (may overlap next slice)
        vdata = loadu_v256((__v256i *)s0) ;      // load data from source array (CONTIGUOUS)
        storeu_v256((__v256i *)d0, vdata) ;      // store into destination array (CONTIGUOUS)
        vminu = min_v8u(vminu, vdata) ;          // min value with data treated as UNSIGNED
        vmaxu = max_v8u(vmaxu, vdata) ;          // max value with data treated as UNSIGNED
        vmaxs = max_v8i(vmaxs, vdata) ;          // maximum SIGNED value
        vmins = min_v8i(vmins, vdata) ;          // minimum SIGNED value
        n -= ni7 ; s0 += ni7 ; d0 += ni7 ;       // bump count and pointers
      }
      while(n > 7){                              // following slice(s) have 8 elements
        vdata = loadu_v256((__v256i *)s0) ;      // load data from source array (CONTIGUOUS)
        storeu_v256((__v256i *)d0, vdata) ;      // store into destination array (CONTIGUOUS)
        vminu = min_v8u(vminu, vdata) ;          // min value with data treated as UNSIGNED
        vmaxu = max_v8u(vmaxu, vdata) ;          // max value with data treated as UNSIGNED
        vmaxs = max_v8i(vmaxs, vdata) ;          // maximum SIGNED value
        vmins = min_v8i(vmins, vdata) ;          // minimum SIGNED value
        n -= 8 ; s0 += 8 ; d0 += 8 ;             // bump count and pointers
      }
      s += lnis ; d += lnid ;                    // pointers to next row (can be NON CONTIGUOUS)
    }
    fold_properties(vmaxs, vmins, vmaxu, vminu, bp) ; // fold results into a single scalar
  }      // (ni  <  8)
  bp->kind   = raw_data ;
// fprintf(stderr,"move_data32_block : mins = %8.8x, maxs = %8.8x, minu = %8.8x, maxu = %8.8x\n",bp->mins.u, bp->maxs.u, bp->minu.u, bp->maxu.u);
  return ninj ;
}

// adjust bp according to data type (see data_kind.h)
// bp    [INOUT] : pointer to block properties struct (min / max / min abs)
// datatype [IN] : data type int_data / uint_data / float_data / raw_data
void adjust_block_properties(block_properties *bp, data_kind datatype){
  if(bp == NULL) return ;
  if(datatype == any_data) datatype = bp->kind ;
  if(datatype == float_data){
    if(bp->maxs.i < 0){           // all numbers are negative
      float max = bp->minu.f ;
      float min = bp->maxu.f ;
      bp->mins.f =  min ;         // most negative value  (minimum value)
      bp->maxs.f =  max ;         // least negative value  (maximum value)
      bp->minu.f = -max ;         // smallest absolute value
      bp->maxu.f = -min ;         // largest absolute value
    }else if(bp->mins.i < 0) {    // negative and non negative numbers
      float max = bp->maxs.f ;    // most positive value
      float min = bp->maxu.f ;    // most negative value
      float mins = bp->mins.f ;   // negative value closest to zero
      float minu = bp->minu.f ;   // positive value closest to zero
      bp->mins.f =  min ;         // largest negative value  (minimum value)
      bp->maxs.f =  max ;         // largest positive value  (maximum value)
      bp->minu.f = (minu < (-mins)) ? minu : (-mins) ;       // smallest absolute value
      bp->maxu.f = ((max > (-min)) ? max : (-min) ) ;        // largest absolute value
    }
    bp->kind = float_data ;
  }else if(datatype == int_data){
    bp->kind = int_data ;
    // bp->minu.i will be the smallest value >= 0
    // bp->maxu.i will be the negative value closest to 0 if negative values are present
    // if no negative values are present, bp->maxu.i will be equal to bp->maxs.i
  }else if(datatype == uint_data){
    bp->kind = uint_data ;
    // bp->maxs and bp->mins  are meaningless
  }else if(datatype == raw_data){
    bp->kind = raw_data ;
  }else{
    bp->kind = bad_data ;
  }
}

// add(merge) properties from bp_extra into bp
// bp     [INOUT] : pointer to block properties struct
// bp_extra  [IN] : pointer to block properties struct
// the properties in bp_extra will be min/max(ed) with the properties in bp
void add_block_properties(block_properties *bp, block_properties *bp_extra){
  if(bp == NULL || bp_extra == NULL) return ;
  data_kind datatype = bp->kind ;
  if(bp_extra->kind != datatype) return ;

  if(datatype == float_data){
    bp->maxs.f = MAX(bp_extra->maxs.f , bp->maxs.f) ;
    bp->mins.f = MIN(bp_extra->mins.f , bp->mins.f) ;
    bp->maxu.f = MAX(bp_extra->maxu.f , bp->maxu.f) ;
    bp->minu.f = MIN(bp_extra->minu.f , bp->minu.f) ;
  }else if(datatype == int_data){
    bp->maxs.i = MAX(bp_extra->maxs.i , bp->maxs.i) ;
    bp->mins.i = MIN(bp_extra->mins.i , bp->mins.i) ;
    bp->maxu.u = MAX(bp_extra->maxu.u , bp->maxu.u) ;
    bp->minu.u = MIN(bp_extra->minu.u , bp->minu.u) ;
  }else if(datatype == uint_data){
    bp->maxu.u = MAX(bp_extra->maxu.u , bp->maxu.u) ;
    bp->minu.u = MIN(bp_extra->minu.u , bp->minu.u) ;
  }
  if( (bp->zeros > 0) && (bp_extra->zeros > 0) ) bp->zeros += bp_extra->zeros ;
}

// move a block (ni x nj) of 32 bit floats from src to dst, set block properties
// src  [IN] : float array to extract data from (NON CONTIGUOUS storage)
// dst [OUT] : array to put extracted data into (NON CONTIGUOUS storage)
// ni   [IN] : row size
// lnis [IN] : row storage size in src
// lnid [IN] : row storage size in dst
// nj   [IN] : number of rows
// bp  [OUT] : pointer to block properties struct (min / max / min abs) (IGNORED if NULL)
// return number of values processed
// if dst is NULL or equal to src, no move will be performed, block_properties will be computed
int move_float_block(float *src, int lnis, void *dst, int lnid, int ni, int nj, block_properties *bp){

  if(bp == NULL) return move_mem32_block(src, lnis, dst, lnid, ni, nj) ;

  int rc = move_data32_block(src, lnis, dst, lnid, ni, nj, bp) ;
  if(rc != 0) adjust_block_properties(bp, float_data) ;
  return rc ;
}

// move a block (ni x nj) of 32 bit signed integers from src and store it into blk, set block properties
// src  [IN] : integer array to extract data from (NON CONTIGUOUS storage)
// lnis [IN] : row storage size in src
// dst [OUT] : array to put extracted data into (NON CONTIGUOUS storage)
// lnid [IN] : row storage size in dst
// ni   [IN] : row size (row storage size in blk)
// nj   [IN] : number of rows
// bp  [OUT] : pointer to block properties struct (min / max / min abs) (IGNORED if NULL)
// return number of values processed
// minu.i will be the smallest value >= 0
// if negative numbers are present, maxu.i will be the negative value closest to 0
// if dst is NULL or equal to src, no move will be performed, block_properties will be computed
int move_int32_block(int32_t *src, int lnis, void *dst, int lnid, int ni, int nj, block_properties *bp){

  if(bp == NULL) return move_mem32_block(src, lnis, dst, lnid, ni, nj) ;

  int rc = move_data32_block(src, lnis, dst, lnid, ni, nj, bp) ;
  if(rc != 0) adjust_block_properties(bp, int_data) ;
  return rc ;
}

// same as above for 32 bit unsigned integers
// maxs and mins components of bp are meanigless and are left untouched in bp
int move_uint32_block(uint32_t *src, int lnis, void *dst, int lnid, int ni, int nj, block_properties *bp){

  if(bp == NULL) return move_mem32_block(src, lnis, dst, lnid, ni, nj) ;

  int rc = move_data32_block(src, lnis, dst, lnid, ni, nj, bp) ;
  if(rc != 0) adjust_block_properties(bp, uint_data) ;
  return rc ;
}

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
#include <stdio.h>

#include <rmn/bitstream.h>

static uint32_t be_pack_u32(bitstream *s, uint32_t *in, int nbits, int n, uint32_t options);
static uint32_t be_unpack_u32(bitstream *s, uint32_t *out, int nbits, int n, uint32_t options);
static uint32_t be_unpack_i32(bitstream *s, int32_t *out, int nbits, int n, uint32_t options);

static uint32_t le_pack_u32(bitstream *s, uint32_t *in, int nbits, int n, uint32_t options);
static uint32_t le_unpack_u32(bitstream *s, uint32_t *out, int nbits, int n, uint32_t options);
static uint32_t le_unpack_i32(bitstream *s, int32_t *out, int nbits, int n, uint32_t options);

// insert the lower nbits bits of n unsigned integers from array in into stream s
uint32_t stream_pack_u32(bitstream *s, void *in, int nbits, int n, uint32_t options){
  if(s == NULL || in == NULL) return 0 ;
  if(nbits < 1 || nbits > 32) return 0 ;
  if(StreamAvailableSpace(s) < n * nbits){
    fprintf(stderr, "not enough space left for packed data, need %d, have %ld\n", n * nbits, StreamAvailableSpace(s)) ;
    return 0 ;
  }
  if(STREAM_IS_BIG_ENDIAN(*s)){
    return be_pack_u32(s, in, nbits, n, options) ;
  }
  if(STREAM_IS_LITTLE_ENDIAN(*s)){
    return le_pack_u32(s, in, nbits, n, options) ;
  }
  return 0 ;
}

// extract n unsigned integers nbits long from stream s and sore them into array out
uint32_t stream_unpack_u32(bitstream *s, void *out, int nbits, int n, uint32_t options){
  if(s == NULL || out == NULL) return 0 ;
  if(nbits < 1 || nbits > 32)  return 0 ;
  if(StreamAvailableBits(s) < n * nbits){
    fprintf(stderr, "not enough data in stream, need %d, have %ld\n", n * nbits, StreamAvailableBits(s)) ;
    return 0 ;
  }
  if(STREAM_IS_BIG_ENDIAN(*s)){
    return be_unpack_u32(s, out, nbits, n, options) ;
  }
  if(STREAM_IS_LITTLE_ENDIAN(*s)){
    return le_unpack_u32(s, out, nbits, n, options) ;
  }
  return 0 ;
}

// insert the lower nbits bits of n signed integers from array in into stream s
uint32_t stream_pack_i32(bitstream *s, void *in, int nbits, int n, uint32_t options){
  return stream_pack_u32(s, in, nbits, n, options) ;
}

// extract n unsigned integers nbits long from stream s and sore them into array out
uint32_t stream_unpack_i32(bitstream *s, void *out, int nbits, int n, uint32_t options){
  if(s == NULL || out == NULL) return 0 ;
  if(nbits < 1 || nbits > 32)  return 0 ;
  if(StreamAvailableBits(s) < n * nbits){
    fprintf(stderr, "not enough data in stream, need %d, have %ld (%d x %d)\n", n * nbits, StreamAvailableBits(s), n, nbits) ;
//     return 0 ;
  }
  if(STREAM_IS_BIG_ENDIAN(*s)){
    return be_unpack_i32(s, out, nbits, n, options) ;
  }
  if(STREAM_IS_LITTLE_ENDIAN(*s)){
    return le_unpack_i32(s, out, nbits, n, options) ;
  }
  return 0 ;
}

#include<rmn/be_stream.h>
uint32_t be_pack_u32(bitstream *s, uint32_t *in, int nbits, int n, uint32_t options){
  int i ;
  bitstream s0 = *s ;
  if(PACK_INIT & options) STREAM_INSERT_BEGIN(s0) ;          // initialize insertion
  for(i=0 ; i<n ; i++){
    STREAM_PUT_NBITS(s0, in[i], nbits) ;
  }
  if(PACK_FINALIZE & options) STREAM_INSERT_FINALIZE(s0) ;   // flush residual data to stream
  if(PACK_ALIGN32  & options) STREAM_INSERT_ALIGN32(s0) ;
  *s = s0 ;
  return n * nbits ;
}

uint32_t be_unpack_u32(bitstream *s, uint32_t *out, int nbits, int n, uint32_t options){
  int i ;
  bitstream s0 = *s ;
  if(UNPACK_INIT & options) STREAM_XTRACT_BEGIN(s0) ;
  for(i=0 ; i<n ; i++){
    STREAM_GET_NBITS(s0, out[i], nbits) ;
  } ;
  if(UNPACK_FINALIZE & options) STREAM_XTRACT_FINAL(s0) ;
  if(UNPACK_ALIGN32  & options) STREAM_XTRACT_ALIGN32(s0) ;
  *s = s0 ;
  return n * nbits ;
}

uint32_t be_unpack_i32(bitstream *s, int32_t *out, int nbits, int n, uint32_t options){
  int i ;
  bitstream s0 = *s ;
  if(UNPACK_INIT & options) STREAM_XTRACT_BEGIN(s0) ;
  for(i=0 ; i<n ; i++){
    STREAM_GET_NBITS(s0, out[i], nbits) ;
    out[i] <<= (32-nbits) ;
    out[i] >>= (32-nbits) ;
  } ;
  if(UNPACK_FINALIZE & options) STREAM_XTRACT_FINAL(s0) ;
  if(UNPACK_ALIGN32  & options) STREAM_XTRACT_ALIGN32(s0) ;
  *s = s0 ;
  return n * nbits ;
}

#include<rmn/le_stream.h>
uint32_t le_pack_u32(bitstream *s, uint32_t *in, int nbits, int n, uint32_t options){
  int i ;
  bitstream s0 = *s ;
  if(PACK_INIT & options) STREAM_INSERT_BEGIN(s0) ;          // initialize insertion
  for(i=0 ; i<n ; i++){
    STREAM_PUT_NBITS(s0, in[i], nbits) ;
  }
  if(PACK_FINALIZE & options) STREAM_INSERT_FINALIZE(s0) ;   // flush residual data to stream
  if(PACK_ALIGN32  & options) STREAM_INSERT_ALIGN32(s0) ;
  *s = s0 ;
  return n * nbits ;
}

uint32_t le_unpack_u32(bitstream *s, uint32_t *out, int nbits, int n, uint32_t options){
  int i ;
  bitstream s0 = *s ;
  if(UNPACK_INIT & options) STREAM_XTRACT_BEGIN(s0) ;
  for(i=0 ; i<n ; i++){
    STREAM_GET_NBITS(s0, out[i], nbits) ;
  } ;
  if(UNPACK_FINALIZE & options) STREAM_XTRACT_FINAL(s0) ;
  if(UNPACK_ALIGN32  & options) STREAM_XTRACT_ALIGN32(s0) ;
  *s = s0 ;
  return n * nbits ;
}

uint32_t le_unpack_i32(bitstream *s, int32_t *out, int nbits, int n, uint32_t options){
  int i ;
  bitstream s0 = *s ;
  if(UNPACK_INIT & options) STREAM_XTRACT_BEGIN(s0) ;
  for(i=0 ; i<n ; i++){
    STREAM_GET_NBITS(s0, out[i], nbits) ;
    out[i] <<= (32-nbits) ;
    out[i] >>= (32-nbits) ;
  } ;
  if(UNPACK_FINALIZE & options) STREAM_XTRACT_FINAL(s0) ;
  if(UNPACK_ALIGN32  & options) STREAM_XTRACT_ALIGN32(s0) ;
  *s = s0 ;
  return n * nbits ;
}


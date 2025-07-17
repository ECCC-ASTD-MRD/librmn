#include <stdio.h>

// #include <rmn/be_stream.h>
#include <rmn/bitstream.h>
#include <rmn/timers.h>

#undef NPTS
#define NPTS 1025

int main(int argc, char **argv){
  int32_t in[NPTS], outs[NPTS], i, nbits ;
  uint32_t outu[NPTS], mask, sbufb[NPTS * 33], sbufl[NPTS * 33], pbits ;
  ssize_t sbits ;
  bitstream sbe, sle, sbe0, sle0, sbe1, sle1 ;

  fprintf(stderr, "==================== %s ====================\n", argv[0]);
  while(--argc > 0){
    argv++ ;
    fprintf(stderr, "%s ", argv[0]);
  }
//   fprintf(stderr, "==================== (debug = %d) ====================\n", StreamDebugGet()) ;

  sbe = NULL_BITSTREAM ;
  sle = NULL_BITSTREAM ;
  //  initialize bit stream, set endianness
//   STREAM_INIT(&sbe, sbufb, sizeof(sbufb), 0) ;
//   if(sbe.endian != PACK_ENDIAN) goto fail ;
  InitStream(&sbe, sbufb, sizeof(sbufb), BIT_FULL_INIT | SET_BIG_ENDIAN) ;
  sbe0 = sbe ;     // save sbe state after initialization
  InitStream(&sle, sbufl, sizeof(sbufb), BIT_FULL_INIT | SET_LITTLE_ENDIAN) ;
  sle0 = sle ;     // save sle state after initialization

  if(StreamAvailableBits(&sbe) != 0){                         // test failed
    fprintf(stderr, "BE StreamAvailableBits = %ld, expecting 0\n", StreamAvailableBits(&sbe)) ;
    goto fail ;
  }
  if(StreamAvailableSpace(&sbe) != 8*sizeof(sbufb)){           // test failed
    fprintf(stderr, "BE StreamAvailableSpace = %ld, expecting %ld\n", StreamAvailableSpace(&sbe), 8*sizeof(sbufb)) ;
    goto fail ;
  }
  fprintf(stderr, "BE Stream created, %s%s , size = %ld bits, buffer at %p\n",
          STREAM_IS_BIG_ENDIAN(sbe)    ? "Big Endian   " : "" ,
          STREAM_IS_LITTLE_ENDIAN(sbe) ? "Little Endian" : "" ,
          StreamAvailableSpace(&sbe), (void *)&sbufb[0]);

  if(StreamAvailableBits(&sle) != 0){                         // test failed
    fprintf(stderr, "LE StreamAvailableBits = %ld, expecting 0\n", StreamAvailableBits(&sle)) ;
    goto fail ;
  }
  if(StreamAvailableSpace(&sle) != 8*sizeof(sbufb)){           // test failed
    fprintf(stderr, "LE StreamAvailableSpace = %ld, expecting %ld\n", StreamAvailableSpace(&sle), 8*sizeof(sbufb)) ;
    goto fail ;
  }
  fprintf(stderr, "LE Stream created, %s%s , size = %ld bits, buffer at %p\n",
          STREAM_IS_BIG_ENDIAN(sle)    ? "Big Endian   " : "" ,
          STREAM_IS_LITTLE_ENDIAN(sle) ? "Little Endian" : "" ,
          StreamAvailableSpace(&sle), (void *)&sbufl[0]);

  // pack into stream, align to 32 bit boundary between values of nbits
  for(i=0 ; i<(int64_t)(sizeof(sbufb)/sizeof(uint32_t)) ; i++){ sbufb[i] = sbufl[i] = 0 ; }
  for(nbits=1 ; nbits<33 ; nbits+=1){
    mask = 0xFFFFFFFFu >> (32-nbits) ;
    for(i=0 ; i<NPTS ; i++){
      in[i] = (i-NPTS/2) ;
    }
    pbits = stream_pack_u32(&sbe, in, nbits, NPTS, PACK_FINALIZE | PACK_ALIGN32) ;
    if(pbits == 0) goto fail ;
    pbits = stream_pack_u32(&sle, in, nbits, NPTS, PACK_FINALIZE | PACK_ALIGN32) ;
    if(pbits == 0) goto fail ;
  }
  fprintf(stderr, "PACK-BE   : StreamAvailableSpace = %ld, StreamAvailableBits = %ld\n",
          StreamAvailableSpace(&sbe), StreamAvailableBits(&sbe)) ;
  fprintf(stderr, "PACK-LE   : StreamAvailableSpace = %ld, StreamAvailableBits = %ld\n",
          StreamAvailableSpace(&sle), StreamAvailableBits(&sle)) ;

  for(nbits=1 ; nbits<33 ; nbits+=1){
    sbits = StreamAvailableBits(&sbe) ;    // check that there is enough data in stream
    if(sbits < NPTS*nbits) goto fail ;
    pbits = stream_unpack_u32(&sbe, outu, nbits, NPTS, UNPACK_ALIGN32 ) ;
    if(pbits == 0) goto fail ;
    mask = 0xFFFFFFFFu >> (32-nbits) ;
    for(i=0 ; i<NPTS ; i++){
      in[i] = (i-NPTS/2) & mask ;
      if(in[i] != (int32_t)outu[i]){
        fprintf(stderr, "UNPACK-BE : i = %2d, expecting %8.8x, got %8.8x\n", i, in[i], outu[i]) ;
        goto fail ;
      }
    }
  }
  fprintf(stderr, "UNPACK-BE : StreamAvailableSpace = %ld, StreamAvailableBits = %ld\n",
          StreamAvailableSpace(&sbe), StreamAvailableBits(&sbe)) ;
  sbits = StreamAvailableBits(&sbe) ;
  if(sbits != 0) goto fail ;        // there should be no data left in stream
  StreamRewind(&sbe, 1) ;
  fprintf(stderr, "REWIND-BE : StreamAvailableSpace = %ld, StreamAvailableBits = %ld\n",
          StreamAvailableSpace(&sbe), StreamAvailableBits(&sbe)) ;

  for(nbits=1 ; nbits<33 ; nbits+=1){
    sbits = StreamAvailableBits(&sle) ;
    if(sbits < NPTS*nbits) goto fail ;    // check that there is enough data in stream
    pbits = stream_unpack_u32(&sle, outu, nbits, NPTS, UNPACK_ALIGN32 ) ;
    if(pbits == 0) goto fail ;
    mask = 0xFFFFFFFFu >> (32-nbits) ;
    for(i=0 ; i<NPTS ; i++){
      in[i] = (i-NPTS/2) & mask ;
      if(in[i] != (int32_t)outu[i]){
        fprintf(stderr, "UNPACK-LE : i = %2d, expecting %8.8x, got %8.8x\n", i, in[i], outu[i]) ;
        goto fail ;
      }
    }
  }
  fprintf(stderr, "UNPACK-LE : StreamAvailableSpace = %ld, StreamAvailableBits = %ld\n",
          StreamAvailableSpace(&sle), StreamAvailableBits(&sle)) ;
  sbits = StreamAvailableBits(&sle) ;
  if(sbits != 0) goto fail ;        // there should be no data left in stream
  StreamRewind(&sle, 1) ;
  fprintf(stderr, "REWIND-LE : StreamAvailableSpace = %ld, StreamAvailableBits = %ld\n",
          StreamAvailableSpace(&sle), StreamAvailableBits(&sle)) ;

  for(nbits=1 ; nbits<33 ; nbits+=1){
    pbits = stream_unpack_i32(&sbe, outs, nbits, NPTS, UNPACK_ALIGN32 ) ;
    if(pbits == 0) goto fail ;
    for(i=0 ; i<NPTS ; i++){
      in[i] = (i-NPTS/2) ;
      in[i] <<= (32-nbits) ;
      in[i] >>= (32-nbits) ;
      if(in[i] != outs[i]){
        fprintf(stderr, "UNPACK-BE : nbits = %2d, i = %2d, expecting %8.8x, got %8.8x\n", nbits, i, in[i], outu[i]) ;
        goto fail ;
      }
    }
  }

  StreamRewrite(&sbe, 1) ;
  fprintf(stderr, "REWRIT-BE : StreamAvailableSpace = %ld, StreamAvailableBits = %ld\n",
          StreamAvailableSpace(&sbe), StreamAvailableBits(&sbe)) ;

  for(nbits=1 ; nbits<33 ; nbits+=1){
    pbits = stream_unpack_i32(&sle, outs, nbits, NPTS, UNPACK_ALIGN32 ) ;
    if(pbits == 0) goto fail ;
    for(i=0 ; i<NPTS ; i++){
      in[i] = (i-NPTS/2) ;
      in[i] <<= (32-nbits) ;
      in[i] >>= (32-nbits) ;
      if(in[i] != outs[i]){
        fprintf(stderr, "UNPACK-LE : nbits = %2d, i = %2d, expecting %8.8x, got %8.8x\n", nbits, i, in[i], outu[i]) ;
        goto fail ;
      }
    }
  }

  StreamRewrite(&sle, 1) ;
  fprintf(stderr, "REWRIT-LE : StreamAvailableSpace = %ld, StreamAvailableBits = %ld\n",
          StreamAvailableSpace(&sle), StreamAvailableBits(&sle)) ;

  fprintf(stderr, "SUCCESS\n") ;

  fprintf(stderr, "==================== timing tests ====================\n");
  uint64_t freq, tpack_be[33], tpack_le[33], tupku_be[33], tupks_be[33], tupku_le[33], tupks_le[33], t0 ;
  double nano ;
  int npts ;
  freq = cycles_counter_freq() ;
  nano = 1000000000.0f ;
  nano = nano / freq ;
  npts = NPTS ;
  fprintf(stderr, "timing freq = %f GHz, timing tick = %8.2G ns, %d values\n", freq/1000000000.0f, nano, npts) ;

  for(i=0 ; i<npts ; i++){ in[i] = (i-npts/2) ; }  // data to be packed

  sbe = sbe0 ;                         // reset to freshly initialized stream
  // Big Endian pack
  for(nbits=1 ; nbits<33 ; nbits+=1){
    t0 = elapsed_cycles() ;
    pbits = stream_pack_u32(&sbe, in, nbits, NPTS, PACK_FINALIZE | PACK_ALIGN32) ;
    tpack_be[nbits] = elapsed_cycles() - t0 ;
    if(pbits == 0) goto fail ;
  }

  sle = sle0 ;                         // reset to freshly initialized stream
  // Little Endian pack
  for(nbits=1 ; nbits<33 ; nbits+=1){
    t0 = elapsed_cycles() ;
    pbits = stream_pack_u32(&sle, in, nbits, NPTS, PACK_FINALIZE | PACK_ALIGN32) ;
    tpack_le[nbits] = elapsed_cycles() - t0 ;
    if(pbits == 0) goto fail ;
  }

  sbe1 = sbe ;                         // save state after insertion
  // Big Endian unsigned unpack
  for(nbits=1 ; nbits<33 ; nbits+=1){
    t0 = elapsed_cycles() ;
    pbits = stream_unpack_u32(&sbe, outu, nbits, NPTS, UNPACK_ALIGN32 ) ;
    tupku_be[nbits] = elapsed_cycles() - t0 ;
    if(pbits == 0) goto fail ;
    // verify unpack results
    mask = 0xFFFFFFFFu >> (32-nbits) ;
    for(i=0 ; i<NPTS ; i++){
      in[i] = (i-NPTS/2) & mask ;
      if(in[i] != (int32_t)outu[i]){
        fprintf(stderr, "UNPACK-BE : i = %2d, expecting %8.8x, got %8.8x\n", i, in[i], outu[i]) ;
        goto fail ;
      }
    }
  }

  sle1 = sle ;                         // save state after insertion
  // Little Endian unsigned unpack
  for(nbits=1 ; nbits<33 ; nbits+=1){
    t0 = elapsed_cycles() ;
    pbits = stream_unpack_u32(&sle, outu, nbits, NPTS, UNPACK_ALIGN32 ) ;
    tupku_le[nbits] = elapsed_cycles() - t0 ;
    if(pbits == 0) goto fail ;
    // verify unpack results
    mask = 0xFFFFFFFFu >> (32-nbits) ;
    for(i=0 ; i<NPTS ; i++){
      in[i] = (i-NPTS/2) & mask ;
      if(in[i] != (int32_t)outu[i]){
        fprintf(stderr, "UNPACK-LE : i = %2d, expecting %8.8x, got %8.8x\n", i, in[i], outu[i]) ;
        goto fail ;
      }
    }
  }

  sbe = sbe1 ;                         // reset to state after insertion
  // Big Endian signed unpack
  for(nbits=1 ; nbits<33 ; nbits+=1){
    t0 = elapsed_cycles() ;
    pbits = stream_unpack_i32(&sbe, outs, nbits, NPTS, UNPACK_ALIGN32 ) ;
    tupks_be[nbits] = elapsed_cycles() - t0 ;
    if(pbits == 0) goto fail ;
    // verify unpack results
    for(i=0 ; i<NPTS ; i++){
      in[i] = (i-NPTS/2) ;
      in[i] <<= (32-nbits) ;
      in[i] >>= (32-nbits) ;
      if(in[i] != outs[i]){
        fprintf(stderr, "UNPACK-BE : nbits = %2d, i = %2d, expecting %8.8x, got %8.8x\n", nbits, i, in[i], outu[i]) ;
        goto fail ;
      }
    }
  }

  sle = sle1 ;                         // reset to state after insertion
  // Little Endian signed unpack
  for(nbits=1 ; nbits<33 ; nbits+=1){
    t0 = elapsed_cycles() ;
    pbits = stream_unpack_i32(&sle, outs, nbits, NPTS, UNPACK_ALIGN32 ) ;
    tupks_le[nbits] = elapsed_cycles() - t0 ;
    if(pbits == 0) goto fail ;
    // verify unpack results
    for(i=0 ; i<NPTS ; i++){
      in[i] = (i-NPTS/2) ;
      in[i] <<= (32-nbits) ;
      in[i] >>= (32-nbits) ;
      if(in[i] != outs[i]){
        fprintf(stderr, "UNPACK-LE : nbits = %2d, i = %2d, expecting %8.8x, got %8.8x\n", nbits, i, in[i], outu[i]) ;
        goto fail ;
      }
    }
  }
  // print timings
  fprintf(stderr, "ns/pt          Pack              Unpack unsigned   Unpack signed\n") ;
  fprintf(stderr, "                BE        LE      BE       LE       BE       LE\n") ;
  for(nbits=1 ; nbits<33 ; nbits+=1){
    fprintf(stderr, "nbits = %2d %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f\n", nbits,
            (tpack_be[nbits])*nano/npts, (tpack_le[nbits])*nano/npts,
            (tupku_be[nbits])*nano/npts, (tupku_le[nbits])*nano/npts,
            (tupks_be[nbits])*nano/npts, (tupks_le[nbits])*nano/npts) ;
  }

  fprintf(stderr, "SUCCESS\n") ;
  return 0 ;

fail:
  fprintf(stderr, "FAIL\n") ;
  return 1 ;
}

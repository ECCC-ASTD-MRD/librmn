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
// set of functions to manage a bit stream
// N.B. this bitstream is a sequence of 32 bit unsigned integers

#include <string.h>
#include <stdio.h>

// stream = NULL_BITSTREAM will initialize it to an invalid endian mode
// if be_stream.h or le_stream.h was not included before bitstream.h
#include <rmn/bitstream.h>

static int stream_debug_mode = 0 ;

// set stream debug mode
int StreamDebugSet(int value){
  int old_mode = stream_debug_mode ;
  stream_debug_mode = value ;
  return old_mode ;
}

// get stream debug mode
int StreamDebugGet(void){
  return stream_debug_mode ;
}
// =======================  stream initialization functions =======================
//
// bit stream (re)initializer, does not allocate the bitstream struct
// s    [OUT] : pointer to an existing bitstream structure (structure will be updated)
// mem   [IN] : pointer to user supplied memory (if NULL, use malloc to allocate memory for bit stream data)
// size  [IN] : size of the memory area (user supplied or auto allocated) (BYTES)
// mode  [IN] : combination of BIT_INSERT, BIT_XTRACT, BIT_FULL_INIT, SET_BIG_ENDIAN, SET_LITTLE_ENDIAN
// if mode == 0, both insertion and extraction operations are allowed
// size is in bytes
void  InitStream(bitstream *s, void *mem, size_t size, int mode){
  uint32_t *buf = (uint32_t *) mem ;

  if(s == NULL) return ;     // invalid stream pointer
  if( ! StreamIsValid(s) ){ mode |= BIT_FULL_INIT ; }  // this will catch some but not all uninitialized streams
  if(mode & BIT_FULL_INIT){
    *s = NULL_BITSTREAM ;    // perform a full (re)initialization, nullify all fields, making stream invalid
  }
  if((mode & (BIT_INSERT | BIT_XTRACT)) == 0) mode = mode | BIT_INSERT | BIT_XTRACT ;  // neither insert nor extract set, set both

  if(StreamIsValid(s)){
    buf = s->first ;        // existing and valid stream, already has a buffer, set buf to first, ignore mem
  }else{                    // not an existing stream, perform a full initialization
    if(buf == NULL){
      s->user   = 0 ;                          // not user supplied space
      s->alloc  = 1 ;                          // auto allocated space (can be freed if resizing or destroying)
      buf    = (uint32_t *) malloc(size) ;     // allocate space to accomodate up to size bytes
    }else{
      s->user   = 1 ;                          // user supplied space
      s->alloc  = 0 ;                          // not auto allocated space
    }
    s->full   = 0 ;                            // not single malloc for both bitstream struct and buffer
    s->spare  = 0 ;
    s->valid   = VALID_STREAM ;                // mark bit stream as valid
    s->first  = buf ;                          // stream storage buffer
    s->limit  = buf + size/sizeof(uint32_t) ;  // potential truncation to 32 bit alignment
  }
  if(buf == NULL){                             // something is very wrong
    *s = NULL_BITSTREAM ;                      // fail miserably, set to NULL bitstream
    return ;
  }

  s->in     = buf ;                            // stream is empty and insertion starts at beginning of buffer
  s->out    = buf ;                            // stream is filled and extraction starts at beginning of buffer
  s->acc_i  = 0 ;                              // insertion accumulator is empty
  s->acc_x  = 0 ;                              // extraction accumulator is empty
  s->insert = 0 ;                              // insertion point at first free bit
  s->xtract = 0 ;                              // extraction point at first available bit
  if((mode & BIT_XTRACT) == 0) s->xtract = -1 ;         // deactivate extract mode (insert only mode)
  if((mode & BIT_INSERT) == 0) s->insert = -1 ;         // deactivate insert mode  (extract only mode)
  if(mode & SET_BIG_ENDIAN   ) s->endian = STREAM_BE ;  // flag stream as Big Endian
  if(mode & SET_LITTLE_ENDIAN) s->endian = STREAM_LE ;  // flag stream as Little Endian
//   if(s->endian == 0) s->endian = STREAM_BE ;   //  default to Big Endian mode if not already defined ?

}

// create (allocate) a bitstream struct, and initialize it
// mem   [IN] : pointer to user supplied memory
//              if NULL, use a single malloc to allocate memory for bit stream data and struct
//              ithe bit stream data will be located in memory after the bitstream struct itself
// size  [IN] : size of the memory area (user supplied or auto allocated) (BYTES)
// mode  [IN] : combination of BIT_INSERT, BIT_XTRACT, BIT_FULL_INIT, SET_BIG_ENDIAN, SET_LITTLE_ENDIAN
// if mode == 0, both insertion and extraction operations are allowed
// size is in bytes
// return pointer to created bitstream
bitstream *CreateStream(void *mem, size_t size, int mode){
  char *data = (char *)mem ;
  size_t size_alloc = sizeof(bitstream) ;
  bitstream *s ;

  // if data buffer not supplied by caller, add size to size_alloc for bit stream data
  if(data == NULL) size_alloc += size ;
  s = (bitstream *) malloc(size_alloc) ;
  if(s == NULL) return NULL ;          // malloc failed

  if(data == NULL){                    // data buffer not supplied by caller
    data = (char *)s ;                 // bit stream data will be stored at address
    data = data + sizeof(bitstream) ;  // just after bitstream struct
  }
  s->valid = 0 ;                       // make sure no mishap happens, set stream as invalid
  InitStream(s, data, size, mode) ;    // perform initialization, supplying data as "user memory"
  s->full = 1 ;                        // single malloc for both bitstream struct and buffer
  return s ;
}

// generic bit stream destructor
// s      [IN] : pointer to an existing bitstream structure
// error [OUT] : error flag, 0 if successful, 1 otherwise
// in case of error, return NULL
// if successful, return address of stream if only the data buffer was freed
// return NULL if the whole struct was freed
bitstream *FreeStream(bitstream *s, int *error){

  if(StreamIsInvalid(s)){     // not a valid stream
    *error = 1 ;
    s = NULL ;
    if(stream_debug_mode) fprintf(stderr, "FreeStream : invalid stream\n");
    goto end ;
  }

  if(s->full){                // the whole bitstream struct was allocated with malloc()
    free(s) ;                 // free the whole rigamarole
    s = NULL ;
    *error = 0 ;              // O.K.
    if(stream_debug_mode) fprintf(stderr, "FreeStream : freed single malloc stream\n");
    goto end ;
  }

  if(s->alloc){
    free(s->first) ;          // the stream buffer was supplied by the user
    *error = 0 ;
    *s = NULL_BITSTREAM ;     // NULL stream
    if(stream_debug_mode) fprintf(stderr, "FreeStream : freed user supplied stream buffer\n");
    goto end ;
  }

end:
  return s ;
}

// =======================  stream utility functions  =======================

// flush any insertion data left in accumulator into stream
// s [IN] : pointer to a bit stream struct
// return 0 if O.K., non zero in case of error
// NOTE : replaced by macro ?
int StreamFlush(bitstream *s){

  if(! StreamIsValid(s))         return 1 ;              // invalid stream

  if(s->insert > 0) {                                    // flush contents of accumulator into buffer
    if(s->endian == STREAM_BE){                          // Big Endian flush
      *(s->in) = (s->acc_i >> 32) ; (s->in)++ ;
      if(s->insert > 32){ *(s->in) = s->acc_i ; (s->in)++ ; }
    }else{                                               // Little Endian flush
      s->acc_i >>= (64 - s->insert) ;                    // right align accumulator
      *(s->in) = s->acc_i ; (s->in)++ ;                  // lower 32 bits
      if(s->insert > 32){ *(s->in) = (s->acc_i >> 32) ; (s->in)++ ; }
    }
    s->insert = 0 ; s->acc_i = 0 ;
  }
  return 0 ;
}

// rewind a bit stream to read it from the beginning (potentially force valid read mode)
// s    [IN] : pointer to an existing bitstream structure
// return 0 if O.K., non zero if error
// NOTE : replaced by macro ?
int StreamRewind(bitstream *s, int force_read){
  if(! StreamIsValid(s)) return 1 ;           // invalid stream
  if(force_read) s->xtract = 0 ;
  if(s->xtract < 0) return 1 ;                // not in read mode, error
  if(s->insert > 0) StreamFlush(s) ;          // data left in insert accumulator ?
  if(s->xtract >= 0){
    s->acc_x  = 0 ;
    s->out = s->first ;
  }
  return 0 ;
}

// rewind a bit stream to rewrite it from the beginning (potentially force valid write mode)
// s    [IN] : pointer to an existing bitstream structure
// return 0 if O.K., non zero if error
// NOTE : replaced by macro ?
int StreamRewrite(bitstream *s, int force_write){
  if(! StreamIsValid(s)) return 1 ;           // invalid stream
  if(force_write) s->insert = 0 ;
  if(s->insert < 0) return 1 ;                // not in write mode, error
  if(s->insert > 0) StreamFlush(s) ;          // data left in insert accumulator ?
  s->acc_i  = 0 ;
  s->in = s->first ;
  if(s->xtract >= 0){                         // there is now no valid data to extract
    s->acc_x  = 0 ;                           // if read mode is allowed
    s->out = s->first ;
    s->xtract = 0 ;
  }
  return 0 ;
}

// reset both read and write pointers to beginning of stream (according to insert/xtract only flags)
// s    [IN] : pointer to an existing bitstream structure
// return 0 if O.K., non zero if error
int StreamReset(bitstream *s){
  if(! StreamIsValid(s))         return 1 ;              // invalid stream
  if(s->insert >= 0){      // insertion allowed
    s->in     = s->first ;
    s->acc_i  = 0 ;
    s->insert = 0 ;
  }
  if(s->xtract >= 0){      // extraction allowed
    s->out    = s->first ;
    s->acc_x  = 0 ;
    s->xtract = 0 ;
  }
  return 0 ;
}
// =======================  stream information  =======================

// is stream valid ?
// s [IN] : pointer to a bit stream struct
int StreamIsValid(bitstream *s){
  if(s == NULL)                                        return 0 ;    // bad pointer
  if(s->valid != VALID_STREAM)                         return 0 ;    // incorrect marker
  if(s->first == NULL)                                 return 0 ;    // no buffer
  if(s->limit == NULL)                                 return 0 ;    // invalid limit
  if(s->in == NULL || s->out == NULL)                  return 0 ;    // invalid in out pointers
  if(s->limit <= s->first)                             return 0 ;    // invalid first/limit combination
  if(s->in < s->first  || s->in > s->limit)            return 0 ;    // in is out of bounds
  if(s->out < s->first || s->out > s->limit)           return 0 ;    // out is out of bounds
  if(s->endian != STREAM_BE && s->endian != STREAM_LE) return 0 ;    // invalid endianness
  return 1 ;                                                         // probably a valid stream
}

int StreamIsInvalid(bitstream *s){
  return (1 - StreamIsValid(s)) ;
}

// s    [IN] : pointer to an existing bitstream structure
// return number of bits available for extraction
ssize_t StreamAvailableBits(bitstream *s){
  if(s->xtract < 0) return -1 ;                               // extraction is not allowed
  int32_t in_xtract = (s->xtract < 0) ? 0 : s->xtract ;       // bits in extract accumulator
  int32_t in_insert = (s->insert < 0) ? 0 : s->insert ;       // bits in insert accumulator
  return (s->in - s->out)*32 + in_insert + in_xtract ;        // stream + bits in accumulators
}

// s    [IN] : pointer to an existing bitstream structure
// return number of bits available for extraction
// in strict mode, bits in insert accumulator are ignored
ssize_t StreamStrictAvailableBits(bitstream *s){
  if(s->xtract < 0) return -1 ;                               // extraction is not allowed
  int32_t in_xtract = (s->xtract < 0) ? 0 : s->xtract ;       // bits in accumulator
  return (s->in - s->out)*32 + in_xtract ;                    // stream + extract accumulator contents
}

// s    [IN] : pointer to an existing bitstream structure
// return number of bits available for insertion
ssize_t StreamAvailableSpace(bitstream *s){
  if(s->insert < 0) return -1 ;   // insertion is not allowd
  return (s->limit - s->in)*32 - s->insert ;   // available space in stream buffer minus accumulator contents
}

// s    [IN] : pointer to an existing bitstream structure
// get stream mode as a string
char *StreamMode(bitstream s){
  if( STREAM_INSERT_MODE(s) && STREAM_XTRACT_MODE(s)) return("RW") ;
  if( STREAM_XTRACT_MODE(s) ) return "R" ;
  if( STREAM_INSERT_MODE(s) ) return "W" ;
  return("Unknown") ;
}

// s    [IN] : pointer to an existing bitstream structure
// get stream mode as a code
int StreamModeCode(bitstream s){
  int32_t mode = 0 ;
  if( STREAM_XTRACT_MODE(s) ) mode |= BIT_XTRACT ;
  if( STREAM_INSERT_MODE(s) ) mode |= BIT_INSERT ;
  return mode ? mode : -1 ;                               // return -1 if neither extract nor insert is set
}

// =======================  stream data copy  =======================
//
// copy stream data into array mem (from beginning up to in pointer and data in accumulator if any)
// the original stream control info remains untouched (up to 2 32 bit items may get added to its buffer)
// stream [IN] : pointer to bit stream struct
// mem   [OUT] : where to copy
// size   [IN] : size of mem array in bytes
// return original size of valid info from stream in bits (-1 in case of error)
ssize_t StreamDataCopy(bitstream *s, void *mem, size_t size){
  size_t nbtot, nborig ;
  bitstream temp ;    // temporary struct used during the copy process

  if(StreamIsInvalid(s) || mem == NULL) return -1 ;        // invalid stream or invalid memory address
  temp = *s ;                                              // copy stream struct to avoid altering original

  // precise number of used bits in stream buffer
  nborig = (temp.in - temp.first) * 8 * sizeof(uint32_t) + ((temp.insert > 0) ? temp.insert : 0) ;
  StreamFlush(&temp);                                      // flush contents of accumulator into buffer
  nbtot = (temp.in - temp.first) * sizeof(uint32_t) ;      // size in bytes when nothing is left in acumulator
  if(nbtot == 0) return 0 ;                                // there was no data in stream
  if(nbtot > size) return -1 ;                             // insufficient space
  if(mem != memmove(mem, temp.first, nbtot)) return -1 ;   // error copying
  if(stream_debug_mode) fprintf(stderr, "StreamDataCopy : nborig = %ld bits, nbtot = %ld bits\n", nborig, nbtot*8) ;
  return nborig ;                                          // return unrounded result
}

// =======================       diagnostic functions       =======================
// =======================  print stream data and metadata  =======================
// print some elements at the beginning and at the end of the bit stream data buffer
// (see bi_endian_pack.h)
// s    [IN] : bitstream structure
// msg  [IN] : user message
// edge [IN] : do not print data elements more that edge positions from first or in
void StreamPrintData(bitstream s, char *msg, int edge){
  uint32_t *in = s.in ;
  uint32_t *first = s.first ;
  uint32_t *cur, *start ;
  int inc, count = 0 ;

  fprintf(stderr, "[%2s] %s : ", STREAM_IS_LITTLE_ENDIAN(s) ? "LE" : "BE", msg) ;
  fprintf(stderr, "accum = %16.16lx", s.acc_i << (64 - s.insert)) ;
  fprintf(stderr, ", guard = %8.8x, data =", *in) ;

  if(STREAM_IS_LITTLE_ENDIAN(s)){
    cur = in ; inc = -1 ;
  }else{                            // big endian or not specified
    cur = first ; inc = +1 ;
  }

  for(start=first ; start <= in ; start++, cur = cur + inc){
    if(in - cur == 0 && s.insert == 0) continue ;          // last element not used
    if(cur-first < edge || in-cur <= edge) {
      fprintf(stderr, " %8.8x ", *cur) ;
    }else{
      count++ ;
      if((count & 0xFF) == 1) fprintf(stderr, ".") ;
    }
  }
  fprintf(stderr, "\n") ;
}

// print bit stream control information
// s             [IN] : bitstream structure
// msg           [IN] : user message
// expected_mode [IN] : "R", "W", or "RW"  read/write/read-write, expected mode for bit stream
void StreamPrintParams(bitstream s, char *msg, char *expected_mode){
  int32_t available        = StreamAvailableBits(&s) ;
  int32_t strict_available = StreamStrictAvailableBits(&s) ;
  int32_t space_available   = StreamAvailableSpace(&s) ;

  available = (available < 0) ? 0 : available ;
  strict_available = (strict_available < 0) ? 0 : strict_available ;
  fprintf(stderr, "%s: filled = %d(%d), free= %d, first/in/out/limit [in - out] = %p/%ld/%ld/%ld [%ld], insert/xtract = %d/%d",
    msg, available, strict_available, space_available, 
    (void *)s.first, s.in-s.first, s.out-s.first, s.limit-s.first, s.in-s.out, s.insert, s.xtract ) ;
  fprintf(stderr, ", full/alloc/user = %d/%d/%d", s.full, s.alloc, s.user) ;
  fprintf(stderr, ", |%8.8x|%2.2x|", s.valid, s.endian) ;
  fprintf(stderr, ", Mode = %s(%d)", StreamMode(s), StreamModeCode(s)) ;
  if(expected_mode){
    fprintf(stderr, " (%s expected)", expected_mode) ;
    if(strcmp(StreamMode(s), expected_mode) != 0) { 
      fprintf(stderr, "\nBad mode, exiting\n") ;
      exit(1) ;
    }
  }
  fprintf(stderr, "\n") ;
}
//
// =======================  stream state save/restore  =======================
//
// save the current bit stream state in a bitstream_state structure
// stream  [IN] : pointer to a bit stream struct
// state  [OUT] : pointer to a bit stream state struct
int StreamSave(bitstream *stream, bitstream_state *state){
  if(! StreamIsValid(stream)) goto error ;
  state->acc_i  = stream->acc_i ;
  state->acc_x  = stream->acc_x ;
  state->first  = stream->first ;
  state->in     = stream->in - stream->first ;
  state->out    = stream->out - stream->first ;
  state->insert = stream->insert ;
  state->xtract = stream->xtract ;
  return 0 ;

error:
  state->in     = -1 ;     // make sure returned state is invalid
  state->out    = -1 ;
  state->insert = -1 ;
  state->xtract = -1 ;
  state->first  = NULL ;
fprintf(stderr, "error in save state\n");
  return 1 ;
}

// restore a bit stream state from the state saved with StreamSaveState
// stream [OUT] : pointer to a bit stream struct
// state   [IN] : pointer to a bit stream state struct
// mode    [IN} : 0, BIT_XTRACT, BIT_INSERT (which mode state(s) to restore)
//                0 restore BOTH insert and extract states
int StreamRestore(bitstream *stream, bitstream_state *state, int mode_in){
  char *msg ;
  int mode = mode_in ;
  bitstream s ;
  if(mode == 0) mode = BIT_XTRACT | BIT_INSERT ;

  msg = "invalid stream" ;
  if(! StreamIsValid(stream)) goto error ;
  msg = "state does not belong to this stream" ;
  if(state->first != stream->first) goto error ;
  msg = "extraction state not available" ;
  if((mode_in & BIT_XTRACT) && (state->xtract < 0)) goto error ;
  msg = "insertion state not available" ;
  if((mode_in & BIT_INSERT) && (state->insert < 0)) goto error ;

  s = *stream ;                                             // local copy of stream
  if((mode & BIT_XTRACT) && (state->xtract >= 0)){                    // restore extract state (if available)
    s.xtract = state->xtract ;
    s.out = stream->first + state->out ;                              // restore extraction pointer
    s.acc_x  = state->acc_x ;                                         // restore accumulator and bit count
  }
  if((mode & BIT_INSERT) && (state->insert >= 0)){                    // restore insert state (if available)
    s.insert = state->insert ;
    s.in = stream->first + state->in ;                                // restore insertion pointer
    s.acc_i  = state->acc_i ;                                         // restore accumulator and bit count
  }
  msg = "extraction pointer beyond limit" ;
  if(s.out > s.limit) goto error ;
  msg = "insertion pointer beyond limit" ;
  if(s.in > s.first) goto error ;
  msg = "extraction pointer beyond insertion pointer" ;
  if(s.out > s.in) goto error ;

  *stream = s ;
  return 0 ;                                                          // no error
error:
  if(stream_debug_mode > 1) fprintf(stderr, "error (%s) in restore state\n", msg);
  return 1 ;
}

// =======================  stream size management =======================
//
// resize a stream
// s    [OUT] : pointer to a bitstream structure
// mem   [IN] : pointer to user memory (if NULL, malloc will be used)
// size  [IN] : size of the memory area (or desired size if mem == NULL)
// funtion return : pointer to resized stream
// N.B. size MUST be larger than the original size or operation will be a NO-OP
//      the contents of the old buffer will be copied to the new buffer if necessary
//      size is in bytes
bitstream *StreamResize(bitstream *s, void *mem, size_t size){
  uint32_t in, out ;
  size_t old_size ;
  int auto_alloc = (mem == NULL) ;           // 1 if no user supplied memory

  if(StreamIsInvalid(s)) return NULL ;                                    // invalid stream
  old_size = sizeof(int32_t) * (s->limit - s->first) ;                    // size of current buffer
  if(size <= old_size) return s ;                                         // new buffer size is <= size of current buffer, resize is not needed
  old_size = sizeof(int32_t) * (s->in - s->first) ;                       // used size in buffer for later copy
  if(stream_debug_mode) fprintf(stderr, "StreamResize : resizing");
  if(s->full == 1){                                                       // the whole struct was malloc(ed)
  if(stream_debug_mode) fprintf(stderr, ", whole struct was malloc(ed)");
    bitstream *snew = CreateStream(mem, size, 0) ;
  if(stream_debug_mode) fprintf(stderr, ", new stream created");
    if(snew == NULL) return NULL ;                                        // allocation failed
    if(auto_alloc){                                                       // copy old buffer into new
// memset(snew->first, 0xFF, sizeof(int32_t) * (s->limit - s->first)) ;
      if(old_size > 0) memmove(snew->first, s->first, old_size) ;
      if(stream_debug_mode) fprintf(stderr, ", copying %ld bytes", old_size) ;
    }
    // first and limit are already set
    snew->in     = snew->first + (s->in - s->first) ;                     // preserve relative positions of in and out
    snew->out    = snew->first + (s->out - s->first) ;
    snew->acc_i  = s->acc_i ;                                             // preserve accumulators and counts
    snew->insert = s->insert ;
    snew->acc_x  = s->acc_x ;
    snew->xtract = s->xtract ;
    // other flags already set
    snew->endian = s->endian ;
    free(s) ;                                                             // free old bitstream struct
    if(stream_debug_mode) fprintf(stderr, ", freed old struct");
    s = snew ;

  }else{

    if(mem == NULL){                                                      // allocate with malloc if mem is NULL
      if(stream_debug_mode) fprintf(stderr, ", allocating new buffer (%ld)", size);
      mem = malloc(size) ;
    }
    if(mem == NULL) return NULL ;                                         // failed to allocate memory
// memset(mem, 0xFF, size) ;
    memmove(mem, s->first, old_size)  ;                                   // copy old (s->first) buffer into new (mem)
    if(stream_debug_mode) fprintf(stderr, ", copying %ld bytes", old_size) ;
    in  = s->in - s->first ;                                              // relative position of in pointer
    out = s->out - s->first ;                                             // relative position of out pointer
    if(s->alloc){                                                         // previous buffer was "malloced", free it
      if(stream_debug_mode) fprintf(stderr, ", freeing old malloc(ed) buffer");
      free(s->first) ;
    }
    s->alloc = (auto_alloc) ? 1 : 0 ;                                     // flag buffer as "malloced" if mem was NULL at entry
    s->first = (uint32_t *) mem ;                                         // updated first pointer
    s->in    = s->first + in ;                                            // updated in pointer
    s->out   = s->first + out ;                                           // updated out pointer
    s->limit = s->first + size / sizeof(int32_t) ;                        // updated limit pointer
  }
  if(stream_debug_mode) fprintf(stderr, "\n");
  return s ;
}
// this function will be useful to make an already filled stream ready for extraction
// stream  [IN] : pointer to a bit stream struct
// pos     [IN] : number of valid bits for extraction from stream buffer
// return 0 if O.K., non zero in case of error
int StreamSetFilledBits(bitstream *stream, size_t pos){
  if(StreamIsInvalid(stream)) return 1 ;                    // invalid stream
  pos = (pos + 7) / 8 ;                                     // round up as bytes
  pos = (pos + sizeof(uint32_t) - 1) / sizeof(uint32_t) ;   // round up as 32 bit words
  size_t size = (stream->limit - stream->first) ;
  if(pos > size) return 1 ;                                 // check for potential buffer overrrun
  stream->in = stream->first + pos ;                        // mark stream as filled up to stream->in
  return 0 ;
}

// mark stream as filled with size bytes
// size [IN] : number of bytes available for extraction (should be a multiple of 4)
int  StreamSetFilledBytes(bitstream *s, size_t size){
  return StreamSetFilledBits(s, size * 8) ;
}

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
// set of macros and functions to manage a bit stream
// N.B. this bitstream is a sequence of 32 bit unsigned integers

#if !defined(VALID_STREAM)

// validity marker
#define VALID_STREAM 0xCAFEFADEu
// stream insert/extract mode (0 or 3 would mean both insert and extract)
// extract mode
#define BIT_XTRACT        1
// insert mode
#define BIT_INSERT        2
// full initialization mode
#define BIT_FULL_INIT     8
// set endianness
#define SET_BIG_ENDIAN      16
#define SET_LITTLE_ENDIAN   32

// stream pack/unpack options
// default is init and finalize for pack and unpack
#define UNPACK_INIT     0x00000001
#define UNPACK_FINALIZE 0x00000002
#define PACK_INIT       0x00000004
#define PACK_FINALIZE   0x00000008
#define PACK_ALIGN32    0x00000010
#define UNPACK_ALIGN32  0x00000020

// endianness codes, MUST MATCH PACK_ENDIAN macro (le_stream.h/be_stream.h)
// #define STREAM_BE 0xBE
#define STREAM_BE 'B'
// #define STREAM_LE 0xEB
#define STREAM_LE 'L'
// endianness information from stream
#define STREAM_ENDIANNESS(s) (s).endian
#define STREAM_IS_BIG_ENDIAN(s) ( (s).endian == STREAM_BE )
#define STREAM_IS_LITTLE_ENDIAN(s) ( (s).endian == STREAM_LE )

// true if stream is in read (extract) mode
#define STREAM_XTRACT_MODE(s) ((s).xtract >= 0)
// true if stream is in write (insert) mode
#define STREAM_INSERT_MODE(s) ((s).insert >= 0)

// size of stream data buffer (in bytes)
#define STREAM_BUFFER_BYTES(s) ( ((s).limit - (s).first) * sizeof(uint32_t) )

// address of stream data buffer
#define STREAM_BUFFER_ADDRESS(s) (s).first

// address of stream insertion pointer
#define STREAM_IN(s) (s).in

// address of stream extraction pointer
#define STREAM_OUT(s) (s).out

// bits used in accumulator (usable if insertion mode is active)
#define STREAM_ACCUM_BITS_USED(s) ((s).insert)
// bits available in accumulator (usable if extraction mode is active)
#define STREAM_ACCUM_BITS_AVAIL(s) ((s).xtract)

// stream bits in stream, not accounting for already extracted bits (usable if insertion mode is active)
#define STREAM_BITS_STORED(s) ( ((s).insert >= 0) ? ( ((s).in - (s).first) * 32l + (s).insert ) : 0 )
// bits available in stream (usable if extraction mode is active)
#define STREAM_BITS_AVAIL(s) ( ((s).xtract >= 0) ? ( (s).xtract + ((s).in - (s).out) * 32l ) : 0 )
// bits left to fill in stream (usable if insertion mode is active)
#define STREAM_BITS_EMPTY(s) ( ((s).insert >= 0) ? ( ((s).limit - (s).in) * 32l - (s).insert ) : 0 )

// bits available in stream (usable if extraction mode is active)

#include <stdint.h>
#include <stdlib.h>

// compile time assert macros
#include <rmn/ct_assert.h>

// bit stream descriptor.
// in insertion only mode, xtract MUST be -1
// in extraction only mode, insert MUST be -1
// both insert and extract are non negative if stream is used in both modes
// in most cases, a bit stream will be unidirectional (either insert or extract mode)
typedef struct{
  uint32_t valid ;    // signature marker
  uint32_t full:  1 , // the whole struct was allocated with malloc
           alloc: 1 , // buffer was allocated with malloc
           user:  1 , // buffer was user supplied
           spare:21 , // spare bits
           endian:8 ; // 0xBE : Big Endian stream, 0xEB : Little Endian stream, other value : invalid
  uint32_t *first ;   // pointer to start of stream data storage
  uint32_t *limit ;   // pointer to end of stream data storage (1 byte beyond stream buffer end)
  uint64_t  acc_i ;   // 64 bit unsigned bit accumulator for insertion
  uint32_t *in ;      // pointer into packed stream (insert mode)
  int32_t   insert ;  // number of bits used in accumulator (insert <= 64)
  int32_t   xtract ;  // number of bits extractable from accumulator (xtract <= 64)
  uint32_t *out ;     // pointer into packed stream (extract mode)
  uint64_t  acc_x ;   // 64 bit unsigned bit accumulator for extraction
} bitstream ;
CT_ASSERT_(sizeof(bitstream) == 64)    // 8 64 bit elements
//
// bit stream state for save/restore operations
typedef struct{
  uint64_t  acc_i ;   // 64 bit unsigned bit accumulator for insertion
  uint64_t  acc_x ;   // 64 bit unsigned bit accumulator for extraction
  uint32_t *first ;   // pointer to start of stream data storage (used for consistency check)
  int32_t   in ;      // insertion offset (-1 if invalid) (bitstream.in - first)
  int32_t   out ;     // extraction offset (-1 if invalid) (bitstream.out - first)
  int32_t   insert ;  // # of bits used in accumulator (-1 <= insert <= 64) (-1 if invalid)
  int32_t   xtract ;  // # of bits extractable from accumulator (-1 <= xtract <= 64) (-1 if invalid)
} bitstream_state ;
CT_ASSERT_(sizeof(bitstream_state) == 40)

int StreamDebugSet(int value);
int StreamDebugGet(void);

int StreamIsValid(bitstream *s);
int StreamIsInvalid(bitstream *s);

bitstream *CreateStream(void *mem, size_t size, int mode);
void  InitStream(bitstream *s, void *mem, size_t size, int mode);
bitstream *FreeStream(bitstream *s, int *error);

int StreamSave(bitstream *stream, bitstream_state *state);
int StreamRestore(bitstream *stream, bitstream_state *state, int mode_in);

ssize_t StreamAvailableBits(bitstream *s);
ssize_t StreamStrictAvailableBits(bitstream *s);
ssize_t StreamAvailableSpace(bitstream *s);

bitstream *StreamResize(bitstream *s, void *mem, size_t size);
int StreamSetFilledBits(bitstream *stream, size_t pos);
int StreamSetFilledBytes(bitstream *s, size_t size);

char *StreamMode(bitstream s);
int StreamModeCode(bitstream s);

int StreamReset(bitstream *s);
int StreamRewind(bitstream *s, int force_read);
int StreamRewrite(bitstream *s, int force_write);

ssize_t StreamDataCopy(bitstream *s, void *mem, size_t size);
int StreamFlush(bitstream *s);

void StreamPrintData(bitstream s, char *msg, int edge);
void StreamPrintParams(bitstream s, char *msg, char *expected_mode);

uint32_t stream_pack_u32(bitstream *s, void *in, int nbits, int n, uint32_t options);
uint32_t stream_unpack_u32(bitstream *s, void *in, int nbits, int n, uint32_t options);

uint32_t stream_pack_i32(bitstream *s, void *in, int nbits, int n, uint32_t options);
uint32_t stream_unpack_i32(bitstream *s, void *in, int nbits, int n, uint32_t options);

#endif    // defined(VALID_STREAM)

#undef NULL_BITSTREAM
#undef SET_STREAM_ENDIANNESS
#undef SET_NULL_BITSTREAM

#if defined(PACK_ENDIAN)
#define SET_STREAM_ENDIANNESS(s) (s).endian = PACK_ENDIAN ;
// all fields set to 0, makes for a fast initialization with xxx = NULL_BITSTREAM
#define NULL_BITSTREAM (bitstream) { .acc_i = 0, .acc_x = 0 , .insert = 0 , .xtract = 0, \
                                     .first = NULL, .in = NULL, .out = NULL, .limit = NULL, .full = 0, \
                                     .alloc = 0, .user = 0, .endian = PACK_ENDIAN, .spare = 0, .valid = 0 } ;
#else
#define NULL_BITSTREAM (bitstream) { .acc_i = 0, .acc_x = 0 , .insert = 0 , .xtract = 0, \
                                     .first = NULL, .in = NULL, .out = NULL, .limit = NULL, .full = 0, \
                                     .alloc = 0, .user = 0, .endian = 0, .spare = 0, .valid = 0 } ;
#endif

#define SET_NULL_BITSTREAM(s) (s) = NULL_BITSTREAM

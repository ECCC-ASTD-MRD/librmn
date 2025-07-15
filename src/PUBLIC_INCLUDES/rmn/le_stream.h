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
// set of macros and functions to manage insertion/extraction into/from a bit stream
// these macros work with the bit stream Little Endian style
// from the Least significant bits towards the Most significant bits
//
#include <stdint.h>

// undefine everything in case of multiple inclusion

#undef PACK_ENDIAN
#define PACK_ENDIAN 'L'

// bit stream macros and functions
#include <rmn/bitstream.h>

// ================================ bit insertion/extraction macros into/from bitstream ===============================
// macro arguments description
// accum  [INOUT] : 64 bit accumulator (normally acc_i or acc_x)
// insert [INOUT] : # of bits already inserted in accumulator (0 <= insert <= 64)
// xtract [INOUT] : # of bits available for extraction from accumulator (0 <= xtract <= 64)
// w32    [IN]    : 32 bit integer containing data to be inserted (expression allowed)
//        [OUT]   : 32 bit integer receiving extracted data (MUST be a variable)
// nbits  [IN]    : number of bits to insert / extract in w32 (> 0 , <= 32 bits)
//
// N.B. : if w32 is a "signed" variable, extraction will produce a "signed" result
// the STREAM macros use implicit accum/xtract/xtract/stream arguments
//
// ===============================================================================================
// little endian (LE) style (right to left) bit stream packing
// insert token into top (most significant part) of accumulator after accumulator has been shifted right
// extract token from the bottom (least significant part) of accumulator then shift accumulator right
// accumulator SHOULD be zeroed before starting to insert
// ===============================================================================================

// insert the lower nbits (<=32) bits from w32 into accumulator
// this macro is unsafe, it assumes that nbits bits can be safely inserted into acumulator
#undef FAST_PUT_NBITS
#define FAST_PUT_NBITS(accum, insert, w32, nbits) \
        { uint64_t t=(uint32_t)(w32) ; t<<=(64-(nbits)) ; accum=(uint64_t)accum>>(nbits) ; accum|=t ; insert+=(nbits) ; }

// insert 1 into accumulator
// this macro is unsafe, it assumes that 1 bit can be safely inserted into acumulator
#undef FAST_PUT_1
#define FAST_PUT_1(accum, insert) { uint64_t t=1u ; t<<=63 ; accum=(uint64_t)accum>>1 ; accum|=t ; insert++ ; }

// insert 0 into accumulator
// this macro is unsafe, it assumes that 1 bit can be safely inserted into acumulator
#undef FAST_PUT_0
#define FAST_PUT_0(accum, insert) { accum=(uint64_t)accum>>1 ; insert++ ; }

// insert a stream of 0s into accumulator (equivalent of SKIP_NBITS in insert mode)
// this macro is unsafe, it assumes that nbits (<64) bits can be safely inserted into acumulator
#undef FAST_PUT_PAD0
#define FAST_PUT_PAD0(accum, insert, nbits) { accum=(uint64_t)accum>>1 ; insert+=nbits ; }

// check that up to 32 bits can be safely inserted into accumulator
// if not possible, store lower 32 useful bits of accum into stream, update accum, insert, stream pointer
#undef INSERT_CHECK
#define INSERT_CHECK(accum, insert, streamptr) \
        { if(insert > 32) { *(streamptr)=(((uint64_t)accum)>>(64-insert)) ; (streamptr)++ ; insert -= 32 ; } ; }

// push pending insertion data into stream without fully updating control info (stream pointer, insert)
#undef INSERT_PUSH
#define INSERT_PUSH(accum, insert, streamptr) \
        { INSERT_CHECK(accum, insert, streamptr) ; { if(insert > 0) { *(streamptr)=(((uint64_t)accum)>>(64-insert)) ; } ; } }

// store any residual data from accum into stream, update insert, stream pointer, accumulator
#undef INSERT_FINALIZE
#define INSERT_FINALIZE(accum, insert, streamptr) \
        { INSERT_CHECK(accum, insert, streamptr) ; { if(insert > 0) { *(streamptr) = ((uint64_t)accum>>(64-insert)) ; (streamptr)++ ; insert = 0; accum = 0 ; } ; } }

// alignment calls should be preceded or followed with INSERT_CHECK
// align insertion point to a 32 bit boundary (accum MUST BE UPDATED) (an appropriate number of 0 bits will be inserted)
// unsafe, assumes that 32 bits can be safely inserted
#undef INSERT_ALIGN32
#define INSERT_ALIGN32(accum, insert) if(insert != 0){ int tbits = 64 - insert ;  tbits &= 31 ; insert += tbits ; accum >>= tbits ; }

// align insertion point to a 16 bit boundary (accum MUST BE UPDATED) (an appropriate number of 0 bits will be inserted)
// unsafe, assumes that 16 bits can be safely inserted
#undef INSERT_ALIGN16
#define INSERT_ALIGN16(accum, insert) if(insert != 0){ int tbits = 64 - insert ;  tbits &= 15 ; insert += tbits ; accum >>= tbits ; }

// align insertion point to a 8 bit boundary (accum MUST BE UPDATED) (an appropriate number of 0 bits will be inserted)
// unsafe, assumes that 8 bits can be safely inserted
#undef INSERT_ALIGN8
#define INSERT_ALIGN8(accum, insert) if(insert != 0){ int tbits = 64 - insert ;  tbits &= 7 ; insert += tbits ; accum >>= tbits ; }

// ===============================================================================================
// if w32 is a "signed" variable, extraction will produce a "signed" result
//
// initialize stream for extraction, load first 32 bits from stream into accum, set available bits count to 32
// extraction from the bottom (least significant) part of accumulator
#undef XTRACT_BEGIN
#define XTRACT_BEGIN(accum, xtract, streamptr) { accum = (uint32_t) *(streamptr) ; (streamptr)++ ; xtract = 32 ; }

// take a peek at the next nbits (<=32) bits from accum into w32 (unsafe, assumes that nbits bits are available)
#undef FAST_PEEK_NBITS
#define FAST_PEEK_NBITS(accum, xtract, w32, nbits) { w32 = accum ; w32 = ( w32 << (32-(nbits)) ) >> (32-(nbits)) ; }

// take a peek at the next bit from accum into w32 (unsafe, assumes that 1 bit is available)
#undef FAST_PEEK_1
#define FAST_PEEK_1(accum, xtract, w32) { w32 = accum & 1 ; }

// skip the next nbits (<=32) bits from accum (unsafe, assumes that nbits bits are available)
#undef FAST_SKIP_NBITS
#define FAST_SKIP_NBITS(accum, xtract, nbits) { accum = (uint64_t) accum >> (nbits) ; xtract -= (nbits) ; }

// skip the next bit from accumulator (unsafe, assumes that 1 bit is available)
#undef FAST_SKIP_1
#define FAST_SKIP_1(accum, xtract) { accum = (uint64_t) accum >> 1 ; xtract-- ; }

// check that 32 bits can be safely extracted from accum (accum contains at least 32 available bits)
// if not possible, get extra 32 bits into accum from stream, update accum, xtract, stream pointer
#undef XTRACT_CHECK
#define XTRACT_CHECK(accum, xtract, streamptr) \
        { if(xtract < 32) { uint64_t t = (uint32_t)(*(streamptr)) ; accum |= (t << xtract) ; (streamptr)++ ; xtract += 32 ; } ; }

// finalize extraction, update accum, xtract
#undef XTRACT_FINAL
#define XTRACT_FINAL(accum, xtract) { accum = 0 ; xtract = 0 ; }

// align extraction point to a 32 bit boundary (unsafe, assumes that 32 bits can be skipped)
#undef XTRACT_ALIGN32
#define XTRACT_ALIGN32(accum, xtract) { uint32_t tbits = xtract ; tbits &= 31 ; accum = (uint64_t) accum >> tbits ; xtract -= tbits ; }

// align extraction point to a 16 bit boundary (unsafe, assumes that 16 bits can be skipped)
#undef XTRACT_ALIGN16
#define XTRACT_ALIGN16(accum, xtract) { uint32_t tbits = xtract ; tbits &= 15 ; accum = (uint64_t) accum >> tbits ; xtract -= tbits ; }

// align extraction point to a 8 bit boundary (unsafe, assumes that 8 bits can be skipped)
#undef XTRACT_ALIGN8
#define XTRACT_ALIGN8(accum, xtract) { uint32_t tbits = xtract ; tbits &= 7 ; accum = (uint64_t) accum >> tbits ; xtract -= tbits ; }

// ===============================================================================================
// Little Endian style token concatenation
// put token2 (ltoken2 bits) to the left of token1 (ltoken1 bits)  ltoken1 will be updated (right to left)
#undef CONCAT_TOKENS
#define CONCAT_TOKENS(token1, ltoken1, token2, ltoken2) { token1 = token1 | (token2 << ltoken1) ; ltoken1 += ltoken2 ; }
// ===============================================================================================
// include macros common to Big and Little Endian mode
#include <rmn/common_stream.h>

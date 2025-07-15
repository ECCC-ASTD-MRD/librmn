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
// undefine macros from le_stream.h or be_stream.h
//
#if defined(PACK_ENDIAN)

#undef PACK_ENDIAN
#undef FAST_PUT_NBITS
#undef FAST_PUT_1
#undef FAST_PUT_0
#undef FAST_PUT_PAD0
#undef INSERT_CHECK
#undef INSERT_PUSH
#undef INSERT_FINALIZE
#undef INSERT_ALIGN32
#undef INSERT_ALIGN16
#undef INSERT_ALIGN8
#undef XTRACT_BEGIN
#undef FAST_PEEK_NBITS
#undef FAST_PEEK_1
#undef FAST_SKIP_NBITS
#undef FAST_SKIP_1
#undef XTRACT_CHECK
#undef XTRACT_FINAL
#undef XTRACT_ALIGN32
#undef XTRACT_ALIGN16
#undef XTRACT_ALIGN8
#undef CONCAT_TOKENS

#endif
//
// undefine macros from common_stream.h
//
#if defined(INSERT_BEGIN)

#undef INSERT_BEGIN
#undef PUT_NBITS
#undef PUT_1
#undef PUT_0
#undef FAST_GET_NBITS
#undef FAST_GET_1
#undef GET_NBITS
#undef GET_1
#undef STREAM_INSERT_BEGIN
#undef STREAM_FAST_PUT_NBITS
#undef STREAM_FAST_PUT_1
#undef STREAM_FAST_PUT_0
#undef STREAM_FAST_PUT_PAD0
#undef STREAM_INSERT_CHECK
#undef STREAM_INSERT_PUSH
#undef STREAM_INSERT_FINALIZE
#undef STREAM_PUT_NBITS
#undef STREAM_PUT_1
#undef STREAM_PUT_0
#undef STREAM_INSERT_ALIGN32
#undef STREAM_INSERT_ALIGN16
#undef STREAM_INSERT_ALIGN8
#undef STREAM_XTRACT_BEGIN
#undef STREAM_PEEK_NBITS
#undef STREAM_FAST_PEEK_NBITS
#undef STREAM_PEEK_1
#undef STREAM_FAST_PEEK_1
#undef STREAM_SKIP_NBITS
#undef STREAM_FAST_SKIP_NBITS
#undef STREAM_SKIP_1
#undef STREAM_FAST_SKIP_1
#undef STREAM_XTRACT_NBITS
#undef STREAM_XTRACT_CHECK
#undef STREAM_XTRACT_FINAL
#undef STREAM_GET_NBITS
#undef STREAM_GET_1
#undef STREAM_XTRACT_ALIGN32
#undef STREAM_XTRACT_ALIGN16
#undef STREAM_XTRACT_ALIGN8
#undef STREAM_REWIND
#undef STREAM_REWRITE
#undef STREAM_FLUSH
#undef STREAM_INIT
#undef STREAM_CREATE
#undef STREAM_FREE

#endif

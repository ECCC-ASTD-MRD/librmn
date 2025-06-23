// BSD Zero Clause License
//
// Copyright (c) 2025 Environnement Canada
//
// Permission to use, copy, modify, and/or distribute this software
// for any purpose with or without fee is hereby granted.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE
// INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
// IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT,
// OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
// DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
// ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
//

#if ! defined(CONCATENATE)

#include <rmn/va_args_num.h>

#define CONCATENATE_(X,Y)  X##Y
#define CONCATENATE(X,Y) CONCATENATE_(X,Y)
#define CAT_0(...)

#define CAT_1(X)          X
#define CAT_2(X,  Y, ...) CONCATENATE_(X, Y)
#define CAT_3(X,  Y ,...) CAT_2( X##Y , CAT_1(__VA_ARGS__) )
#define CAT_4(X,  Y ,...) CAT_2( X##Y , CAT_2(__VA_ARGS__) )
#define CAT_5(X,  Y ,...) CAT_2( X##Y , CAT_3(__VA_ARGS__) )
#define CAT_6(X,  Y ,...) CAT_2( X##Y , CAT_4(__VA_ARGS__) )
#define CAT_7(X,  Y ,...) CAT_2( X##Y , CAT_5(__VA_ARGS__) )
#define CAT_8(X,  Y ,...) CAT_2( X##Y , CAT_6(__VA_ARGS__) )
#define CAT_9(X,  Y ,...) CAT_2( X##Y , CAT_7(__VA_ARGS__) )
#define CAT_10(X, Y ,...) CAT_2( X##Y , CAT_8(__VA_ARGS__) )

#define CAT_11(X, Y ,...) CAT_2( X##Y , CAT_9(__VA_ARGS__)  )
#define CAT_12(X, Y ,...) CAT_2( X##Y , CAT_10(__VA_ARGS__) )
#define CAT_13(X, Y ,...) CAT_2( X##Y , CAT_11(__VA_ARGS__) )
#define CAT_14(X, Y ,...) CAT_2( X##Y , CAT_12(__VA_ARGS__) )
#define CAT_15(X, Y ,...) CAT_2( X##Y , CAT_13(__VA_ARGS__) )
#define CAT_16(X, Y ,...) CAT_2( X##Y , CAT_14(__VA_ARGS__) )
#define CAT_17(X, Y ,...) CAT_2( X##Y , CAT_15(__VA_ARGS__) )
#define CAT_18(X, Y ,...) CAT_2( X##Y , CAT_16(__VA_ARGS__) )
#define CAT_19(X, Y ,...) CAT_2( X##Y , CAT_17(__VA_ARGS__) )
#define CAT_20(X, Y ,...) CAT_2( X##Y , CAT_18(__VA_ARGS__) )

#define CAT_21(X, Y ,...) CAT_2( X##Y , CAT_19(__VA_ARGS__) )
#define CAT_22(X, Y ,...) CAT_2( X##Y , CAT_20(__VA_ARGS__) )
#define CAT_23(X, Y ,...) CAT_2( X##Y , CAT_21(__VA_ARGS__) )
#define CAT_24(X, Y ,...) CAT_2( X##Y , CAT_22(__VA_ARGS__) )
#define CAT_25(X, Y ,...) CAT_2( X##Y , CAT_23(__VA_ARGS__) )
#define CAT_26(X, Y ,...) CAT_2( X##Y , CAT_24(__VA_ARGS__) )
#define CAT_27(X, Y ,...) CAT_2( X##Y , CAT_25(__VA_ARGS__) )
#define CAT_28(X, Y ,...) CAT_2( X##Y , CAT_26(__VA_ARGS__) )
#define CAT_29(X, Y ,...) CAT_2( X##Y , CAT_27(__VA_ARGS__) )
#define CAT_30(X, Y ,...) CAT_2( X##Y , CAT_28(__VA_ARGS__) )

// CAT(a,b,c,d) expands to abcd  (up to 30 arguments)
// CAT( CAT(a,b) , CAT(c,d) ) also expands to abcd (recursive use)
#define CAT__(NARGS, ...) CAT_##NARGS(__VA_ARGS__)
#define CAT_(NARGS, ...)  CAT__(NARGS, __VA_ARGS__)
#define CAT(...)          CAT_(VA_ARGS_NUM(__VA_ARGS__), __VA_ARGS__)

// #define CONCAT(A,B)   CAT(A,B)

#endif

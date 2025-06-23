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
#include <rmn/va_args_num.h>

#define SEQ_1(FN) FN(0)
#define SEQ_2(FN) SEQ_1(FN) FN(1)
#define SEQ_3(FN) SEQ_2(FN) FN(2)
#define SEQ_4(FN) SEQ_3(FN) FN(3)
#define SEQ_5(FN) SEQ_4(FN) FN(4)
#define SEQ_6(FN) SEQ_5(FN) FN(5)
#define SEQ_7(FN) SEQ_6(FN) FN(6)
#define SEQ_8(FN) SEQ_7(FN) FN(7)
#define SEQ_9(FN) SEQ_8(FN) FN(8)
#define SEQ_10(FN) SEQ_9(FN) FN(9)

#define SEQ_11(FN) SEQ_10(FN) FN(10)
#define SEQ_12(FN) SEQ_11(FN) FN(11)
#define SEQ_13(FN) SEQ_12(FN) FN(12)
#define SEQ_14(FN) SEQ_13(FN) FN(13)
#define SEQ_15(FN) SEQ_14(FN) FN(14)
#define SEQ_16(FN) SEQ_15(FN) FN(15)
#define SEQ_17(FN) SEQ_16(FN) FN(16)
#define SEQ_18(FN) SEQ_17(FN) FN(17)
#define SEQ_19(FN) SEQ_18(FN) FN(18)
#define SEQ_20(FN) SEQ_19(FN) FN(19)

#define SEQ_21(FN) SEQ_20(FN) FN(20)
#define SEQ_22(FN) SEQ_21(FN) FN(21)
#define SEQ_23(FN) SEQ_22(FN) FN(22)
#define SEQ_24(FN) SEQ_23(FN) FN(23)
#define SEQ_25(FN) SEQ_24(FN) FN(24)
#define SEQ_26(FN) SEQ_25(FN) FN(25)
#define SEQ_27(FN) SEQ_26(FN) FN(26)
#define SEQ_28(FN) SEQ_27(FN) FN(27)
#define SEQ_29(FN) SEQ_28(FN) FN(28)
#define SEQ_30(FN) SEQ_29(FN) FN(29)

// SEQ(macro, n) expands to macro(0) ... macro(n-1)
#define SEQ(FN, N) SEQ_##N(FN)

#define FOR_0(FN, ...) 
#define FOR_1(FN, E, ...)  FN(E) 
#define FOR_2(FN, E, ...)  FN(E) FOR_1(FN, __VA_ARGS__)
#define FOR_3(FN, E, ...)  FN(E) FOR_2(FN, __VA_ARGS__)
#define FOR_4(FN, E, ...)  FN(E) FOR_3(FN, __VA_ARGS__)
#define FOR_5(FN, E, ...)  FN(E) FOR_4(FN, __VA_ARGS__)
#define FOR_6(FN, E, ...)  FN(E) FOR_5(FN, __VA_ARGS__)
#define FOR_7(FN, E, ...)  FN(E) FOR_6(FN, __VA_ARGS__)
#define FOR_8(FN, E, ...)  FN(E) FOR_7(FN, __VA_ARGS__)
#define FOR_9(FN, E, ...)  FN(E) FOR_8(FN, __VA_ARGS__)
#define FOR_10(FN, E, ...)  FN(E) FOR_9(FN, __VA_ARGS__)

#define FOR_11(FN, E, ...)  FN(E) FOR_10(FN, __VA_ARGS__)
#define FOR_12(FN, E, ...)  FN(E) FOR_11(FN, __VA_ARGS__)
#define FOR_13(FN, E, ...)  FN(E) FOR_12(FN, __VA_ARGS__)
#define FOR_14(FN, E, ...)  FN(E) FOR_13(FN, __VA_ARGS__)
#define FOR_15(FN, E, ...)  FN(E) FOR_14(FN, __VA_ARGS__)
#define FOR_16(FN, E, ...)  FN(E) FOR_15(FN, __VA_ARGS__)
#define FOR_17(FN, E, ...)  FN(E) FOR_16(FN, __VA_ARGS__)
#define FOR_18(FN, E, ...)  FN(E) FOR_17(FN, __VA_ARGS__)
#define FOR_19(FN, E, ...)  FN(E) FOR_18(FN, __VA_ARGS__)
#define FOR_20(FN, E, ...)  FN(E) FOR_19(FN, __VA_ARGS__)

#define FOR_21(FN, E, ...)  FN(E) FOR_20(FN, __VA_ARGS__)
#define FOR_22(FN, E, ...)  FN(E) FOR_21(FN, __VA_ARGS__)
#define FOR_23(FN, E, ...)  FN(E) FOR_22(FN, __VA_ARGS__)
#define FOR_24(FN, E, ...)  FN(E) FOR_23(FN, __VA_ARGS__)
#define FOR_25(FN, E, ...)  FN(E) FOR_24(FN, __VA_ARGS__)
#define FOR_26(FN, E, ...)  FN(E) FOR_25(FN, __VA_ARGS__)
#define FOR_27(FN, E, ...)  FN(E) FOR_26(FN, __VA_ARGS__)
#define FOR_28(FN, E, ...)  FN(E) FOR_27(FN, __VA_ARGS__)
#define FOR_29(FN, E, ...)  FN(E) FOR_28(FN, __VA_ARGS__)
#define FOR_30(FN, E, ...)  FN(E) FOR_29(FN, __VA_ARGS__)

// FOR(macro, arg1, ..., argn) expands to macro(arg1) ... macro(argn)
#define FOR__(FN, NARGS, ...) FOR_##NARGS(FN, __VA_ARGS__) 
#define FOR_(FN, NARGS, ...) FOR__(FN, NARGS, __VA_ARGS__)
#define FOR(FN, ...) FOR_(FN, VA_ARGS_NUM(__VA_ARGS__), __VA_ARGS__)


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

// VA_ARGS_NUM returns the number of its arguments (up to 30)

#if ! defined(VA_ARGS_NUM)

#undef VA_ARGS_NUM_
#define VA_ARGS_NUM_(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28, A29, A30, N, ...)    N
#if defined(__INTEL_COMPILER) && ! defined(__INTEL_LLVM_COMPILER)
// when called with 0 arguments, it will return 1 ( Intel icc compiler does not support __VA_OPT__(,) )
#define VA_ARGS_NUM(...)  VA_ARGS_NUM_(__VA_ARGS__ , 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#else
#define VA_ARGS_NUM(...)  VA_ARGS_NUM_(__VA_ARGS__ __VA_OPT__(,) 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#endif

#endif

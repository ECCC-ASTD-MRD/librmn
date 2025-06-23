//
// Copyright (C) 2022-2024  Environnement Canada
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
// M. Valin,   Recherche en Prevision Numerique, 2024
//
// static assertion macros
// CT_ASSERT  with message
// CT_ASSERT_ without message
#if ! defined(CT_ASSERT)

#if (__STDC_VERSION__ >= 201112L)  // C11+

#define CT_ASSERT(e, message) _Static_assert(e, message) ;

#else

// inspired by https://www.pixelbeat.org/programming/gcc/static_assert.html
#define CT_ASSERT(e, message) extern char (*DuMmY_NaMe(void)) [sizeof(char[1 - 2*!(e)])] ;

#endif

#define CT_ASSERT_(e) CT_ASSERT(e, "")

#endif   // ! defined(CT_ASSERT)

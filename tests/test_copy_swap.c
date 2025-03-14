// Hopefully useful functions for C and FORTRAN
// Copyright (C) 2024  Recherche en Prevision Numerique
//
// This code is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation,
// version 2.1 of the License.
//
// This code is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Library General Public License for more details.
//
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// #include <rmn/timers.h>
#include <rmn/copy_swap.h>
// #include <rmn/test_helpers.h>
// #include <rmn/tee_print.h>
#if ! defined(TEE_FPRINTF)

#define TEE_FPRINTF(file, level,...) { fprintf(file,  __VA_ARGS__) ; }
#define TEE_ERROR 0

// print bytes / halfwords / words / doublewords in hexadecimal ( 68 - 96 characters per line)

// print n bytes into file f in hexadecimal format
// f     [IN] : file to print into
// what  [IN] : address of bytes to be printed
// n     [IN] : number of bytes to print
// level [IN] : message level TEE_DEBUG/.../TEE_FATAL (see rmn/tee_print.h)
void hexprintf_08(FILE *f, void *what, int n, char *msg, int level){
  uint8_t *c08 = (uint8_t *) what ;
  int i ;
  TEE_FPRINTF(f, level, " %s", msg) ;
  for(i=0 ; i<n ; i++) { TEE_FPRINTF(f, level, " %2.2x",c08[i]); if((i&31) == 31) TEE_FPRINTF(f, level, "\n"); }
  TEE_FPRINTF(f, level, "\n");
}

// print n halfwords into file f in hexadecimal format
// f     [IN] : file to print into
// what  [IN] : address of halfwords to be printed
// n     [IN] : number of halfwords to print
// level [IN] : message level TEE_DEBUG/.../TEE_FATAL (see rmn/tee_print.h)
void hexprintf_16(FILE *f, void *what, int n, char *msg, int level){
  uint16_t *h16 = (uint16_t *) what ;
  int i ;
  TEE_FPRINTF(f, level, " %s", msg) ;
  for(i=0 ; i<n ; i++) { TEE_FPRINTF(f, level, " %4.4x",h16[i]); if((i&15) == 15) TEE_FPRINTF(f, level, "\n"); }
  TEE_FPRINTF(f, level, "\n");
}

// print n words into file f in hexadecimal format
// f     [IN] : file to print into
// what  [IN] : address of words to be printed
// n     [IN] : number of words to print
// level [IN] : message level TEE_DEBUG/.../TEE_FATAL (see rmn/tee_print.h)
void hexprintf_32(FILE *f, void *what, int n, char *msg, int level){
  uint32_t *w32 = (uint32_t *) what ;
  int i ;
  TEE_FPRINTF(f, level, " %s", msg) ;
  for(i=0 ; i<n ; i++) { TEE_FPRINTF(f, level, " %8.8x",w32[i]); if((i&7) == 7) TEE_FPRINTF(f, level, "\n"); }
  TEE_FPRINTF(f, level, "\n");
}

// print n doublewords into file f in hexadecimal format
// f     [IN] : file to print into
// what  [IN] : address of doublewords to be printed
// n     [IN] : number of doublewords to print
// level [IN] : message level TEE_DEBUG/.../TEE_FATAL (see rmn/tee_print.h)
void hexprintf_64(FILE *f, void *what, int n, char *msg, int level){
  uint64_t *l64 = (uint64_t *) what ;
  int i ;
  TEE_FPRINTF(f, level, " %s", msg) ;
  for(i=0 ; i<n ; i++) { TEE_FPRINTF(f, level, " %16.16lx",l64[i]); if((i&31) == 3) TEE_FPRINTF(f, level, "\n"); }
  TEE_FPRINTF(f, level, "\n");
}

#endif

static uint8_t s08[] = { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
                         0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
                         0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
                         0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
                         0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
                         0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
                         0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
                         0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
                         0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
                         0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
                         0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
                         0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
                         0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
                         0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
                         0x09, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
                         0x17, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7 } ;

static uint16_t r16[] = { 0x0201, 0x0403, 0x0605, 0x0807,
                          0x1009, 0x1211, 0x1413, 0x1615,
                          0x1009, 0x1211, 0x1413, 0x1615,
                          0x1009, 0x1211, 0x1413, 0x1615,
                          0x1009, 0x1211, 0x1413, 0x1615,
                          0x1009, 0x1211, 0x1413, 0x1615,
                          0x1009, 0x1211, 0x1413, 0x1615,
                          0x1009, 0x1211, 0x1413, 0x1615,
                          0x1009, 0x1211, 0x1413, 0x1615,
                          0x1009, 0x1211, 0x1413, 0x1615,
                          0x1009, 0x1211, 0x1413, 0x1615,
                          0x1009, 0x1211, 0x1413, 0x1615,
                          0x1009, 0x1211, 0x1413, 0x1615,
                          0x1009, 0x1211, 0x1413, 0x1615,
                          0x1009, 0x1211, 0x1413, 0x1615,
                          0xF117, 0xF3F2, 0xF5F4, 0xF7F6} ;
static uint16_t l16[] = { 0x0102, 0x0304, 0x0506, 0x0708,
                          0x0910, 0x1112, 0x1314, 0x1516,
                          0x0910, 0x1112, 0x1314, 0x1516,
                          0x0910, 0x1112, 0x1314, 0x1516,
                          0x0910, 0x1112, 0x1314, 0x1516,
                          0x0910, 0x1112, 0x1314, 0x1516,
                          0x0910, 0x1112, 0x1314, 0x1516,
                          0x0910, 0x1112, 0x1314, 0x1516,
                          0x0910, 0x1112, 0x1314, 0x1516,
                          0x0910, 0x1112, 0x1314, 0x1516,
                          0x0910, 0x1112, 0x1314, 0x1516,
                          0x0910, 0x1112, 0x1314, 0x1516,
                          0x0910, 0x1112, 0x1314, 0x1516,
                          0x0910, 0x1112, 0x1314, 0x1516,
                          0x0910, 0x1112, 0x1314, 0x1516,
                          0x17F1, 0xF2F3, 0xF4F5, 0xF6F7} ;

static uint32_t r32[] = { 0x04030201, 0x08070605,
                          0x12111009, 0x16151413,
                          0x12111009, 0x16151413,
                          0x12111009, 0x16151413,
                          0x12111009, 0x16151413,
                          0x12111009, 0x16151413,
                          0x12111009, 0x16151413,
                          0x12111009, 0x16151413,
                          0x12111009, 0x16151413,
                          0x12111009, 0x16151413,
                          0x12111009, 0x16151413,
                          0x12111009, 0x16151413,
                          0x12111009, 0x16151413,
                          0x12111009, 0x16151413,
                          0x12111009, 0x16151413,
                          0xF3F2F117, 0xF7F6F5F4 } ;
static uint32_t l32[] = { 0x01020304, 0x05060708,
                          0x09101112, 0x13141516,
                          0x09101112, 0x13141516,
                          0x09101112, 0x13141516,
                          0x09101112, 0x13141516,
                          0x09101112, 0x13141516,
                          0x09101112, 0x13141516,
                          0x09101112, 0x13141516,
                          0x09101112, 0x13141516,
                          0x09101112, 0x13141516,
                          0x09101112, 0x13141516,
                          0x09101112, 0x13141516,
                          0x09101112, 0x13141516,
                          0x09101112, 0x13141516,
                          0x09101112, 0x13141516,
                          0x17F1F2F3, 0xF4F5F6F7 } ;

static uint64_t r64[] = { 0x0807060504030201lu,
                          0x1615141312111009lu,
                          0x1615141312111009lu,
                          0x1615141312111009lu,
                          0x1615141312111009lu,
                          0x1615141312111009lu,
                          0x1615141312111009lu,
                          0x1615141312111009lu,
                          0x1615141312111009lu,
                          0x1615141312111009lu,
                          0x1615141312111009lu,
                          0x1615141312111009lu,
                          0x1615141312111009lu,
                          0x1615141312111009lu,
                          0x1615141312111009lu,
                          0xF7F6F5F4F3F2F117lu } ;
static uint64_t l64[] = { 0x0102030405060708lu,
                          0x0910111213141516lu,
                          0x0910111213141516lu,
                          0x0910111213141516lu,
                          0x0910111213141516lu,
                          0x0910111213141516lu,
                          0x0910111213141516lu,
                          0x0910111213141516lu,
                          0x0910111213141516lu,
                          0x0910111213141516lu,
                          0x0910111213141516lu,
                          0x0910111213141516lu,
                          0x0910111213141516lu,
                          0x0910111213141516lu,
                          0x0910111213141516lu,
                          0x17F1F2F3F4F5F6F7lu } ;

// static uint16_t l2r_16[] = { 0x0102, 0x0304, 0x0506, 0x0708, 0x0910, 0x1112, 0x1314, 0x1516, 0x17F1, 0xF2F3, 0xF4F5, 0xF6F7 } ;
// static uint32_t l2r_32[] = { 0x01020304, 0x05060708, 0x09101112, 0x13141516, 0x17F1F2F3, 0xF4F5F6F7 } ;
// static uint64_t l2r_64[] = { 0x0102030405060708lu, 0x0910111213141516lu, 0x17F1F2F3F4F5F6F7lu } ;

// compare 2 memory areas a and b
// nitems [IN] : number of items in a and b
// nbytes [IN] : item length in bytes (1/2/4/8)
// msg    [IN] : extra message for the error line
void compare_mem(void *a, void *b, int nitems, char *msg, int nbytes){
  size_t memsize = nitems * nbytes ;
  int errors = memcmp(a, b, memsize) ;
  if(errors){
    TEE_FPRINTF(stderr, TEE_ERROR, "ERROR (%d) : %s\n", nbytes, msg) ;
    switch(nbytes) {
      case 1:
        hexprintf_08(stderr, a, nitems, "expected :", TEE_ERROR) ;
        hexprintf_08(stderr, b, nitems, "found    :", TEE_ERROR) ;
        break ;
      case 2:
        hexprintf_16(stderr, a, nitems, "expected :", TEE_ERROR) ;
        hexprintf_16(stderr, b, nitems, "found    :", TEE_ERROR) ;
        break ;
      case 4:
        hexprintf_32(stderr, a, nitems, "expected :", TEE_ERROR) ;
        hexprintf_32(stderr, b, nitems, "found    :", TEE_ERROR) ;
        break ;
      case 8:
        hexprintf_64(stderr, a, nitems, "expected :", TEE_ERROR) ;
        hexprintf_64(stderr, b, nitems, "found    :", TEE_ERROR) ;
        break ;
    }
    exit(1) ;
  }else{
    TEE_FPRINTF(stderr, TEE_ERROR, "SUCCESS : %s\n", msg) ;
  }
}

int main(int argc, char **argv){
#if defined(TIME_LOOP_DATA)
  TIME_LOOP_DATA ;
#endif
  uint8_t  t08[128] ;   // at least 128 bytes
  uint16_t t16[ 64] ;   // at least 128 bytes
  uint32_t t32[ 32] ;   // at least 128 bytes
  uint64_t t64[ 16] ;   // at least 128 bytes
  int nitems;

  TEE_FPRINTF(stderr, TEE_INFO, "=================== right -> left copy ===================\n") ;
  nitems = Copy_items_r2l(s08, 1, t16, 2,    127) ; compare_mem(r16, t16, nitems, "copy  8 to 16", 2) ;
  nitems = Copy_items_r2l(t16, 2, t08, 1, nitems) ; compare_mem(s08, t08, nitems, "copy 16 to  8", 1) ;
  nitems = Copy_items_r2l(t08, 1, t08, 2,    127) ; compare_mem(r16, t08, nitems, "copy  8 to 16 (in place)", 2) ;
  nitems = Copy_items_r2l(t08, 2, t08, 1, nitems) ; compare_mem(s08, t08, nitems, "copy 16 to  8 (in place)", 1) ;

  nitems = Copy_items_r2l(s08, 1, t32, 4,    125) ; compare_mem(r32, t32, nitems, "copy  8 to 32", 4) ;
  nitems = Copy_items_r2l(t32, 4, t08, 1, nitems) ; compare_mem(s08, t08, nitems, "copy 32 to  8", 1) ;
  nitems = Copy_items_r2l(t08, 1, t08, 4,     17) ; compare_mem(r32, t08, nitems, "copy  8 to 32 (in place)", 4) ;
  nitems = Copy_items_r2l(t08, 4, t08, 1, nitems) ; compare_mem(s08, t08, nitems, "copy 32 to  8 (in place)", 1) ;

  nitems = Copy_items_r2l(s08, 1, t64, 8,    123) ; compare_mem(r64, t64, nitems, "copy  8 to 64", 8) ;
  nitems = Copy_items_r2l(t64, 8, t08, 1, nitems) ; compare_mem(s08, t08, nitems, "copy 64 to  8", 1) ;
  nitems = Copy_items_r2l(t08, 1, t08, 8,     17) ; compare_mem(r64, t08, nitems, "copy  8 to 64 (in place)", 8) ;
  nitems = Copy_items_r2l(t08, 8, t08, 1, nitems) ; compare_mem(s08, t08, nitems, "copy 64 to  8 (in place)", 1) ;

  nitems = Copy_items_r2l(r16, 2, t32, 4,      9) ; compare_mem(r32, t32, nitems, "copy 16 to 32", 4) ;
  nitems = Copy_items_r2l(t32, 4, t16, 2, nitems) ; compare_mem(r16, t16, nitems, "copy 32 to 16", 2) ;
  nitems = Copy_items_r2l(t16, 2, t16, 4,      9) ; compare_mem(r32, t16, nitems, "copy 16 to 32 (in place)", 4) ;
  nitems = Copy_items_r2l(t16, 4, t16, 2, nitems) ; compare_mem(r16, t16, nitems, "copy 32 to 16 (in place)", 2) ;

  nitems = Copy_items_r2l(r16, 2, t64, 8,      9) ; compare_mem(r64, t64, nitems, "copy 16 to 64", 8) ;
  nitems = Copy_items_r2l(t64, 8, t16, 2, nitems) ; compare_mem(r16, t16, nitems, "copy 64 to 16", 2) ;
  nitems = Copy_items_r2l(t16, 2, t16, 8,      9) ; compare_mem(r64, t16, nitems, "copy 16 to 64 (in place)", 8) ;
  nitems = Copy_items_r2l(t16, 8, t16, 2, nitems) ; compare_mem(r16, t16, nitems, "copy 64 to 16 (in place)", 2) ;

  nitems = Copy_items_r2l(r32, 4, t64, 8,      5) ; compare_mem(r64, t64, nitems, "copy 32 to 64", 8) ;
  nitems = Copy_items_r2l(t64, 8, t32, 4, nitems) ; compare_mem(r32, t32, nitems, "copy 64 to 32", 4) ;
  nitems = Copy_items_r2l(t32, 4, t32, 8,      5) ; compare_mem(r64, t32, nitems, "copy 32 to 64 (in place)", 8) ;
  nitems = Copy_items_r2l(t32, 8, t32, 4, nitems) ; compare_mem(r32, t32, nitems, "copy 64 to 32 (in place)", 4) ;

  TEE_FPRINTF(stderr, TEE_INFO, "=================== left -> right copy ===================\n") ;
  nitems = Copy_items_l2r(s08, 1, t16, 2,    125) ; compare_mem(l16, t16, nitems, "copy  8 to 16", 2) ;
  nitems = Copy_items_l2r(t16, 2, t08, 1, nitems) ; compare_mem(s08, t08, nitems, "copy 16 to  8", 1) ;
  nitems = Copy_items_l2r(t08, 1, t08, 2,    124) ; compare_mem(l16, t08, nitems, "copy  8 to 16 (in place)", 2) ;
  nitems = Copy_items_l2r(t08, 2, t08, 1, nitems) ; compare_mem(s08, t08, nitems, "copy 16 to  8 (in place)", 1) ;

  nitems = Copy_items_l2r(s08, 1, t32, 4,    123) ; compare_mem(l32, t32, nitems, "copy  8 to 32", 4) ;
  nitems = Copy_items_l2r(t32, 4, t08, 1, nitems) ; compare_mem(s08, t08, nitems, "copy 32 to  8", 1) ;
  nitems = Copy_items_l2r(t08, 1, t08, 4,    122) ; compare_mem(l32, t08, nitems, "copy  8 to 32 (in place)", 4) ;
  nitems = Copy_items_l2r(t08, 4, t08, 1, nitems) ; compare_mem(s08, t08, nitems, "copy 32 to  8 (in place)", 1) ;

  nitems = Copy_items_l2r(s08, 1, t64, 8,     93) ; compare_mem(l64, t64, nitems, "copy  8 to 64", 8) ;
  nitems = Copy_items_l2r(t64, 8, t08, 1, nitems) ; compare_mem(s08, t08, nitems, "copy 64 to  8", 1) ;
  nitems = Copy_items_l2r(t08, 1, t08, 8,     79) ; compare_mem(l64, t08, nitems, "copy  8 to 64 (in place)", 8) ;
  nitems = Copy_items_l2r(t08, 8, t08, 1, nitems) ; compare_mem(s08, t08, nitems, "copy 64 to  8 (in place)", 1) ;

  nitems = Copy_items_l2r(l16, 2, t32, 4,     59) ; compare_mem(l32, t32, nitems, "copy 16 to 32", 4) ;
  nitems = Copy_items_l2r(t32, 4, t16, 2, nitems) ; compare_mem(l16, t16, nitems, "copy 32 to 16", 2) ;
  nitems = Copy_items_l2r(t16, 2, t16, 4,     58) ; compare_mem(l32, t16, nitems, "copy 16 to 32 (in place)", 4) ;
  nitems = Copy_items_l2r(t16, 4, t16, 2, nitems) ; compare_mem(l16, t16, nitems, "copy 32 to 16 (in place)", 2) ;

  nitems = Copy_items_l2r(l16, 2, t64, 8,     58) ; compare_mem(l64, t64, nitems, "copy 16 to 64", 8) ;
  nitems = Copy_items_l2r(t64, 8, t16, 2, nitems) ; compare_mem(l16, t16, nitems, "copy 64 to 16", 2) ;
  nitems = Copy_items_l2r(t16, 2, t16, 8,     57) ; compare_mem(l64, t16, nitems, "copy 16 to 64 (in place)", 8) ;
  nitems = Copy_items_l2r(t16, 8, t16, 2, nitems) ; compare_mem(l16, t16, nitems, "copy 64 to 16 (in place)", 2) ;

  nitems = Copy_items_l2r(l32, 4, t64, 8,     31) ; compare_mem(l64, t64, nitems, "copy 32 to 64", 8) ;
  nitems = Copy_items_l2r(t64, 8, t32, 4, nitems) ; compare_mem(l32, t32, nitems, "copy 64 to 32", 4) ;
  nitems = Copy_items_l2r(t32, 4, t32, 8,     29) ; compare_mem(l64, t32, nitems, "copy 32 to 64 (in place)", 8) ;
  nitems = Copy_items_l2r(t32, 8, t32, 4, nitems) ; compare_mem(l32, t32, nitems, "copy 64 to 32 (in place)", 4) ;

#if defined(TIME_LOOP_DATA)
  TEE_FPRINTF(stderr, TEE_INFO, "=================== timing tests ===================\n") ;

  uint8_t tmp1[1024*1024*1024] ;   // 1 GByte array
  uint8_t tmp2[1024*1024*1024] ;   // 1 GByte array
  int npts ;
  for(i=0 ; i<sizeof(tmp1) ; i++) tmp1[i] = i & 0xFF ;
  for(i=0 ; i<sizeof(tmp2) ; i++) tmp2[i] = i & 0xEE ;

  npts = 4096 ;            // level2 cache
  TIME_LOOP_EZ(8*1024*1024, npts, Copy_items_l2r(tmp1, 1, tmp1, 8, npts)) ;
  TEE_FPRINTF(stderr, TEE_INFO, "Copy_items_l2r (in place): %s\n", timer_msg);

  TIME_LOOP_EZ(4*1024*1024, npts, Copy_items_l2r(tmp1, 1, tmp2, 8, npts)) ;
  TEE_FPRINTF(stderr, TEE_INFO, "Copy_items_l2r           : %s\n", timer_msg);
  TEE_FPRINTF(stderr, TEE_INFO, "\n");

  npts = 65536 ;            // level2 cache
  TIME_LOOP_EZ(1024*1024, npts, Copy_items_l2r(tmp1, 1, tmp1, 8, npts)) ;
  TEE_FPRINTF(stderr, TEE_INFO, "Copy_items_l2r (in place): %s\n", timer_msg);

  TIME_LOOP_EZ(512*1024, npts, Copy_items_l2r(tmp1, 1, tmp2, 8, npts)) ;
  TEE_FPRINTF(stderr, TEE_INFO, "Copy_items_l2r           : %s\n", timer_msg);
  TEE_FPRINTF(stderr, TEE_INFO, "\n");

  npts = 32768*1024 ;       // level3 cache
  TIME_LOOP_EZ(2*1024, npts, Copy_items_l2r(tmp1, 1, tmp1, 8, npts)) ;
  TEE_FPRINTF(stderr, TEE_INFO, "Copy_items_l2r (in place): %s\n", timer_msg);

  npts = 32768*1024 ;       // level3 cache
  TIME_LOOP_EZ(1024, npts, Copy_items_l2r(tmp1, 1, tmp2, 8, npts)) ;
  TEE_FPRINTF(stderr, TEE_INFO, "Copy_items_l2r           : %s\n", timer_msg);
  TEE_FPRINTF(stderr, TEE_INFO, "\n");

  npts = 1024*1024*1024 ;   // memory
  TIME_LOOP_EZ(40, npts, Copy_items_l2r(tmp1, 1, tmp1, 8, npts)) ;
  TEE_FPRINTF(stderr, TEE_INFO, "Copy_items_l2r (in place): %s\n", timer_msg);

  npts = 1024*1024*1024 ;   // memory
  TIME_LOOP_EZ(20, npts, Copy_items_l2r(tmp1, 1, tmp2, 8, npts)) ;
  TEE_FPRINTF(stderr, TEE_INFO, "Copy_items_l2r           : %s\n", timer_msg);
#endif
}

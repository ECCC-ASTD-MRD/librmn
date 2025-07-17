/*
 * Hopefully useful code for C
 * Copyright (C) 2022-2025  Recherche en Prevision Numerique
 *
 * This code is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This code is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * in the ARM V8 case, all the functions use the same instruction
 * in the X86_64 case, several instructions and fences are used
 *
 * useful references :
 * https://sites.utexas.edu/jdm4372/2018/07/   John McCalpin's blog
 * https://github.com/jdmccalpin/low-overhead-timers
 */

// protect against multiple include
#if ! defined(TIME_LOOP_TOP)
#include <stdio.h>
#include <stdint.h>

#if ! defined(STATIC)
#define STATIC static inline  __attribute__((always_inline))
#define STATIC_DEFINED_HERE
#endif

static double NaNoSeC = 0.0 ;

STATIC uint64_t cycles_counter_freq(void) ;
#define TIME_CONVERT_INIT if(NaNoSeC == 0) NaNoSeC = 1.0E+9f / cycles_counter_freq() ;

STATIC double cycles_to_ns(uint64_t t){
  TIME_CONVERT_INIT ;
  return t * NaNoSeC ;
}

#define TIME_LOOP_DATA \
  uint64_t timer_min, timer_max ; \
  double timer_avg ; \
  char timer_msg[1024] ; \
  size_t timer_msg_size = sizeof(timer_msg) ;

// niter is expected to be an integer scalar variable
//
// TIME_LOOP_TOP : timing loop top part
// niter : number of iterations
// to is used to compensate elapsed_cycles() overhead ( 7/8 of elapsed_cycles() time)
//
// TIME_ONCE_TOP does not need niter
#define TIME_LOOP_TOP(niter) \
{ uint64_t t, to , mint = ~0, maxt = 0, avgt = 0.0 ; int iter = niter ; int j ; \
  TIME_CONVERT_INIT ; \
  to = elapsed_cycles() ; t = elapsed_cycles() ; to = t - to ; to = to - (to >> 3) ; \
  for(j=0 ; j < iter ; j++) { t = elapsed_cycles() ;
#define TIME_ONCE_TOP \
{ uint64_t t, to ; TIME_CONVERT_INIT ; \
  to = elapsed_cycles() ; t = elapsed_cycles() ; to = t - to ; to = to - (to >> 3) ;
//
// code to be timed in a loop goes between TIME_LOOP_TOP and TIME_LOOP_BOT
// timer_min, timer_max should be uint64_t
// timer_avg can be float or double (scalar variable)
//
// TIME_LOOP_BOT : timing loop bottom part
// timer_min  : best timing in cycles
// timer_max  : worst timing in cycles
// timer_avg  : average timing in cycles
// npts       : number of points processed
// timer_msg  : buffer to receive diagnostic text
// timer_msg_size: size of timer_msg
//
// TIME_ONCE_BOT does not need timer_min, timer_max, timer_avg
#define TIME_LOOP_BOT(timer_min, timer_max, timer_avg, npts, timer_msg, timer_msg_size) \
    t = elapsed_cycles() -t -to ; \
    avgt += t ; mint = (t < mint) ? t : mint ; maxt = (t > maxt) ? t : maxt ; \
  } \
  timer_min = mint ; timer_max = maxt ; timer_avg = avgt/iter ; \
  if(npts > 0 && timer_msg != NULL) \
    snprintf(timer_msg, (size_t)timer_msg_size, " npts = %d, niter = %d, ns = %6.0f (%6.0f), %9.2e (%9.2e) ns/pt", \
             npts, iter, timer_min*NaNoSeC, timer_avg*NaNoSeC, timer_min*NaNoSeC/(npts), timer_avg*NaNoSeC/(npts)) ; \
}
#define TIME_LOOP_BOT_EZ(npts) TIME_LOOP_BOT(timer_min, timer_max, timer_avg, npts, timer_msg, timer_msg_size)
#define TIME_ONCE_BOT(npts, timer_msg, timer_msg_size) \
    t = elapsed_cycles() -t -to ; \
    if(npts > 0 && timer_msg != NULL) \
    snprintf(timer_msg, (size_t)timer_msg_size, " npts = %d, ns = %6.0f, %8.2g ns/pt", \
             npts, t*NaNoSeC, t*NaNoSeC/(npts)) ; \
}
#define TIME_ONCE_BOT_EZ(npts) TIME_ONCE_BOT(npts, timer_msg, timer_msg_size)

// timer_min, timer_max should be uint64_t (64 bit scalar variable)
// timer_avg can be float or double (scalar variable)
// niter is expected to be an integer scalar variable
//
// TIME_LOOP : time a piece of code in a loop, get min, max, average time
// timer_min  : best timing in cycles
// timer_max  : worst timing in cycles
// timer_avg  : average timing in cycles
// niter      : number of iterations
// npts       : number of points processed
// TimedCode  : code to be timed n (>=1) statement(s)
// timer_msg  : buffer to receive diagnostic text
// timer_msg_size: size of timer_msg
//
// TIME_ONCE does not need timer_min, timer_max, timer_avg, niter
#define TIME_LOOP(timer_min, timer_max, timer_avg, niter, npts, timer_msg, timer_msg_size, TimedCode) \
  TIME_LOOP_TOP(niter) ; \
  TimedCode ; \
  TIME_LOOP_BOT(timer_min, timer_max, timer_avg, npts, timer_msg, timer_msg_size) ;
#define TIME_LOOP_EZ(niter, npts, TimedCode) TIME_LOOP(timer_min, timer_max, timer_avg, niter, npts, timer_msg, timer_msg_size, TimedCode)
#define TIME_ONCE(npts, timer_msg, timer_msg_size, TimedCode) \
  TIME_ONCE_TOP ; \
  TimedCode ; \
  TIME_ONCE_BOT(npts, timer_msg, timer_msg_size) ;
#define TIME_ONCE_EZ(npts, TimedCode) TIME_ONCE(npts, timer_msg, timer_msg_size, TimedCode)
#if ! defined(MISC_TIMERS)
#define MISC_TIMERS

#include <stdint.h>
#include <sys/time.h>
#include <stddef.h>

// get core number using rdtscp instruction to get info from special register set by linux
STATIC int core_number(){
  uint64_t a, d, c ;
  __asm__ volatile("rdtscp" : "=a" (a), "=d" (d), "=c" (c));
  if(a == 0 && d == 0) c &= 0xFFFFUL ;  // neutral code to get rid of unused variable warning
  return (c & 0xFFFUL) ;
}

// get socket number using rdtscp instruction to get info from special register set by linux
STATIC int socket_number(){
  uint64_t a, d, c ;
  __asm__ volatile("rdtscp" : "=a" (a), "=d" (d), "=c" (c));
  if(a == 0 && d == 0) c &= 0xFFFFUL ;  // neutral code to get rid of unused variable warning
  return ( (c & 0xF000UL)>>12 ) ;
}

// elapsed microseconds of wall clock time
// an effective resolution of O(microsecond) is assumed for gettimeofday
STATIC uint64_t elapsed_us(void){
  struct timeval t ;
  uint64_t elapsed ;
  gettimeofday(&t, NULL) ;
  elapsed = t.tv_sec ;
  elapsed *= 1000000 ;
  elapsed += t.tv_usec ;
  return elapsed ;
}

// elapsed timer ticks, NO serializing, NO fencing
STATIC uint64_t elapsed_cycles_fast(void) {
#if defined(__x86_64__)
  uint64_t lo, hi ;
  __asm__ volatile ("rdtsc" : /* outputs   */ "=a" (lo), "=d" (hi) );
  return lo | (hi << 32);
#elif defined(__aarch64__)
  uint64_t time0 ;
  asm volatile ("isb ; mrs %0, cntvct_el0" : "=r" (time0));
  return time0;
#else
  return elapsed_us() * 1000 ;  // nanoseconds
#endif
}

static uint64_t misc = 0ul ;
// elapsed timer ticks, WITH serializing, NO fencing
STATIC uint64_t elapsed_cycles_nofence(void) {
#if defined(__x86_64__)
  uint64_t lo, hi ;
  __asm__ volatile ("rdtscp": /* outputs   */ "=a" (lo), "=d" (hi), "=c" (misc) );
  return lo | (hi << 32);
#elif defined(__aarch64__)
  uint64_t time0 ;
  asm volatile ("isb ; mrs %0, cntvct_el0" : "=r" (time0));
  return time0;
#else
  return elapsed_us() * 1000 ;  // nanoseconds
#endif
}

// elapsed timer ticks, WITH serializing, WITH memory fencing after
STATIC uint64_t elapsed_cycles_fenced(void) {
#if defined(__x86_64__)
  uint64_t lo, hi ;
  __asm__ volatile ("rdtscp": /* outputs   */ "=a" (lo), "=d" (hi), "=c" (misc) );
  __asm__ volatile ("mfence");
  return lo | (hi << 32);
#elif defined(__aarch64__)
  uint64_t time0 ;
  asm volatile ("isb ; mrs %0, cntvct_el0" : "=r" (time0));
  return time0;
#else
  return elapsed_us() * 1000 ;  // nanoseconds
#endif
}

// elapsed timer ticks, NO serializing, LOCAL fencing before
// default version
// elapsed_cycles_fast is used by get_cycles_overhead
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
static uint64_t cycles_overhead = 0 ;
#pragma GCC diagnostic pop
STATIC uint64_t elapsed_cycles_raw(void) {
#if defined(__x86_64__)
  uint64_t lo, hi ;
  __asm__ volatile ("lfence");
  __asm__ volatile ("rdtsc" : /* outputs   */ "=a" (lo), "=d" (hi) );
  return lo | (hi << 32);
#elif defined(__aarch64__)
  uint64_t time0 ;
  asm volatile ("isb ; mrs %0, cntvct_el0" : "=r" (time0));
  return time0;
#else
  return elapsed_us() * 1000 ;  // nanoseconds
#endif
}
STATIC uint64_t get_cycles_overhead(){
  uint64_t t0, t1, overhead ;
  int i ;
  t0 = elapsed_cycles_fast() ;
  overhead = elapsed_cycles_fast() - t0 ;
  for(i=0 ; i<32 ; i++){
    t0 = elapsed_cycles_fast() ;
    t1 = (elapsed_cycles_fast() - t0) ;
    overhead = (t1 - t0) < overhead ? (t1 - t0) : overhead ;
  }
//   overhead = (t1 - t0) >> 5 ;             // (t1 - t0) / 32
//   overhead = overhead - (overhead >> 3) ; // keep 7/8 of value
  return overhead ;
}
STATIC uint64_t elapsed_cycles(void) {
//   if(cycles_overhead == 0) cycles_overhead = get_cycles_overhead() ;
#if defined(__x86_64__)
  uint64_t lo, hi, t ;
  __asm__ volatile ("lfence");
  __asm__ volatile ("rdtsc" : /* outputs   */ "=a" (lo), "=d" (hi) );
  t = lo | (hi << 32) ;
  return t ; // - cycles_overhead ;
#elif defined(__aarch64__)
  uint64_t time0 ;
  asm volatile ("isb ; mrs %0, cntvct_el0" : "=r" (time0));
  return time0 ; // - cycles_overhead ;
#else
  return elapsed_us() * 1000 ;  // nanoseconds
#endif
}

// determine the timer tick frequency (in Hz)
STATIC uint64_t cycles_counter_freq(void){
  static uint64_t timerfreq = 0;
  uint64_t t1, t2, tc1, tc2 ;

  if(timerfreq != 0) return timerfreq ;

#if defined(__aarch64__)
  // we are lucky, ther is an instruction that does it
  asm volatile("isb ; mrs %0, cntfrq_el0" : "=r"(timerfreq));
  return timerfreq ;
#endif
  // loop using gettimeofday for about 1 millisecond
  // assumes O(microsecond) effective resolution for gettimeofday
  t2 = t1 = elapsed_us() ;
  tc1 = elapsed_cycles() ;
  while(t2-t1 < 1000){    // 1 millisecond
    t2 = elapsed_us() ;
    tc2 = elapsed_cycles() ;
  }
  timerfreq = 1000000 * (tc2-tc1) / (t2-t1);
  return timerfreq ;
}

#endif

#if defined(STATIC_DEFINED_HERE)
#undef STATIC
#undef STATIC_DEFINED_HERE
#endif

#endif

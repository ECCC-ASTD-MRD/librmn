#include <time.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <rpnmacros.h>

/* under AIX, rtools_wtime calls time_base to get real time information */
/* for PowerPC architectures, time_base is written in assembler and reads */
/* the time base register (64 bits ) that contains bigticks (8 clock ticks on Power4) */
/* rtools_wtime returns a value in seconds  */

static double ticks_to_sec2 = 0;
#ifdef AIX
long long f77name(time_base)()
{
  register long long rval;
  __fence();
  rval=__mftb();
  __fence();
  return(rval);
}

double f77_name(rtools_wtime)()
{
/*
  long long t1 = time_base();
*/
  long long t1;
  __fence();
  t1 = __mftb();
  __fence();
  if(ticks_to_sec2 == 0) { /* printf("init\n"); */ ticks_to_sec2=8e-9 ; ticks_to_sec2/=1.9;};
  return(t1*ticks_to_sec2);
}
#else
word f77name(mclock)()
{
  clock_t returnvalue=clock();
  return(returnvalue);
}
/* under other OSes, time_base calls rtools_wtime to get real time information */
/* and returns a number of bigticks (a microsecond) */
static long ticks_to_sec=0;
static clock_t tim0=0;

unsigned long long f77_name(c_time_base)()
{
  long long t1;
  struct tms buffer;
  clock_t tim;

  if(tim0==0) tim0=times(&buffer);
  if(ticks_to_sec == 0) ticks_to_sec = (1000000/sysconf(_SC_CLK_TCK));
   if(ticks_to_sec2 == 0) ticks_to_sec2 = .000001;
  tim=times(&buffer);
  t1=tim - tim0;
  t1 = t1 * ticks_to_sec;
  return(t1);
}
unsigned long long f77_name(time_base)()
{
#ifdef i386
   static unsigned long long tim00=0;
   unsigned long long x;
   int func=0;
   int ax,bx,cx,dx;
/*   __asm__ volatile(".byte 0x0f,0x31" : "=A" (x)); */
   __asm__ __volatile__("rdtsc": "=A" (x));
   if(tim00==0) tim00=x;
   return(x-tim00);
#else
  return(f77_name(c_time_base)());
#endif
}
double f77_name(rtools_wtime)()
{
  unsigned long long t1=f77_name(c_time_base)();
  double t2;
  t2=t1*ticks_to_sec2;
  return(t2);
}
#endif
unsigned long long f77_name(rtools_clock)()
{
  long long t1=clock();
  return(t1);
}
double f77_name(rtools_cp_time)()
{
  struct rusage usage;
  double t1;

#ifdef AIX
  t1 = clock();
  return(t1 / CLOCKS_PER_SEC);
#else
  if( getrusage(RUSAGE_SELF,&usage) ) return(0);
  t1  = usage.ru_utime.tv_sec + usage.ru_utime.tv_usec * 1.0e-6;
  t1 += usage.ru_stime.tv_sec + usage.ru_stime.tv_usec * 1.0e-6;
  return(t1);
#endif
}
#ifdef TEST
main()
{
  double sec_ticks=(1000000/sysconf(_SC_CLK_TCK));
  int i;
  double sum,temp;
  unsigned long long tt1,tt2,clk1,clk2;
  double rcp1,rcp2;
  double rwc1,rwc2,t1;
  word mclk1,mclk2;

  clk1=f77_name(rtools_clock)();
  tt1=f77_name(time_base)();
  rcp1=f77_name(rtools_cp_time)();
  rwc1=f77_name(rtools_wtime)();
#ifdef AIX
  t1 = 1.0;
  t1 = t1 / CLOCKS_PER_SEC;
  printf("AIX clock interval = %g\n",t1);
#else
  printf("microseconds per tick for times()=%g\n",sec_ticks);
#endif
  sleep(1);
  sum = 0.0;
  printf("sum = %lg\n",sum);
  for ( i=0 ; i<1000000 ; i++ ) { temp = i*1.00 ; sum = sum + temp*temp ;}
  printf("sum = %lg\n",sum);
  tt2=f77_name(time_base)()-tt1;
  rcp2=f77_name(rtools_cp_time)()-rcp1;
  rwc2=f77_name(rtools_wtime)()-rwc1;
  clk2=f77_name(rtools_clock)()-clk1;
  printf("Wall Clock=%lu bigticks,",tt2);
  printf(" %lg seconds,",rwc2);
  printf(" CPU = %lu ticks",clk2);
  printf(" CPU = %lg seconds\n",rcp2);
  exit(0);
}
#endif

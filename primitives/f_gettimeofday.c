#include <unistd.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <rpnmacros.h>

/* time of day in seconds.microseconds */
double f77name(f_gettimeofday)()
{
  struct timeval tvnow;
  struct timezone tz;
  int ier;
  double t1;

  ier = gettimeofday(&tvnow,&tz);
  if (ier != 0) printf("gettimeofday error: ier=%d\n",ier);
  t1 = tvnow.tv_usec * 1.0e-6 ;
  t1 = tvnow.tv_sec + t1;
  return(t1);
}
/* time of day in microseconds */
long long f77name(f_gettimeofday_micro)()
{
  struct timeval tvnow;
  struct timezone tz;
  int ier;
  long long t1;

  ier = gettimeofday(&tvnow,&tz);
  if (ier != 0) printf("gettimeofday error: ier=%d\n",ier);
  t1 = tvnow.tv_sec ;
  t1 = t1 * 1000000;
  t1 = t1 + tvnow.tv_usec ;
  return(t1);
}

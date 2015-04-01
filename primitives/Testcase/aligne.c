#include <malloc.h>
#include <stdio.h>

int * aligne(int nm, int p2)
{
  int lng, nbytes;
  int ptint;
  unsigned char *pt,*pt_ori;

  lng = sizeof(int) * nm;
  nbytes = 1 << p2;

  pt = malloc(lng+nbytes);
  pt_ori = pt;
  printf(" pt = %X nbytes = %d\n",pt,nbytes);
  pt += nbytes;
  pt = ((int) pt) >> p2;
  pt = ((int) pt) << p2;
  printf(" pt aligne = %X diff = %d\n",pt,pt-pt_ori);
  return((int *) pt);
}


main ()
{
  int i;
  int *addr;

  for (i=1; i<= 10; i++)
    addr = aligne(100*i+i,7);
}

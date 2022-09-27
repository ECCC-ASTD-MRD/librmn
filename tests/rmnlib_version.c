#include <stdlib.h>
#include <stdio.h>

#include "rmn/rpnmacros.h"

extern int f77name(rmnlib_version)(char *rmn,int *print,int len);

int main(void){

   char rmn[128];
   int print=0;

   f77name(rmnlib_version)(rmn,&print,127);
   rmn[126]='\0';
   fprintf(stderr,"%s\n",&rmn[0]);
}

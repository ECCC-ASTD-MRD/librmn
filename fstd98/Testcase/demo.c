#include <stdio.h>

int intvar1, intvar2, erroth, errcod, ierr, nb;
char *name, *type, *typemax, *typemax2;

void ouvfrm() 
{
  int i;

  intvar1 =	14;
  intvar2 =	0;
  name    =	"fstdfile";
  type    =	"STD+RND";
  typemax  = "RND+OLD+R/O";
  typemax2="RND+OLD";
  
  for( i=0;i<10;i++ ){
    erroth  =	c_fnom(intvar1,name,typemax,intvar2);
    nb = c_fstouv( intvar1, typemax2);

    ierr = c_fstfrm(intvar1);
    errcod  = c_fclos(intvar1);
    printf("oth=%d, nb=%d, ierr=%d, errcod=%d\n", erroth,nb,ierr,errcod);
  }
 
  return 1;
}


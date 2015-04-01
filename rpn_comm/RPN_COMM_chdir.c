#include <rpnmacros.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <rmnlib.h>

/*ftnword f77name(RPN_COMM_chdir)(char *in_reper, int l1);*/

ftnword f77name(rpn_comm_chdir)(char *in_reper, int l1)
{
  int ierr, lng;
  char loc_reper[1024];


  lng = (l1 <= 1023) ? l1 : 1023;
  strncpy(loc_reper,in_reper,lng);
  loc_reper[lng] = '\0';
  while ((loc_reper[lng-1] == ' ') && (lng > 1)) {  /* strip trailing blanks */
      lng--;
      loc_reper[lng] = '\0';
      }
/*  fprintf(stderr,"reper %s \n",in_reper);
  fprintf(stderr,"reper %s \n",loc_reper);
*/
  ierr = chdir(loc_reper);

  return((ftnword) ierr);
       }

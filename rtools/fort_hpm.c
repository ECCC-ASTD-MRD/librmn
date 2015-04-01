#ifdef AIX
#include <libhpm.h>
#include <rpnmacros.h>
char *fstring_to_cstring();

/* quick and dirty fix to get rid once and for all of unsatisfied externals with
   a trailing underscore when using the IBM hpm package in -qextname mode.
   a new set of FORTRAN entry points has been created that call the equivalent
   C entry points. this also gets rid of the FORTRAN macro file f_hpm.h
   fort_hpminit replaces f_hpminit
   fort_hpmterminate replaces f_hpmterminate
   fort_hpmstart replaces f_hpmstart
   fort_hpmtstart replaces f_hpmtstart
   fort_hpmstop replaces f_hpmstop
   fort_hpmtstop replaces f_hpmtstop
   the calling sequences are unchanged
*/

void f77name(fort_hpminit)(ftnword *taskid, char *name, F2Cl lname)
{
  char *localname;

  localname=fstring_to_cstring(name,lname,1);
  hpmInit(*taskid,localname);
  free(localname);
}
void f77name(fort_hpmterminate)(ftnword *taskid)
{
  hpmTerminate(*taskid);
}
void f77name(fort_hpmstop)(ftnword *taskid)
{
  hpmStop(*taskid);
}
void f77name(fort_hpmtstop)(ftnword *taskid)
{
  hpmTstop(*taskid);
}
void f77name(fort_hpmstart)(ftnword *taskid, char *name, F2Cl lname)
{
  char *localname;

  localname=fstring_to_cstring(name,lname,1);
  hpmStart(*taskid,localname);
  free(localname);
}
void f77name(fort_hpmtstart)(ftnword *taskid, char *name, F2Cl lname)
{
  char *localname;

  localname=fstring_to_cstring(name,lname,1);
  hpmTstart(*taskid,localname);
  free(localname);
}
#endif

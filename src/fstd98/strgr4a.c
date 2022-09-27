/* M.Valin 2007 */
#include <stdint.h>

#include <rmn/rpnmacros.h>

void f77name(strgr4a)(char *strg, char *r4a, int32_t *posdeb, int32_t *posfin, F2Cl lstrg)
{
  int itrois=3;
  char *trois=(char *)&itrois;
  register int xor=*trois;
  int i;
  char *endstr=strg+lstrg;

    for(i=*posdeb; (i<=*posfin) && (strg<endstr) ;i++)
    {
      r4a[i^xor]=*strg ; strg++;   /* i^xor fait compter 0 1 2 3 big endian et 3 2 1 0 little endian */
    }
}

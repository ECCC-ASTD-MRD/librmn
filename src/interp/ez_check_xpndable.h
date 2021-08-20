#include "ezscint.h"
#include "ez_funcdef.h"

wordint c_ez_check_xpndable(int32_t *extension, int32_t ni, int32_t nj, char grtyp, int32_t ig1, int32_t ig2, int32_t ig3, int32_t ig4)
   {
   char lcl_grtyp;
   int32_t lcl_ig1, lcl_ig2, lcl_ig3, lcl_ig4;
   float swlat, swlon, dlat, dlon, last_lon, tolrnc_lon, extra_lon;
   
   extern void f77name(cigaxg)();

   lcl_grtyp = grtyp;
   lcl_ig1 = ig1, lcl_ig2 = ig2, lcl_ig3 = ig3; lcl_ig4 = ig4;
   

   f77name(cigaxg)(&lcl_grtyp,&swlat, &swlon, &dlat, &dlon,
                              &lcl_ig1, &lcl_ig2, &lcl_ig3, &lcl_ig4);
   
   if (swlon < 0)
      {
      swlon += 360.0;
      }
      
   last_lon = swlon + (dlon * (ni-1));
   extra_lon = last_lon + dlon;
   tolrnc_lon = fabs(360.0 - (extra_lon - swlon));
   if (tolrnc_lon < (dlon * 0.01))
      {
      *extension = 2;
      }
   else
      {
      *extension = 0;
      }
   
   }

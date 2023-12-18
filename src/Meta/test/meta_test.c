#include "Meta.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {

 	const char *output;
   double levels[4]= { 1000.0, 900.0, 800.0, 700.0 };
   json_object *prof_file,*prof_fld;

   setenv("ARMNLIB","/fs/homeu2/eccc/cmd/cmds/nil000/Projects/RPN/Meta",1);
   Meta_Init();

   // Load metadata template
   prof_fld=Meta_LoadProfile("field","0.1.0");
   prof_file=Meta_LoadProfile("file","0.1.0");

   /// Define metadata
   Meta_DefVar(prof_fld,"air_temperature","TT","air temperature","Air temperature is the bulk temperature of the air, not the surface (skin) temperature");
   Meta_DefSize(prof_fld,1000,2000,3);
   Meta_DefBound(prof_fld,-60,50,"celsius");
   Meta_DefForecastTime(prof_fld,1672556400,2,1230,"millisecond");   //2023-01-01T00:00:00
   Meta_DefHorizontalRef(prof_fld,"RPN_GDPS_2020_25KM",false);

   Meta_DefVerticalRef(prof_fld,"PRESSURE",levels,4,false);
   Meta_DefData(prof_fld,"float","lorenzo",32,32);
   Meta_AddCellMethod(prof_fld,"interpolation:linear");
   Meta_AddCellMethod(prof_fld,"filter:gaussian");
   Meta_AddCellMethod(prof_fld,"time:mean(interval 5 minute)");
   Meta_AddQualifier(prof_fld,"prognosis");
   Meta_AddQualifier(prof_fld,"prognosiss");                         // This should cause an error
   Meta_AddQualifier(prof_fld,"operational");
   Meta_AddQualifier(prof_fld,"member:12");
   Meta_AddQualifier(prof_fld,"centile>75");
   Meta_AddQualifier(prof_fld,"fdsscentile>75");                     // This should cause an error
   Meta_AddMissingValue(prof_fld,"out_of_domain",-999);
   Meta_AddMissingValue(prof_fld,"bad value",-998);

   // Extract metadata
   char  *c1,*c2,*c3,*c4;
   int    i,i1,i2,i3;
   double d1,d2;
   time_t t0;
   json_object *obj=NULL,*tok=NULL;

   Meta_GetVar(prof_fld,&c1,&c2,&c3,&c4);
   fprintf(stdout,"StandardName=%s\nRPNName=%s\nLongName=%s\nDescription=%s\n",c1,c2,c3,c4);
 
   Meta_GetSize(prof_fld,&i1,&i2,&i3);
   fprintf(stdout,"size(%i,%i,%i)\n",i1,i2,i3);

   Meta_GetBound(prof_fld,&d1,&d2,&c1);
   fprintf(stdout,"Min=%f\nMax=%f\nUnit=%s\n",d1,d2,c1);
 
   Meta_GetForecastTime(prof_fld,&t0,&i1,&d1,&c1);
   fprintf(stdout,"T0=%d\nStep=%i\nDuration=%f\nUnit=%s\n",t0,i1,d1,c1);
 
   Meta_GetVerticalRef(prof_fld,0,&c1,&d1);
   fprintf(stdout,"VerticalRef=%s\nValue=%f\n",c1,d1);
 
   Meta_GetHorizontalRef(prof_fld,&c1);
   fprintf(stdout,"HorizontalRef=%s\n",c1);
 
   Meta_GetData(prof_fld,&c1,&c2,&i1,&i2);
   fprintf(stdout,"Type=%s\nCompression=%s\nPacking=%i\nSize=%i",c1,c2,i1,i2);

   obj=Meta_GetObject(prof_fld,"/cell_methods");
   for(i=0;i<Meta_ArrayLength(obj);i++) {
      fprintf(stdout,"cell_method %i=%s\n",i,Meta_GetObjectString(Meta_ArrayGetObject(obj,i)));
   }
 
   if (Meta_ArrayFind(obj,"interpolation:linear")) {
     fprintf(stdout,"cell_method interpolation:linear found\n");
   }
   if (!Meta_ArrayFind(obj,"qqq")) {
     fprintf(stdout,"cell_method qqq not found\n");
   }
   if ((tok=Meta_ArrayFind(obj,"interpolation"))) {
     fprintf(stdout,"cell_method interpolation found: %s \n",Meta_GetObjectString(tok));
   }

   obj=Meta_GetObject(prof_fld,"/qualifiers");
   for(i=0;i<Meta_ArrayLength(obj);i++) {
      fprintf(stdout,"qualifier %i=%s\n",i,Meta_GetObjectString(Meta_ArrayGetObject(obj,i)));
   }
 
   obj=Meta_GetObject(prof_fld,"/missing_values");
   for(i=0;i<Meta_ArrayLength(obj);i++) {
      fprintf(stdout,"missing_value %i=%s\n",i,Meta_GetObjectString(Meta_ArrayGetObject(obj,i)));
   }

   Meta_GetMissingValue(prof_fld,0,&c1,&d1);
   fprintf(stdout,"reason =%s\nvalue=%f",c1,d1);

   Meta_ClearCellMethods(prof_fld);
   Meta_ClearQualifiers(prof_fld);
   Meta_ClearMissingValues(prof_fld);

   if (Meta_Equivalent(prof_fld,prof_fld)) {
      fprintf(stdout,"\nEquivalent\n");
   } else {
      fprintf(stdout,"\nNot Equivalent\n");
   }

   // Output formatted
   output=Meta_Stringify(prof_fld);
   printf("%s\n", output);

   // Output for saves
//   output=json_object_to_json_string_ext(prof_fld,JSON_C_TO_STRING_PLAIN);
//   printf("%s\n", output);

   Meta_DefFile(prof_file,"CMC","Meteorology","G100","GDPS 5.2.0","Global model");
   Meta_AddHorizontalRef(prof_file,"RPN_GDPS_2020_25KM",true);
   Meta_AddVerticalRef(prof_file,"PRESSURE",true);
   Meta_AddVerticalRef(prof_file,"SIGMA",true);
   output=Meta_Stringify(prof_file);
//   printf("%s\n", output);
 
   Meta_Free(prof_fld);
   // Write metadata
//   json_object_to_file_ext(filename,metafld,JSON_C_TO_STRING_PLAIN);
	exit(EXIT_SUCCESS);
}

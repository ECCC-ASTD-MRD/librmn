
#include <math.h>
#include <stdio.h>
#include <string.h>

#include <App.h>
#include <rmn/fst_file.h>
#include <rmn/Meta.h>

const char* test_file_name = "fst123_meta.fst";
json_object *prof_file,*prof_fld,*meta=NULL,*search_meta=NULL;
double levels[1]= { 1000.0 };


int test_fst23_interface(const int is_rsf) {

    // Create some arbitrary data field
    const int DATA_SIZE = 1024;
    float data[DATA_SIZE][DATA_SIZE];
    for (int i = 0; i < DATA_SIZE; i++) {
        for (int j = 0; j < DATA_SIZE; j++) {
            const float powi = pow((1.0 * i / DATA_SIZE), 2);
            const float powj = pow((1.0 * j / DATA_SIZE), 2);
            const float numerator = 1 + cos(12 * sqrt( powi + powj ));
            const float denominator = 0.5 * ( powi + powj ) + 2;
            data[i][j] = numerator / denominator;
        }
    }

    // Create file
    remove(test_file_name);
    const char* options1 = is_rsf ? "RND+R/W+RSF" : "RND+R/W";
    fst_file* test_file = fst23_open(test_file_name, options1);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open new test file with name %s and options %s\n", test_file_name, options1);
        return -1;
    }

   if (Meta_Init() != 1) return -1;

   // Load metadata template
   prof_fld=Meta_LoadProfile("field",NULL);
   prof_file=Meta_LoadProfile("file",NULL);

   search_meta=Meta_New();
   Meta_SetCellMethods(search_meta,(char*[2]){ "time:mean\\(interval 5 minute\\)",NULL });
//   Meta_SetCellMethods(search_meta,(char*[2]){ "time:mean(interval 5 minute)",NULL });
//   search_meta=Meta_Parse("{ \"rpn_name\" : \"TT\" }");

   fprintf(stderr,"Valid json:   %i\n", Meta_Is(prof_fld));
   fprintf(stderr,"Invalid json: %i\n", Meta_Is((json_object*)test_file));

   // Define file metadata
   Meta_DefFile(prof_file,"CMC","Weather","G100","GDPS-5.2.0","Global forecast at 15km","Operational");
   Meta_AddHorizontalRef(prof_file,"RPN_GDPS_2020_25KM",TRUE);
   Meta_AddVerticalRef(prof_file,"PRESSURE",TRUE);

   fprintf(stderr,"JSON: %s\n",Meta_Stringify(prof_file));

   // Define field metadata
   Meta_DefForecastTime(prof_fld,1672556400,2,1230,"millisecond"); //2023-01-01T00:00:00
   Meta_DefHorizontalRef(prof_fld,"RPN_GDPS_2020_25KM",FALSE);
   Meta_DefVerticalRef(prof_fld,"PRESSURE",levels,1,FALSE);

   Meta_SetCellMethods(prof_fld,(char*[4]){ "interpolation:linear","time:mean(interval 5 minute)","filter:gaussian",NULL });
//   Meta_AddCellMethod(prof_fld,"interpolation:linear");
//   Meta_AddCellMethod(prof_fld,"time:mean(interval 5 minute)");
//   Meta_AddCellMethod(prof_fld,"filter:gaussian");

   Meta_SetQualifiers(prof_fld,(char*[4]){ "prognosis","operational","tag:ETKGG22",NULL });
//   Meta_AddQualifier(prof_fld,"prognosis");
//   Meta_AddQualifier(prof_fld,"operational");
//   Meta_AddQualifier(prof_fld,"tag:ETKGG22");
   Meta_AddMissingValue(prof_fld,"out_of_domain",-999);

    // Write a record
   {

      fst_record file_meta = fst23_record_init(data,FST_TYPE_REAL,32,1,1,1);
      file_meta.npak = -32;
      file_meta.dateo = 0;
      file_meta.deet = 0;
      file_meta.npas = 0;
      file_meta.ip1  = 0;
      file_meta.ip2  = 0;
      file_meta.ip3  = 0;
      file_meta.ig1   = 0;
      file_meta.ig2   = 0;
      file_meta.ig3   = 0;
      file_meta.ig4   = 0;
      strcpy(file_meta.typvar, "X");
      strcpy(file_meta.nomvar, "JSON");
      strcpy(file_meta.etiket, "FILE_META");
      strcpy(file_meta.grtyp, "X");
      file_meta.metadata = prof_file;
      if (fst23_write(test_file, &file_meta,FALSE) < 0) {
         App_Log(APP_ERROR, "Unable to write record to new file %s\n", test_file_name);
         return -1;
      }
        
      fst_record record = fst23_record_init(data,FST_TYPE_REAL,32,DATA_SIZE,DATA_SIZE,1);
      record.npak = -32;
      int32_t date;
      Meta_StampEncode(&date,2022,06,10,0,0,0);
      record.dateo = date;
      record.deet = 300;
      record.npas = 0;
      record.ip1  = 1;
      record.ip2  = 1;
      record.ip3  = 1;
      record.ig1   = 1;
      record.ig2   = 2;
      record.ig3   = 3;
      record.ig4   = 4;
      strcpy(record.typvar, "P");
      strcpy(record.nomvar, "WAVE");
      strcpy(record.etiket, "float");
      strcpy(record.grtyp, "X");

      record.metadata = prof_fld;

      if (fst23_write(test_file, &record,FALSE) < 0) {
         App_Log(APP_ERROR, "Unable to write record to new file %s\n", test_file_name);
         return -1;
      }
      strcpy(record.nomvar, "Sun ");
      Meta_DefVar(prof_fld,"sun qquechose","Sun","fuiosdfsdf","sdfsd sef encore plus","hot");
      if (fst23_write(test_file, &record,FALSE) < 0) {
         App_Log(APP_ERROR, "Unable to write record to new file %s\n", test_file_name);
         return -1;
      }

      strcpy(record.nomvar, "Not ");
      Meta_DefVar(prof_fld,"Not qquechose","Not","fuiosdfsdf","sdfsd sef encore plus","cold");
      strcpy(record.typvar, "A");
      if (fst23_write(test_file, &record,FALSE) < 0) {
         App_Log(APP_ERROR, "Unable to write record to new file %s\n", test_file_name);
         return -1;
      }

      Meta_DefVar(prof_fld,"air_temperature","TT","air temperature","Air temperature is the bulk temperature of the air, not the surface (skin) temperature","celsius");
      Meta_To89(prof_fld,&record);
      if (fst23_write(test_file, &record,FALSE) < 0) {
         App_Log(APP_ERROR, "Unable to write record to new file %s\n", test_file_name);
         return -1;
      }
   }

    // Close the new file
   if (fst23_close(test_file) < 0) {
      App_Log(APP_ERROR, "Unable to close new file %s\n", test_file_name);
      return -1;
   }

   // Open existing file
   const char* options2 = "RND+R/O";
   test_file = fst23_open(test_file_name, options2);
   if (test_file == NULL) {
      App_Log(APP_ERROR, "Unable to open newly-created test file with name %s and options %s\n", test_file_name, options2);
      return -1;
   }

   {
      int32_t key;
      fst_record search_criteria = default_fst_record;
      fst_record search_extra = default_fst_record;
      fst_record record_find = default_fst_record;
      fst_record record = default_fst_record;
/*
      search_extra.ig1=68839;
      search_extra.ni=1024;
      search_extra.grtyp[0]='Z';
      fst23_set_search_criteria(test_file, &search_extra);
      fst23_find_next(test_file, &record);
      key=fst23_read(test_file, &record);
*/
      fst23_set_search_criteria(test_file, &record);
      fst23_find_next(test_file, &record);
      key=fst23_read(test_file, &record);
      meta=Meta_Parse(record.metadata);
      fprintf(stderr,"JSON: %s\n",Meta_Stringify(meta));    

      // Test find loop
      fprintf(stdout,"\nfind loop:\n");
      int num_found = 0;
      strcpy(search_criteria.typvar, "P");
      fst23_set_search_criteria(test_file, &search_criteria);
      while(key=fst23_find_next(test_file, &record_find)) {
         fst23_read(test_file,&record_find);
//         fst23_read_meta(test_file,&record_find);
         if (!(meta=Meta_Parse(record_find.metadata)))  {
            App_Log(APP_ERROR, "Metadata not found %s\n", test_file_name);
            return -1; 
         }
         if (Meta_Match(search_meta,meta,TRUE)) {      
            Meta_Resolve(meta,prof_file);
            fprintf(stderr,"JSON: %i %s\n",num_found,Meta_Stringify(meta));
            num_found++;
         }
      }

      if (num_found < 3) {
         fprintf(stderr, "Could not find all (3) records we should, found %i\n",num_found);
         return -1;
      }

      meta=Meta_LoadProfile("field",NULL);
      Meta_From89(meta,&record_find);
      fprintf(stderr,"JSON: %s\n",Meta_Stringify(meta));
   }

   if (fst23_close(test_file) < 0) {
      App_Log(APP_ERROR, "Unable to close file %s\n", test_file_name);
      return -1;
   }

   return 0;
}

int main(void) {

   App_Log(APP_INFO, "Testing RSF\n");
   if (test_fst23_interface(1) != 0) return -1; // RSF files

//    App_Log(APP_INFO, "Testing XDF\n");
//    if (test_fst23_interface(0) != 0) return -1; // XDF files

   App_Log(APP_INFO, "Tests successful\n");
   return 0;
}

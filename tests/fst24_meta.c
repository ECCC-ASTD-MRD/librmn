
#include <math.h>
#include <stdio.h>
#include <string.h>

#include <App.h>
#include <Meta.h>
#include <rmn/fst24_file.h>

const char* test_file_name = "fst123_meta.fst";
json_object *prof_file,*prof_fld,*meta=NULL,*search_meta=NULL;
double levels[1]= { 1000.0 };


int test_fst24_meta(void) {

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
    const char* options1 = "RND+R/W+RSF";
    fst_file* test_file = fst24_open(test_file_name, options1, 0);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to open new test file with name %s and options %s\n", test_file_name, options1);
        return -1;
    }

   // Load metadata template
   prof_fld=Meta_New(META_TYPE_FIELD,NULL);
   prof_file=Meta_New(META_TYPE_FILE,NULL);

   search_meta=Meta_NewObject();
//   Meta_SetCellMethods(search_meta,(char*[2]){ "time:mean\\(interval 5 minute\\)",NULL });
   Meta_SetCellMethods(search_meta,(char*[2]){ "time:mean(interval 5 minute)",NULL });
//   search_meta=Meta_Parse("{ \"rpn_name\" : \"TT\" }");
   fprintf(stderr,"Search JSON: %s\n",Meta_Stringify(search_meta));

   fprintf(stderr,"Valid json:   %i\n", Meta_Is(prof_fld));
   fprintf(stderr,"Invalid json: %i\n", Meta_Is((json_object*)test_file));

   // Define file metadata
   Meta_DefFile(prof_file,"CMC","Weather","G100","GDPS-5.2.0","Global forecast at 15km","Operational");
   Meta_AddHorizontalRef(prof_file,"RPN_GDPS_2020_25KM",TRUE);
   Meta_AddVerticalRef(prof_file,"PRESSURE",TRUE);

   fprintf(stderr,"File JSON: %s\n",Meta_Stringify(prof_file));

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

    // Write records
   {

      // Write file level metadata
      if (!Meta_WriteFile(test_file,prof_file)) {
         return(-1);
      }
        
      fst_record record = fst24_record_new(data,FST_TYPE_REAL,32,DATA_SIZE,DATA_SIZE,1);
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

      if (fst24_write(test_file, &record,FALSE) < 0) {
         App_Log(APP_ERROR, "Unable to write record to new file %s\n", test_file_name);
         return -1;
      }
      strcpy(record.nomvar, "Sun ");
      Meta_DefVar(prof_fld,"sun qquechose","Sun","fuiosdfsdf","sdfsd sef encore plus","hot");
      if (fst24_write(test_file, &record,FALSE) < 0) {
         App_Log(APP_ERROR, "Unable to write record to new file %s\n", test_file_name);
         return -1;
      }

      strcpy(record.nomvar, "Not ");
      Meta_DefVar(prof_fld,"Not qquechose","Not","fuiosdfsdf","sdfsd sef encore plus","cold");
      strcpy(record.typvar, "A");
      if (fst24_write(test_file, &record,FALSE) < 0) {
         App_Log(APP_ERROR, "Unable to write record to new file %s\n", test_file_name);
         return -1;
      }

//      Meta_DefVar(prof_fld,"air_temperature","TT","air temperature","Air temperature is the bulk temperature of the air, not the surface (skin) temperature","celsius");
      Meta_DefVarFromDict(prof_fld,"TT");
      Meta_To89(prof_fld,&record);
      if (fst24_write(test_file, &record,FALSE) < 0) {
         App_Log(APP_ERROR, "Unable to write record to new file %s\n", test_file_name);
         return -1;
      }
   }

   // Close the new file
   if (fst24_close(test_file) < 0) {
      App_Log(APP_ERROR, "Unable to close new file %s\n", test_file_name);
      return -1;
   }

   // Open existing file
   const char* options2 = "RND+R/O";
   test_file = fst24_open(test_file_name, options2, 0);
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

      // Read file level metadata
      if (Meta_ReadFile(test_file,&meta)) {
         fprintf(stderr,"File JSON: %s\n",Meta_Stringify(meta));    
      } else {
         App_Log(APP_ERROR, "Failed reading file metadata\n");
         return -1;    
      }

      // Test search on all keys
      search_extra.ig1=68839;
      search_extra.ni=1024;
      search_extra.grtyp[0]='Z';
      fst24_set_search_criteria(test_file, &search_extra);
      if (fst24_find_next(test_file, &record)) {
         fprintf(stderr,"Found search extra\n");    
         fst24_read_metadata(&record);
         Meta_Resolve(record.metadata,prof_file);
         fprintf(stderr,"Extra JSON: %s\n",Meta_Stringify(record.metadata));    
      } else {
         App_Log(APP_ERROR, "Failed search extra test\n");
         return -1;    
      }

      // Test find on extended metadata
      fprintf(stdout,"\nfind loop:\n");
      int num_found = 0;
      strcpy(search_criteria.typvar, "P");
      search_criteria.metadata=search_meta;
      fst24_set_search_criteria(test_file, &search_criteria);
      while(key=fst24_find_next(test_file, &record_find)) {
//         fst24_read_metadata(&record_find);
      
         if (!record_find.metadata)  {
            App_Log(APP_ERROR, "Metadata not found in file %s\n", test_file_name);
            return -1; 
         }
         if (TRUE || Meta_Match(search_meta,record_find.metadata,FALSE)) {
            fprintf(stderr,"Matched JSON: %i %s\n",num_found,Meta_Stringify(record_find.metadata));
            num_found++;
         }
      }

      if (num_found < 3) {
         fprintf(stderr, "Could not find all (3) records we should, found %i\n",num_found);
         return -1;
      }

      meta=Meta_New(META_TYPE_FIELD,NULL);
      Meta_From89(meta,&record_find);
      fprintf(stderr,"JSON: %s\n",Meta_Stringify(meta));
   }

   if (fst24_close(test_file) < 0) {
      App_Log(APP_ERROR, "Unable to close file %s\n", test_file_name);
      return -1;
   }

   return 0;
}

int main(void) {

   App_Log(APP_INFO, "Testing RSF\n");
   if (test_fst24_meta() != 0) return -1; // RSF files

   App_Log(APP_INFO, "Tests successful\n");
   return 0;
}

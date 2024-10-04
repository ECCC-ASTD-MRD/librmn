
#include <math.h>
#include <stdio.h>
#include <string.h>

#include <App.h>
#include <rmn.h>

const char* test_file_name = "test_fst24_meta_find.fst";
const int NBLEVEL=10;
const int NBVAR  =10;
const int NBSTEP =100;
double levels[NBLEVEL]= { 1000.0,900.0,800.0,700.0,600.0,500.0,400.0,300.0,200.0,100.0 };
char*  vars[NBVAR] = { "TT","UU","VV","WW","GZ","HU","HR","ME","LA","LO" };
json_object *prof_file,*prof_fld,*meta=NULL,*search_meta=NULL;

int test_fst24_create(int size) {


   // Create some arbitrary data field
   float data[size][size];
   for (int i = 0; i < size; i++) {
      for (int j = 0; j < size; j++) {
         const float powi = pow((1.0 * i / size), 2);
         const float powj = pow((1.0 * j / size), 2);
         const float numerator = 1 + cos(12 * sqrt( powi + powj ));
         const float denominator = 0.5 * ( powi + powj ) + 2;
         data[i][j] = numerator / denominator;
      }
   }

   // Create file
   remove(test_file_name);
   fst_file* test_file = fst24_open(test_file_name,"RND+R/W+RSF");
   if (test_file == NULL) {
      App_Log(APP_ERROR, "Unable to open new test file with name %s for writing\n",test_file_name);
      return -1;
   }

   // Load metadata template
   prof_fld=Meta_New(META_TYPE_RECORD,NULL);
   prof_file=Meta_New(META_TYPE_FILE,NULL);

   // Define file metadata
   Meta_DefFile(prof_file,"CMC","Weather","G100","GDPS-5.2.0","Global forecast at 15km","Operational");
   Meta_AddHorizontalRef(prof_file,"RPN_GDPS_2020_25KM",TRUE);
   Meta_AddVerticalRef(prof_file,"PRESSURE",TRUE);
//   fprintf(stderr,"File JSON: %s\n",Meta_Stringify(prof_file,JSON_C_TO_STRING_PRETTY));

   // Write file level metadata
   if (!Meta_WriteFile(test_file,prof_file)) {
      return(-1);
   }

   // Create records
   int32_t date;
   fst_record *record = fst24_record_new(data,FST_TYPE_REAL_IEEE,32,size,size,1);

   Meta_StampEncode(&date,2025,1,1,0,0,0);
   record->dateo     = date;
   record->pack_bits = 32;
   record->ig1       = 1;
   record->ig2       = 2;
   record->ig3       = 3;
   record->ig4       = 4;
   strcpy(record->typvar,"P");
   strcpy(record->grtyp,"X");
   
   Meta_AddMissingValue(prof_fld,"out_of_domain",-999); 

   for(int l=0;l<NBLEVEL;l++) {
      for(int v=0;v<NBVAR;v++) {
         for(int s=0;s<NBSTEP;s++) {

            App_Log(APP_INFO, "Creating record %s %i %f\n",vars[v],s,levels[l]);
            // Define field metadata
            Meta_DefVarFromDict(prof_fld,vars[v]);
            Meta_DefForecastTime(prof_fld,Meta_Stamp2Seconds(date),s,60,"second");
            Meta_DefHorizontalRef(prof_fld,"RPN_GDPS_2020_25KM",FALSE);
            Meta_DefVerticalRef(prof_fld,"PRESSURE",&levels[l],1,FALSE);

            if (v==0) Meta_SetCellMethods(prof_fld,(char*[4]){ "interpolation:linear","time:mean(interval 5 minute)","filter:gaussian",NULL });
            if (v==1) Meta_SetCellMethods(prof_fld,(char*[4]){ "interpolation:cubic","time:mean(interval 10 minute)","filter:mean",NULL });
            if (v>1) Meta_SetCellMethods(prof_fld,(char*[4]){ "interpolation:nearest","time:mean(interval 30 minute)","filter:linear",NULL });
            Meta_SetQualifiers(prof_fld,(char*[4]){ "prognosis","operational","tag:ETKGG22",NULL });

            // Define legacy metadata
            record->deet = 60;
            record->npas = s;
            record->ip1  = (int)levels[l];
            record->ip2  = 1;
            record->ip3  = 1;
            strcpy(record->nomvar, vars[v]);
            strcpy(record->etiket, "ETKGG22");

            record->metadata = prof_fld;

            if (fst24_write(test_file, record,FALSE) < 0) {
               App_Log(APP_ERROR, "Unable to write record to new file %s\n", test_file_name);
               return -1;
            }
         }
      }
   }

   // Close the new file
   if (fst24_close(test_file) < 0) {
      App_Log(APP_ERROR, "Unable to close new file %s\n", test_file_name);
      return -1;
   }
   return 0;
}

int test_fst24_search(void) {

   double level=100.0;

   search_meta=Meta_NewObject();
   Meta_DefVar(search_meta,NULL,"TT",NULL,NULL,NULL);
   Meta_DefFromTypVar(search_meta,"P");
   Meta_SetCellMethods(search_meta,(char*[2]){ "time:mean(interval 5 minute)",NULL });
   Meta_DefVerticalRef(search_meta,"PRESSURE",&level,1,FALSE);
   fprintf(stderr,"Search JSON: %s\n",Meta_Stringify(search_meta,JSON_C_TO_STRING_PRETTY));

   // Open existing file
   fst_file* test_file = fst24_open(test_file_name, "RND+R/O");
   if (test_file == NULL) {
      App_Log(APP_ERROR, "Unable to open newly-created test file with name %s for reading\n", test_file_name);
      return -1;
   }

   
      fst_record search_legacy = default_fst_record;
      fst_record search_new = default_fst_record;
      fst_record record_find = default_fst_record;
      fst_query* query =NULL;

      // Test find on legacy metadata
      fprintf(stdout,"\nfinding legacy:\n");
      int num_legacy = 0;
      strcpy(search_legacy.nomvar,"TT");
      strcpy(search_legacy.typvar,"P");
      search_legacy.ip1=(int)level;
      query = fst24_new_query(test_file, &search_legacy, NULL);

      TApp_Timer * const legacy_timer = App_TimerCreate();
      App_TimerStart(legacy_timer);

      while(fst24_find_next(query, &record_find) > 0) {
         num_legacy++;
      }
      App_TimerStop(legacy_timer);
      fprintf(stderr,"Legacy search time (%i): %.3f ms\n",num_legacy,App_TimerLatestTime_ms(legacy_timer));
      fst24_query_free(query);
   
      // Test find on new metadata
      fprintf(stdout,"\nfinding new:\n");
      int num_new = 0;
      search_new.metadata=search_meta;
      query = fst24_new_query(test_file, &search_new, NULL);

      TApp_Timer * const new_timer = App_TimerCreate();
      App_TimerStart(new_timer);

      while(fst24_find_next(query, &record_find) > 0) {
           num_new++;
      }
      App_TimerStop(new_timer);
      fprintf(stderr,"Legacy search time (%i): %.3f ms\n",num_new,App_TimerLatestTime_ms(new_timer));
      fst24_query_free(query);

 
   if (fst24_close(test_file) < 0) {
      App_Log(APP_ERROR, "Unable to close file %s\n", test_file_name);
      return -1;
   }

   return (num_legacy!=num_new);
}

int main(void) {

   App_Log(APP_INFO, "Testing meta search\n");
   if (test_fst24_create(10) != 0) return -1;
   if (test_fst24_search() != 0) return -1;

   App_Log(APP_INFO, "Tests successful\n");
   return 0;
}

#include "indexing.h"
#include "App.h"
#include "App_Timer.h"
#include "rmn.h"
#include "rmn/fnom.h"
    
RecordData *NewRecordDataNew(size_t nb_records)
{    
   int n=0,size=sizeof(RecordData);
   RecordData *data=NULL;
    
   if ((data=malloc(size*nb_records))) {

        data->nb_records=nb_records;
        data->ni = &data[(n+=sizeof(data->nb_records))];
        data->nj = &data[(n+=(nb_records*sizeof(*(data->ni))))];
        data->nk = &data[(n+=(nb_records*sizeof(*(data->nj))))];
        data->dateo = &data[(n+=(nb_records*sizeof(*(data->nk))))];
        data->deet = &data[(n+=(nb_records*sizeof(*(data->dateo))))];
        data->npas = &data[(n+=(nb_records*sizeof(*(data->deet))))];
        data->pack_bits = &data[(n+=(nb_records*sizeof(*(data->npas))))];
        data->data_bits = &data[(n+=(nb_records*sizeof(*(data->pack_bits))))];
        data->data_type = &data[(n+=(nb_records*sizeof(*(data->data_bits))))];
        data->ip1 = &data[(n+=(nb_records*sizeof(*(data->data_type))))];
        data->ip2 = &data[(n+=(nb_records*sizeof(*(data->ip1))))];
        data->ip3 = &data[(n+=(nb_records*sizeof(*(data->ip2))))];

        data->typvar = &data[(n+=(nb_records*sizeof(*(data->ip3))))];
        data->nomvar = &data[(n+=(nb_records*FST_TYPVAR_LEN * sizeof(char)))];
        data->etiket = &data[(n+=(nb_records*FST_NOMVAR_LEN * sizeof(char)))];
        data->grtyp = &data[(n+=(nb_records*FST_ETIKET_LEN * sizeof(char)))];
        data->path = &data[(n+=(nb_records*FST_GTYP_LEN * sizeof(char)))];

        data->ig1 = &data[(n+=(nb_records*(PATH_MAX+1) * sizeof(char)))];
        data->ig2 = &data[(n+=(nb_records*sizeof(*(data->ig1))))];
        data->ig3 = &data[(n+=(nb_records*sizeof(*(data->ig2))))];
        data->ig4 = &data[(n+=(nb_records*sizeof(*(data->ig3))))];

        data->file_index = &data[(n+=(nb_records*sizeof(*(data->ig4))))];
   }
   return data;
}

RecordData *NewRecordData(size_t nb_records)
{    
    RecordData *data = malloc(sizeof(*data));
    data->ni = malloc(nb_records*sizeof(*(data->ni)));
    data->nj = malloc(nb_records*sizeof(*(data->nj)));
    data->nk = malloc(nb_records*sizeof(*(data->nk)));
    data->dateo = malloc(nb_records*sizeof(*(data->dateo)));
    data->deet = malloc(nb_records*sizeof(*(data->deet)));
    data->npas = malloc(nb_records*sizeof(*(data->npas)));
    data->pack_bits = malloc(nb_records*sizeof(*(data->pack_bits)));
    data->data_bits = malloc(nb_records*sizeof(*(data->data_bits)));
    data->data_type = malloc(nb_records*sizeof(*(data->data_type)));
    data->ip1 = malloc(nb_records*sizeof(*(data->ip1)));
    data->ip2 = malloc(nb_records*sizeof(*(data->ip2)));
    data->ip3 = malloc(nb_records*sizeof(*(data->ip3)));

    data->typvar = malloc(nb_records*FST_TYPVAR_LEN * sizeof(char));
    data->nomvar = malloc(nb_records*FST_NOMVAR_LEN * sizeof(char));
    data->etiket = malloc(nb_records*FST_ETIKET_LEN * sizeof(char));
    data->grtyp = malloc(nb_records*FST_GTYP_LEN * sizeof(char));
    data->path = malloc(nb_records*(PATH_MAX+1) * sizeof(char));

    data->ig1 = malloc(nb_records*sizeof(*(data->ig1)));
    data->ig2 = malloc(nb_records*sizeof(*(data->ig2)));
    data->ig3 = malloc(nb_records*sizeof(*(data->ig3)));
    data->ig4 = malloc(nb_records*sizeof(*(data->ig4)));

    data->file_index = malloc(nb_records*sizeof(*(data->file_index)));

    // Check if realloc was successful
    if (data->ni == NULL || data->nj == NULL || data->nk == NULL ||
        data->dateo == NULL || data->deet == NULL || data->npas == NULL ||
        data->pack_bits == NULL || data->data_type == NULL || data->ip1 == NULL ||
        data->ip2 == NULL || data->ip3 == NULL || data->typvar == NULL ||
        data->nomvar == NULL || data->etiket == NULL || data->grtyp == NULL ||
        data->ig1 == NULL || data->ig2 == NULL || data->ig3 == NULL ||
        data->ig4 == NULL || data->path == NULL){
        return NULL;
    }

    return data;
}

RecordData *rmn_get_index_columns_raw(const char **filenames, int nb_files)
{
    TApp_Timer t; App_TimerInit(&t);
    RecordData *raw_columns = NULL;
    int i,total_nb_records = 0;

    #define MAXFILES 4096
    
    fst_file *f[MAXFILES];
    int     nb[MAXFILES],p[MAXFILES],pos[MAXFILES];

nb_files=2;
    memset(f,0x0,MAXFILES*sizeof(fst_file*));
    fprintf(stderr, "===> Collecting all records ...\n");
    App_TimerStart(&t);

   // Open all files in parallel
   #pragma omp parallel for default(none) private(i) shared(nb_files,filenames,f,nb)
   for(i = 0; i < nb_files; i++){
      f[i] = fst24_open(filenames[i],"RND+R/O");
      if(!f[i]) {
         App_Log(APP_ERROR,"%s: Unable to open file %s\n", __func__, filenames[i]);
         nb[i]=0;    
         exit; 
         continue;
      }
      nb[i]=fst24_get_num_records(f[i]);     
   }
   App_TimerStop(&t);
   fprintf(stderr, ".... Openning files took %f ms\n", App_TimerTime_ms(&t));

   // Calculate indexing position for each file
   pos[0]=0;
   for(i = 1; i < nb_files; i++){
      pos[i]=pos[i-1]+nb[i-1];    
   }
   total_nb_records=pos[i-1]+nb[i-1];
   fprintf(stderr, "%d\n", total_nb_records);

   App_TimerStart(&t);

   fprintf(stderr, "===> Reading record matadata into columns for DataFrame\n");

   if (!(raw_columns = NewRecordData(total_nb_records))) {
      return NULL;
   }

   fst_query *q=NULL;
   fst_record result = default_fst_record;
   int n=0;
   #pragma omp parallel for default(none) private(i,n,q,result) shared(stderr,nb_files,f,nb,pos,raw_columns,filenames)
   for(int i = 0; i < nb_files; i++){
      fprintf(stderr, "Parsing file %s\n",f[i]);
      n=pos[i];
      if (!(q = fst24_new_query(f[i], &default_fst_record, NULL))) {
        exit;
      }
      while(fst24_find_next(q, &result)){
         raw_columns->ni[i] = result.ni;
         raw_columns->nj[i] = result.nj;
         raw_columns->nk[i] = result.nk;
         raw_columns->dateo[i] = result.dateo;
         raw_columns->deet[i] = result.deet;
         raw_columns->npas[i] = result.npas;
         raw_columns->pack_bits[i] = result.pack_bits;
         raw_columns->data_type[i] = result.data_type;

         raw_columns->ip1[i] = result.ip1;
         raw_columns->ip2[i] = result.ip2;
         raw_columns->ip3[i] = result.ip3;

         // Remove 'trm' if we want to keep the trailing spaces
         strncpy(raw_columns->typvar + i*FST_TYPVAR_LEN, result.typvar, FST_TYPVAR_LEN);
         strncpy(raw_columns->nomvar + i*FST_NOMVAR_LEN, result.nomvar, FST_NOMVAR_LEN);
         strncpy(raw_columns->etiket + i*FST_ETIKET_LEN, result.etiket, FST_ETIKET_LEN);
         strncpy(raw_columns->grtyp  + i*FST_GTYP_LEN, result.grtyp, FST_GTYP_LEN);
         // Discuss with JP how we can maintain the filepath association with
         // the records.
         strncpy(raw_columns->path + i*(PATH_MAX+1), filenames[i], PATH_MAX);

         raw_columns->ig1[i] = result.ig1;
         raw_columns->ig2[i] = result.ig2;
         raw_columns->ig3[i] = result.ig3;
         raw_columns->ig4[i] = result.ig4;

         raw_columns->file_index[i] = result.file_index;
     }

     fst24_query_free(q);
     fst24_close(f[i]);
   }

   App_TimerStop(&t);
 
   raw_columns->nb_records = total_nb_records; // TODO SHould be set in RecordDataNew obviously

   fprintf(stderr, ".... Reorganizing data into columns for DataFrame took %f ms\n", App_TimerTime_ms(&t));

   return raw_columns;
}

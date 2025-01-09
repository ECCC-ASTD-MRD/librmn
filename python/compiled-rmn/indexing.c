#include "indexing.h"
#include "App.h"
#include "App_Timer.h"
#include "rmn.h"
#include "rmn/fnom.h"
    
RecordData *NewRecordData(size_t nb_records) {    

   uint32_t n=0,size=17*sizeof(int32_t)+(FST_TYPVAR_LEN+FST_NOMVAR_LEN+FST_ETIKET_LEN+FST_GTYP_LEN)*sizeof(char);
   char *data=NULL;
   RecordData *rdata=NULL;

   if ((data=(char*)malloc(size*nb_records)) &&
       (rdata=(RecordData*)malloc(sizeof(RecordData)))) {
   
      rdata->nb_records=nb_records;
      rdata->ni = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));
      rdata->nj = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));
      rdata->nk = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));

      rdata->dateo = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));
      rdata->deet = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));
      rdata->npas = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));
      rdata->pack_bits = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));
      rdata->data_bits = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));
      rdata->data_type = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));
      rdata->ip1 = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));
      rdata->ip2 = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));
      rdata->ip3 = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));

      rdata->typvar = (char*)(&data[n]);n+=(nb_records*FST_TYPVAR_LEN * sizeof(char));
      rdata->nomvar = (char*)(&data[n]);n+=(nb_records*FST_NOMVAR_LEN * sizeof(char));
      rdata->etiket = (char*)(&data[n]);n+=(nb_records*FST_ETIKET_LEN * sizeof(char));
      rdata->grtyp = (char*)(&data[n]);n+=(nb_records*FST_GTYP_LEN * sizeof(char));

      rdata->ig1 = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));
      rdata->ig2 = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));
      rdata->ig3 = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));
      rdata->ig4 = (uint32_t*)(&data[n]);n+=(nb_records*sizeof(uint32_t));

      rdata->file_index = (uint32_t*)(&data[n]);
   }
   return(rdata);
}

#define MAXFILES 4096

RecordData *rmn_get_index_columns_raw(const char **filenames, int nb_files)
{
   TApp_Timer t; App_TimerInit(&t);
   RecordData *raw_columns,*lraw[MAXFILES];
   int i,n,total_nb_records = 0;
 
   fst_file *f[MAXFILES];
   int     nb[MAXFILES],pos[MAXFILES];
   fst_query *q=NULL;
   fst_record result;

   App_TimerStart(&t);

   // Open all files in parallel
   #pragma omp parallel for ordered default(none) private(i,n,q,result) shared(filenames,nb_files,f,nb,lraw,default_fst_record)
   for(i = 0; i < nb_files; i++){
      f[i] = fst24_open(filenames[i],"RND+R/O");
      if(!f[i]) {
         App_Log(APP_ERROR,"%s: Unable to open file %s\n", __func__, filenames[i]);
         nb[i]=0;
         continue;
      }

      nb[i]=fst24_get_num_records(f[i]);     
      if (!nb[i] || !(lraw[i] = NewRecordData(nb[i]))) {
         nb[i]=0;
         continue;
      }

      result = default_fst_record;
      n=0;
      if (!(q = fst24_new_query(f[i], &default_fst_record, NULL))) {
        nb[i]=0;
        continue;
      }
      while(fst24_find_next(q, &result)){
         lraw[i]->ni[n] = result.ni;
         lraw[i]->nj[n] = result.nj;
         lraw[i]->nk[n] = result.nk;
         lraw[i]->dateo[n] = result.dateo;
         lraw[i]->deet[n] = result.deet;
         lraw[i]->npas[n] = result.npas;
         lraw[i]->pack_bits[n] = result.pack_bits;
         lraw[i]->data_bits[n] = result.data_bits;
         lraw[i]->data_type[n] = result.data_type;

         lraw[i]->ip1[n] = result.ip1;
         lraw[i]->ip2[n] = result.ip2;
         lraw[i]->ip3[n] = result.ip3;

         strncpy(lraw[i]->typvar + n*FST_TYPVAR_LEN, result.typvar, FST_TYPVAR_LEN);
         strncpy(lraw[i]->nomvar + n*FST_NOMVAR_LEN, result.nomvar, FST_NOMVAR_LEN);
         strncpy(lraw[i]->etiket + n*FST_ETIKET_LEN, result.etiket, FST_ETIKET_LEN);
         strncpy(lraw[i]->grtyp  + n*FST_GTYP_LEN, result.grtyp, FST_GTYP_LEN);

         lraw[i]->ig1[n] = result.ig1;
         lraw[i]->ig2[n] = result.ig2;
         lraw[i]->ig3[n] = result.ig3;
         lraw[i]->ig4[n] = result.ig4;

         lraw[i]->file_index[n] = result.file_index;
         n++;
     }

     fst24_query_free(q);
     fst24_close(f[i]);
   }
   App_TimerStop(&t);
   fprintf(stderr, ".... Gathering metadata took %f ms\n", App_TimerTotalTime_ms(&t));

   // Calculate indexing position for each file
   pos[0]=0;
   for(i = 1; i < nb_files; i++){
      pos[i]=pos[i-1]+nb[i-1]-1;    
   }
   total_nb_records=pos[i-1]+nb[i-1];

   App_TimerInit(&t);
   App_TimerStart(&t);

   if (!(raw_columns = NewRecordData(total_nb_records))) {
      return NULL;
   }

   #pragma omp parallel for ordered default(none) private(i) shared(nb_files,f,nb,pos,raw_columns,lraw)
   for(int i = 0; i < nb_files; i++){

      if (!nb[i]) continue;

        memcpy(&raw_columns->ni[pos[i]],lraw[i]->ni,nb[i]*sizeof(int32_t));
        memcpy(&raw_columns->nj[pos[i]],lraw[i]->nj,nb[i]*sizeof(int32_t));
        memcpy(&raw_columns->nk[pos[i]],lraw[i]->nk,nb[i]*sizeof(int32_t));
        memcpy(&raw_columns->dateo[pos[i]],lraw[i]->dateo,nb[i]*sizeof(int32_t));
        memcpy(&raw_columns->deet[pos[i]],lraw[i]->deet,nb[i]*sizeof(int32_t));
        memcpy(&raw_columns->npas[pos[i]],lraw[i]->npas,nb[i]*sizeof(int32_t));
        memcpy(&raw_columns->pack_bits[pos[i]],lraw[i]->pack_bits,nb[i]*sizeof(int32_t));
        memcpy(&raw_columns->data_bits[pos[i]],lraw[i]->data_bits,nb[i]*sizeof(int32_t));
        memcpy(&raw_columns->data_type[pos[i]],lraw[i]->data_type,nb[i]*sizeof(int32_t));

        memcpy(&raw_columns->ip1[pos[i]],lraw[i]->ip1,nb[i]*sizeof(int32_t));
        memcpy(&raw_columns->ip2[pos[i]],lraw[i]->ip2,nb[i]*sizeof(int32_t));
        memcpy(&raw_columns->ip3[pos[i]],lraw[i]->ip3,nb[i]*sizeof(int32_t));

        memcpy(&raw_columns->typvar[pos[i]],lraw[i]->typvar,nb[i]*FST_TYPVAR_LEN);
        memcpy(&raw_columns->nomvar[pos[i]],lraw[i]->nomvar,nb[i]*FST_NOMVAR_LEN);
        memcpy(&raw_columns->etiket[pos[i]],lraw[i]->etiket,nb[i]*FST_ETIKET_LEN);
        memcpy(&raw_columns->grtyp[pos[i]],lraw[i]->grtyp  ,nb[i]*FST_GTYP_LEN);

        memcpy(&raw_columns->ig1[pos[i]],lraw[i]->ig1,nb[i]*sizeof(int32_t));
        memcpy(&raw_columns->ig2[pos[i]],lraw[i]->ig2,nb[i]*sizeof(int32_t));
        memcpy(&raw_columns->ig3[pos[i]],lraw[i]->ig3,nb[i]*sizeof(int32_t));
        memcpy(&raw_columns->ig4[pos[i]],lraw[i]->ig4,nb[i]*sizeof(int32_t));

        memcpy(&raw_columns->file_index[pos[i]],lraw[i]->file_index,nb[i]*sizeof(int32_t));
   }

   App_TimerStop(&t);
 
   fprintf(stderr, ".... Reorganizing data into columns for DataFrame took %f ms\n", App_TimerTotalTime_ms(&t));

   return raw_columns;
}

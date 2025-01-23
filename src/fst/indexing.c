#include "indexing.h"
#include "App.h"
#include "App_Timer.h"
#include "rmn.h"
#include "rmn/fnom.h"

RecordData *NewRecordData(size_t nb_records)
{

    // uint32_t n = 0, size = 23 * sizeof(int32_t) + (FST_TYPVAR_LEN + FST_NOMVAR_LEN + FST_ETIKET_LEN + FST_GTYP_LEN + FST_GRID_LEN + FST_LABEL_LEN + FST_RUN_LEN + FST_IMPLEMENTATION_LEN + FST_ENSEMBLE_MEMBER_LEN + FST_ETIKET_FORMAT_LEN + FST_DATA_TYPE_STR_LEN + FST_INTERVAL_IP_LEN + FST_PATH_LEN + FST_GRID_DESC_LEN) * sizeof(char) + 6 * sizeof(float) + 13 * sizeof(bool) + sizeof(size_t);
    uint32_t n = 0, size = 17 * sizeof(int32_t) +
                           (FST_TYPVAR_LEN + FST_NOMVAR_LEN + FST_ETIKET_LEN + FST_GTYP_LEN + FST_PATH_LEN) * sizeof(char);
    char *data = NULL;
    RecordData *rdata = NULL;

    if ((data = (char *)malloc(size * nb_records)) &&
        (rdata = (RecordData *)malloc(sizeof(RecordData))))
    {

        rdata->nb_records = nb_records;

        rdata->nomvar = (char *)(&data[n]);
        n += (nb_records * FST_NOMVAR_LEN * sizeof(char));
        rdata->typvar = (char *)(&data[n]);
        n += (nb_records * FST_TYPVAR_LEN * sizeof(char));
        rdata->etiket = (char *)(&data[n]);
        n += (nb_records * FST_ETIKET_LEN * sizeof(char));

        rdata->ni = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));
        rdata->nj = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));
        rdata->nk = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));

        rdata->dateo = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));

        rdata->ip1 = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));
        rdata->ip2 = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));
        rdata->ip3 = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));

        rdata->deet = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));
        rdata->npas = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));

        rdata->data_type = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));
        rdata->pack_bits = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));
        rdata->data_bits = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));

        rdata->grtyp = (char *)(&data[n]);
        n += (nb_records * FST_GTYP_LEN * sizeof(char));

        rdata->ig1 = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));
        rdata->ig2 = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));
        rdata->ig3 = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));
        rdata->ig4 = (uint32_t *)(&data[n]);
        n += (nb_records * sizeof(uint32_t));

        // Add path field
        rdata->path = (char *)(&data[n]);
        n += (nb_records * FST_PATH_LEN * sizeof(char));

        rdata->file_index = (uint32_t *)(&data[n]);
        // n += (nb_records * sizeof(uint32_t));
    }
    return (rdata);
}

#define MAXFILES 4096

// High-performance string copy with SIMD and cache optimization
static inline void safe_string_copy(char *dest, const char *src, size_t dest_offset, size_t src_offset, size_t length)
{
    // Use 64-bit wide copies for better performance
    uint64_t *dest64 = (uint64_t *)(dest + dest_offset);
    const uint64_t *src64 = (const uint64_t *)(src + src_offset);

    // Copy 64-bit chunks
    size_t i = 0;
    size_t vec_length = length / sizeof(uint64_t);

    // Prefetch source and destination for better cache performance
    __builtin_prefetch(src64, 0, 1);
    __builtin_prefetch(dest64, 1, 1);

    // Vectorized copy of 64-bit chunks
    for (; i < vec_length; i++)
    {
        dest64[i] = src64[i];
    }

    // Handle remaining bytes
    char *dest_tail = dest + dest_offset;
    const char *src_tail = src + src_offset;
    for (size_t j = i * sizeof(uint64_t); j < length; j++)
    {
        dest_tail[j] = src_tail[j];
    }

    // Ensure null-termination for the last string
    dest[dest_offset + length - 1] = '\0';
}

RecordData *rmn_get_index_columns_raw(const char **filenames, int nb_files)
{
    TApp_Timer t;
    if(nb_files < 0 || MAXFILES < nb_files){
        Lib_Log(APP_LIBRMN, APP_ERROR, "%s() nb_files=%d is negative or greater than MAXFILES=%d\n", __func__, nb_files, MAXFILES);
        return NULL;
    }
    App_TimerInit(&t);
    RecordData *raw_columns, *lraw[MAXFILES];
    int i, n, total_nb_records = 0;

    fst_file *f[MAXFILES];
    int nb[MAXFILES], pos[MAXFILES];
    fst_query *q = NULL;
    fst_record result;
    fst_record def = default_fst_record;

    App_TimerStart(&t);

// Open all files in parallel
#pragma omp parallel for ordered default(none) private(i, n, q, result) shared(filenames, nb_files, f, nb, lraw, def)
    for (i = 0; i < nb_files; i++)
    {
        f[i] = fst24_open(filenames[i], "RND+R/O");
        if (!f[i])
        {
            App_Log(APP_ERROR, "%s: Unable to open file %s\n", __func__, filenames[i]);
            nb[i] = 0;
            continue;
        }

        nb[i] = fst24_get_num_records(f[i]);
        if (!nb[i] || !(lraw[i] = NewRecordData(nb[i])))
        {
            nb[i] = 0;
            continue;
        }

        result = default_fst_record;
        n = 0;
        if (!(q = fst24_new_query(f[i], &default_fst_record, NULL)))
        {
            nb[i] = 0;
            continue;
        }
        while (fst24_find_next(q, &result))
        {
            strncpy(lraw[i]->nomvar + n * FST_NOMVAR_LEN, result.nomvar, FST_NOMVAR_LEN);
            strncpy(lraw[i]->typvar + n * FST_TYPVAR_LEN, result.typvar, FST_TYPVAR_LEN);
            strncpy(lraw[i]->etiket + n * FST_ETIKET_LEN, result.etiket, FST_ETIKET_LEN);

            // Copy the file path
            strncpy(lraw[i]->path + n * FST_PATH_LEN, filenames[i], FST_PATH_LEN - 1);
            // Ensure null-termination
            lraw[i]->path[n * FST_PATH_LEN + FST_PATH_LEN - 1] = '\0';

            lraw[i]->ni[n] = result.ni;
            lraw[i]->nj[n] = result.nj;
            lraw[i]->nk[n] = result.nk;
            lraw[i]->dateo[n] = result.dateo;

            lraw[i]->ip1[n] = result.ip1;
            lraw[i]->ip2[n] = result.ip2;
            lraw[i]->ip3[n] = result.ip3;

            lraw[i]->deet[n] = result.deet;
            lraw[i]->npas[n] = result.npas;

            lraw[i]->pack_bits[n] = result.pack_bits;
            lraw[i]->data_bits[n] = result.data_bits;
            lraw[i]->data_type[n] = result.data_type;

            strncpy(lraw[i]->grtyp + n * FST_GTYP_LEN, result.grtyp, FST_GTYP_LEN);

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
    pos[0] = 0;
    for (i = 1; i < nb_files; i++)
    {
        pos[i] = pos[i - 1] + nb[i - 1] - 1;
    }
    total_nb_records = pos[i - 1] + nb[i - 1];

    App_TimerInit(&t);
    App_TimerStart(&t);

    if (!(raw_columns = NewRecordData(total_nb_records)))
    {
        return NULL;
    }

#pragma omp parallel for ordered default(none) private(i) shared(nb_files, f, nb, pos, raw_columns, lraw)
    for (int i = 0; i < nb_files; i++)
    {

        if (!nb[i])
            continue;

        // Use safe_string_copy for string fields
        safe_string_copy(raw_columns->typvar, lraw[i]->typvar, pos[i] * FST_TYPVAR_LEN, 0, nb[i] * FST_TYPVAR_LEN);
        safe_string_copy(raw_columns->nomvar, lraw[i]->nomvar, pos[i] * FST_NOMVAR_LEN, 0, nb[i] * FST_NOMVAR_LEN);
        safe_string_copy(raw_columns->etiket, lraw[i]->etiket, pos[i] * FST_ETIKET_LEN, 0, nb[i] * FST_ETIKET_LEN);
        safe_string_copy(raw_columns->grtyp, lraw[i]->grtyp, pos[i] * FST_GTYP_LEN, 0, nb[i] * FST_GTYP_LEN);
        safe_string_copy(raw_columns->path, lraw[i]->path, pos[i] * FST_PATH_LEN, 0, nb[i] * FST_PATH_LEN);

        memcpy(&raw_columns->ni[pos[i]], lraw[i]->ni, nb[i] * sizeof(int32_t));
        memcpy(&raw_columns->nj[pos[i]], lraw[i]->nj, nb[i] * sizeof(int32_t));
        memcpy(&raw_columns->nk[pos[i]], lraw[i]->nk, nb[i] * sizeof(int32_t));

        memcpy(&raw_columns->dateo[pos[i]], lraw[i]->dateo, nb[i] * sizeof(int32_t));

        memcpy(&raw_columns->ip1[pos[i]], lraw[i]->ip1, nb[i] * sizeof(int32_t));
        memcpy(&raw_columns->ip2[pos[i]], lraw[i]->ip2, nb[i] * sizeof(int32_t));
        memcpy(&raw_columns->ip3[pos[i]], lraw[i]->ip3, nb[i] * sizeof(int32_t));

        memcpy(&raw_columns->deet[pos[i]], lraw[i]->deet, nb[i] * sizeof(int32_t));
        memcpy(&raw_columns->npas[pos[i]], lraw[i]->npas, nb[i] * sizeof(int32_t));
        memcpy(&raw_columns->pack_bits[pos[i]], lraw[i]->pack_bits, nb[i] * sizeof(int32_t));
        memcpy(&raw_columns->data_bits[pos[i]], lraw[i]->data_bits, nb[i] * sizeof(int32_t));
        memcpy(&raw_columns->data_type[pos[i]], lraw[i]->data_type, nb[i] * sizeof(int32_t));

        memcpy(&raw_columns->ig1[pos[i]], lraw[i]->ig1, nb[i] * sizeof(int32_t));
        memcpy(&raw_columns->ig2[pos[i]], lraw[i]->ig2, nb[i] * sizeof(int32_t));
        memcpy(&raw_columns->ig3[pos[i]], lraw[i]->ig3, nb[i] * sizeof(int32_t));
        memcpy(&raw_columns->ig4[pos[i]], lraw[i]->ig4, nb[i] * sizeof(int32_t));

        memcpy(&raw_columns->file_index[pos[i]], lraw[i]->file_index, nb[i] * sizeof(int32_t));
    }

    App_TimerStop(&t);

    fprintf(stderr, ".... Reorganizing data into columns for DataFrame took %f ms\n", App_TimerTotalTime_ms(&t));

    return raw_columns;
}

void print_record(RecordData *data)
{
    for (int i = 0; i < data->nb_records; i++)
    {
        fprintf(stderr, "%.4s %.2s %.12s %5d %5d %5d %9d %9d %9d %9d %4d %4d %3d %3d %5d %.1s %6d %6d %6d %6d %6d %s\n",
                &data->nomvar[i * FST_NOMVAR_LEN],
                &data->typvar[i * FST_TYPVAR_LEN],
                &data->etiket[i * FST_ETIKET_LEN],
                data->ni[i],
                data->nj[i],
                data->nk[i],
                data->dateo[i],
                data->ip1[i],
                data->ip2[i],
                data->ip3[i],
                data->deet[i],
                data->npas[i],
                data->data_type[i],
                data->pack_bits[i],
                data->data_bits[i],
                &data->grtyp[i * FST_GTYP_LEN],
                data->ig1[i],
                data->ig2[i],
                data->ig3[i],
                data->ig4[i],
                data->file_index[i],
                &data->path[i * FST_PATH_LEN]);
    }
}


int get_opdict_metadata(char *rpn_name, char *result, size_t result_buffer_size)
{
    json_object *prof_fld = Meta_New(META_TYPE_RECORD, NULL);
    if(prof_fld == NULL){
        Lib_Log(APP_LIBRMN, APP_ERROR, "%s(): Meta_New: Failed to get JSON object\n", __func__);
        return 1;
    }

    json_object *def_var = Meta_DefVarFromDict(prof_fld, rpn_name);
    if(def_var == NULL){
        Lib_Log(APP_LIBRMN, APP_ERROR, "%s(): Could not get var from dict for '%s'\n", __func__, rpn_name);
        json_object_put(prof_fld);
        return 1;
    }

    char *json_str = Meta_Stringify(def_var, JSON_C_TO_STRING_PLAIN);
    if(json_str == NULL){
        Lib_Log(APP_LIBRMN, APP_DEBUG, "%s(): Could not serialize JSON object for var '%s'\n", __func__, rpn_name);
        result[0] = '\0';
        json_object_put(prof_fld);
        return 0;
    }

    size_t result_size = strlen(json_str) + 1;
    Lib_Log(APP_LIBRMN, APP_DEBUG, "%s(): result_buffer_size: %d, result_size: %d\n", __func__, result_buffer_size, result_size);
    if(result_buffer_size < result_size) {
        Lib_Log(APP_LIBRMN, APP_ERROR, "%s(): provided buffer size %d is too small to contain result of size %d\n", __func__, result_buffer_size, result_size);
        json_object_put(prof_fld);
        return 1;
    }

    strncpy(result, json_str, result_size);

    // Doing free(json_str) causes double-free if we also have
    // json_object_put(prof_fld) which shows that it frees the
    // string.  We also get a segfault if we do json_object_put(def_var)
    // and json_object_put(prof_fld) so we just need to free
    // the root one.
    json_object_put(prof_fld);
    return 0;
}

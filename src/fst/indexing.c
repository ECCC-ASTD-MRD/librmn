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

        // rdata->grid = (char *)(&data[n]);
        // n += (nb_records * FST_GRID_LEN * sizeof(char));

        // rdata->label = (char *)(&data[n]);
        // n += (nb_records * FST_LABEL_LEN * sizeof(char));

        // rdata->run = (char *)(&data[n]);
        // n += (nb_records * FST_RUN_LEN * sizeof(char));

        // rdata->implementation = (char *)(&data[n]);
        // n += (nb_records * FST_IMPLEMENTATION_LEN * sizeof(char));

        // rdata->ensemble_member = (char *)(&data[n]);
        // n += (nb_records * FST_ENSEMBLE_MEMBER_LEN * sizeof(char));

        // rdata->etiket_format = (char *)(&data[n]);
        // n += (nb_records * FST_ETIKET_FORMAT_LEN * sizeof(char));

        // rdata->date_of_observation = (uint32_t *)(&data[n]);
        // n += (nb_records * sizeof(uint32_t));

        // rdata->date_of_validity = (uint32_t *)(&data[n]);
        // n += (nb_records * sizeof(uint32_t));

        // rdata->forecast_hour = (float *)(&data[n]);
        // n += (nb_records * sizeof(float));

        // rdata->data_type_str = (char *)(&data[n]);
        // n += (nb_records * FST_DATA_TYPE_STR_LEN * sizeof(char));

        // rdata->level = (float *)(&data[n]);
        // n += (nb_records * sizeof(float));

        // rdata->ip1_kind = (uint32_t *)(&data[n]);
        // n += (nb_records * sizeof(uint32_t));
        // rdata->ip1_kind_str = (char *)(&data[n]);
        // n += (nb_records * sizeof(char));
        // rdata->ip2_dec = (float *)(&data[n]);
        // n += (nb_records * sizeof(float));
        // rdata->ip2_kind = (uint32_t *)(&data[n]);
        // n += (nb_records * sizeof(uint32_t));
        // rdata->ip2_kind_str = (char *)(&data[n]);
        // n += (nb_records * sizeof(char));
        // rdata->ip3_dec = (float *)(&data[n]);
        // n += (nb_records * sizeof(float));
        // rdata->ip3_kind = (uint32_t *)(&data[n]);
        // n += (nb_records * sizeof(uint32_t));
        // rdata->ip3_kind_str = (char *)(&data[n]);
        // n += (nb_records * sizeof(char));

        // rdata->surface = (bool *)(&data[n]);
        // n += (nb_records * sizeof(bool));
        // rdata->follow_topography = (bool *)(&data[n]);
        // n += (nb_records * sizeof(bool));
        // rdata->ascending = (bool *)(&data[n]);
        // n += (nb_records * sizeof(bool));

        // rdata->interval_ip = (char *)(&data[n]);
        // n += (nb_records * FST_INTERVAL_IP_LEN * sizeof(char));
        // rdata->interval_low = (float *)(&data[n]);
        // n += (nb_records * sizeof(float));
        // rdata->interval_high = (float *)(&data[n]);
        // n += (nb_records * sizeof(float));
        // rdata->interval_kind = (uint32_t *)(&data[n]);
        // n += (nb_records * sizeof(uint32_t));

        // rdata->path = (char *)(&data[n]);
        // n += (nb_records * FST_PATH_LEN * sizeof(char));

        // rdata->multiple_modifications = (bool *)(&data[n]);
        // n += (nb_records * sizeof(bool));
        // rdata->zapped = (bool *)(&data[n]);
        // n += (nb_records * sizeof(bool));
        // rdata->filtered = (bool *)(&data[n]);
        // n += (nb_records * sizeof(bool));
        // rdata->interpolated = (bool *)(&data[n]);
        // n += (nb_records * sizeof(bool));
        // rdata->unit_converted = (bool *)(&data[n]);
        // n += (nb_records * sizeof(bool));
        // rdata->bounded = (bool *)(&data[n]);
        // n += (nb_records * sizeof(bool));
        // rdata->missing_data = (bool *)(&data[n]);
        // n += (nb_records * sizeof(bool));
        // rdata->ensemble_extra_info = (bool *)(&data[n]);
        // n += (nb_records * sizeof(bool));
        // rdata->masks = (bool *)(&data[n]);
        // n += (nb_records * sizeof(bool));
        // rdata->masked = (bool *)(&data[n]);
        // n += (nb_records * sizeof(bool));

        // rdata->grid_desc = (char *)(&data[n]);
        // n += (nb_records * FST_GRID_DESC_LEN * sizeof(char));

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

// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// static PyObject *make_array_owning_data(int ndims, npy_intp *dims, int type, void *data)
// {
//     PyObject *array = PyArray_SimpleNewFromData(ndims, dims, NPY_INT32, data);
//     if (array == NULL)
//     {
//         return NULL;
//     }
//     PyObject *capsule = PyCapsule_New(data, "raw_columns ni", (PyCapsule_Destructor)free_capsule_ptr);
//     if (capsule == NULL)
//     {
//         Py_DECREF(array);
//         return NULL;
//     }
//     if (PyArray_SetBaseObject((PyArrayObject *)array, capsule) < 0)
//     {
//         Py_DECREF(array);
//         Py_DECREF(capsule);
//         return NULL;
//     }

//     return array;
// }

// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// int make_1d_string_array_and_add_to_dict(PyObject *dict, const char *key, int nb_items, int max_str_length, char *data)
// {
//     npy_intp dims[] = {nb_items};
//     PyObject *array_1d = PyArray_New(&PyArray_Type, 1, dims, NPY_STRING, NULL, data, max_str_length, NPY_ARRAY_OWNDATA, NULL);
//     if (array_1d == NULL)
//     {
//         PyErr_Format(PyExc_RuntimeError, "Could not create numpy array for column '%s'", key);
//         return 1;
//     }
//     if (PyDict_SetItemString(dict, key, array_1d))
//     {
//         Py_DECREF(array_1d);
//         return 1;
//     }
//     return 0;
// }
// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// int make_1d_array_and_add_to_dict(PyObject *dict, const char *key, int nb_items, int type, void *data)
// {
//     npy_intp dims[] = {nb_items};
//     PyObject *array_1d = make_array_owning_data(1, dims, NPY_INT32, data);
//     if (array_1d == NULL)
//     {
//         PyErr_Format(PyExc_RuntimeError, "Could not create numpy array for column '%s'", key);
//         return 1;
//     }
//     if (PyDict_SetItemString(dict, key, array_1d))
//     {
//         Py_DECREF(array_1d);
//         return 1;
//     }
//     return 0;
// }
// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// static PyObject *rmn_get_index_columns(PyObject *self, PyObject *args)
// {

//     PyObject *file_list = NULL;
//     int ok = PyArg_ParseTuple(args, "O", &file_list);
//     if (!ok)
//     {
//         // Exception already set by PyArg_ParseTuple
//         return NULL;
//     }

//     /*
//      * Convert list of Python unicode strings into an array of char *
//      */
//     // TODO: Verify that the object is a list and throw exception otherwise
//     Py_ssize_t nb_files = PyList_Size(file_list);
//     const char *filenames[nb_files]; // TODO: Automatic arrays, some people don't like them and if they're too big they can blow the stack
//     for (int i = 0; i < nb_files; i++)
//     {
//         PyObject *item = PyList_GetItem(file_list, i);
//         if (item == NULL)
//         {
//             Lib_Log(APP_LIBRMN, APP_DEBUG, "%s(): ERROR: OOPSIE: Better error handling is required to not leak memory (Phil)\n", __func__);
//             return NULL;
//         }
//         const char *filename = PyUnicode_AsUTF8AndSize(item, NULL);
//         if (filename == NULL)
//         {
//             return NULL;
//         }
//         filenames[i] = filename;
//     }

//     /*
//      * Get raw column data
//      */
//     RecordData *raw_columns = rmn_get_index_columns_raw(filenames, nb_files);
//     if (raw_columns == NULL)
//     {
//         PyErr_SetString(PyExc_RuntimeError, "Could not get column data");
//         return NULL;
//     }

//     /*
//      * Create a dictionnary where the keys are column names and the values
//      * are numpy arrays created from the raw column data
//      */
//     Lib_Log(APP_LIBRMN, APP_DEBUG, "Creating numpy arrays and assigning them as keys in a dictionnary\n");
//     PyObject *columns = PyDict_New();

//     if (make_1d_string_array_and_add_to_dict(columns, "nomvar", raw_columns->nb_records, FST_NOMVAR_LEN, raw_columns->nomvar))
//     {
//         goto error;
//     }
//     if (make_1d_string_array_and_add_to_dict(columns, "typvar", raw_columns->nb_records, FST_TYPVAR_LEN, raw_columns->typvar))
//     {
//         goto error;
//     }
//     if (make_1d_string_array_and_add_to_dict(columns, "etiket", raw_columns->nb_records, FST_ETIKET_LEN, raw_columns->etiket))
//     {
//         goto error;
//     }

//     if (make_1d_array_and_add_to_dict(columns, "ni", raw_columns->nb_records, NPY_INT32, raw_columns->ni))
//     {
//         goto error;
//     }
//     if (make_1d_array_and_add_to_dict(columns, "nj", raw_columns->nb_records, NPY_INT32, raw_columns->nj))
//     {
//         goto error;
//     }
//     if (make_1d_array_and_add_to_dict(columns, "nk", raw_columns->nb_records, NPY_INT32, raw_columns->nk))
//     {
//         goto error;
//     }

//     if (make_1d_array_and_add_to_dict(columns, "dateo", raw_columns->nb_records, NPY_INT32, raw_columns->dateo))
//     {
//         goto error;
//     }

//     if (make_1d_array_and_add_to_dict(columns, "ip1", raw_columns->nb_records, NPY_INT32, raw_columns->ip1))
//     {
//         goto error;
//     }
//     if (make_1d_array_and_add_to_dict(columns, "ip2", raw_columns->nb_records, NPY_INT32, raw_columns->ip2))
//     {
//         goto error;
//     }
//     if (make_1d_array_and_add_to_dict(columns, "ip3", raw_columns->nb_records, NPY_INT32, raw_columns->ip3))
//     {
//         goto error;
//     }

//     if (make_1d_array_and_add_to_dict(columns, "deet", raw_columns->nb_records, NPY_INT32, raw_columns->deet))
//     {
//         goto error;
//     }
//     if (make_1d_array_and_add_to_dict(columns, "npas", raw_columns->nb_records, NPY_INT32, raw_columns->npas))
//     {
//         goto error;
//     }
//     if (make_1d_array_and_add_to_dict(columns, "data_type", raw_columns->nb_records, NPY_INT32, raw_columns->data_type))
//     {
//         goto error;
//     }
//     if (make_1d_array_and_add_to_dict(columns, "pack_bits", raw_columns->nb_records, NPY_INT32, raw_columns->pack_bits))
//     {
//         goto error;
//     }

//     if (make_1d_string_array_and_add_to_dict(columns, "grtyp", raw_columns->nb_records, FST_GTYP_LEN, raw_columns->grtyp))
//     {
//         goto error;
//     }

//     if (make_1d_array_and_add_to_dict(columns, "ig1", raw_columns->nb_records, NPY_INT32, raw_columns->ig1))
//     {
//         goto error;
//     }
//     if (make_1d_array_and_add_to_dict(columns, "ig2", raw_columns->nb_records, NPY_INT32, raw_columns->ig2))
//     {
//         goto error;
//     }
//     if (make_1d_array_and_add_to_dict(columns, "ig3", raw_columns->nb_records, NPY_INT32, raw_columns->ig3))
//     {
//         goto error;
//     }
//     if (make_1d_array_and_add_to_dict(columns, "ig4", raw_columns->nb_records, NPY_INT32, raw_columns->ig4))
//     {
//         goto error;
//     }

//     if (make_1d_array_and_add_to_dict(columns, "file_index", raw_columns->nb_records, NPY_INT32, raw_columns->file_index))
//     {
//         goto error;
//     }

//     // if(make_1d_array_and_add_to_dict(columns, "swa", raw_columns->nb_records, NPY_INT32, raw_columns->swa)){ goto error; }
//     // if(make_1d_array_and_add_to_dict(columns, "lng", raw_columns->nb_records, NPY_INT32, raw_columns->lng)){ goto error; }
//     // if(make_1d_array_and_add_to_dict(columns, "dltf", raw_columns->nb_records, NPY_INT32, raw_columns->dltf)){ goto error; }
//     // if(make_1d_array_and_add_to_dict(columns, "dltc", raw_columns->nb_records, NPY_INT32, raw_columns->dltc)){ goto error; }

//     // if(make_1d_array_and_add_to_dict(columns, "extra1", raw_columns->nb_records, NPY_INT32, raw_columns->extra1)){ goto error; }
//     // if(make_1d_array_and_add_to_dict(columns, "extra2", raw_columns->nb_records, NPY_INT32, raw_columns->extra2)){ goto error; }
//     // if(make_1d_array_and_add_to_dict(columns, "extra3", raw_columns->nb_records, NPY_INT32, raw_columns->extra3)){ goto error; }
//     // if(make_1d_string_array_and_add_to_dict(columns, "path", raw_columns->nb_records, PATH_MAX+1, raw_columns->path)){ goto error; }

//     free(raw_columns); // The struct contains a bunch of arrays but we don't want
//                        // to free them since we gave them away to some numpy arrays
//     Lib_Log(APP_LIBRMN, APP_DEBUG, "Handing control back to Python\n");
//     return columns;
// error:
//     free(raw_columns);
//     // TODO: Error handling for all the numpy arrays we created.
//     // Doing DECREF(columns) will destroy the dictionnary, and every array
//     // we added to the dictionnary will get DECREF'd by the destruction of the
//     // dictionnary, and all the make_1d... destroy the array if they can't
//     // add it to the dictionnary so I think we're good here.
//     // What about the ones that we didn't get to yet, we need to free those
//     // but not the ones that we gave to the dictionnary.
//     // We'll have to think about that one.
//     Py_XDECREF(columns);
//     // All functions called here that could fail will set the exception.
//     return NULL;
// }

// TODO Replace with normal python record data access but for Seb's info:
void *fst_read_data_at_index(const char *path, const int32_t index, void *data)
{
    void *output_data = NULL;

    Lib_Log(APP_LIBFST, APP_DEBUG, "Starting to read data at index %d from file %s\n", index, path);

    // Open the file first
    fst_file *file = fst24_open(path, "R");
    if (!file)
    {
        Lib_Log(APP_LIBFST, APP_ERROR, "Failed to open file %s\n", path);
        return NULL;
    }
    Lib_Log(APP_LIBFST, APP_DEBUG, "Successfully opened file\n");

    // Initialize record structure
    fst_record record = {0}; // Zero initialize all fields
    Lib_Log(APP_LIBFST, APP_DEBUG, "Initialized record structure\n");

    // Get record info at index
    int get_record_result = fst24_get_record_by_index(file, index, &record);
    if (get_record_result <= 0)
    {
        Lib_Log(APP_LIBFST, APP_ERROR, "Failed to get record at index %d (result: %d)\n", index, get_record_result);
        fst24_close(file);
        return NULL;
    }

    Lib_Log(APP_LIBFST, APP_DEBUG, "Got record at index %d: ni=%d, nj=%d, nk=%d, data_type=%d\n",
            index, record.ni, record.nj, record.nk, record.data_type);

    // Read the actual data
    record.data = data;  // Now read will put data there and not allocate
                         // Python allocated our data so we don't have to worry
    int read_record_result = fst24_read_record(&record);
    if (read_record_result <= 0)
    {
        Lib_Log(APP_LIBFST, APP_ERROR, "Failed to read record data (result: %d)\n", read_record_result);
        // fst24_record_free(&record);
        fst24_close(file);
        return NULL;
    }

    // Validate record data
    if (record.data == NULL)
    {
        Lib_Log(APP_LIBFST, APP_ERROR, "Record data pointer is NULL\n");
        // fst24_record_free(&record);
        fst24_close(file);
        return NULL;
    }

    fst24_close(file);

    return output_data;
}

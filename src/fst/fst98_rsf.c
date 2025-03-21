//! \file
//! Implementation of the FSTD98 interface to RSF files

#include <App.h>
#include "fst98_internal.h"
#include "fst24_file_internal.h"
#include "fst24_record_internal.h"
#include "primitives/fnom_internal.h"


//! Checks whether the given unit corresponds to an RSF file
int32_t is_rsf(
    //! [in] Unit number associated to the file
    const int32_t iun,
    //! [out] (Optional) The index given to this file by fnom
    int32_t * const out_index_fnom
) {
    //! \return 1 if the unit is an RSF, 0 if not, something else (negative) if there was an error
    if (out_index_fnom != NULL) *out_index_fnom = -1;

    const int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not connected with fnom\n", __func__, iun);
        return(ERR_NO_FNOM);
    }

    if (out_index_fnom != NULL) *out_index_fnom = index_fnom;

    return FGFDT[index_fnom].attr.rsf == 1 ? 1 : 0;
}

//! \copydoc c_fstecr
//! RSF version
int c_fstecr_rsf(
    //! [in] Field to write to the file
    void *field_in,
    //! [in] Work field (kept for backward compatibility)
    void *work,
    //! [in] Number of bits kept for the elements of the field
    int npak,
    //! [in] Unit number associated to the file in which to write the field
    int iun,
    //! [in] Index of the file given by fnom
    int index_fnom,
    //! [in] Date timestamp
    int date,
    //! [in] Length of the time steps in seconds
    int deet,
    //! [in] Time step number
    int npas,
    //! [in] First dimension of the data field
    int ni,
    //! [in] Second dimension of the data field
    int nj,
    //! [in] Thierd dimension of the data field
    int nk,
    //! [in] Vertical level
    int ip1,
    //! [in] Forecast hour
    int ip2,
    //! [in] User defined identifier
    int ip3,
    //! [in] Type of field (forecast, analysis, climatology)
    char *in_typvar,
    //! [in] Variable name
    char *in_nomvar,
    //! [in] Label
    char *in_etiket,
    //! [in] Type of geographical projection
    char *in_grtyp,
    //! [in] First grid descriptor
    int ig1,
    //! [in] Second grid descriptor
    int ig2,
    //! [in] Third grid descriptor
    int ig3,
    //! [in] Fourth grid descriptor
    int ig4,
    //! [in] Data type of elements
    int in_datyp_ori,
    //! [in] Rewrite existing record, append otherwise
    int rewrit
) {
    (void)work; // Unused

    if (! FGFDT[index_fnom].attr.std) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: File (unit=%d) is not a RPN standard file\n", __func__, iun);
        return ERR_NO_FILE;
    }

    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    fst_record rec = default_fst_record;

    // Set number of data bits (based on global variables short/byte/double)
    rec.data_bits = 32;
    if (xdf_double) rec.data_bits = 64;
    else if (xdf_short) rec.data_bits = 16;
    else if (xdf_byte) rec.data_bits = 8;

    xdf_double = 0;
    xdf_short = 0;
    xdf_byte = 0;

    // Set number of packed bits (based on npak, unless it's positive, in which case it's the same as data bits)
    rec.pack_bits = -npak;
    if (npak >= 0) {
        // Warn once
        static int npak_pos_warning = 1;
        if (npak_pos_warning == 1) {
            npak_pos_warning = 0;
            Lib_Log(APP_LIBFST, APP_WARNING, "%s: Must specify number of bits (npak < 0) for RSF files\n", __func__);
        }

        rec.pack_bits = rec.data_bits;
    }

    rec.data  = field_in;
    rec.dateo = date;
    rec.deet  = deet;
    rec.npas  = npas;
    rec.ni = ni;
    rec.nj = nj;
    rec.nk = nk;
    rec.ip1 = ip1;
    rec.ip2 = ip2;
    rec.ip3 = ip3;
    rec.ig1 = ig1;
    rec.ig2 = ig2;
    rec.ig3 = ig3;
    rec.ig4 = ig4;
    rec.data_type = in_datyp_ori;
    strncpy(rec.typvar, in_typvar, FST_TYPVAR_LEN);
    strncpy(rec.nomvar, in_nomvar, FST_NOMVAR_LEN);
    strncpy(rec.etiket, in_etiket, FST_ETIKET_LEN);
    strncpy(rec.grtyp,  in_grtyp,  FST_GTYP_LEN);

    if (rewrit != FST_NO) {
        // Find handle for rewrite operation
        int niout, njout, nkout;
        const int handle = c_fstinfx_rsf(
            -2, iun, index_fnom, &niout, &njout, &nkout, -1, in_etiket, ip1, ip2, ip3, in_typvar, in_nomvar, 1);

        if (handle >= 0) {
            if (rewrit == FST_SKIP) {
                // A similar record already exists, so nothing more to do
                return 0; // Success
            }

            // Delete the record before writing the new one
            if (RSF_Delete_record(file_handle, RSF_Key64(handle)) != 1) return -1;
        }
    }

    const int32_t result = fst24_write_rsf(file_handle, &rec, xdf_stride);

    return result > 0 ? 0 : -1;
}

//! \copydoc c_fstnbr
//! RSF version
int c_fstnbr_rsf(
    //! [in] Index given by fnom associated with the file
    const int index_fnom
) {
    if (FGFDT[index_fnom].rsf_fh.p != NULL)
        return RSF_Get_num_records_at_open(FGFDT[index_fnom].rsf_fh);

    Lib_Log(APP_LIBFST, APP_ERROR, "%s: file at index %d is not open\n", __func__, index_fnom);
    return ERR_NO_FILE;
}

//! Wrapper on the RSF call to open a file. Also sets appropriate flag on the fnom-given slot
//! \return 0 if the opening was successful, -1 otherwise
int c_fstouv_rsf(
    //!> [in] Index given to this file by fnom
    const int index_fnom,
    //!> [in] Opening mode (read/write/append)
    const rsf_open_mode_type mode,
    //!> [in] Size in MB of segments, if open for parallel write (not parallel if <= 0)
    const int32_t parallel_segment_size_mb
) {
    FGFDT[index_fnom].attr.rsf = 1;
    if (FGFDT[index_fnom].fd >= 0)
        c_waclos2(FGFDT[index_fnom].iun); // Because fnom (almost) always opens a wafile...

    const int32_t meta_dim = (sizeof(search_metadata) + 3)/ sizeof(int32_t); // In 32-bit units
    int64_t segment_size = 0;
    if (parallel_segment_size_mb > 0) {
        segment_size = ((int64_t)parallel_segment_size_mb) << 20;
    }

    const char appl[4] = {'S', 'T', 'D', 'F'};

    // Lib_Log(APP_LIBFST, APP_WARNING, "%s: segment size = %ld\n", __func__, segment_size);
    FGFDT[index_fnom].rsf_fh = RSF_Open_file(FGFDT[index_fnom].file_name, mode, meta_dim, appl, &segment_size);

    // VOLATILE mode, unlink the file so it gets erased at end of process
    if (FGFDT[index_fnom].attr.volatil) {
        if (mode == RSF_RW) {
           unlink(FGFDT[index_fnom].file_name);
        } else {
           Lib_Log(APP_LIBFST,APP_WARNING,"%s: File %s opened read only, VOLATILE mode not used\n", __func__,FGFDT[index_fnom].file_name);
        }
    }

    if (RSF_Is_handle_valid(FGFDT[index_fnom].rsf_fh)) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Opened file %s, mode %d, fnom index %d, meta dim 0x%x\n", __func__,
                FGFDT[index_fnom].file_name, mode, index_fnom, meta_dim);
        return 0;
    }


    Lib_Log(APP_LIBFST, APP_WARNING, "%s: Failed to open file %s, mode %d, fnom index %d, meta dim 0x%x\n",
            __func__, FGFDT[index_fnom].file_name, mode, index_fnom, meta_dim);

    return -1;
}

//! \copydoc c_fstinfx
//! RSF version
int c_fstinfx_rsf(
    //! [in] Handle from which the search begins.  Start from the beginning when handle = -2
    const int handle,
    //! [in] Unit number associated to the file
    const int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom,
    //! [out] Dimension 1 of the data field
    int * const ni,
    //! [out] Dimension 2 of the data field
    int * const nj,
    //! [out] Dimension 3 of the data field
    int * const nk,
    //! [in] Validity date
    const int datev,
    //! [in] Label
    const char * const in_etiket,
    //! [in] Vertical level
    const int ip1,
    //! [in] Forecast hour
    const int ip2,
    //! [in] User defined identifier
    const int ip3,
    //! [in] Type of field
    const char * const in_typvar,
    //! [in] Variable name
    const char * const in_nomvar,
    //! [in] Whether to skip the global file filter
    const int skip_filter
) {
    fst_record criteria = default_fst_record;

    copy_record_string(criteria.etiket, in_etiket, FST_ETIKET_LEN);
    copy_record_string(criteria.typvar, in_typvar, FST_TYPVAR_LEN);
    copy_record_string(criteria.nomvar, in_nomvar, FST_NOMVAR_LEN);

    criteria.ip1 = ip1;
    criteria.ip2 = ip2;
    criteria.ip3 = ip3;
    criteria.datev = datev;

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: iun %d recherche: datev=%d etiket=[%s] ip1=%d ip2=%d ip3=%d typvar=[%s] "
            "nomvar=[%s] (handle = %d, ipflags = %d%d%d)\n",
            __func__, iun, datev, criteria.etiket, ip1, ip2, ip3, criteria.typvar, criteria.nomvar, handle,
            ip1s_flag, ip2s_flag, ip3s_flag);

    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    // Initialize search parameters
    fst_query* query = &fst98_open_files[index_fnom].query;
    query->search_index = 0;
    query->search_done = 0;
    stdf_dir_keys *search_criteria = &(query->criteria.fst98_meta);
    stdf_dir_keys *search_mask     = &(query->mask.fst98_meta);

    query->options = default_query_options;

    if (ip1s_flag) query->options.ip1_all = 1;
    if (ip2s_flag) query->options.ip2_all = 1;
    if (ip3s_flag) query->options.ip3_all = 1;

    make_search_criteria(&criteria, &fst98_open_files[index_fnom].query);

    // Start after [handle], if one is specified
    if (handle > 0) {
        const int64_t key64 = RSF_Key64(handle);
        const uint32_t file_slot = RSF_Key64_to_file_slot(key64);
        if ((int32_t)file_slot != RSF_Get_file_slot(file_handle)) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid handle=%d, or iun=%d\n", __func__, handle, iun);
            return(ERR_BAD_HNDL);
        }

        query->search_index = RSF_Key64_to_index(key64);
    }

    // Perform the search itself
    int64_t rsf_key = -1;

    while ((rsf_key = find_next_rsf(file_handle, &fst98_open_files[index_fnom].query)) >= 0) {

        const int32_t lhandle = RSF_Key32(rsf_key);

        RSF_record_info record_info = RSF_Get_record_info(file_handle, rsf_key);
        fst_record rec = default_fst_record;
        fill_with_search_meta(&rec, (search_metadata*)record_info.meta, FST_RSF);


        // Verify additional criteria
        if (!skip_filter &&
            !C_fst_rsf_match_req(rec.datev, rec.ni, rec.nj, rec.nk, rec.ip1, rec.ip2, rec.ip3,
                                 rec.typvar, rec.nomvar, rec.etiket, rec.grtyp, rec.ig1, rec.ig2, rec.ig3, rec.ig4)) {
            continue;
        }
        if (ip1s_flag && ip1 >= 0 && ip_is_equal(ip1, rec.ip1, 1) == 0) continue;
        if (ip2s_flag && ip2 >= 0 && ip_is_equal(ip2, rec.ip2, 2) == 0) continue;
        if (ip3s_flag && ip3 >= 0 && ip_is_equal(ip3, rec.ip3, 3) == 0) continue;

        // We have a match!
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: (unit=%d) Found record at key 0x%x\n", __func__, iun, lhandle);
        if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_EXTRA) {
            fst24_record_print(&rec);
        }

        *ni = rec.ni;
        *nj = rec.nj;
        *nk = rec.nk;

        // arranger les masques de recherches pour un fstsui
        if (ip1s_flag) {
            search_criteria->ip1 = rec.ip1;
            search_mask->ip1 = 0xFFFFFFF;
        }
        if (ip2s_flag) {
            search_criteria->ip2 = rec.ip2;
            search_mask->ip2 = 0xFFFFFFF;
        }
        if (ip3s_flag) {
            search_criteria->ip3 = rec.ip3;
            search_mask->ip3 = 0xFFFFFFF;
        }
        if (ip1s_flag || ip2s_flag || ip3s_flag) {
            init_ip_vals();
        }

        return lhandle;
    }

    // If we got out, it's because we didn't find anything
    Lib_Log(APP_LIBFST, APP_TRIVIAL, "%s: (unit=%d) record not found, errcode=%ld\n", __func__, iun, rsf_key);
    return (int32_t)rsf_key;
}

//! \copydoc c_fstfrm
//! RSF version
int c_fstfrm_rsf(
    //! [in] Unit number associated to the file
    const int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom
) {
    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    const int32_t status = RSF_Close_file(file_handle);

    return status == 1 ? 0 : -1;
}

//! \copydoc c_fstlirx
//! RSF version
int c_fstlirx_rsf(
    //! [out] Field to be read
    void *field,
    //! [in] Record handle from which the search begins
    int handle,
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom,
    //! [out] First of the data field
    int *ni,
    //! [out] Second of the data field
    int *nj,
    //! [out] Third of the data field
    int *nk,
    //! [in] Validity date
    int datev,
    //! [in] Label
    char *etiket,
    //! [in] Vertical level
    int ip1,
    //! [in] Forecast hour
    int ip2,
    //! [in] User defined identifier
    int ip3,
    //! [in] Type of field
    char *typvar,
    //! [in] Variable name
    char *nomvar
) {
    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    const int32_t key32 = c_fstinfx_rsf(handle, iun, index_fnom, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, 0);
    if (key32 < 0) {
        Lib_Log(APP_LIBFST,APP_WARNING,"%s: (unit=%d) record not found, errcode=%d\n", __func__, iun, key32);
        return key32;
    }

    const int ier = c_fstluk_rsf(field, file_handle, key32, ni, nj, nk);
    if (ier < 0) {
        return ier;
    } else {
        return key32;
    }
}

//! \copydoc c_fstluk
//! RSF version
int c_fstluk_rsf(
    //! [out] Pointer to where the data read will be placed.  Must be allocated!
    void * const vfield,
    //! Handle to the file we are reading
    RSF_handle file_handle,
    //! [in] Handle (key) of the record to be read
    const int key32,
    //! [out] Dimension 1 of the data field
    int * const ni,
    //! [out] Dimension 2 of the data field
    int * const nj,
    //! [out] Dimension 3 of the data field
    int * const nk
) {
    int64_t rsf_key = RSF_Key64(key32);
    fst_record rec = default_fst_record;
    rec.data = vfield;

    if (get_record_from_key_rsf(file_handle, rsf_key, &rec) <= 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not retrieve record with key %ld\n", __func__, rsf_key);
        return ERR_BAD_HNDL;
    }

    const size_t work_size_bytes = fst24_record_data_size(&rec) +     // The data itself
                                   rec.num_meta_bytes +               // The metadata
                                   sizeof(RSF_record) +               // Space for the RSF struct itself
                                   128 * sizeof(uint32_t);            // Enough space for the largest compression scheme + rounding up for alignment

    void* work_space = malloc(work_size_bytes);
    if (work_space == NULL) {
        Lib_Log(APP_LIBFST, APP_FATAL, "%s: Unable to allocate workspace for reading record (%zu bytes)\n",
                __func__, work_size_bytes);
        return ERR_MEM_FULL;
    }
    memset(work_space, 0, work_size_bytes);

    RSF_record* record_rsf = RSF_Get_record(file_handle, rec.do_not_touch.handle, 0, (void*)work_space);

    if ((uint64_t*)record_rsf != work_space) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not get record corresponding to key 0x%lx\n",
                __func__, rec.do_not_touch.handle);
        return ERR_BAD_HNDL;
    }

    fill_with_search_meta(&rec, (search_metadata*)record_rsf->meta, FST_RSF);

    *ni = rec.ni;
    *nj = rec.nj;
    *nk = rec.nk;

    xdf_datatyp = rec.data_type & ~FSTD_MISSING_FLAG;

    const int original_num_bits = rec.data_bits;

    if (xdf_double) rec.data_bits = 64;
    else if (xdf_short) rec.data_bits = 16;
    else if (xdf_byte) rec.data_bits = 8;

    xdf_double = 0;
    xdf_short = 0;
    xdf_byte = 0;

    // // Extract metadata from record if present
    // rec.metadata = NULL;
    // if (record_rsf->rec_meta > record_rsf->dir_meta) {
    //     rec.metadata = Meta_Parse((char*)((search_metadata*)record_rsf->meta+1));
    // }

    // Extract data
    const int32_t ier = fst24_unpack_data(rec.data, record_rsf->data, &rec, image_mode_copy, 1, original_num_bits);

    if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_INFO) {
        fst_record_fields f = default_fields;
        // f.grid_info = 1;
        f.deet = 1;
        f.npas = 1;
        fst24_record_print_short(&rec, &f, 0, "Read : ");
    }

    free(work_space);

    if (ier < 0) return ier;

    return key32;
}

//! Delete a record
//! RSF version
int c_fsteff_rsf(
    //! Handle to the file that contains the record to delete
    RSF_handle file_handle,
    //! Handle of the record to delete
    int handle
) {
    if (RSF_Delete_record(file_handle, RSF_Key64(handle)) == 1) return 0;
    return ERR_BAD_FTYPE;
}

//! \copydoc c_fstinl
//! RSF version
int c_fstinl_rsf(
    //! [in] Unit number associated to the file in which to search
    int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom,
    //! [out] First dimension of the field
    int *ni,
    //! [out] Second dimension of the field
    int *nj,
    //! [out] Third dimension of the field
    int *nk,
    //! [in] Validity date
    int datev,
    //! [in] Label
    char *etiket,
    //! [in] Vertical level
    int ip1,
    //! [in] Forecast hour
    int ip2,
    //! [in] User defined identifier
    int ip3,
    //! [in] Type of field
    char *typvar,
    //! [in] Variable name
    char *nomvar,
    //! [out] List of handles of the matching records
    int *liste,
    //! [out] Number of elements for the list (number of matching records)
    int *infon,
    //! [in] List size (maximum number of matches)
    int nmax
) {
    Lib_Log(APP_LIBFST, APP_DEBUG,
            "%s: iun %d recherche: datev=%d etiket=[%s] ip1=%d ip2=%d ip3=%d typvar=[%s] nomvar=[%s]\n",
            __func__, iun, datev, etiket, ip1, ip2, ip3, typvar, nomvar);

    int handle = c_fstinfx_rsf(-2, iun, index_fnom, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar, 0);
    
    if (handle < 0) {
        *infon = 0;
        return 0;
    }

    int nijkmax = (*ni) * (*nj) * (*nk);
    int nimax = *ni;
    int njmax = *nj;
    int nkmax = *nk;

    int nfound = 0;
    while ((handle >= 0) && (nfound < nmax)) {
        liste[nfound] = handle;
        nfound++;
        if (nfound >= nmax) break;
        handle = c_fstsui_rsf(iun, index_fnom, ni, nj, nk);
        if ( ((*ni) * (*nj) * (*nk)) > nijkmax ) {
            nimax = *ni;
            njmax = *nj;
            nkmax = *nk;
            nijkmax = (*ni) * (*nj) * (*nk);
        }
    }

    *ni = nimax;
    *nj = njmax;
    *nk = nkmax;
    *infon = nfound;

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: nombre trouve=%d nmax=%d\n", __func__, nfound, nmax);

    while ( (handle = c_fstsui_rsf(iun, index_fnom, ni, nj, nk)) >= 0 ) {
        nfound++;
    }

    if (nfound > nmax) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: number of records found (%d) > nmax specified (%d)\n",
                 __func__, nfound, nmax);
        return -nfound;
    } else {
        return 0;
    }
}

//! \copydoc c_fstlis
//! RSF version
int c_fstlis_rsf(
    //! [out] Field to be read
    void *field,
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom,
    //! [out] First of the data field
    int *ni,
    //! [out] Second of the data field
    int *nj,
    //! [out] Third of the data field
    int *nk
) {
    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    /* Get the next record that matches the last search criterias */
    const int64_t rsf_key = find_next_rsf(file_handle, &fst98_open_files[index_fnom].query);

    if (rsf_key < 0) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: record not found, errcode=%ld\n", __func__, rsf_key);
        return (int32_t)rsf_key;
    }

    return c_fstluk_rsf(field, file_handle, RSF_Key32(rsf_key), ni, nj, nk);
}

//! Helper function for c_fstmsq
static inline char isignore(const char chr) {
    return (chr == '*') ? 0 : 0x3f;
}


//! Helper function for c_fstmsq
static inline char inv_isignore(const char chr) {
    return (chr == 0x3f) ? ' ' : '*';
}

//! \copydoc c_fstmsq
//! RSF version
int c_fstmsq_rsf(
    //! [in] Unit number associated to the file
    const int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom,
    //! [in,out] Mask for vertical level
    int *mip1,
    //! [in,out] Mask for the forecast hour
    int *mip2,
    //! [in,out] Mask for the user defined identifier
    int *mip3,
    //! [in,out] Mask for the label
    char *metiket,
    //! [in] Operation: Set when 0, Get otherwise
    const int getmode
) {
    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    stdf_dir_keys* search_mask = &fst98_open_files[index_fnom].query.background_mask.fst98_meta;

    if (getmode == 0) {
        search_mask->ip1 = ~(*mip1) & 0xfffffff;
        search_mask->ip2 = ~(*mip2) & 0xfffffff;
        search_mask->ip3 = ~(*mip3) & 0xfffffff;
        search_mask->etik15 =
            (isignore(metiket[0]) << 24) |
            (isignore(metiket[1]) << 18) |
            (isignore(metiket[2]) << 12) |
            (isignore(metiket[3]) <<  6) |
            (isignore(metiket[4]));
        search_mask->etik6a =
            (isignore(metiket[5]) << 24) |
            (isignore(metiket[6]) << 18) |
            (isignore(metiket[7]) << 12) |
            (isignore(metiket[8]) <<  6) |
            (isignore(metiket[9]));
        search_mask->etikbc =
            (isignore(metiket[10]) <<  6) |
            (isignore(metiket[11]));
    } else {
        *mip1 = ~(search_mask->ip1) & 0xfffffff;
        *mip2 = ~(search_mask->ip2) & 0xfffffff;
        *mip3 = ~(search_mask->ip3) & 0xfffffff;
        for (int i = 0; i <= 4; i++) {
            metiket[i] = inv_isignore( ((search_mask->etik15 >> ((4-i)*6)) & 0x3f) );
        }

        for (int i = 5; i <= 9; i++) {
            metiket[i] = inv_isignore( ((search_mask->etik6a >> ((9-i)*6)) & 0x3f) );
        }

        metiket[10] = inv_isignore( ((search_mask->etikbc >> 6) & 0x3f) );
        metiket[11] = inv_isignore((search_mask->etikbc  & 0x3f));
        metiket[12] = '\0';
    }

    return 0;
}

//! Get the number of valid records (excluding deleted records, including ones added since opening) in a file
//! RSF version
int c_fstnbrv_rsf(
    //! [in] Index of the file given by fnom
    const int index_fnom
) {
    if (FGFDT[index_fnom].rsf_fh.p != NULL)
        return RSF_Get_num_records(FGFDT[index_fnom].rsf_fh);

    Lib_Log(APP_LIBFST, APP_ERROR, "%s: file at index %d is not open\n", __func__, index_fnom);
    return ERR_NO_FILE;
}

//! Get all the descriptors of a record
//! RSF version
int c_fstprm_rsf(
    //! Handle to the file we are reading
    RSF_handle file_handle,
    //! [in] Handle of the record from which to retreive the descriptors
    int handle,
    //! [out] Origin date time stamp
    int *dateo,
    //! [out] Duration of time steps in seconds
    int *deet,
    //! [out] Time step number
    int *npas,
    //! [out] First dimension of the data field
    int *ni,
    //! [out] Second dimension of the data field
    int *nj,
    //! [out] Third dimension of the data field
    int *nk,
    //! [out] Number of bits kept for each elements of the field
    int *nbits,
    //! [out] Data type of the elements
    int *datyp,
    //! [out] Vertical level
    int *dasiz,
    //! [out] Vertical level
    int *ip1,
    //! [out] Forecast hour
    int *ip2,
    //! [out] User defined identifier
    int *ip3,
    //! [out] Type of field (forecast, analysis, climatology)
    char *typvar,
    //! [out] Variable name
    char *nomvar,
    //! [out] Label
    char *etiket,
    //! [out] Type of geographical projection
    char *grtyp,
    //! [out] First grid descriptor
    int *ig1,
    //! [out] Second grid descriptor
    int *ig2,
    //! [out] Third grid descriptor
    int *ig3,
    //! [out] Fourth grid descriptor
    int *ig4,
    //! [out] Starting word address
    int *swa,
    //! [out] Record length in 32-bit units
    int *lng,
    //! [out] Delete flag
    int *dltf,
    //! [out] Unused bit count
    int *ubc,
    //! [out] Extra parameter
    int *extra1,
    //! [out] Extra parameter
    int *extra2,
    //! [out] Extra parameter
    int *extra3
) {
    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file is not open\n", __func__);
        return ERR_NO_FILE;
    }

    // Get record information from handle
    int64_t rsf_key = RSF_Key64(handle);
    RSF_record_info record_info = RSF_Get_record_info(file_handle, rsf_key);
    if (record_info.rl <= 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not retrieve record with key %ld\n", __func__, rsf_key);
        return -1;
    }

    stdf_dir_keys *stdf_entry = &((search_metadata *)record_info.meta)->fst98_meta;
    stdf_special_parms cracked;
    crack_std_parms(stdf_entry, &cracked);

    *ni = stdf_entry->ni;
    *nj = stdf_entry->nj;
    *nk = stdf_entry->nk;
    *dateo = cracked.date_stamp;
    *deet = stdf_entry->deet;
    *npas = stdf_entry->npas;
    *nbits = stdf_entry->nbits;
    *datyp = stdf_entry->datyp;
    *dasiz = stdf_entry->dasiz;
    *ip1 = stdf_entry->ip1;
    *ip2 = stdf_entry->ip2;
    *ip3 = stdf_entry->ip3;
    *ig1 = stdf_entry->ig1;
    *ig2 = cracked.ig2;
    *ig3 = stdf_entry->ig3;
    *ig4 = stdf_entry->ig4;
    *dltf = stdf_entry->deleted;
    *ubc = stdf_entry->ubc;

    // Address and size in 64-bit units
    const uint64_t max_val = 0x8fffffff;
    const uint64_t addr_word32        = ((record_info.wa - 1) >> 2) + 1; // wa / 4, rounded up
    const uint64_t record_size_word32 = ((record_info.rl - 1) >> 2) + 1; // rl / 4, rounded up
    *swa = addr_word32 <= max_val ? (int32_t)addr_word32 : 0;
    *lng = record_size_word32 & max_val;

    if (addr_word32 > max_val || record_size_word32 > max_val) {
        Lib_Log(APP_LIBFST, APP_WARNING,
                "%s: record address or size (in 32-bit units) is larger than what can be handled by this interface. "
                "Address = %lu, size = %lu, max = %d\n",
                __func__, addr_word32, record_size_word32, (int32_t)max_val);
    }

    /* new, use to be undefined */
    *extra1 = cracked.date_valid;
    *extra2 = 0;
    *extra3 = 0;

    strncpy(typvar, cracked.typvar, FST_TYPVAR_LEN);
    strncpy(nomvar, cracked.nomvar, FST_NOMVAR_LEN);
    strncpy(etiket, cracked.etiket, FST_ETIKET_LEN);
    strncpy(grtyp, cracked.gtyp, FST_GTYP_LEN);

    return 0;
}

//! \copydoc c_fstsui
//! RSF version
int c_fstsui_rsf(
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom,
    //! [out] Dimension 1 of the data field
    int *ni,
    //! [out] Dimension 2 of the data field
    int *nj,
    //! [out] Dimension 3 of the data field
    int *nk
) {
    // uint32_t *primk = NULL;

    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    int64_t rsf_key = -1;
    while ((rsf_key = find_next_rsf(file_handle, &fst98_open_files[index_fnom].query)) >= 0) {

        RSF_record_info record_info = RSF_Get_record_info(file_handle, rsf_key);
        fst_record rec = default_fst_record;
        fill_with_search_meta(&rec, (search_metadata*)record_info.meta, FST_RSF);

        // Check if that record passes the global excdes filter
        if (C_fst_rsf_match_req(rec.datev, rec.ni, rec.nj, rec.nk, rec.ip1, rec.ip2, rec.ip3, rec.typvar, rec.nomvar,
                                rec.etiket, rec.grtyp, rec.ig1, rec.ig2, rec.ig3, rec.ig4)) {
            *ni = rec.ni;
            *nj = rec.nj;
            *nk = rec.nk;

            const int32_t xdf_handle = RSF_Key32(rsf_key);
            return xdf_handle;
        }

    }

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: record not found, errcode=%ld\n", __func__, rsf_key);
    return ERR_NOT_FOUND;
}

//! \copydoc c_fstvoi
//! RSF version
int c_fstvoi_rsf(
    //! [in] Unit number associated to the file
    const int iun,
    //! [in] Index of the file given by fnom
    const int index_fnom,
    //! [in] List of fields to print
    const char * const options,
    const int print_stats,              //!< Whether to print stats after record info
    int64_t* const total_num_entries,         //!< [in,out] Accumulate number of entries (for linked files)
    int64_t* const total_num_valid_records,   //!< [in,out] Accumulate number of valid record  (for linked files)
    int64_t* const total_file_size,           //!< [in,out] Accumulate total size of linked files
    int64_t* const total_num_writes,          //!< [in,out] Accumulate write count (for linked files)
    int64_t* const total_num_rewrites,        //!< [in,out] Accumulate rewrite count (for linked files)
    int64_t* const total_num_erasures,        //!< [in,out] Accumulate erasure count (for linked files)
    int64_t* const total_erased_size          //!< [in,out] Accumulate size of erased records (for linked files)
) {
    // Unused parameters, this information is not exposed by RSF files
    (void)total_num_rewrites;
    (void)total_num_erasures;
    (void)total_erased_size;


    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    size_t single_file_size = 0;
    int64_t num_records = 0;

    // Initialize search parameters
    fst_record criteria = default_fst_record;
    fst_query* query = &fst98_open_files[index_fnom].query;
    query->search_index = 0;
    query->search_done = 0;

    query->options = default_query_options;

    make_search_criteria(&criteria, &fst98_open_files[index_fnom].query);

    // Perform the search itself
    int64_t rsf_key = -1;
    while ((rsf_key = find_next_rsf(file_handle, &fst98_open_files[index_fnom].query)) >= 0) {
        const RSF_record_info info = RSF_Get_record_info(file_handle, rsf_key);
        const stdf_dir_keys* metadata = &((const search_metadata *) info.meta)->fst98_meta;
        single_file_size += info.rl;
        char string[20];
        sprintf(string, "%5ld-", *total_num_valid_records + num_records);
        print_std_parms(metadata, string, options, (((*total_num_valid_records + num_records) % 70) == 0));
        num_records++;
    }

    *total_num_entries += num_records;
    *total_num_valid_records += num_records;
    *total_file_size += single_file_size;
    *total_num_writes += num_records;

    if (print_stats) {
        fprintf(stdout, "\n%ld records in RPN standard file (RSF version). Size %ld bytes (%.3f MB).\n",
                num_records, single_file_size, single_file_size / (1024.f * 1024.f));
    }

    return 0;
}

int32_t c_fstckp_rsf(const int iun, const int index_fnom) {
    RSF_handle file_handle = FGFDT[index_fnom].rsf_fh;

    if (file_handle.p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file (unit=%d) is not open\n", __func__, iun);
        return ERR_NO_FILE;
    }

    return RSF_Checkpoint(file_handle);
}

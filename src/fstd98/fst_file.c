#include "rmn/fst_file.h"

#include <stdlib.h>

#include <App.h>
#include "fst_record_internal.h"
#include "qstdir.h"
#include "rmn/fnom.h"
#include "xdf98.h"

int32_t fst23_file_is_open(const fst_file* file) {
    return (file != NULL && file->type != FST_NONE && file->file_index >= 0 && file->iun != 0);
}

//! Open a standard file (FST). Will create it if it does not already exist
//! \return A handle to the opened file. NULL if there was an error
fst_file* fst23_open(
    const char* file_name,  //!< Path of the file to open
    const char* options     //!< A list of options, as a string, with each pair of options separated by a comma or a '+'
) {
    fst_file* the_file = (fst_file *)malloc(sizeof(fst_file));
    if (the_file == NULL) return NULL;

    *the_file = default_fst_file;

    const int MAX_LENGTH = 1024;
    char local_options[MAX_LENGTH];
    if (options == NULL) {
        strncpy(local_options, "", MAX_LENGTH);
    } else {
        strncpy(local_options, options, MAX_LENGTH);
    }

    if (c_fnom(&(the_file->iun), file_name, local_options, 0) != 0) return NULL;
    if (c_fstouv(the_file->iun, local_options) < 0) return NULL;

    // Find type of newly-opened file (RSF or XDF)
    int index_fnom;
    const int rsf_status = is_rsf(the_file->iun, &index_fnom);
    the_file->file_index = index_fnom;
    if (rsf_status == 1) {
        the_file->type = FST_RSF;
    }
    else {
        the_file->type = FST_XDF;
    }

    return the_file;
}

//! Close the given standard file
//! \return 0 if no error, a negative number otherwise
int32_t fst23_close(fst_file* file) {
    int status;
    status = c_fstfrm(file->iun);   // Close the actual file
    if (status < 0) return status;

    status = c_fclos(file->iun);    // Reset file entry in global table
    if (status < 0) return status;

    *file = default_fst_file;

    return 0;
}

//! Write the given record into the given standard file
//! \return 0 if everything was a success, a negative error code otherwise
int32_t fst23_write(fst_file* file, const fst_record* record,int rewrit) {
    if (!fst23_record_is_valid(record)) return ERR_BAD_INIT;

    char typvar[FST_TYPVAR_LEN];
    char nomvar[FST_NOMVAR_LEN];
    char etiket[FST_ETIKET_LEN];
    char grtyp[FST_GTYP_LEN];

    strncpy(typvar, record->typvar, FST_TYPVAR_LEN);
    strncpy(nomvar, record->nomvar, FST_NOMVAR_LEN);
    strncpy(etiket, record->etiket, FST_ETIKET_LEN);
    strncpy(grtyp, record->grtyp, FST_GTYP_LEN);

    return c_fstecr(
        record->data, NULL, record->npak, file->iun, record->date, record->deet, record->npas,
        record->ni, record->nj, record->nk, record->ip1, record->ip2, record->ip3,
        typvar, nomvar, etiket, grtyp, record->ig1, record->ig2, record->ig3, record->ig4, record->datyp, rewrit);
}

//! Look for a record in the given file matching certain criteria
//! The criteria are given as a fst_record struct, with the value of -1 acting as a wildcard.
//! For the character-type variables, the wildcard is a space
fst_record fst23_find(fst_file* file, const fst_record* criteria) {
    fst_record result = default_fst_record;

    if (!fst23_file_is_open(file)) return result;
    if (!fst23_record_is_valid(criteria)) return result;

    // Pack criteria into format used by both backends
    stdf_dir_keys* search_criteria;
    stdf_dir_keys* search_mask;
    make_search_criteria(criteria, &search_criteria, &search_mask);

    int64_t key = -1;
    const stdf_dir_keys* record_meta = NULL;

    // Look for the record in the file, depending on backend
    if (file->type == FST_RSF) {
        RSF_handle file_handle = FGFDT[file->file_index].rsf_fh;
        fstd_open_files[file->file_index].search_start_key = 0;
        fstd_open_files[file->file_index].search_criteria = *search_criteria;
        fstd_open_files[file->file_index].search_mask = *search_mask;

        key = find_next_record(file_handle, &fstd_open_files[file->file_index]);

        if (key >= 0) {
            const RSF_record_info record_info = RSF_Get_record_info(file_handle, key);
            record_meta = (const stdf_dir_keys*)record_info.meta;
        }
    }
    else {
        uint32_t* pkeys = (uint32_t *) search_criteria;
        uint32_t* pmask = (uint32_t *) search_mask;

        pkeys += W64TOWD(1);
        pmask += W64TOWD(1);

        key = c_xdfloc2(file->iun, 0, pkeys, 16, pmask);

        if (key >= 0) {
            int addr, lng, idtyp;
            c_xdfprm(key, &addr, &lng, &idtyp, pkeys, 16);
            record_meta = search_criteria;
        }
    }

    if (key >= 0) {
        // Got the record. Now unpack it into a more convenient format
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: (unit=%d) Found record at key 0x%x\n", __func__, file->iun, key);
        result = record_from_dir_keys(record_meta);
        result.handle = key;
    }

    free(search_criteria);
    free(search_mask);

    return result;
}

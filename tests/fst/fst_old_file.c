#include <libgen.h>
#include <string.h>

#include <App.h>
#include <rmn.h>

static fst_record criteria = default_fst_record;

int compare_records(const fst_record* original, const fst_record* mirror, const fst_record* similar) {

    const size_t num_bytes = fst24_record_num_elem(original) * mirror->data_bits / 8;
    if (memcmp(original->data, mirror->data, num_bytes) != 0) {
        App_Log(APP_ERROR, "%s: Mirror is not identical to original\n", __func__);
        return -1;
    }

    switch(base_fst_type(original->data_type)) {
        case FST_TYPE_SIGNED:
        case FST_TYPE_UNSIGNED:
        case FST_TYPE_BINARY:
        case FST_TYPE_CHAR:
        case FST_TYPE_STRING:
            if (memcmp(original->data, similar->data, num_bytes) != 0) {
                App_Log(APP_ERROR, "%s: Fields are not identical\n", __func__);
                return -1;
            }
            break;

        case FST_TYPE_REAL:
        case FST_TYPE_REAL_IEEE:
        case FST_TYPE_REAL_OLD_QUANT:
        {
            int num_printed = 0;
            int has_diff = 0;
            double total_diff = 0.0;
            double total_val = 0.0;
            const float* data1 = original->data;
            const float* data2 = similar->data;
            for (int k = 0; k < original->nk; k++) {
                for (int j = 0; j < original->nj; j++) {
                    for (int i = 0; i < original->ni; i++) {
                        const size_t index = (k * original->nj + j) * original->ni + i;
                        const float d1 = data1[index];
                        const float d2 = data2[index];
                        const float diff = d2 - d1;
                        const float rel_diff = d1 != 0 ? diff / d1 : diff;
                        total_diff += diff;
                        total_val += d1;
                        if (rel_diff > 3e-16) {
                            if (num_printed < 10) {
                                App_Log(APP_ERROR,
                                        "%s: Large relative difference at element (%4d, %4d, %d): %.2e (%.3e vs %.3e)"
                                        " (0x%08x vs 0x%08x)\n",
                                        __func__, i, j, k, rel_diff, d1, d2, *((uint32_t*)&d1), *((uint32_t*)&d2));
                            }
                            // return -1;
                            has_diff = 1;
                            num_printed++;
                        }
                    }
                }
            }

            if (has_diff) {
                fst24_record_print(original);
                fst24_record_print(similar);
                App_Log(APP_VERBATIM, "Total diff = %e (out of %e, rel %.2e)\n",
                        total_diff, total_val, total_diff / total_val);
                return -1;
            }
            break;
        }

        default:
            App_Log(APP_ERROR, "%s: Unhandled data type %d (base %d)\n",
                    __func__, original->data_type, base_fst_type(original->data_type));
            return -1;
    }

    return 0;
}

int check_file(const char* filepath_orig) {
    App_Log(APP_INFO, "Checking file %s\n", filepath_orig);

    const int BUF_SIZE = 1024 * 2;

    char filepath[BUF_SIZE];
    strncpy(filepath, filepath_orig, BUF_SIZE);
    const char* filename = basename(filepath);

    char filename_mirror[BUF_SIZE];
    char filename_similar[BUF_SIZE];

    snprintf(filename_mirror, BUF_SIZE, "%s%s", filename, ".mirror.rsf");
    snprintf(filename_similar, BUF_SIZE, "%s%s", filename, ".similar.rsf");
    
    fst_file* old_file = fst24_open(filepath, NULL);
    if (old_file == NULL) {
        App_Log(APP_ERROR, "Unable to open original file %s\n", filepath);
        return -1;
    }

    remove(filename_mirror);
    remove(filename_similar);
    fst_file* mirror_copy = fst24_open(filename_mirror, "RSF+R/W");
    fst_file* similar_copy = fst24_open(filename_similar, "RSF+R/W");

    if (mirror_copy == NULL || similar_copy == NULL) {
        App_Log(APP_ERROR, "Unable to open files for copying\n");
        return -1;
    }

    // sprintf(criteria.nomvar, "QC  ");
    // sprintf(criteria.typvar, "P");
    // sprintf(criteria.etiket, "R1_V800_N");
    // criteria.ip1 = 2102;

    fst_record rec = default_fst_record;
    fst_query* q = fst24_new_query(old_file, &criteria, NULL);

    App_Log(APP_INFO, "Making copies...\n");

    int num_copied_records = 0;

    const int old_loglevel = Lib_LogLevel(APP_LIBFST, "WARNING");
    c_fstopl("IMAGE", 1, 0);
    while (fst24_read_next(q, &rec) == TRUE) {
        if (fst24_write(mirror_copy, &rec, FST_NO) != TRUE) {
            App_Log(APP_ERROR, "Unable to write record\n");
            return -1;
        }
        num_copied_records++;
        // if (num_copied_records > 66) break;
    }
    c_fstopl("IMAGE", 0, 0);
    fst24_query_free(q);

    App_Log(APP_ALWAYS, "%s: Copied %d records (mirror image)\n", __func__, num_copied_records);

    // q = fst24_new_query(old_file, &criteria, NULL);
    q = fst24_new_query(mirror_copy, &criteria, NULL);
    int count = 0;
    while (fst24_read_next(q, &rec) == TRUE && count < num_copied_records) {
        if (fst24_write(similar_copy, &rec, FST_NO) != TRUE) {
            App_Log(APP_ERROR, "Unable to write record\n");
            return -1;
        }
        count++;
    }
    fst24_query_free(q);

    App_Log(APP_ALWAYS, "%s: Copied %d records (re-encode)\n", __func__, num_copied_records);

    if (num_copied_records != fst24_get_num_records(old_file)) {
        App_Log(APP_ERROR, "Did not copy all records from old file\n");
        return -1;
    }

    if (!fst24_close(old_file) || !fst24_close(mirror_copy) || !fst24_close(similar_copy)) {
        App_Log(APP_ERROR, "Error while closing %s and its copies\n", filename);
        return -1;
    }

    App_Log(APP_ALWAYS, "Comparing copies with original\n");

    old_file = fst24_open(filepath, NULL);
    mirror_copy = fst24_open(filename_mirror, NULL);
    similar_copy = fst24_open(filename_similar, NULL);

    if (old_file == NULL || mirror_copy == NULL || similar_copy == NULL) {
        App_Log(APP_ERROR, "Unable to open files after copy\n");
        return -1;
    }

    q = fst24_new_query(old_file, &criteria, NULL);
    fst_query* q_mirror = fst24_new_query(mirror_copy, &criteria, NULL);
    fst_query* q_similar = fst24_new_query(similar_copy, &criteria, NULL);

    fst_record rec_mirror = default_fst_record;
    fst_record rec_similar = default_fst_record;

    int num_checked_records = 0;
    while (fst24_read_next(q, &rec) == TRUE &&
           fst24_read_next(q_mirror, &rec_mirror) == TRUE &&
           fst24_read_next(q_similar, &rec_similar) == TRUE) {
        if (compare_records(&rec, &rec_mirror, &rec_similar) != 0) {
            App_Log(APP_ERROR, "Different records! %d\n", num_checked_records);
            return -1;
        }
        num_checked_records++;
    }
    Lib_LogLevelNo(APP_LIBFST, old_loglevel);

    if (num_checked_records != num_copied_records) {
        App_Log(APP_ERROR, "Could not check all records!\n");
        return -1;
    }

    fst24_query_free(q);
    fst24_query_free(q_mirror);
    fst24_query_free(q_similar);

    fst24_record_free(&rec);
    fst24_record_free(&rec_mirror);
    fst24_record_free(&rec_similar);

    if (!fst24_close(old_file) || !fst24_close(mirror_copy) || !fst24_close(similar_copy)) {
        App_Log(APP_ERROR, "Error while closing %s and its copies\n", filename);
        return -1;
    }

    return 0;
}

int main(int argc, char* argv[]) {

    if (argc <= 1) {
        App_Log(APP_VERBATIM, "No file given\n");
        return 1;
    }

    for (int i = 1; i < argc; i++) {
        if (check_file(argv[i]) != 0) return -1;
    }
    return 0;
}

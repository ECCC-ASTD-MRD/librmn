/*
 * Minimal C test that creates two standard FST files, each containing a single
 * record with arbitrary parameters and data:
 *   - one using the legacy fst98 (c_fst*) API
 *   - one using the modern fst24 (fst24_*) API
 */
#include <stdio.h>
#include <string.h>

#include <App.h>
#include <rmn.h>

#define NI 4
#define NJ 3
#define NK 1

// Fill the data field with arbitrary values
static void fill_data(float* data) {
    for (int i = 0; i < NI * NJ * NK; i++) {
        data[i] = (float)i * 1.5f;
    }
}

// Create a file with a single record using the legacy fst98 (c_fst*) API
static int write_fst98(const char* filename) {
    remove(filename);

    float data[NI * NJ * NK];
    fill_data(data);

    // Open a new file for writing on a fresh unit
    int iun = 0;
    if (c_fnom(&iun, filename, "RND+R/W", 0) < 0) {
        App_Log(APP_ERROR, "%s: c_fnom failed to open %s for writing\n", __func__, filename);
        return -1;
    }
    if (c_fstouv(iun, "RND") < 0) {
        App_Log(APP_ERROR, "%s: c_fstouv failed\n", __func__);
        return -1;
    }

    // Write a single record:
    const int ier = c_fstecr(data, NULL, -32, iun,
                             20261029, 300, 0,
                             NI, NJ, NK,
                             1, 10, 100,
                             "P", "WAVE", "SINGLE", "X",
                             0, 0, 0, 0,
                             FST_TYPE_REAL_IEEE, 0);
    if (ier != 0) {
        App_Log(APP_ERROR, "%s: c_fstecr failed (ier = %d)\n", __func__, ier);
        return -1;
    }

    // Checkpoint the directory and close the file
    c_fstfrm(iun);
    c_fclos(iun);

    App_Log(APP_ALWAYS, "%s: Wrote 1 record to %s (ni=%d nj=%d nk=%d)\n", __func__, filename, NI, NJ, NK);
    return 0;
}

// Create a file with a single record using the modern fst24 (fst24_*) API
static int write_fst24(const char* filename) {
    remove(filename);

    // Open a new file for writing (default backend)
    fst_file* file = fst24_open(filename, "R/W");
    if (file == NULL) {
        App_Log(APP_ERROR, "%s: fst24_open failed to create %s\n", __func__, filename);
        return -1;
    }

    float data[NI * NJ * NK];
    fill_data(data);

    // Build the record from the default (wildcard) template
    fst_record rec = default_fst_record;
    rec.data      = data;
    rec.data_type = FST_TYPE_REAL_IEEE;
    rec.data_bits = 32;
    rec.pack_bits = 32;
    rec.ni = NI;
    rec.nj = NJ;
    rec.nk = NK;

    rec.dateo = 20261029;
    rec.deet  = 300;
    rec.npas  = 0;

    rec.ip1 = 1;
    rec.ip2 = 10;
    rec.ip3 = 100;
    rec.ig1 = 0;
    rec.ig2 = 0;
    rec.ig3 = 0;
    rec.ig4 = 0;

    strcpy(rec.typvar, "P");
    strcpy(rec.nomvar, "WAVE");
    strcpy(rec.etiket, "SINGLE");
    strcpy(rec.grtyp,  "X");

    // Write the single record (rewrite = FST_NO)
    if (fst24_write(file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: fst24_write failed\n", __func__);
        return -1;
    }

    // Close the file
    if (fst24_close(file) != TRUE) {
        App_Log(APP_ERROR, "%s: fst24_close failed\n", __func__);
        return -1;
    }

    App_Log(APP_ALWAYS, "%s: Wrote 1 record to %s (ni=%d nj=%d nk=%d)\n", __func__, filename, NI, NJ, NK);
    return 0;
}

int main(void) {
    if (write_fst98("fst98_single.fst") != 0) return -1;
    if (write_fst24("fst24_single.fst") != 0) return -1;
    App_Log(APP_ALWAYS, "Test successful\n");
    return 0;
}

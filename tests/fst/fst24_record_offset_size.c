#include <App.h>
#include <rmn/fst24_file.h>

const char * const filename_rsf = "record_offset_size.rsf";
const char * const filename_xdf = "record_offset_size.xdf";

// const float data1[NUM_DATA] = {1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f, 9.0f, 99.0f};
// const float data2[NUM_DATA] = {0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f, 0.8f, 0.9f, 9.9f};
const int data3[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 99};
// const int   data4[NUM_DATA] = {-1, -2, -3, -4, -5, -6, -7, -8, -9, -99};

int create_file(const int is_rsf) {
    const char * const filename = is_rsf ? filename_rsf : filename_xdf;
    const char * const options = is_rsf ? "RSF+R/W" : "XDF+R/W";

    remove(filename);
    fst_file* test_file = fst24_open(filename, options);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to create file \"%s\"\n", __func__, filename);
        return -1;
    }

    fst_record rec = default_fst_record;
    rec.ni = sizeof(data3) / sizeof(int);
    rec.nj = 1;
    rec.nk = 1;

    rec.ig1 = 0;
    rec.ig2 = 0;
    rec.ig3 = 0;
    rec.ig4 = 0;
    rec.ip1 = 0;
    rec.ip2 = 0;
    rec.ip3 = 0;
    rec.deet = 1;
    rec.npas = 1;

    rec.dateo = 1;
    rec.datev = 1;

    // rec.data = (void*)data1;
    // rec.data_bits = 32;
    // rec.pack_bits = 16;
    // rec.data_type = FST_TYPE_REAL;
    // rec.ip1 = 1;
    // strcpy(rec.nomvar, "A");
    // strcpy(rec.etiket, "FLOAT1");

    rec.data = (void*)data3;
    rec.data_bits = 32;
    rec.pack_bits = 32;
    rec.data_type = FST_TYPE_SIGNED;
    rec.ip1 = 3;
    strcpy(rec.nomvar, "C");
    strcpy(rec.etiket, "INT1");

    if (fst24_write(test_file, &rec, FST_NO) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to write record \n", __func__);
        return -1;
    }

    fst24_record_free(&rec);

    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Unable to close test file %s\n", __func__, filename);
        return -1;
    }

    return 0;
}

int run_test(const int is_rsf) {
    if (create_file(is_rsf) != 0) return -1;

    const char* const filename = is_rsf ? filename_rsf : filename_xdf;

    fst_file* test_file = fst24_open(filename, NULL);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "%s: Unable to open test file (FST) %s\n", __func__, filename);
        return -1;
    }

    fst_query* q = fst24_new_query(test_file, NULL, NULL);
    if (q == NULL) {
        App_Log(APP_ERROR, "%s: Unable to create query\n", __func__);
        return -1;
    }

    fst_record rec = default_fst_record;
    if (fst24_find_next(q, &rec) != TRUE) {
        App_Log(APP_ERROR, "%s: Could not find record\n", __func__);
        return -1;
    }

    App_Log(APP_INFO, "%s: Record is located at offset 0x%lx and contains %zu bytes\n",
            __func__, rec.file_offset, rec.total_stored_bytes);

    uint32_t* raw_record = (uint32_t*)malloc(rec.total_stored_bytes + 3);

    // Read the bytes
    if (fst24_read_raw_record(filename, rec.file_offset, rec.total_stored_bytes, raw_record) != TRUE) {
        return -1;
    }

    // Dump the data that was read
    {
        char buffer[1024 * 2];
        char* ptr = buffer;
        for (int i = 0; i < rec.total_stored_bytes / sizeof(uint32_t) && i < 512; i++) {
            if (i % 4 == 0) ptr += sprintf(ptr, "\n");
            ptr += sprintf(ptr, "%8x ", raw_record[i]);
        }
        App_Log(APP_VERBATIM, "Raw record content (%p): %s\n", raw_record, buffer);
    }

    // Try to decode the bytes
    fst_record local_rec = default_fst_record;
    void* decoded_data = malloc(fst24_record_data_size(&rec));
    if (is_rsf) {
        local_rec = fst24_decode_data_rsf(raw_record, decoded_data);
    }
    else {
        local_rec = fst24_decode_data_xdf(raw_record, decoded_data);
    }
    if (local_rec.data == NULL || local_rec.data != decoded_data) {
        App_Log(APP_ERROR, "%s: Could not unpack raw record (%s)\n", __func__, is_rsf ? "RSF" : "XDF");
        return -1;
    }

    // Print record data
    {
        char buffer[1024];
        char* ptr = buffer;
        for (int i = 0; i < local_rec.ni; i++) {
            ptr += sprintf(ptr, "%3d ", ((int32_t*)local_rec.data)[i]);
        }
        App_Log(APP_ALWAYS, "Record data, after extraction: %s\n", buffer);
    }

    // Verify the data
    for (int i = 0; i < sizeof(data3) / sizeof(int); i++) {
        if (((int32_t*)local_rec.data)[i] != data3[i]) {
            App_Log(APP_ERROR, "%s: Extracted data does not match original data at index %d, expected %d, got %d\n",
                    __func__, i, data3[i], ((int32_t*)local_rec.data)[i]);
            return -1;
        }
    }

    fst24_record_free(&local_rec);
    free(decoded_data);

    free(raw_record);
    fst24_query_free(q);
    fst24_record_free(&rec);
    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Error while closing test file %s\n", __func__, filename);
        return -1;
    }

    return 0;
}

int main(void) {

    App_Log(APP_ALWAYS, "Running RSF test\n");
    if (run_test(1) != 0) return -1;
    App_Log(APP_ALWAYS, "Running XDF test\n");
    if (run_test(0) != 0) return -1;


    App_Log(APP_ALWAYS, "Test successful\n");
    return 0;
}

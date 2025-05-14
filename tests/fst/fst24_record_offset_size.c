#include <App.h>
#include <rmn/fst24_file.h>

const char * const filename_rsf = "record_offset_size.rsf";
const char * const filename_xdf = "record_offset_size.xdf";

#define NUM_DATA 10
// const float data1[NUM_DATA] = {1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f, 8.0f, 9.0f};
// const float data2[NUM_DATA] = {0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f, 0.8f, 0.9f};
const int   data3[NUM_DATA] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
// const int   data4[NUM_DATA] = {-1, -2, -3, -4, -5, -6, -7, -8, -9};

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
    rec.ni = NUM_DATA;
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

    const int fd = open(filename, O_RDONLY);
    if (fd < 0) {
        App_Log(APP_ERROR, "%s: Unable to open test file (system call)\n", __func__);
        return -1;
    }

    lseek(fd, rec.file_offset, SEEK_SET);
    const ssize_t num_read = read(fd, raw_record, rec.total_stored_bytes);

    {
        char buffer[1024 * 2];
        char* ptr = buffer;
        for (int i = 0; i < num_read / sizeof(uint32_t); i++) {
            if (i % 4 == 0) ptr += sprintf(ptr, "\n");
            ptr += sprintf(ptr, "%8x ", raw_record[i]);
        }
        App_Log(APP_VERBATIM, "Raw record content: %s\n", buffer);
    }

    if (num_read != rec.total_stored_bytes) {
        App_Log(APP_ERROR, "%s: Unable to read raw record from file. %lu\n", __func__, num_read);
        return -1;
    }

    close(fd);
    free(raw_record);
    fst24_record_free(&rec);
    if (fst24_close(test_file) != TRUE) {
        App_Log(APP_ERROR, "%s: Error while closing test file %s\n", __func__, filename);
        return -1;
    }

    return 0;
}

int main(void) {

    App_Log(APP_INFO, "Running RSF test\n");
    if (run_test(1) != 0) return -1;
    App_Log(APP_INFO, "Running XDF test\n");
    if (run_test(0) != 0) return -1;


    App_Log(APP_INFO, "Test successful\n");
    return 0;
}

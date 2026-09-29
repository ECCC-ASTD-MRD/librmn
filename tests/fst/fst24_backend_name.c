#include <string.h>

#include <App.h>
#include <rmn.h>

//! Check that fst24_backend_name returns the expected name for a file opened with the given backend,
//! and NULL when the input does not point to an open file (here, a NULL pointer).
int do_test(const char* backend) {
    App_Log(APP_ALWAYS, "Testing %s\n", backend);

    char test_filename[64];
    snprintf(test_filename, sizeof(test_filename), "backend_name.%s", backend);
    remove(test_filename);

    char options[64];
    snprintf(options, sizeof(options), "RND+R/W+%s", backend);

    fst_file* test_file = fst24_open(test_filename, options);
    if (test_file == NULL) {
        App_Log(APP_ERROR, "Unable to create test file with options %s\n", options);
        return -1;
    }

    // Open file: should return the backend name
    const char* name = fst24_backend_name(test_file);
    if (name == NULL || strcmp(name, backend) != 0) {
        App_Log(APP_ERROR, "fst24_backend_name returned '%s', expected '%s'\n",
                name ? name : "(null)", backend);
        fst24_close(test_file);
        return -1;
    }

    // Not open (NULL pointer): should return NULL
    if (fst24_backend_name(NULL) != NULL) {
        App_Log(APP_ERROR, "fst24_backend_name(NULL) should return NULL\n");
        fst24_close(test_file);
        return -1;
    }

    fst24_close(test_file);
    remove(test_filename);
    return 0;
}

int main(void) {
    if (do_test("RSF") != 0) return -1;

    if (do_test("XDF") != 0) return -1;

    App_Log(APP_ALWAYS, "Test successful\n");
    return 0;
}

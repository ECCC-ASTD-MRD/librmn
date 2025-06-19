#include "rmn/fst24_record.h"

fst_record get_default_fst_record(void) {
    // Used to get a default fst record and by the __new__() method
    // of the fst_record class
    return default_fst_record;
}

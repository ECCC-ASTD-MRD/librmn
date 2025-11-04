#include "rmn/primitives.h"

#include <string.h>

int32_t c_exdb(const char* titre, const char* revis, const char* flag) {
    return f77_name(exdb)(titre, revis, flag, strlen(titre), strlen(revis), strlen(flag));
}

int32_t c_exfin(const char* titre, const char* revis, const char* flag) {
    return f77_name(exfin)(titre, revis, flag, strlen(titre), strlen(revis), strlen(flag));
}

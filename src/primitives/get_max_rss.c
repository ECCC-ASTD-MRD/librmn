#include <stdint.h>
#include <sys/time.h>
#include <sys/resource.h>

#include <rmn/rpnmacros.h>

int32_t f77name(get_max_rss)() {
    struct rlimit limits;
    struct rusage mydata;

    getrusage(RUSAGE_SELF, &mydata);
    return (mydata.ru_maxrss);
}

#include <stdint.h>
#include <sys/time.h>
#include <sys/resource.h>

#include <rmn/rpnmacros.h>

//! Get the maximum resident set size (peak memory usage) of the current process.
//! \return The peak memory usage in kilobytes, as reported by getrusage
int32_t get_max_rss(void) {
    struct rusage mydata;

    getrusage(RUSAGE_SELF, &mydata);
    return (mydata.ru_maxrss);
}

//! Fortran-mangled name for get_max_rss, kept for backward compatibility with
//! code that calls the routine through the mangled symbol (f77name).
//! \return The peak memory usage in kilobytes, as reported by getrusage
int32_t f77name(get_max_rss)(void) {
    return get_max_rss();
}

/* Test the rdtsc()/rdtscp() cycle counters from src/primitives/cpu_type.c in C.
 *
 * Only the raw cycle counts are exercised here. The seconds-returning
 * functions (wall_clock_seconds, rdtsc_seconds, rdtscp_seconds) are
 * deliberately NOT tested: they depend on the CPU frequency calibration in
 * get_cpu_capabilities(), which mis-parses brand strings that do not end in
 * "@ <freq>GHz" (e.g. "Intel(R) Xeon(R) 6767P"), and wall_clock_seconds has a
 * separate Fortran argument-passing issue.
 */
#include <stdio.h>
#include <stdint.h>

#include <rmn/cpu_type.h>

static int failures = 0;

#define CHECK(cond, ...) do { \
    if (cond) { printf("PASS: "); printf(__VA_ARGS__); printf("\n"); } \
    else      { printf("FAIL: "); printf(__VA_ARGS__); printf("\n"); failures++; } \
} while (0)

/* ~1e8 iterations of a volatile accumulate: tens of ms, not optimizable away */
static void busy_loop(void) {
    volatile uint64_t x = 0;
    for (uint64_t i = 0; i < 100000000; i++) x += i;
}

int main(void) {
    uint64_t t1, t2;

    /* rdtsc(): monotonic */
    t1 = rdtsc(); t2 = rdtsc();
    CHECK(t2 >= t1, "rdtsc() is monotonic (%lu -> %lu)",
          (unsigned long)t1, (unsigned long)t2);

    /* rdtscp(): monotonic */
    t1 = rdtscp(); t2 = rdtscp();
    CHECK(t2 >= t1, "rdtscp() is monotonic (%lu -> %lu)",
          (unsigned long)t1, (unsigned long)t2);

    /* rdtsc(): a busy loop must advance the counter by a large amount */
    t1 = rdtsc();
    busy_loop();
    t2 = rdtsc();
    CHECK(t2 > t1 && (t2 - t1) > 1000000,
          "rdtsc() advances by %lu ticks over a busy loop (> 1e6)",
          (unsigned long)(t2 - t1));

    /* rdtscp(): a busy loop must advance the counter by a large amount */
    t1 = rdtscp();
    busy_loop();
    t2 = rdtscp();
    CHECK(t2 > t1 && (t2 - t1) > 1000000,
          "rdtscp() advances by %lu ticks over a busy loop (> 1e6)",
          (unsigned long)(t2 - t1));

    if (failures) {
        printf("\n%d check(s) FAILED\n", failures);
        return 1;
    }
    printf("\nAll checks passed\n");
    return 0;
}

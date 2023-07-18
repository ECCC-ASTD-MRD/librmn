#ifndef FTNSTRLEN_H
#define FTNSTRLEN_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

unsigned int ftnStrLen(
    const char * const str,
    const uint32_t maxLen
);

#ifdef __cplusplus
}
#endif

#endif

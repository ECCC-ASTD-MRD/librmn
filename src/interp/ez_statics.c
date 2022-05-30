#include <stddef.h>

#include "ez_def.h"

// Ezscint static object definitions
_Grille **Grille  = NULL;
_Grille **gr_list = NULL;
int32_t nGrilles = 0;
int32_t nGrillesMax = CHUNK * CHUNK;
int32_t cur_log_chunk = 7;

__thread int32_t nsets    = 0;
__thread int32_t iset     = -1;
__thread int32_t iset_gdin = -1;
__thread int32_t iset_gdout = -1;
__thread _gridset *gridset = NULL;
 __thread _groptions groptions = { 1, CUBIQUE,  MAXIMUM, 0, -1, SYM, SCALAIRE, 0, 0, 1, 16, 0, DISTANCE, NEAREST, 0.5, 3.0, 0.0  };

int32_t log_chunks[] = {0, 1,  2, 3,    4,    5,    6,      7,     8,      9,      10,     11,        12};
int32_t primes[]     = {0, 0,  3, 7,   13,   31,   61,    127,   251,    509,    1021,   2039,      4093};
int32_t chunks[]     = {0, 0,  4, 8,   16,   32,   64,    128,   256,    512,    1024,   2048,      4096};
int32_t primes_sq[]  = {0, 0,  3, 61, 251, 1021, 4093,  16381, 65521, 262139, 1048573, 4194301, 16777213};
int32_t chunks_sq[]  = {0, 0, 16, 64, 256, 1024, 4096,  16384, 65536, 262144, 1048576, 4194304, 16777216};

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

int compact_p_integer(
    const void * const unpackedArrayOfInt,
    void * const packedHeader,
    void * const packedArrayOfInt,
    int intCount,
    int bitSizeOfPackedToken,
    int offset,
    int stride,
    const int sign
);
int compact_u_integer(
    void * const unpackedArrayOfInt,
    const void * const packedHeader,
    const void * const packedArrayOfInt,
    int intCount,
    int bitSizeOfPackedToken,
    int offset,
    int stride,
    const int sign
);

int compact_p_short(
    const void * const unpackedArray,
    void * const packedHeader,
    void * const packedArray,
    int intCount,
    const int bitSizeOfPackedToken,
    const int offset,
    const int stride
);
int compact_u_short(
    void * const unpackedArray,
    void * const packedHeader,
    const void * const packedArray,
    int intCount,
    const int bitSizeOfPackedToken,
    const int offset,
    const int stride
);

int compact_p_char(
    const void * const unpackedArrayOfBytes,
    void * const packedHeader,
    void * const packedArrayOfInt,
    int intCount,
    int bitSizeOfPackedToken,
    const int offset,
    const int stride
);
int compact_u_char(
    void * const unpackedArrayOfBytes,
    const void * const packedHeader,
    const void * const packedArrayOfInt,
    int intCount,
    int bitSizeOfPackedToken,
    const int offset,
    const int stride
);


int main() {
    const uint32_t ni = 4111;
    const uint32_t nj = 4093;
    const uint32_t nbits = 5;

    printf("nbits = %d\n", nbits);

    printf("\nUnsigned int32 with width auto detection\n");
    {
        uint32_t (* const orig)[nj] = malloc(sizeof(uint32_t[ni][nj]));

        srand(42);
        uint32_t max = 0;
        uint32_t min = UINT32_MAX;
        for (uint32_t i = 0; i < ni; i++) {
            for (uint32_t j = 0; j < nj; j++) {
                orig[i][j] = rand() % (1 << nbits);
                if (orig[i][j] > max) max = orig[i][j];
                if (orig[i][j] < min) min = orig[i][j];
            }
        }
        printf("%d <= origU[i][j] <= %d\n", min, max);

        //! The full size of the unpacked field is allocated.
        //! This is certainly too much, but we will have to check what compact_integer actually uses.
        uint32_t * const packed = malloc(sizeof(uint32_t[ni][nj]));

        int bitsNeeded = compact_p_integer(orig, NULL, packed, ni * nj, -1, 0, 1, 0);
        printf("compact_p_integer = %d\n", bitsNeeded);

        uint32_t (* const unpacked)[nj] = malloc(sizeof(uint32_t[ni][nj]));
        int retVal = compact_u_integer(unpacked, NULL, packed, ni * nj, bitsNeeded, 0, 1, 0);
        printf("compact_u_integer = %d\n", retVal);

        int same = 1;
        for (uint32_t i = 0; i < ni; i++) {
            for (uint32_t j = 0; j < nj; j++) {
                if (orig[i][j] != unpacked[i][j]) {
                    same = 0;
                    printf("orig[%d][%d](%d) != unpacked[%d][%d](%d)\n", i, j, orig[i][j], i, j, unpacked[i][j]);
                    break;
                }
            }
            if (!same) return !same;
        }

        free(unpacked);
        free(packed);
        free(orig);
    }


    printf("\nSigned int32 with width auto detection\n");
    {
        int32_t (* const orig)[nj] = malloc(sizeof(int32_t[ni][nj]));

        srand(42);
        int32_t max = INT32_MIN;
        int32_t min = INT32_MAX;
        for (uint32_t i = 0; i < ni; i++) {
            for (uint32_t j = 0; j < nj; j++) {
                orig[i][j] = (rand() % (1 << nbits)) - (1 << nbits) / 2;
                if (orig[i][j] > max) max = orig[i][j];
                if (orig[i][j] < min) min = orig[i][j];
            }
        }
        printf("%d <= orig[i][j] <= %d\n", min, max);

        //! The full size of the unpacked field is allocated.
        //! This is certainly too much, but we will have to check what compact_integer actually uses.
        uint32_t * const packed = malloc(sizeof(uint32_t[ni][nj]));

#ifndef NDEBUG
        for (uint32_t j = 0; j < 16; j++) {
            printf("orig[0][%d] = 0x%08x\n", j, orig[0][j]);
        }
#endif

        int bitsNeeded = compact_p_integer(orig, NULL, packed, ni * nj, -1, 0, 1, 1);
        printf("compact_p_integer = %d\n", bitsNeeded);

#ifndef NDEBUG
        for (uint32_t j = 0; j < 2; j++) {
            printf("packed[%d] = 0x%x\n", j, packed[j]);
        }
#endif

        int32_t (* const unpacked)[nj] = malloc(sizeof(int32_t[ni][nj]));
        int retVal = compact_u_integer(unpacked, NULL, packed, ni * nj, bitsNeeded, 0, 1, 1);
        printf("compact_u_integer = %d\n", retVal);

        int same = 1;
        for (uint32_t i = 0; i < ni; i++) {
            for (uint32_t j = 0; j < nj; j++) {
                if (orig[i][j] != unpacked[i][j]) {
                    same = 0;
                    printf("orig[%d][%d](%d) != unpacked[%d][%d](%d)\n", i, j, orig[i][j], i, j, unpacked[i][j]);
                    break;
                }
            }
            if (!same) return !same;
        }

        free(orig);
        free(packed);
        free(unpacked);
    }


    printf("\nUnsigned short with width auto detection\n");
    {
        uint16_t (* const orig)[nj] = malloc(sizeof(uint16_t[ni][nj]));

        srand(42);
        uint16_t max = 0;
        uint16_t min = UINT16_MAX;
        for (uint32_t i = 0; i < ni; i++) {
            for (uint32_t j = 0; j < nj; j++) {
                orig[i][j] = (rand() % (1 << nbits));
                if (orig[i][j] > max) max = orig[i][j];
                if (orig[i][j] < min) min = orig[i][j];
            }
        }
        printf("%d <= orig[i][j] <= %d\n", min, max);

        //! The full size of the unpacked field is allocated.
        //! This is certainly too much, but we will have to check what compact_integer actually uses.
        uint32_t * const packed = malloc(sizeof(uint32_t[ni][nj]));

#ifndef NDEBUG
        for (uint32_t j = 0; j < 16; j++) {
            printf("orig[0][%d] = 0x%08x\n", j, orig[0][j]);
        }
#endif

        int bitsNeeded = compact_p_short(orig, NULL, packed, ni * nj, -1, 0, 1);
        printf("compact_p_short = %d\n", bitsNeeded);

#ifndef NDEBUG
        for (uint32_t j = 0; j < 2; j++) {
            printf("packed[%d] = 0x%x\n", j, packed[j]);
        }
#endif

        uint16_t (* const unpacked)[nj] = malloc(sizeof(uint16_t[ni][nj]));
        int retVal = compact_u_short(unpacked, NULL, packed, ni * nj, bitsNeeded, 0, 1);
        printf("compact_u_short = %d\n", retVal);

        int same = 1;
        for (uint32_t i = 0; i < ni; i++) {
            for (uint32_t j = 0; j < nj; j++) {
                if (orig[i][j] != unpacked[i][j]) {
                    same = 0;
                    printf("orig[%d][%d](%d) != unpacked[%d][%d](%d)\n", i, j, orig[i][j], i, j, unpacked[i][j]);
                    break;
                }
            }
            if (!same) return !same;
        }

        free(orig);
        free(packed);
        free(unpacked);
    }

    printf("\nUnsigned short with fixed width\n");
    {
        uint16_t (* const orig)[nj] = malloc(sizeof(uint16_t[ni][nj]));

        srand(42);
        uint16_t max = 0;
        uint16_t min = UINT16_MAX;
        for (uint32_t i = 0; i < ni; i++) {
            for (uint32_t j = 0; j < nj; j++) {
                orig[i][j] = (rand() % (1 << nbits));
                if (orig[i][j] > max) max = orig[i][j];
                if (orig[i][j] < min) min = orig[i][j];
            }
        }
        printf("%d <= orig[i][j] <= %d\n", min, max);

        //! The full size of the unpacked field is allocated.
        //! This is certainly too much, but we will have to check what compact_integer actually uses.
        uint32_t * const packed = malloc(sizeof(uint32_t[ni][nj]));

#ifndef NDEBUG
        for (uint32_t j = 0; j < 16; j++) {
            printf("orig[0][%d] = 0x%08x\n", j, orig[0][j]);
        }
#endif

        int bitsNeeded = compact_p_short(orig, NULL, packed, ni * nj, nbits, 0, 1);
        printf("compact_p_short = %d\n", bitsNeeded);

#ifndef NDEBUG
        for (uint32_t j = 0; j < 2; j++) {
            printf("packed[%d] = 0x%x\n", j, packed[j]);
        }
#endif

        uint16_t (* const unpacked)[nj] = malloc(sizeof(uint16_t[ni][nj]));
        int retVal = compact_u_short(unpacked, NULL, packed, ni * nj, nbits, 0, 1);
        printf("compact_u_short = %d\n", retVal);

        int same = 1;
        for (uint32_t i = 0; i < ni; i++) {
            for (uint32_t j = 0; j < nj; j++) {
                if (orig[i][j] != unpacked[i][j]) {
                    same = 0;
                    printf("orig[%d][%d](%d) != unpacked[%d][%d](%d)\n", i, j, orig[i][j], i, j, unpacked[i][j]);
                    break;
                }
            }
            if (!same) return !same;
        }

        free(orig);
        free(packed);
        free(unpacked);
    }

    printf("\nUnsigned char with width auto detection\n");
    {
        uint8_t (* const orig)[nj] = malloc(sizeof(uint8_t[ni][nj]));

        srand(42);
        uint8_t max = 0;
        uint8_t min = UINT8_MAX;
        for (uint32_t i = 0; i < ni; i++) {
            for (uint32_t j = 0; j < nj; j++) {
                orig[i][j] = (rand() % (1 << nbits));
                if (orig[i][j] > max) max = orig[i][j];
                if (orig[i][j] < min) min = orig[i][j];
            }
        }
        printf("%d <= orig[i][j] <= %d\n", min, max);

        //! The full size of the unpacked field is allocated.
        //! This is certainly too much, but we will have to check what compact_integer actually uses.
        uint32_t * const packed = malloc(sizeof(uint32_t[ni][nj]));

#ifndef NDEBUG
        for (uint32_t j = 0; j < 16; j++) {
            printf("orig[0][%d] = 0x%08x\n", j, orig[0][j]);
        }
#endif

        int bitsNeeded = compact_p_char(orig, NULL, packed, ni * nj, -1, 0, 1);
        printf("compact_p_char = %d\n", bitsNeeded);

#ifndef NDEBUG
        for (uint32_t j = 0; j < 2; j++) {
            printf("packed[%d] = 0x%x\n", j, packed[j]);
        }
#endif

        uint8_t (* const unpacked)[nj] = malloc(sizeof(uint8_t[ni][nj]));
        int retVal = compact_u_char(unpacked, NULL, packed, ni * nj, bitsNeeded, 0, 1);
        printf("compact_u_char = %d\n", retVal);

        int same = 1;
        for (uint32_t i = 0; i < ni; i++) {
            for (uint32_t j = 0; j < nj; j++) {
                if (orig[i][j] != unpacked[i][j]) {
                    same = 0;
                    printf("orig[%d][%d](%d) != unpacked[%d][%d](%d)\n", i, j, orig[i][j], i, j, unpacked[i][j]);
                    break;
                }
            }
            if (!same) return !same;
        }

        free(orig);
        free(packed);
        free(unpacked);
    }

    printf("\nUnsigned char with fixed width\n");
    {
        uint8_t (* const orig)[nj] = malloc(sizeof(uint8_t[ni][nj]));

        srand(42);
        uint8_t max = 0;
        uint8_t min = UINT8_MAX;
        for (uint32_t i = 0; i < ni; i++) {
            for (uint32_t j = 0; j < nj; j++) {
                orig[i][j] = (rand() % (1 << nbits));
                if (orig[i][j] > max) max = orig[i][j];
                if (orig[i][j] < min) min = orig[i][j];
            }
        }
        printf("%d <= orig[i][j] <= %d\n", min, max);

        //! The full size of the unpacked field is allocated.
        //! This is certainly too much, but we will have to check what compact_integer actually uses.
        uint32_t * const packed = malloc(sizeof(uint32_t[ni][nj]));

#ifndef NDEBUG
        for (uint32_t j = 0; j < 16; j++) {
            printf("orig[0][%d] = 0x%08x\n", j, orig[0][j]);
        }
#endif

        int bitsNeeded = compact_p_char(orig, NULL, packed, ni * nj, nbits, 0, 1);
        printf("compact_p_char = %d\n", bitsNeeded);

#ifndef NDEBUG
        for (uint32_t j = 0; j < 2; j++) {
            printf("packed[%d] = 0x%x\n", j, packed[j]);
        }
#endif

        uint8_t (* const unpacked)[nj] = malloc(sizeof(uint8_t[ni][nj]));
        int retVal = compact_u_char(unpacked, NULL, packed, ni * nj, nbits, 0, 1);
        printf("compact_u_char = %d\n", retVal);

        int same = 1;
        for (uint32_t i = 0; i < ni; i++) {
            for (uint32_t j = 0; j < nj; j++) {
                if (orig[i][j] != unpacked[i][j]) {
                    same = 0;
                    printf("orig[%d][%d](%d) != unpacked[%d][%d](%d)\n", i, j, orig[i][j], i, j, unpacked[i][j]);
                    break;
                }
            }
            if (!same) return !same;
        }

        free(orig);
        free(packed);
        free(unpacked);
    }
    return 0;
}

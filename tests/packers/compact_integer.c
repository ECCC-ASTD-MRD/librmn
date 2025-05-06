#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

int compact_integer(
    void *unpackedArrayOfInt,
    void *packedHeader,
    void *packedArrayOfInt,
    int elementCount,
    int bitSizeOfPackedToken,
    int offset,
    int stride,
    int opCode
);

int main() {
    const uint32_t ni = 4111;
    const uint32_t nj = 4093;
    const uint32_t nbits = 5;

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

        int bitsNeeded = compact_integer(orig, NULL, packed, ni * nj, -1, 0, 1, 1);
        printf("compact_integer(pack) = %d\n", bitsNeeded);

        uint32_t (* const unpacked)[nj] = malloc(sizeof(uint32_t[ni][nj]));
        int retVal = compact_integer(unpacked, NULL, packed, ni * nj, bitsNeeded, 0, 1, 2);
        printf("compact_integer(unpack) = %d\n", retVal);

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

        int bitsNeeded = compact_integer(orig, NULL, packed, ni * nj, -1, 0, 1, 3);
        printf("compact_integer(pack) = %d\n", bitsNeeded);

        int32_t (* const unpacked)[nj] = malloc(sizeof(int32_t[ni][nj]));
        int retVal = compact_integer(unpacked, NULL, packed, ni * nj, bitsNeeded, 0, 1, 4);
        printf("compact_integer(unpack) = %d\n", retVal);

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
    }

    return 0;
}
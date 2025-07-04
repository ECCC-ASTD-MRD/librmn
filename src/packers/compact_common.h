    // Declare header type
    typedef struct {
#if defined(Little_Endian)
        uint32_t counter : 20, marker : 12, minSign : 4, minExpo : 12, rangeExpo : 16;
        uint32_t minMantisa32 : 32, emptySpace : 8, bitSize : 8, minMantisa16 : 16;
#else
        uint32_t marker : 12, counter : 20, rangeExpo : 16, minExpo : 12, minSign : 4;
        uint32_t minMantisa32 : 32, minMantisa16 : 16, bitSize : 8, emptySpace : 8;
#endif
    } xxpack_struct_data;

    // Handle abnormal condition
    if ( packedTokenBitSize == 0 ) {
        // token size is 0
        return NULL;
    }
    if (( packedTokenBitSize == 1 ) &&  hasMissing ) {
        // missing value handling routine fails if token size is 1
        return NULL;
    }

    int packedTokenBS = packedTokenBitSize;
    int EffectivePackedTokenSize = 0;      // only set with special case when packedTokenBS > 64
    if (packedTokenBS > 64) {
        EffectivePackedTokenSize = packedTokenBS >> 6;
        packedTokenBS &= 0x3F;
    } else {
        EffectivePackedTokenSize = packedTokenBS;
    }

    // Obtain an array of power of 2
    if ( ! powerOf2sInitialized ) {
        powerOf2s[0] = 1.0;
        for (int i = 1; i < powerSpan; i++) {
            powerOf2s[i] = 2.0 *powerOf2s[i-1];
        }
        powerOf2sInitialized = 1;
    }

    // Determine wordsize
    int wordSize = 8 * sizeof(int32_t);

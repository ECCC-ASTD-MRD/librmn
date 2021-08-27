/**
 * \file crc32_.h
 * Functions and types for CRC checks.
 *
 * Generated on Fri Feb  8 10:07:03 2013,
 * by pycrc v0.8, http://www.tty1.net/pycrc/
 * using the configuration:
 *    Width        = 32
 *    Poly         = 0x04c11db7
 *    XorIn        = 0xffffffff
 *    ReflectIn    = True
 *    XorOut       = 0xffffffff
 *    ReflectOut   = True
 *    Algorithm    = table-driven
 *****************************************************************************/
#ifndef __CRC32__H__
#define __CRC32__H__

#include <stdlib.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif


//! The definition of the used algorithm
#define CRC_ALGO_TABLE_DRIVEN 1


/**
 * The type of the CRC values.
 *
 * This type must be big enough to contain at least 32 bits.
 *****************************************************************************/
typedef uint32_t crc32_t;


//! Reflect all bits of a \a data word of \a data_len bytes
//! \return The reflected data
crc32_t crc32_reflect(
    //! The data word to be reflected
    crc32_t data,
    //! [in] The width of \a data expressed in number of bits
    size_t data_len
);



//! Calculate the initial crc value.
//! \return The initial crc value.
static crc32_t crc32_init(void)
{
    return 0xffffffff;
}


//! Update the crc value with new data.
//! \return The updated crc value
crc32_t crc32_update(
    //! [in] The current crc value
    crc32_t crc,
    //! [in] Pointer to a buffer of \a data_len bytes
    const unsigned char *data,
    //! Number of bytes in the \a data buffer
    size_t data_len
);
crc32_t crc32_update_le(crc32_t crc, const unsigned char *data, size_t data_len, int mask);


//! Calculate the final crc value.
//! \return The final crc value.
static crc32_t crc32_finalize(
    //! [in] The current crc value
    crc32_t crc
) {
    return crc ^ 0xffffffff;
}


#ifdef __cplusplus
}           /* closing brace for extern "C" */
#endif

#endif      /* __CRC32__H__ */

/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
 *                          Environnement Canada
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

//! \file fread32.c FREAD and FWRITE interface with bytes swap

#include <stdio.h>
#include <stdint.h>

#include <App.h>
#include <rmn/rpnmacros.h>


static int endian_int = 1;
static char *little_endian = (char *)&endian_int;


//! Read data and swap 8 bits by 8 bits of each 2 bytes elements
size_t fread16(
    //! [in] Pointer to array where data will be written
    void *ptr,
    //! [in] Size in bytes of elements of data
    size_t size,
    //! [in] Number of items to read
    size_t nitems,
    //! [in] File from which to read
    FILE *stream
) {
    size_t nr;
    // Number of 2 bytes
    int n2 = (size * nitems) / 2;
    unsigned short *pt2 = (unsigned short *) ptr;

    if (*little_endian) {
        if ((size & 1) != 0) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: size=%d must be a multiple of 2\n",__func__,size);
            return -1;
        }

        nr = fread(ptr, size, nitems, stream);

        for (int i=0; i < n2; i++) {
            *pt2 = (*pt2 >> 8) | (*pt2 << 8);
            pt2++;
        }
    } else {
        nr = fread(ptr, size, nitems, stream);
    }
    return (size_t)nr;
}


//! Read data and swap each bytes for each 4 bytes elements
size_t fread32(
    //! [in] Pointer to array where data will be written
    void *ptr,
    //! [in] Size in bytes of elements of data
    size_t size,
    //! [in] Number of items to read
    size_t nitems,
    //! [in] File pointer to the file from which to read
    FILE *stream
) {
    size_t nr;
    // Number of 4 bytes
    int n4 = (size * nitems) / 4;
    uint32_t *pt4 = (uint32_t *) ptr;

    if (*little_endian) {
        if ((size & 3) != 0) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: size=%d must be a multiple of 4\n",__func__,size);
            return -1;
        }

        nr = fread(ptr, size, nitems, stream);

        for (int i = 0; i < n4; i++) {
            *pt4 = (*pt4 >> 24) | (*pt4 << 24) | ((*pt4 >> 8) & 0xFF00) | ((*pt4 & 0xFF00) << 8);
            pt4++;
        }
    } else {
        nr = fread(ptr, size, nitems, stream);
    }

    return (size_t)nr;
}


//! Read data and swap each bytes of each 32 bits elements and then swap every 2 32 bits elements
size_t fread64(
    //! [in] Pointer to array where data will be written
    void *ptr,
    //! [in] Size in bytes of elements of data
    size_t size,
    //! [in] Number of items to read
    size_t nitems,
    //! [in] File pointer to the file from which to read
    FILE *stream
) {
    size_t nr;
    // Number of 4 bytes
    int n4 = (size * nitems) / 4;
    uint32_t *pt4 = (uint32_t *)ptr;
    int32_t temp;

    if (*little_endian) {
        if ((size & 3) != 0) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: size=%d must be a multiple of 4\n",__func__,size);
            return -1;
        }

        nr = fread(ptr, size, nitems, stream);

        for (int i = 0; i < n4; i++) {
            *pt4 = (*pt4 >> 24) | (*pt4 << 24) | ((*pt4 >> 8) & 0xFF00) | ((*pt4 & 0xFF00) << 8);
            pt4++;
        }

        pt4 = (uint32_t *) ptr;
        for (int i = 0; i < n4/2; i++) {
            temp = *pt4;
            *pt4 = *(pt4+1);
            pt4++;
            *pt4 = temp;
            pt4++;
        }
    } else {
        nr = fread(ptr, size, nitems, stream);
    }

    return (size_t)nr;
}


//! Write data to file after swapping 8 bits by 8 bits of each 2 bytes elements
size_t fwrite16(
    //! [in] Pointer to array from which to read data
    void *ptr,
    //! [in] Size in bytes of elements of data
    size_t size,
    //! [in] Number of items to write
    size_t nitems,
    //! [in] File pointer to the file to write into
    FILE *stream
) {
    size_t nr;
    // Number of 2 bytes
    int n2 = (size * nitems) / 2;
    unsigned short *pt2 = (unsigned short *) ptr;

    if (*little_endian) {
        if ((size & 1) != 0) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: size=%d must be a multiple of 2\n",__func__,size);
            return -1;
        }

        for (int i = 0; i < n2; i++) {
            *pt2 = (*pt2 >> 8) | (*pt2 << 8);
            pt2++;
        }

        nr = fwrite(ptr, size, nitems, stream);

        pt2 = (unsigned short *) ptr;
        for (int i = 0; i < n2; i++) {
            *pt2 = (*pt2 >> 8) | (*pt2 << 8);
            pt2++;
        }
    } else {
        nr = fwrite(ptr, size, nitems, stream);
    }

    return (size_t)nr;
}


//! Write data to file after swapping each bytes for each 4 bytes elements
size_t fwrite32(
    //! [in] Pointer to array from which to read data
    void *ptr,
    //! [in] Size in bytes of elements of data
    size_t size,
    //! [in] Number of items to write
    size_t nitems,
    //! [in] File pointer to the file to write into
    FILE *stream
) {
    size_t nr;
    // Number of 4 bytes
    int n4 = (size * nitems) / 4;
    uint32_t *pt4 = (uint32_t *) ptr;

    if (*little_endian) {
        if ((size & 3) != 0) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: size=%d must be a multiple of 4\n",__func__,size);
            return -1;
        }

        for (int i = 0; i < n4; i++) {
            *pt4 = (*pt4 >> 24) | (*pt4 << 24) | ((*pt4 >> 8) & 0xFF00) | ((*pt4 & 0xFF00) << 8);
            pt4++;
        }

        nr = fwrite(ptr, size, nitems, stream);

        pt4 = (uint32_t *) ptr;
        for (int i = 0; i < n4; i++) {
            *pt4 = (*pt4 >> 24) | (*pt4 << 24) | ((*pt4 >> 8) & 0xFF00) | ((*pt4 & 0xFF00) << 8);
            pt4++;
        }
    } else {
        nr = fwrite(ptr, size, nitems, stream);
    }

    return (size_t)nr;
}


//! Write data to file after swapping each bytes of each 32 bits elements and then swap every 2 32 bits elements
size_t fwrite64(
    //! [in] Pointer to array from which to read data
    void *ptr,
    //! [in] Size in bytes of elements of data
    size_t size,
    //! [in] Number of items to write
    size_t nitems,
    //! [in] File pointer to the file to write into
    FILE *stream
) {
    size_t nr;
    // Number of 4 bytes
    int n4 = (size * nitems) / 4;
    uint32_t *pt4 = (uint32_t *)ptr;
    int32_t temp;

    if (*little_endian) {
        if ((size & 3) != 0) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: size=%d must be a multiple of 4\n",__func__,size);
            return -1;
        }

        for (int i = 0; i < n4; i++) {
            *pt4 = (*pt4 >> 24) | (*pt4 << 24) | ((*pt4 >> 8) & 0xFF00) | ((*pt4 & 0xFF00) << 8);
            pt4++;
        }

        pt4 = (uint32_t *)ptr;
        for (int i = 0; i < n4 / 2; i++) {
            temp = *pt4;
            *pt4 = *(pt4+1);
            pt4++;
            *pt4 = temp;
            pt4++;
        }

        nr = fwrite(ptr, size, nitems, stream);

        pt4 = (uint32_t *)ptr;
        for (int i = 0; i < n4; i++) {
            *pt4 = (*pt4 >> 24) | (*pt4 << 24) | ((*pt4 >> 8) & 0xFF00) | ((*pt4 & 0xFF00) << 8);
            pt4++;
        }

        pt4 = (uint32_t *)ptr;
        for (int i = 0; i < n4/2; i++) {
            temp = *pt4;
            *pt4 = *(pt4+1);
            pt4++;
            *pt4 = temp;
            pt4++;
        }
    } else {
        nr = fwrite(ptr, size, nitems, stream);
    }

    return (size_t)nr;
}

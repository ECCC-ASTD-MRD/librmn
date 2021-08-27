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

#include <rpnmacros.h>
#include <fnom.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#ifdef WIN32    /*CHC/NRC*/
#include <io.h>
#endif

#define MAX_FILES 10

int32_t f77name(open_db_file) (int32_t * );
int32_t f77name(close_db_file) (int32_t * );
int32_t f77name(read_db_file) (int32_t *, int32_t *, int32_t *);
int32_t f77name(write_db_file) (int32_t *, int32_t *, int32_t  *);
int32_t f77name(rewind_db_file) (int32_t *);

static int file_index(int );
static void init_TB_package(int);

static int bufSize = 2 * 1024 * 1024 ;
static int bufSizeW;
static int initialized = 0;

typedef struct {
    int iun, fd;
    int pos_in, pos_out;
    int nb_elmt_lu, nb_elmt_ecrit, nb_elmt_Buf_IN;
    int32_t *pBuf_IN, *pBuf_OUT;
} Twin_Buffer;

static Twin_Buffer TB[MAX_FILES];


//! Find the index of a file in the file table
//! \return Index of the file in the file table
static int fnom_index(
    //! [in] Fortran unit number of the file
    int iun
) {
    /* find file index in file table */
    for (int i = 0; i < MAXFILES; i++) {
      if (FGFDT[i].iun == iun) return(i);
    }
    return -1;
}


//! Initialize Twin_Buffer structure table
static void init_TB_package(int inutile)
{
    char *envar;

    envar = getenv("DB_FILE_CONFIG");
    if ( envar != NULL ) {
        bufSize = atoi (envar) * sizeof(int32_t);
    }

    bufSizeW = (bufSize / sizeof(int32_t));
    for (int i = 0; i < MAX_FILES; i++) {
        TB[i].fd = -1;
        TB[i].iun = -1;
        TB[i].pos_in = 0;
        TB[i].pos_out = 0;
        TB[i].nb_elmt_lu = 0;
        TB[i].nb_elmt_ecrit = 0;
        TB[i].nb_elmt_Buf_IN = 0;
        TB[i].pBuf_IN = NULL;
        TB[i].pBuf_OUT = NULL;
    }
    initialized = 1;
}


//! Return file descriptor. Allocate space for buffers.
int32_t f77name(open_db_file)(
    //! [in] Fortran unit number of the file
    int32_t *iun
) {
    int indx, f, i;

    if (! initialized) {
        fprintf(stderr, "Initializing  Dual Buffer Package\n");
        init_TB_package(123);
        fprintf(stderr, "Dual Buffer Package Initialized\n");
        fprintf(stderr, "Buffer size =%u Words\n", bufSizeW);
    }

    indx = fnom_index(*iun);
    if (indx < 0) {
        fprintf(stderr, "open_db_file error: file (unit=%d) not connected with fnom\n", *iun);
        exit(4);
    }

    f = -1;
    for (i = 0; i < MAX_FILES; i++) {
        if (TB[i].iun == -1) {
            f = i;
            break;
        }
    }
    if (f == -1) {
        fprintf(stderr, "open_db_file error: too may files\n");
        exit(5);
    }

    TB[f].fd = FGFDT[indx].fd;
    TB[f].iun = *iun;

    if((TB[f].pBuf_IN = malloc (bufSize)) == NULL) {
        fprintf(stderr, "CAN'T ALLOCATE MEMORY FOR BUFFER IN\n");
        exit(1);
    }

    if((TB[f].pBuf_OUT = malloc (bufSize)) == NULL){
        fprintf(stderr, "CAN'T ALLOCATE MEMORY FOR BUFFER OUT\n");
        exit(1);
    }

    return TB[f].fd;
}


//! Read data into the bucket
int32_t f77name(read_db_file) (
    //! [in] Fortran unit number corresponding to the file from which to read
    int32_t *iun,
    //! [out] Buffer were data read will be written
    int32_t *bucket,
    //! [in] number of elements to read
    int32_t *nElem
) {
   int nbytes;
   int data_a_lire = *nElem;
   int fd, fileIndex;
   int32_t *p_bucket, *p_bufin;

    fileIndex = -1;
    for (int i = 0; i < MAX_FILES; i++) {
        if (TB[i].iun == *iun) {
            fileIndex = i;
            break;
        }
    }
    if (fileIndex == -1) {
        fprintf(stderr, "read_db_file error: file (unit=%d) not open\n", *iun);
        exit(7);
    }

    fd = TB[fileIndex].fd;
    // Why use this weird pointer thing instead of using the buffer pointer directly?
    p_bucket = &(bucket[0]);

    while (data_a_lire) {
        p_bufin = &(TB[fileIndex].pBuf_IN[TB[fileIndex].pos_in]);

        if (data_a_lire <= TB[fileIndex].nb_elmt_Buf_IN) {
            for (int i = 0; i < data_a_lire; i++) {
                p_bucket[i] = p_bufin[i];
            }
            TB[fileIndex].pos_in += data_a_lire;
            TB[fileIndex].nb_elmt_Buf_IN -= data_a_lire;
            data_a_lire = 0;
        } else {
            for (int i = 0; i < TB[fileIndex].nb_elmt_Buf_IN; i++) {
                p_bucket[i] = p_bufin[i];
            }
            p_bucket += TB[fileIndex].nb_elmt_Buf_IN;
            p_bufin += TB[fileIndex].nb_elmt_Buf_IN;
            data_a_lire -= TB[fileIndex].nb_elmt_Buf_IN;
            lseek(fd, TB[fileIndex].nb_elmt_lu * sizeof(int32_t), SEEK_SET);
            nbytes = read(fd, TB[fileIndex].pBuf_IN, bufSize);
            if (nbytes <= 0) {
                fprintf(stderr, "read_db_file error: try to read past end of file\n");
                return -1;
            }
            TB[fileIndex].nb_elmt_lu += nbytes / sizeof(int32_t);
            TB[fileIndex].nb_elmt_Buf_IN = nbytes / sizeof(int32_t);
            TB[fileIndex].pos_in = 0;
        }
    }

    return TB[fileIndex].nb_elmt_lu;
}


//! Write bucket in the file
int32_t f77name(write_db_file)(
    //! [in] Fortran unit number corresponding to the file into which to write
    int32_t *iun,
    //! [in] Data to write
    int32_t *bucket,
    //! [in] Number of elements to write from bucket
    int32_t *NB
) {
    int data_a_ecrire = *NB;
    int nbytes, room_left, f, fd;
    int32_t *p_bucket, *p_bufout;

    f = -1;
    for (int i = 0; i < MAX_FILES; i++) {
        if (TB[i].iun == *iun) {
            f = i;
            break;
        }
    }
    if (f == -1) {
        fprintf(stderr, "write_db_file error: file (unit=%d) not open\n", *iun);
        exit(7);
    }

    fd = TB[f].fd;

    if (((TB[f].nb_elmt_ecrit + data_a_ecrire + TB[f].pos_out) >=
        (TB[f].nb_elmt_lu + TB[f].pos_in)) && (TB[f].nb_elmt_lu != 0)) {
        fprintf(stderr, "write_db_file error: rewriting over unread data\n");
        exit(1);
    }

    room_left = bufSizeW - TB[f].pos_out;
    p_bucket = &(bucket[0]);

    while (data_a_ecrire) {
        p_bufout = &(TB[f].pBuf_OUT[TB[f].pos_out]);

        if (data_a_ecrire <= room_left) {
            for (int i = 0; i < data_a_ecrire; i++) {
                p_bufout[i] = p_bucket[i];
            }
            p_bufout += data_a_ecrire;
            TB[f].pos_out += data_a_ecrire;
            room_left -= data_a_ecrire;
            data_a_ecrire = 0;
        } else {
            for (int i = 0; i < room_left; i++) {
                p_bufout[i] = p_bucket[i];
            }
            p_bufout += room_left;
            p_bucket += room_left;
            lseek(fd, TB[f].nb_elmt_ecrit * sizeof(int32_t), SEEK_SET);
            nbytes = write(fd, TB[f].pBuf_OUT, bufSize);
            if (nbytes != bufSize) {
                fprintf(stderr, "write_db_file error: can't write of file\n");
                exit(2);
            }
            TB[f].nb_elmt_ecrit += bufSizeW;
            data_a_ecrire -= room_left;
            TB[f].pos_out = 0;
            room_left = bufSizeW;
        }
    }

    return TB[f].nb_elmt_ecrit;
}



//! Clear buffers and return in the initial position of the file and the two buffers
int32_t f77name(rewind_db_file)(
    //! [in] Fortran unit number corresponding to the file to rewind
    int32_t *iun
) {
    int fd, nbytes, f;

    f = -1;
    for (int i = 0; i < MAX_FILES; i++) {
        if (TB[i].iun == *iun) {
            f = i;
            break;
        }
    }
    if (f == -1) {
        fprintf(stderr, "rewind_db_file error: file (unit=%d) not open\n", *iun);
        exit(7);
    }

    fd = TB[f].fd;

    TB[f].nb_elmt_lu = 0;

    if (TB[f].pos_out > 0) {
        lseek(fd, TB[f].nb_elmt_ecrit * sizeof(int32_t), SEEK_SET);
        nbytes = write(fd, TB[f].pBuf_OUT, TB[f].pos_out*sizeof(int32_t));
        if (nbytes != TB[f].pos_out*sizeof(int32_t)) {
            fprintf(stderr, "rewind_db_file error: can't write of file\n");
            exit(2);
        }
    }
    // Rewind the file
    lseek(fd, 0L, SEEK_SET);

    // Initialiser les buffer a zero
    for(int i = 0; i < bufSizeW; i++) {
        TB[f].pBuf_IN[i] = 0;
        TB[f].pBuf_OUT[i] = 0;
    }
    TB[f].nb_elmt_ecrit = 0;
    TB[f].nb_elmt_Buf_IN = 0;
    TB[f].pos_out = 0;
    TB[f].pos_in = 0;

    return 0;
}


//! Close file and flush output buffer
int32_t f77name(close_db_file)(
    //! [in] Fortran unit number corresponding to the file into which to close
    int32_t *iun
) {
    int ier, f;

    f = -1;
    for (int i = 0; i < MAX_FILES; i++) {
        if (TB[i].iun == *iun) {
            f = i;
            break;
        }
    }
    if (f == -1) {
        fprintf(stderr, "close_db_file error: file (unit=%d) not open\n", *iun);
        exit(7);
    }

    ier = f77name(rewind_db_file)(iun);

    TB[f].fd = -1;
    TB[f].iun = -1;
    TB[f].pos_in = 0;
    TB[f].pos_out = 0;
    TB[f].nb_elmt_lu = 0;
    TB[f].nb_elmt_ecrit = 0;
    free(TB[f].pBuf_IN);
    free(TB[f].pBuf_OUT);
    TB[f].pBuf_IN = NULL;
    TB[f].pBuf_OUT = NULL;

    return 0;
}

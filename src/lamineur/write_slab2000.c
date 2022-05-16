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
/* write_slab.c
   original author: Karim Tegguiche
   Revision V2.0 - V.Lee July 1997
   Revision V3.0 - V.Lee March 1998 (to catch more bugs)
   Revision V4.0 - V.Lee November 1998 (slabini will not allow append)
   Revision V5.0 - V.Lee January 1999  (slabopt added for parallel code)
   Revision V6.0 - V.Lee Dec 2000(convip1 determines IP1, remove IP1 check)
   Revision V7.0 - M.Valin Jan 2001 added # type grids and little_endian patch
                                     bugfix: added nbytes to SLB9 marker
                                     added: capability to write to pipes/sockets
   Revision V8.0 - V.Lee May 2001 (introduced slabig34 to determine IG3 and IG4
                            for # type grids, and open file even if it exists)
   Revision V9.0 - V.Lee Jan 2003(28bit check for IP1, IP2, IP3;24bit for NIO, NJO)
*/

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <errno.h>

#include <rpnmacros.h>
#include <slab.h>


#ifdef DEBUG
#define BUFSIZE 64
#else
#define BUFSIZE 1048576
#endif

#define GRTYPES "ABGLNSXYZ#EH"

//! Insert 32 bit integers into buffer. If buffer is full it gets written to appropriate file / pipe / socket
#define put_in_buffer(file_desc, from, Ibuffer, pos, nbre) \
    {\
        uint32_t *ptemp = (uint32_t *)from;\
        uint32_t *buffer = (uint32_t *)Ibuffer;\
        int nbmots = nbre;\
        while (nbmots--) { \
            if (pos == BUFSIZE) {\
                write_buf(file_desc, buffer, BUFSIZE);\
                pos = 0;\
            }\
            buffer[pos++] = *ptemp++;\
        }\
    }

/* character pointer aliased to integer. if value returned is 1
   (low byte) machine is little endian */

static int entier_quelconque = 1;
static char *little_endian = (char *)&entier_quelconque;

// Tolerance level for abort
static int ERROR_LEVEL = -2;

static file_table_desc file_table[MAX_SLAB_FILES];
static int f_index[MAX_SLAB_FILES];

static Id_Block_file  id_block;         /* SLB0 block */
static Slab_Descrt_file slab_descrt;    /* SLB1 block */
static Data_Block_file data_block;      /* SLB2 block */
static Slab_End slab_end;               /* SLB9 block */

static int *intBuffer, pos;
static int *iVal;
static int32_t *pVal;

static float *fBuffer;
static float *fVal;

// Global initialization flag
static int init = 0;
// am I processor 0
static int proc0 = 1;
// number of processors in run
static int numproc = 1;


static void init_index();
static int slab_exit(int level);
static int get_free_index(int fd);
static int get_file_index(int fd);


//! Write integer buffer to file / socket / pipe
static int write_buf(
    const int fd,
    const void * const Ibuffer,
    const int nitems
) {
    uint32_t *buffer = (uint32_t *)Ibuffer;
    int n = nitems;
    int nwritten;
    char *cbuf = (char *)buffer;

    /* slab files are BIG ENDIAN */
    if (*little_endian) {
        uint32_t *tmpbuf = buffer;
        while (n--) {
            uint32_t temp = *tmpbuf;
            *tmpbuf = SWAP32(temp);
            tmpbuf++;
        }
    }
    n = nitems * sizeof(int32_t);
    while (n > 0) {
        nwritten = write(fd, cbuf, n);
        // if (errno != 0) {
        //     char hname[64];
        //     perror("error type");
        //     gethostname(hname, sizeof(hname));
        //     fprintf(stderr, "\n***ERROR in write_buf, hostname=%s errno=%d\n", hname, errno);
        // }
        if (nwritten <= 0) return nwritten;
        n -= nwritten;
        cbuf += nwritten;
    }
    return nitems * sizeof(int32_t);
}


//! Quit if error is severe enough (ERROR_LEVEL)
//! \return Sevirity of the error
static int slab_exit(
    int level
) {
    if (level <= ERROR_LEVEL) {
        exit(level);
    }
    return level;
}


//! Initialize f_index values to all -1. Should only be used once.
static void init_index() {
    char *slab_config;

    if ( (slab_config=getenv("SLAB_CONFIG")) != NULL) {
        ERROR_LEVEL = atoi(slab_config);
        printf("NOTE: ERROR_LEVEL set to %d\n", ERROR_LEVEL);
    }
    for (int i = 0; i < MAX_SLAB_FILES; i++) {
        f_index[i] = -1;
        file_table[i].file_name[0] = '\0';
        for (int j = 0; j < MAX_SLAB_TYPES; j++) {
            file_table[i].nrows[j] = 0;
            file_table[i].count[j] = 0;
            file_table[i].nio[j] = 0;
            file_table[i].ni[j] = 0;
            file_table[i].i1[j] = 0;
            file_table[i].njo[j] = 0;
            file_table[i].nj[j] = 0;
            file_table[i].j1[j] = 0;
        }
        file_table[i].buffer = NULL;
        file_table[i].pos = 0;
    }
    init = 1;
 }


//! Get an available index entry in file table and record file information
//! \return Index of an available entry in the file table
static int get_free_index(
    //! [in] File descriptor
    const int fd
) {
    for (int i = 0; i < MAX_SLAB_FILES; i++) {
        if (f_index[i] == -1) {
            f_index[i] = fd;
            return i;
        }
    }

    fprintf(stderr, "\n***ERROR in GET_FREE_INDEX: slab file table is full\n");
    fprintf(stderr, "   MAX_SLAB_FILES = %d\n", MAX_SLAB_FILES);
    return slab_exit(ERR_TAB_FULL);
}


//! Find the index of the file table entry
//! \return Index of the corresponding entry
static int get_file_index(
    //! [in] File descriptor
    const int fd
) {
    for (int i = 0; i < MAX_SLAB_FILES; i++)
        if (f_index[i] == fd) {
            return i;
        }

    fprintf(stderr, "\n***ERROR in GET_FILE_INDEX: slab file not initialized\n");
    return slab_exit(ERR_NO_FILE);
}


//! Get number of processors in run and current processor number
int32_t f77name(slabopt)(
    //! [in] My processor number
    const int32_t * const f_proc,
    //! [in] Number of processors in run
    const int32_t * const f_numproc
) {
    numproc = *f_numproc;
    if (*f_proc != 0) {
        proc0 = 0;
    } else {
        proc0 = 1;
    }
    return proc0;
}


//! Initialize slab file
//! \return File descriptor of the slab file
int32_t f77name(slabini)(
    //! [in] File name
    const char * const f_name,
    //! [in] Origin date.  dateo[0] AAAAMMJJ, dateo[1] HHMMSS00
    const int32_t dateo[2],
    //! [in] Time step number
    const int32_t * const f_npas,
    //! [in] Duration of timestep (s)
    const int32_t * const f_deet,
    //! [in] Label
    const char * const f_etiket,
    F2Cl l1,
    F2Cl l2
) {
    int fd, ix, taille;
    char name[MAX_LEN], etiket[MAX_ETIKET];

    if (init == 0) init_index();

    /* get desired file name */
    int ll1 = (l1 < MAX_LEN) ? l1 : MAX_LEN - 1;
    strncpy(name, f_name, ll1);
    name[ll1] = '\0';

    while ((name[ll1-1] == ' ') && (ll1 > 1)) {
        ll1--;
        name[ll1] = '\0';
    }

    /* initialize etiket to nulls */
    for (int i = 0; i < MAX_ETIKET; i++) etiket[i] = '\0';

    int ll2 = (l2 < 12) ? l2 : MAX_ETIKET - 1;
    /* copy etiket */
    strncpy(etiket, f_etiket, ll2);
    etiket[ll2] = '\0';

    while ((etiket[ll2 - 1] == ' ') && (ll2 > 1)) {
        ll2--;
        etiket[ll2] = '\0';
    }

    if ((fd = open(name, O_RDWR | O_CREAT , 0744)) == ERR_NO_FILE) {
        char hname[64];
        gethostname(hname, sizeof(hname));
        fprintf(stderr, "\n***ERROR in SLABINI: error opening file %s errno=%d hostname=%s\n", name, errno, hname);
        slab_exit(-3);
    }

    ix = get_free_index(fd);
    if (ix == ERR_TAB_FULL) {
        fprintf(stderr, "\n***ERROR in SLABINI(%s): slab file table is full\n", name);
        return slab_exit(-2);
    }

    /* initialize tables associated to file */
    strcpy(file_table[ix].file_name, name);
    for (int j = 0; j < MAX_SLAB_TYPES; j++){
        file_table[ix].count[j] = 0;
        file_table[ix].nrows[j] = 0;
        file_table[ix].nio[j] = 0;
        file_table[ix].i1[j] = 0;
        file_table[ix].ni[j] = 0;
        file_table[ix].njo[j] = 0;
        file_table[ix].j1[j] = 0;
        file_table[ix].nj[j] = 0;
    }

    if (file_table[ix].buffer) {
        fprintf(stderr, "\n***ERROR in SLABINI(%s): memory for buffer already allocated\n", name);
        return slab_exit(-3);
    }

    if ((intBuffer = (int *) malloc(BUFSIZE * sizeof(int))) == NULL) {
        fprintf(stderr, "\n***ERROR in SLABINI(%s): Cannot allocate memory for buffer\n", name);
        return slab_exit(-3);
    }

    file_table[ix].buffer = (uint32_t*)intBuffer;

    id_block.slb0    = 'SLB0';
    id_block.nBytes  = 32;
    id_block.deet    = (int ) *f_deet;
    id_block.npas    = (int ) *f_npas;
    /* stuff etiket into id_block.Ietiket in endian proof fashion */
    id_block.Ietiket[0] = etiket[0]<<24 | etiket[1]<<16 | etiket[2]<<8 | etiket[3];
    id_block.Ietiket[1] = etiket[4]<<24 | etiket[5]<<16 | etiket[6]<<8 | etiket[7];
    id_block.Ietiket[2] = etiket[8]<<24 | etiket[9]<<16 | etiket[10]<<8 | etiket[11];
    id_block.dateo1  = (int ) dateo[0];
    id_block.dateo2  = (int ) dateo[1];
    id_block.val15   = 1.5;

    pos = 0;

    iVal = (int *) &id_block;
    taille = (sizeof(id_block) / sizeof(int));

    /* write SLB0 block */
    put_in_buffer(fd, iVal, intBuffer, pos, taille);

    file_table[ix].pos = pos;
    /* return file descriptor to caller */
    return (int32_t) fd;
}


//! Determine the values of IG3 and IG4 for '#' grids
int32_t f77name(slabig34)(
    //! [out] Grid descriptor 3 for '#' grids (istart + nipoints << 20)
    uint32_t * const f_ig3,
    //! [out] Grid descriptor 4 for '#' grids (jstart + njpoints << 20)
    uint32_t * const f_ig4,
    //! [in] istart
    const int32_t * const f_xmin,
    //! [in] iend
    const int32_t * const f_xmax,
    //! [in] jstart
    const int32_t * const f_ymin,
    //! [in] jend
    const int32_t * const f_ymax
) {
    if (*f_xmin > 0xFFFFF) {
        fprintf(stderr, "\n***ERROR in SLABIG34: (XMIN=%d) > 2**19\n", *f_xmin);
        return slab_exit(-2);
    }
    if (*f_xmin < 0) {
        fprintf(stderr, "\n***ERROR in SLABIG34: (XMIN=%d) < 0\n", *f_xmin);
        return slab_exit(-2);
    }
    if (*f_ymin > 0xFFFFF) {
        fprintf(stderr, "\n***ERROR in SLABIG34: (YMIN=%d) > 2**19\n", *f_ymin);
        return slab_exit(-2);
    }
    if (*f_ymin < 0) {
        fprintf(stderr, "\n***ERROR in SLABIG34: (YMIN=%d) < 0\n", *f_ymin);
        return slab_exit(-2);
    }

    uint32_t niout = *f_xmax - *f_xmin + 1;
    uint32_t njout = *f_ymax - *f_ymin + 1;

    if (niout > 0xFFF) {
        fprintf(stderr, "\n***ERROR in SLABIG34: (XMAX-XMIN=%d) > 2**11\n", niout);
        return slab_exit(-2);
    }
    if (njout > 0xFFF) {
        fprintf(stderr, "\n***ERROR in SLABIG34: (YMAX-YMIN=%d) > 2**11\n", njout);
        return slab_exit(-2);
    }

    *f_ig3 = (niout << 20) | *f_xmin;
    *f_ig4 = (njout << 20) | *f_ymin;

    return 0;
}


/*****************************************************************************
 *                              S L A B D S C                                *
 *Object :                                                                   *
 *  Description slab file.                                                   *
 *Arguments :                                                                *
 *     IN   f_hand   :   file handler of slab file                           *
 *     IN   f_snum   :   slab number                                         *
 *     IN   f_gxtyp  :   grid type for >> and ^^                             *
 *     IN   f_ixyg1  :   grid descriptor 1 for >> and ^^                     *
 *     IN   f_ixyg2  :   grid descriptor 2 for >> and ^^                     *
 *     IN   f_ixyg3  :   grid descriptor 3 for >> and ^^                     *
 *     IN   f_ixyg4  :   grid descriptor 4 for >> and ^^                     *
 *     IN   f_nio    :   dimension x of grid                                 *
 *     IN   f_njo    :   dimension y of grid                                 *
 *     IN   f_nxgrid :   dimension of f_xgrid                                *
 *     IN   f_nygrid :   dimension of f_ygrid                                *
 *     IN   f_xgrid  :   >> fields (dim : f_nxgrid)                          *
 *     IN   f_ygrid  :   ^^ fields (dim : f_nygrid)                          *
 *     IN   f_grtyp  :   grid type (A, B, G, L, N, S, X, Y, Z, E, H)                   *
 *     IN   f_ig1    :   grid descriptor 1                                   *
 *     IN   f_ig2    :   grid descriptor 2                                   *
 *     IN   f_ig3    :   grid descriptor 3 (except for '#' grids)            *
 *                       istart + nipoints<<20  (for '#' grids)              *
 *     IN   f_ig4    :   grid descriptor 4 (except for '#' grids)            *
 *                       jstart + njpoints<<20  (for '#' grids)              *
 *                                                                           *
 *     IN   f_mtout  :   dimension of ip(1..3), nomvar, datyp, nbits and first  *
 *                       dimension of f_xp                                   *
 *     IN   f_np     :   second dimension of f_xp                            *
 *     IN   f_typvar :   type field                                          *
 *     IN   f_nomvar :   variable name                                       *
 *     IN   f_ip1    :   level descriptor                                    *
 *     IN   f_ip2    :   time descriptor                                     *
 *     IN   f_ip3    :   descriptor no 3                                     *
 *     IN   f_datyp  :   data type                                           *
 *     IN   f_nbits  :   bits number for each data field                     *
 *     IN   f_iflt   :   number of filter passes for each data field         *
 *     IN   f_xp     :   optional variables (dim : mtout, np)                 *
 *                                                                           *
 *****************************************************************************/

int32_t f77name(slabdsc)(int32_t *f_hand, int32_t *f_snum, char *f_gxtyp,
           int32_t *f_ixyg1, int32_t *f_ixyg2,
                     int32_t *f_ixyg3, int32_t *f_ixyg4, int32_t *f_nio,
           int32_t *f_njo, int32_t *f_nxgrid,
                     int32_t *f_nygrid, float *f_xgrid, float *f_ygrid,
           char *f_grtyp, int32_t *f_ig1,
           int32_t *f_ig2, int32_t *f_ig3, int32_t *f_ig4,
           int32_t *f_mtout, int32_t *f_np,
                     char *f_typvar, char *f_nomvar, int32_t *f_ip1,
           int32_t *f_ip2, int32_t *f_ip3,
                     int32_t *f_datyp, int32_t *f_nbits, int32_t *f_iflt,
           float *f_xp, F2Cl l1, F2Cl l2, F2Cl l3, F2Cl l4)
{
 int nrows, nio, njo, nxtra, taille, MAX_GRTYP;
 int lng, ix, snum;
 char grtyp[MAX_LEN], grtyp_[MAX_LEN];
 char *p_nomvar, *p_typvar;
 int *typvars_0, *nomvars_0, *nomvars, *typvars;
 int32_t *p_ip1, *p_ip2, *p_ip3, *p_datyp, *p_nbits, *p_iflt;
 int fd;

/* for debug only
  int ip1[20], ip2[20], ip3[20], datyp[20], nbits[20], iflt[20];
  int32_t *p_ip1, *p_ip2, *p_ip3, *p_datyp, *p_nbits, *p_iflt;
*/

    MAX_GRTYP = strlen(GRTYPES);
    nio = (int ) *f_nio;
    njo = (int ) *f_njo;
    nrows = (int ) *f_mtout;
    nxtra = (int ) *f_np;

    if ( (ix = get_file_index( (int ) *f_hand)) < 0 ) return slab_exit(-3);
    fd = f_index[ix];

    intBuffer = (int *)file_table[ix].buffer;
    pos = file_table[ix].pos;

    snum = *f_snum;

    // Slab type number valid?
    if (snum < MAX_SLAB_TYPES && snum >= 0) {
        // Slab type already defined ?
        if (file_table[ix].nrows[snum] == 0) {
            file_table[ix].nrows[snum] = nrows;
            file_table[ix].count[snum] = nio*njo;
            file_table[ix].nio[snum] = nio;
            file_table[ix].ni[snum] = nio;
            file_table[ix].i1[snum] = 1;
            file_table[ix].njo[snum] = njo;
            file_table[ix].nj[snum] = njo;
            file_table[ix].j1[snum] = 1;
            if (*f_grtyp == '#') {
                file_table[ix].ni[snum] = (*f_ig3 >> 20) & 0xFFF;  /* number of points along X axis */
                file_table[ix].i1[snum] = *f_ig3 & 0xFFFFF;        /* X offset of first point */
                file_table[ix].nj[snum] = (*f_ig4 >> 20) & 0xFFF;  /* number of points along Y axis */
                file_table[ix].j1[snum] = *f_ig4 & 0xFFFFF;        /* Y offset of first point */
                file_table[ix].count[snum] = file_table[ix].ni[snum] * file_table[ix].nj[snum];
            }
        } else {
            fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d is already defined\n", file_table[ix].file_name, snum);
            fprintf(stderr, "   mtout=%d, nio*njo=%d\n", file_table[ix].nrows[snum], file_table[ix].count[snum]);
            fprintf(stderr, "   set to: mtout=%d, nio=%d njo=%d ??\n", nrows, nio, njo);
            return slab_exit(-2);
        }
    } else {
        fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d is out of range\n", file_table[ix].file_name, snum);
        fprintf(stderr, "  slabid MUST be from 0 to %d\n", MAX_SLAB_TYPES - 1);
        return slab_exit(-2);
    }
    if (nio < 1 || nio > 16777000) {
        fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: invalid NIO=%d\n", file_table[ix].file_name, snum, nio);
        return slab_exit(-2);
    }
    if (njo < 1 || njo > 16777000) {
        fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: invalid NJO=%d\n", file_table[ix].file_name, snum, njo);
        return slab_exit(-2);
    }
    if ((nomvars_0 = (int *)malloc(4 * nrows)) == NULL) {
        fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: Cannot allocate memory for nomvars\n", file_table[ix].file_name, snum);
        return slab_exit(-3);
    }

    if ((typvars_0 = (int *)malloc(4 * nrows)) == NULL) {
        fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: Can not allocate memory for typvars\n", file_table[ix].file_name, snum);
        return slab_exit(-3);
    }

    nomvars = nomvars_0;
    typvars = typvars_0;
    p_nomvar = f_nomvar;
    p_typvar = f_typvar;

    // copy nomvar into an integer using an endian proof method
    lng = (l4 < 4) ? l4 : 4;
    for (int n = 0; n < nrows; n++) {
        int temp = 0;
        for (int i = 0; i < lng; i++) {
            temp = (temp << 8) | (*p_nomvar & 0xFF);
            p_nomvar++;
        }
        for (int i = lng; i < 4; i++) {
            temp = (temp <<= 8) | ' ';
        }
        *nomvars = temp;
        nomvars++;
    }

    // copy typvar into an integer using an endian proof method
    lng = (l3 < 4) ? l3 : 4;
    for (int n = 0; n < nrows; n++) {
        int temp = 0;
        for (int i = 0; i < lng; i++) {
            temp = (temp << 8) | (*p_typvar & 0xFF);
            p_typvar++;
        }
        for (int i = lng; i < 4; i++) {
            temp = (temp <<= 8) | ' ';
        }
        *typvars = temp;
        typvars++;
    }

    // check ip1s
    p_ip1 = f_ip1;
    for (int n = 0; n < nrows; n++) {
        if ( (*p_ip1 & 0xF0000000) != 0) {
            fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: ip1[%d]=%d\n", file_table[ix].file_name, snum, n+1, (int) *p_ip1);
            return slab_exit(-2);
        }
        p_ip1++;
    }

    // check ip2s
    p_ip2 = f_ip2;
    for (int n = 0; n < nrows; n++) {
        if ( (*p_ip2 & 0xF0000000) != 0) {
            fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: ip2[%d]=%d\n", file_table[ix].file_name, snum, n+1, (int) *p_ip2);
            return slab_exit(-2);
        }
        p_ip2++;
    }

    // check ip3s
    p_ip3 = f_ip3;
    for (int n = 0; n < nrows; n++) {
        if ( (*p_ip3 & 0xF0000000) != 0) {
            fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: ip3[%d]=%d\n", file_table[ix].file_name, snum, n+1, (int) *p_ip3);
            return slab_exit(-2);
        }
        p_ip3++;
    }

    // check datyps
    p_datyp = f_datyp;
    for (int n = 0; n < nrows; n++) {
        if ( (int) *p_datyp < 0  || (int) *p_datyp > 5) {
            fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: datyp[%d]=%d\n", file_table[ix].file_name, snum, n+1, (int) *p_datyp);
            return slab_exit(-2);
        }
        p_datyp++;
    }

    // check nbits
    p_nbits = f_nbits;
    for (int n = 0; n < nrows; n++) {
        if ( (int) *p_nbits < 0 || (int) *p_nbits >  32767) {
            fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: nbits[%d]=%d\n", file_table[ix].file_name, snum, n+1, (int) *p_nbits);
            return slab_exit(-2);
        }
        p_nbits++;
    }

    // check iflts
    p_iflt = f_iflt;
    for (int n = 0; n < nrows; n++) {
        if ( (int) *p_iflt < 0 || (int) *p_iflt >  32767) {
            fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: iflt[%d]=%d\n", file_table[ix].file_name, snum, n+1, (int) *p_iflt);
            return slab_exit(-2);
        }
        p_iflt++;
    }

    /* for debug only
        printf("INSIDE SLABDSC ix=%d\n", ix);
        printf("l1=%d, l2=%d, l3=%d, l4=%d\n", l1, l2, l3, l4);
        printf("nio=%d, njo=%d, snum=%d, nrows=%d, nxtra=%d\n",
                            nio, njo, snum, nrows, nxtra);
        printf("ip1= ");
        p_ip1 = f_ip1;
        for (n=0; n< nrows; n++){
        ip1[n] = (int )*p_ip1;
        p_ip1++;
        printf("%d, ", ip1[n]);
        }
        printf("\n");
        printf("ip2= ");
        p_ip2 = f_ip2;
        for (n=0; n< nrows; n++){
        ip2[n] = (int )*p_ip2;
        p_ip2++;
        printf("%d, ", ip2[n]);
        }
        printf("\n");
        printf("ip3= ");
        p_ip3 = f_ip3;
        for (n=0; n< nrows; n++){
        ip3[n] = (int )*p_ip3;
        p_ip3++;
        printf("%d, ", ip3[n]);
        }
        printf("\n");
        printf("datyp= ");
        p_datyp = f_datyp;
        for (n=0; n< nrows; n++){
        datyp[n] = (int )*p_datyp;
        p_datyp++;
        printf("%d, ", datyp[n]);
        }
        printf("\n");
        printf("nbits= ");
        p_nbits = f_nbits;
        for (n=0; n< nrows; n++){
        nbits[n] = (int )*p_nbits;
        p_nbits++;
        printf("%d, ", nbits[n]);
        }
        printf("\n");
        printf("iflt= ");
        p_iflt = f_iflt;
        for (n=0; n< nrows; n++){
        iflt[n] = (int )*p_iflt;
        p_iflt++;
        printf("%d, ", iflt[n]);
        }
        printf("\n");
        printf("typvars=%s\n", typvars_0);
        printf("nomvars=%s\n", nomvars_0);
        printf("grtyp=%s, ", f_grtyp);
        printf("nxgrid=%d, nygrid=%d\n", (int )*f_nxgrid, (int )*f_nygrid);
        printf("gxtyp=%s\n", f_gxtyp);
        printf("ig1=%d, ig2=%d, ig3=%d, ig4=%d\n",
            (int) *f_ig1, (int) *f_ig2, (int) *f_ig3, (int) *f_ig4);
        printf("ixyg1=%d, ixyg2=%d, ixyg3=%d, ixyg4=%d\n",
            (int) *f_ixyg1, (int) *f_ixyg2, (int) *f_ixyg3, (int) *f_ixyg4);
        printf("\n");
    debug only*/

    slab_descrt.slb1 = 'SLB1';
    slab_descrt.nBytes = 4 * (15 + nrows * (8 + nxtra) + (int) *f_nxgrid + (int) *f_nygrid);
    slab_descrt.slab_id = (int ) *f_snum;

    int ll3 = (l3 < MAX_LEN) ? l3 : MAX_LEN-1;
    strncpy(grtyp, f_grtyp, ll3);
    grtyp[ll3] ='\0';

    // check gridtype requested
    if (ll3 == 0 || *grtyp==' ') {
        fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: invalid GRTYP='%s'\n", file_table[ix].file_name, snum, grtyp);
        return slab_exit(-2);
    }
    int n = 0;
    while (*grtyp != GRTYPES[n] && n < MAX_GRTYP) n++;
    if (*grtyp != GRTYPES[n]) {
        fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: invalid GRTYP='%s'\n", file_table[ix].file_name, snum, grtyp);
        return slab_exit(-2);
    }
    slab_descrt.Igrtyp = *grtyp; /* endian proof method */
    slab_descrt.Igrtyp <<= 24;

    slab_descrt.ig1    = (int ) *f_ig1;
    slab_descrt.ig2    = (int ) *f_ig2;
    slab_descrt.ig3    = (int ) *f_ig3;
    slab_descrt.ig4    = (int ) *f_ig4;
    slab_descrt.Nrows  = (int ) *f_mtout;
    slab_descrt.Niout  = (int ) *f_nio;
    slab_descrt.Njout  = (int ) *f_njo;
    slab_descrt.nxgrid = (int ) *f_nxgrid;
    slab_descrt.nygrid = (int ) *f_nygrid;
    slab_descrt.Nextra = (int ) *f_np;

    slab_descrt.Igrtyp_ = ' ' << 24;  /* endian proof method */
    slab_descrt.ig1_   = -2;
    slab_descrt.ig2_   = -2;
    slab_descrt.ig3_   = -2;
    slab_descrt.ig4_   = -2;

    // check descriptor dimensions vs grid dimensions for Z and # grids
    if (*f_grtyp == 'Z' || *f_grtyp == '#') {
        if (njo != *f_nygrid) {
            fprintf(stderr,
                "\n***ERROR in SLABDSC(%s)slabid %d: nygrid should be equal to njo for Z grid\n",
                file_table[ix].file_name, snum);
            fprintf(stderr, " nygrid = %d njo = %d\n", (int )*f_nygrid, njo);
            return slab_exit(-2);
        }
        if( *f_nxgrid != nio && *f_nxgrid != (nio + 1) ) {
            fprintf(stderr,
                 "\n***ERROR in SLABDSC(%s)slabid %d: nxgrid should be equal to nio or (nio+1) for Z grid\n",
                 file_table[ix].file_name, snum);
            fprintf(stderr, " nxgrid = %d nio = %d\n", (int )*f_nxgrid, nio);
            return slab_exit(-2);
        }
    }

    if (*f_grtyp == 'Y') {
        if ( (nio * njo) != *f_nxgrid) {
            fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: nxgrid should be equal to nio*njo for Y grid\n",
                         file_table[ix].file_name, snum);
            fprintf(stderr, " nxgrid = %d nio = %d njo = %d\n", (int )*f_nxgrid, nio, njo);
            return slab_exit(-2);
        }
        if ( (nio * njo) !=  *f_nygrid) {
            fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: nygrid should be equal to nio*njo for Y grid\n",
                         file_table[ix].file_name, snum);
            fprintf(stderr, " nygrid = %d nio = %d njo = %d\n", (int )*f_nygrid, nio, njo);
            return slab_exit(-2);
        }
    }
    // collect >> and ^^ projection descriptors
    if (*f_grtyp == 'Z' || *f_grtyp == 'Y' || *f_grtyp == '#') {
        int ll1 = (l1 < MAX_LEN) ? l1 : MAX_LEN-1;
        strncpy(grtyp_, f_gxtyp, ll1);
        grtyp_[ll1] = '\0';
        slab_descrt.Igrtyp_ = *grtyp_; /* endian proof method */
        slab_descrt.Igrtyp_ <<= 24;

        if (ll1 == 0 || *grtyp_==' '){
        fprintf(stderr, "\n***ERROR in SLABDSC(%s)slabid %d: invalid GXTYP='%s'\n", file_table[ix].file_name, snum, grtyp_);
        return slab_exit(-2);
        }

        slab_descrt.ig1_ = (int ) *f_ixyg1;
        slab_descrt.ig2_ = (int ) *f_ixyg2;
        slab_descrt.ig3_ = (int ) *f_ixyg3;
        slab_descrt.ig4_ = (int ) *f_ixyg4;
    }


    taille = (sizeof(slab_descrt) / sizeof(int));
    iVal = (int *) &slab_descrt;
    put_in_buffer(fd, iVal, intBuffer, pos, taille); /* SLB1 block header */
    pVal = f_ip1;
    put_in_buffer(fd, pVal, intBuffer, pos, nrows);
    pVal = f_ip2;
    put_in_buffer(fd, pVal, intBuffer, pos, nrows);
    pVal = f_ip3;
    put_in_buffer(fd, pVal, intBuffer, pos, nrows);
    pVal = f_nbits;
    put_in_buffer(fd, pVal, intBuffer, pos, nrows);
    pVal = f_datyp;
    put_in_buffer(fd, pVal, intBuffer, pos, nrows);
    iVal = nomvars_0;
    put_in_buffer(fd, iVal, intBuffer, pos, nrows);
    iVal = typvars_0;
    put_in_buffer(fd, iVal, intBuffer, pos, nrows);

    fBuffer = (float *) intBuffer;

    // write optional part for >> and ^^ descriptors
    if ( (*f_grtyp == 'Z')  || ( *f_grtyp == 'Y') || ( *f_grtyp == '#') ) {
#ifdef DEBUG
            printf("f_xgrid= ");
            {int npts=*f_nxgrid ; float *ffVal = (float *)f_xgrid; while(npts--){printf("  %f ", ffVal[npts]);}}
            printf("\n");
            printf("f_xgrid= ");
            {int npts=*f_nygrid ; float *ffVal = (float *)f_ygrid; while(npts--){printf("  %f ", ffVal[npts]);}}
            printf("\n");
#endif
        fVal = f_xgrid;
        put_in_buffer(fd, fVal, fBuffer, pos, *f_nxgrid);
        fVal = f_ygrid;
        put_in_buffer(fd, fVal, fBuffer, pos, *f_nygrid);
    }

    pVal = f_iflt;
    put_in_buffer(fd, pVal, intBuffer, pos, nrows);  /* filtering information (deprecated) */

    // write optional extras if supplied
    if (nxtra != 0) {
        taille = (int )(*f_mtout)  *  (int )(*f_np);
        fVal = f_xp;
        put_in_buffer(fd, fVal, fBuffer, pos, taille);
    }

    file_table[ix].pos = pos;
    free (nomvars_0);
    nomvars_0 = NULL;
    free (typvars_0);
    typvars_0 = NULL;
    return 0;
}


//! Extract valid data field into the slab file
//! Outputs into the file table buffer corresponding to *f_hand
int32_t f77name (slabxtr)(
    //! [in] File handle of slab file
    const int32_t * const f_hand,
    //! [in] Slab number
    const int32_t * const f_snum,
    //! [in] f_xnio dimension
    const int32_t * const f_nx,
    //! [in] Output indicator for grid position (dim : f_nx)
    const int32_t * const f_xnio,
    //! [in] mtas dimension
    const int32_t * const f_mt,
    //! [in] Output indicator for row in slab  (dim: f_mt)
    const int32_t * const f_mtas,
    //! [in] Values to add for each row in slab
    const float * const f_mtadd,
    //! [in] Values to multiply for each row in slab
    const float * const f_mtmult,
    //! [in] Raw slab; value of elements (dim : f_nx, f_mt)
    float *f_mtval
) {
    int Nrows = (int ) *f_mt;
    int Nx    = (int ) *f_nx;
    int snum  = (int ) *f_snum;

    int ix = 0;
    if ( (ix = get_file_index( (int ) *f_hand)) < 0 ) return slab_exit(-3);
    int fd = (int) *f_hand;

    intBuffer = (int *)file_table[ix].buffer;
    pos = file_table[ix].pos;
    fBuffer = (float *) intBuffer;


    // take all rows if no indicator (null pointer)
    int  nrows = 0;
    if (f_mtas) {
        for (int j = 0; j < Nrows; j++) {
            if (f_mtas[j] != 0) {
                nrows++;
            }
        }
    } else {
        nrows = Nrows;
    }

    if (nrows != file_table[ix].nrows[snum]) {
        fprintf(stderr, "***ERROR in SLABXTR(%s)slabid %d:\n",
                file_table[ix].file_name, snum);
        fprintf(stderr, "  nrows in mtas(=%d) must be equal to SLABDSC mtout(=%d)\n",
                nrows, file_table[ix].nrows[snum]);
        return slab_exit(-2);
    }

    int nX = 0;
    for (int i = 0; i < Nx; i++) {
        if (f_xnio[i] != 0) {
            nX++;
        }
    }

    if ((f_mtas == NULL) && (nX != Nx)) {
        fprintf(stderr, "***ERROR in SLABXTRF(%s)slabid %d:\n", file_table[ix].file_name, snum);
        fprintf(stderr, " HOLES NOT ALLOWED along X\n");
        return slab_exit(-2);
    }

    file_table[ix].count[snum] -= nX;

    data_block.slb2 = 'SLB2';
    data_block.nBytes = 4 * (3 + nX * (1 + nrows));
    data_block.slab_id = (int ) *f_snum;
    data_block.nX = nX;
    data_block.Nrows = nrows;

    iVal = (int *) &data_block;
    int taille = (sizeof(data_block) / sizeof(int));

#ifdef DEBUG
    fprintf(stderr, "SLB2 header, pos avant=%d\n", pos);
#endif
    put_in_buffer(fd, iVal, intBuffer, pos, taille);
#ifdef DEBUG
    fprintf(stderr, "SLB2 header, pos apres=%d\n", pos);
#endif

    // insert column position markers into buffer
    put_in_buffer(fd, f_xnio, intBuffer, pos, Nx);

    // bounced call from slabxtrf
    if (f_mtas == NULL && f_mtmult == NULL && f_mtadd == NULL) {
        put_in_buffer(fd, f_mtval, intBuffer, pos, Nx*Nrows);
        file_table[ix].pos = pos;
        return 0;
    }

    // insert data into buffer after scaling
    int ij = 0;
    for (int j = 0; j < Nrows; j++){
        if (f_mtas[j] != 0) {
            if ((pos + nX) <= BUFSIZE) {
                // Le nombre de colonnes a extraire est egal au nombre
                if (nX == Nx ) {
                    for (int i = 0; i < Nx; i++) {
                        //  de colonnes de chaque slab
                        fBuffer[pos] = (float)(f_mtval[ij] * f_mtmult[j] + f_mtadd[j]);
                        pos++;
                        ij++;
                    }
                } else {
                    // Le nombre de colonnes a extraire est different de celui de la slab
                    for (int i = 0; i < Nx; i++) {
                        if (f_xnio[i] != 0) {
                            // je cherche les valeurs valides une par une
                            fBuffer[pos] = (float)(f_mtval[ij] * f_mtmult[j] + f_mtadd[j]);
                            pos++;
                        }
                        ij++;
                    }
                }
            } else {
                // La taille qui reste dans le buffer est insuffisante pour le reste des donnees
                for (int i = 0; i < Nx; i++) {
                    if (pos >= BUFSIZE) {
                        taille = sizeof(float) * pos;  /* alors, on ecrit le buffer dans */
                        int nBytes =  write_buf(fd, (uint32_t *)fBuffer, pos);

                        if (nBytes != (sizeof(float) *  pos)) {
                            fprintf(stderr, "\n***ERROR in SLABXTR(%s)slabid %d: WRITE ERROR in slab file\n",
                                        file_table[ix].file_name, snum);
                            fprintf(stderr, "tried to write %d bytes, wrote %d, pos*sizeof(float)=%ld\n",
                                        taille, nBytes, BUFSIZE * sizeof(float));
                            fprintf(stderr, "Trying to write slab data\n");
                            return slab_exit(-2);
                        }
                        pos = 0;
                    }

                    if (f_xnio[i] != 0) {
                        // a extraire
                        fBuffer[pos] = (float)(f_mtval[ij] * f_mtmult[j] + f_mtadd[j]);
                        pos++;
                    }
                    ij++;
                }
            }
        } else {
            // f_mtas[j] == 0
            ij += Nx;
        }
    }

    file_table[ix].pos = pos;
    return 0;
 }


/****************************************************************************
 *                              S L A B X T R F                             *
 *                                                                          *
 *Object:                                                                   *
 *       Fast extraction of valid data field into the slab file             *
 *Arguments:                                                                *
 *     IN   f_hand   :    file handler of slab file                         *
 *     IN   f_snum   :    slab number                                       *
 *     IN   f_nx     :    f_xnio dimension                                  *
 *     IN   f_xnio   :    output indicator for grid position (dim : f_nx)   *
 *     IN   f_mt     :    mtas dimension                                    *
 *     IN   f_mtval  :    raw slab; value of elements (dim : f_nx, f_mt)    *
 *                                                                          *
 ****************************************************************************/
int32_t f77name(slabxtrf)(int32_t *f_hand, int32_t *f_snum, int32_t *f_nx,
            int32_t *f_xnio, int32_t *f_mt, float *f_mtval)
{
    return f77name(slabxtr)(f_hand, f_snum, f_nx, f_xnio, f_mt, NULL, NULL, NULL, f_mtval);
}


//! Put an end slab indicator into a file
int32_t f77name(slabend)(
    //! [in] Slab file handle
    const int32_t * const f_hand,
    //! [in] Section or file end indicator
    const char * const f_sf_hand,
    F2Cl l1
) {
    int end, taille, fd;
    int nBytes;
    int n;

    int ix;
    if ((ix = get_file_index( (int ) *f_hand)) < 0 ) return slab_exit(-3);
    fd = (int) *f_hand;

    intBuffer = (int *)file_table[ix].buffer;
    pos = file_table[ix].pos;
    fBuffer = (float *) intBuffer;

    /* end marker (SLB9) in endian proof way */
    end = (f_sf_hand[0] << 24) | (f_sf_hand[1]  << 16) |
            (f_sf_hand[2] << 8) | (f_sf_hand[3]);

    /* check to see if number of values written is equal to what was requested */
    /* the count field originally contains the number of columns to write and  */
    /* is decremented by slabxtr for each column written. must be zero now     */
    for (int i = 0; i < MAX_SLAB_TYPES; i++) {
        if (0 != file_table[ix].count[i]) {
            fprintf(stderr, "\n***ERROR in SLABEND(%s)slabid %d\n", file_table[ix].file_name, i);
            fprintf(stderr, "   Value of nio*njo must be equal to number of valid values in xnio\n");
            fprintf(stderr, "   No. of selected elements in xnio = %d, nio*njo=%d\n",
                            file_table[ix].count[i], file_table[ix].count[i]);
            return slab_exit(-2);
        }
    }

    slab_end.id_end = end;
    slab_end.nBytes = 0;

    if (slab_end.id_end != 'SLB9') {
        fprintf(stderr, "\n***ERROR in SLABEND(%s):end indicator of slabend must be -> SLB9\n\n", file_table[ix].file_name);
        return slab_exit(-2);
    }

    iVal = (int *) &slab_end;

    taille = sizeof(slab_end) / sizeof(int32_t);
    /* add SLB9 block to buffer */
    put_in_buffer(fd, iVal, intBuffer, pos, taille);

    if (pos == 0) {
        taille=sizeof(slab_end);
        nBytes = write_buf(fd, (uint32_t *)intBuffer, taille/sizeof(int32_t));
        if (nBytes != taille) {
            fprintf(stderr, "\n***ERROR in SLABEND(%s): WRITE ERROR in slab file, errno=%d\n", file_table[ix].file_name, errno);
            return slab_exit(-2);
        }
    } else {
        taille = sizeof(int) * pos;
        nBytes = write_buf(fd, (uint32_t *)intBuffer, pos);
        if (nBytes != taille) {
            fprintf(stderr, "\n***ERROR in SLABEND(%s): WRITE ERROR in slab file, errno=%d\n", file_table[ix].file_name, errno);
            return slab_exit(-2);
        }
    }

    free(intBuffer);
    file_table[ix].buffer = NULL;
    file_table[ix].pos = 0;
    for (int i = 0; i < MAX_SLAB_TYPES; i++){
        file_table[ix].count[i] = 0;
        file_table[ix].nrows[i] = 0;
        file_table[ix].nio[i] = 0;
        file_table[ix].i1[i] = 0;
        file_table[ix].ni[i] = 0;
        file_table[ix].njo[i] = 0;
        file_table[ix].j1[i] = 0;
        file_table[ix].nj[i] = 0;
        }
    close (fd) ;
    f_index[ix] = -1; /* reset to -1 */
    return 0; /* indicates that SLB9 was added */
}

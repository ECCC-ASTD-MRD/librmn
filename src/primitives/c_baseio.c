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

#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#include <sys/file.h>
#include <sys/signal.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef __linux__
# include <linux/limits.h>
#endif


#ifdef __APPLE__ && __MACH__
#   include <sys/limits.h>
#endif

#include <fcntl.h>
#include <errno.h>

#include <App.h>
#include <rmn/rpnmacros.h>
#include <rmn/gossip.h>
#include <rmn/swap_buffer.h>

#include <ftn_c_protos.h>

#define FNOM_OWNER
#include <rmn/fnom.h>
#include "wafile.h"

//! Maximum length of the type string for fnom
#define FNOM_TYPE_MAX 256

// _SC_HOST_NAME_MAX is defined by POSIX in unistd.h
#ifdef _SC_HOST_NAME_MAX
#   define HOST_NAME_MAX _SC_HOST_NAME_MAX
#else
    // Provide a fallback for non POSIX compliant platforms
#   define HOST_NAME_MAX 256
#endif

static int c_qqqfscr(const char * const type);
static int fnom_rem_connect( const int ind, const char * const remote_host);
static void wa_pages_flush(const int fileIdx);
static int qqcopen(const int indf);
static void qqcwawr(const uint32_t * const buf, const unsigned int wadr, const int nwords, const int indf);
static void qqcward(uint32_t * const buf, const unsigned int woffset, const int lnmots, const int indf);
static void arrayZero(uint32_t * const dest, const int nwords);
static void arrayCopy(const uint32_t * const src, uint32_t * const dest, const int nwords);

// Defined inf f_baseio.F90 but only used here
int32_t f77name(qqqf7op)(
    const int32_t * const iun,
    const char * const name,
    const int32_t * const lrec,
    const int32_t * const rndflag,
    const int32_t * const unfflag,
    const int32_t * const lmult,
    F2Cl name_len
);
int32_t f77name(ftnclos)(const int32_t * const iun);

static ENTETE_CMCARC cmcarc;
static ENTETE_CMCARC_V5 cmcarc64;

static FILEINFO wafile[MAXWAFILES];
static uint32_t *free_list[MAXWAFILES * MAXPAGES];
static int dastat[MAXWAFILES] = {MAXWAFILES * 0};

static int blkSize = 512;

static int WA_PAGE_SIZE = 0;
static int WA_PAGE_NB   = 1;
static int WA_PAGE_LIMIT = 0;

static int global_count = 0;
static int nfree = -1;
static int init = 0;
static int subfile_length = 0;
static int fnom_initialized = 0;
static int stdoutflag = 0;
static int stdinflag = 0;

static int endian_int = 1;
static char *little_endian = (char *)&endian_int;

static char *CMCCONST = NULL;
static char *ARMNLIB = NULL;
static char *LOCALDIR = "./";


//! Seek in a file at a given word
static off64_t wseek(int fdesc,off64_t offst, int posi) {
    return lseek64(fdesc, offst * sizeof(uint32_t), posi);
}


//! Kept only for backward compatibility; only returns 0
//! \return Always zero
//! \deprecated
int c_fretour(
    //! [in] Unit number, ignored
    const int iun
) {
    return 0;
}
//! \copydoc c_fretour
int32_t f77name(fretour)(
    //! [in] Unit number, ignored
    const int32_t * const fiun
){
    return 0;
}


//! Print the characteristics and attributes of one file of the master file table(for debugging use)
static void dump_file_entry(
    //! [in] Index of the entry to dump
    const int idx
) {
      Lib_Log(APP_LIBRMN,APP_VERBATIM,"FGFDT[%d] file_name=%s subname=%s file_type=%s",idx,FGFDT[idx].file_name,FGFDT[idx].subname,FGFDT[idx].file_type);
      Lib_Log(APP_LIBRMN,APP_VERBATIM,"iun=%d,fd=%d,size=%d,esize=%d,lrec=%d,flags=%s%s%s%s%s%s%s%s%s%s%s%s\n",
              FGFDT[idx].iun,
              FGFDT[idx].fd,
              FGFDT[idx].file_size,
              FGFDT[idx].eff_file_size,
              FGFDT[idx].lrec,
              FGFDT[idx].open_flag?"OPEN":"",
              FGFDT[idx].attr.stream?"+STREAM":"",
              FGFDT[idx].attr.std?"+STD":"",
              FGFDT[idx].attr.burp?"+BURP":"",
              FGFDT[idx].attr.rnd?"+RND":"+SEQ",
              FGFDT[idx].attr.wa?"+WA":"",
              FGFDT[idx].attr.ftn?"+FTN":"",
              FGFDT[idx].attr.unf?"+UNF":"+FMT",
              FGFDT[idx].attr.read_only?"+R/O":"+R/W",
              FGFDT[idx].attr.old?"+OLD":"",
              FGFDT[idx].attr.notpaged?"+NOT PAGED":"",
              FGFDT[idx].attr.scratch?"+SCRATCH":"");
}

//! Print file characteristics and attributes of in use files in the master file table(for debugging use)
void f77name(d_fgfdt)()
{
    Lib_Log(APP_LIBRMN,APP_ALWAYS,"%s: DUMP of MASTER FILE TABLE\n",__func__);
    for (int i = 0 ; i < MAXFILES ; i++) {
        if(FGFDT[i].iun != 0) {
            dump_file_entry(i);
        }
    }
}

//! Resets a file entry in the master file table to "not in use" values
static void reset_file_entry(
    //! [in] Index of the file to reset
    int idx
) {
    if (FGFDT[idx].file_name) free(FGFDT[idx].file_name);
    if (FGFDT[idx].subname)   free(FGFDT[idx].subname);
    if (FGFDT[idx].file_type) free(FGFDT[idx].file_type);

    FGFDT[idx].file_name      = (char *) NULL;
    FGFDT[idx].subname        = (char *) NULL;
    FGFDT[idx].file_type      = (char *) NULL;
    FGFDT[idx].iun            = 0;
    FGFDT[idx].fd             = -1;
    FGFDT[idx].file_size      = 0;
    FGFDT[idx].eff_file_size  = 0;
    FGFDT[idx].lrec           = 0;
    FGFDT[idx].open_flag      = 0;
    FGFDT[idx].attr.stream    = 0;
    FGFDT[idx].attr.std       = 0;
    FGFDT[idx].attr.burp      = 0;
    FGFDT[idx].attr.rnd       = 0;
    FGFDT[idx].attr.wa        = 0;
    FGFDT[idx].attr.ftn       = 0;
    FGFDT[idx].attr.unf       = 0;
    FGFDT[idx].attr.read_only = 0;
    FGFDT[idx].attr.old       = 0;
    FGFDT[idx].attr.scratch   = 0;
    FGFDT[idx].attr.notpaged  = 0;
    FGFDT[idx].attr.write_mode= 0;
    // Remote file, socket wa file
    FGFDT[idx].attr.remote    = 0;
}


//! Find the index of a file in the master file table
//! \return Index of the file in the master file table
static int find_file_entry(
    //! [in] Name of the function calling find_file_entry
    const char * const caller,
    //! [in] Unit number to search
    const int iun
) {
    for (int i = 0; i < MAXFILES; i++) {
        if (FGFDT[i].iun == iun) {
            return i;
        }
    }

    Lib_Log(APP_LIBRMN,APP_ERROR,"%s: unit %d is not associated with any file\n",caller,iun);
    return -1;
}


//! Open a file and make the connection with a unit number and process record file attributes
//! \return 0 if connection is successful, non-zero otherwise.
//
//! If name is all in upper case it will be converted to lower case.
//! c_fnom is intended to be called from C. fnom is intended to be called from Fortran.
int c_fnom(
    //! [in,out] Unit number
    int * const iun,
    //! [in] File path
    //! - If the file name contains the special character @ then it refers to a CMCARC file.
    //!   The first part of the name (up to the @ character) is then recognized to be the actual
    //!   file name and the second part of the name (after the @ character) refers to the sub-file
    //!   name within the CMCARC file.
    //! - If the file name ends with ":" (column) and the type argument contains the attribute
    //!   "remote" then the file is perceived to be remote and to reside on another computer.
    //!   The remote attribute works only with "word adressable" (waread, wawrit) type of file as
    //!   the RPN standard file and BURP files are. It then instructs the package to perform I/O
    //!   with the use of UNIX sockets instead of conventional NFS. This feature could be useful
    //!   for files that are not accessible via NFS.
    const char * const nom,
    //! [in] String that contains the desired file attributes
    //! - 'STREAM' : BYTE STREAM file type (a la C)
    //! - 'RND' : random type (word addressable, XDF, BURP, RPN standard file ...)
    //! - 'FTN' : FORTRAN type ('SEQ' is implicit)
    //! - 'FMT' : file to be read with FORMAT (FTN or D77 only)
    //! - 'D77' : direct access file (in FORTRAN ANSI 77 sense). READ/WRITE(...,REC= ,...)
    //! - 'STD' : RPN standard file
    //! - 'SEQ' : sequential file (not necessarily FTN)
    //! - 'OLD' : file must already exist
    //! - 'SCRATCH' : temporarily file. The name will be generated by fnom based on fname
    //! - ' APPEND' : file to be opened in append mode. Doesn't work or not implemented ?
    //! - 'R/W' : file to be opened in read/write mode
    //! - 'R/O' : file to be opened in read only mode
    //! - 'REMOTE' : remote file (file name must end with ':', all I/O will be performed using UNIX sockets) 
    //!
    //! Attribute can be combined as shown blow:
    //!       +--------STREAM-------+
    //!       |                     |
    //!       +---------RND---------+
    //!       |                     |
    //!       +--FTN--+  +--FMT--+  |
    //!       |       |  |       |  |
    //!       +--D77--+--+-------+--+                 +--APPEND--+
    //!       |                     |                 |          |
    //!       |          +--RND--+  |  +----OLD----+  +---R/W----+
    //!       |          |       |  |  |           |  |          |
    //!    >--+---STD----+--SEQ--+--+--+-----------+--+---R/O----+--+----->
    //!                  |       |     |                            |
    //!                  +--FTN--+     +--SCRATCH-------------------+
    const char * const type,
    //! [in] Length of record (must be 0 except if type contains D77)
    const int lrec
) {
    if (fnom_initialized == 0) {
        //  Make sure that file descriptor 0 (stdin) will not be returned by open for use with a regular file
        // This is a workaround for a particular case on Linux in batch mode with PBS
        ARMNLIB = getenv("ARMNLIB");
        if( ARMNLIB == NULL ) ARMNLIB = LOCALDIR;
        CMCCONST = getenv("CMCCONST");
        if( CMCCONST == NULL ) CMCCONST = LOCALDIR;
        for (int i = 0; i < MAXFILES; i++) {
            reset_file_entry(i);
        }
        fnom_initialized = 1;
    }

    int liun;
    if (((intptr_t)iun > 0) && ((intptr_t)iun < 1000)) {
        // An integer value has been passed to c_fnom as iun
        intptr_t ptr_as_int = (intptr_t)iun ;
        liun = ptr_as_int;
    } else {
        // a pointer has been passed to c_fnom as iun
        if (*iun == 0) {
            *iun = c_qqqfscr(type);
        }
        if (*iun == -1) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: no more units available\n",__func__);
            return -1;
        }
        liun = *iun;
    }

    if ((liun == 6) && ((strcmp(nom, "$OUT") == 0) || (strcmp(nom, "$OUTPUT") == 0) ||
                       (strcmp(nom, "OUTPUT") == 0) || (strcmp(nom, "output") == 0))) {
        stdoutflag = 1;
        return 0;
    }
    if ((liun == 5) && ((strcmp(nom, "$IN") == 0) || (strcmp(nom, "$INPUT") == 0) ||
                       (strcmp(nom, "INPUT") == 0) || (strcmp(nom, "input") == 0))) {
        stdinflag = 1;
        return 0;
    }

    if (liun == 6) {
        fclose(stdout);
        freopen(nom, "a" , stdout);
        stdoutflag = 1;
        return 0;
    } else if (liun == -2) {
        fclose(stderr);
        freopen(nom, "a", stderr);
        return 0;
    }
    for (int i = 0; i < MAXFILES; i++) {
        if (FGFDT[i].iun == liun) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: unit %d is already in use\n",__func__,liun);
            return -1;
        }
    }
    int entry = 0;
    while (entry < MAXFILES && FGFDT[entry].iun != 0) {
        entry++;
    }
    if (FGFDT[entry].iun == 0) {
        FGFDT[entry].iun = liun;
    } else {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: too many files, file table is full\n",__func__);
        return -1;
    }

    // Record file attributes
    int lngt = strlen(type) + 1;
    FGFDT[entry].file_type = malloc(lngt + 1);
    strncpy(FGFDT[entry].file_type, type, lngt);
    FGFDT[entry].attr.stream = 0;
    FGFDT[entry].attr.std = 0;
    FGFDT[entry].attr.burp = 0;
    FGFDT[entry].attr.rnd = 0;
    FGFDT[entry].attr.wa = 0;
    FGFDT[entry].attr.ftn = 0;
    FGFDT[entry].attr.unf = 0;
    FGFDT[entry].attr.read_only = 0;
    FGFDT[entry].attr.old = 0;
    FGFDT[entry].attr.notpaged = 0;
    FGFDT[entry].attr.scratch = 0;
    FGFDT[entry].attr.pipe = 0;
    FGFDT[entry].attr.remote = 0;

    if (strstr(type, "STREAM") || strstr(type, "stream")) {
        FGFDT[entry].attr.stream = 1;
        FGFDT[entry].attr.rnd = 1;
    }
    if (strstr(type, "STD") || strstr(type, "std")) {
        FGFDT[entry].attr.std = 1;
        FGFDT[entry].attr.rnd = 1;
    }
    if (strstr(type, "BURP") || strstr(type, "burp")) {
        FGFDT[entry].attr.burp = 1;
        FGFDT[entry].attr.rnd = 1;
    }
    if (strstr(type, "RND") || strstr(type, "rnd")) {
        FGFDT[entry].attr.rnd = 1;
    }
    if (strstr(type, "WA") || strstr(type, "wa")) {
        // wa attribute will be set by waopen
        FGFDT[entry].attr.rnd = 1;
    }
    if (strstr(type, "FTN") || strstr(type, "ftn")) {
        FGFDT[entry].attr.ftn = 1;
        FGFDT[entry].attr.rnd = 0;
    }
    if (strstr(type, "UNF") || strstr(type, "unf")) {
        FGFDT[entry].attr.unf = 1;
        FGFDT[entry].attr.ftn = 1;
        FGFDT[entry].attr.rnd = 0;
    }
    if (strstr(type, "OLD") || strstr(type, "old")) {
        FGFDT[entry].attr.old = 1;
    }
    if (strstr(type, "R/O") || strstr(type, "r/o")) {
        FGFDT[entry].attr.read_only = 1;
        FGFDT[entry].attr.old = 1;
    }
    if (strstr(type, "R/W") || strstr(type, "r/w")) {
        FGFDT[entry].attr.read_only = 0;
        FGFDT[entry].attr.write_mode = 1;
    }
    if (strstr(type, "D77") || strstr(type, "d77")) {
        FGFDT[entry].attr.ftn = 1;
        FGFDT[entry].attr.rnd = 1;
    }
    if (strstr(type, "SCRATCH") || strstr(type, "scratch")) {
        FGFDT[entry].attr.scratch = 1;
    }
    if (strstr(type, "REMOTE") || strstr(type, "remote")) {
        FGFDT[entry].attr.remote = 1;
    }

    if (!FGFDT[entry].attr.std && !FGFDT[entry].attr.burp && !FGFDT[entry].attr.wa && !FGFDT[entry].attr.rnd  && !FGFDT[entry].attr.stream) {
        FGFDT[entry].attr.ftn = 1;
    }
    FGFDT[entry].lrec = lrec;
    FGFDT[entry].open_flag = 0;

    // If scratch file, add tmpdir directory and pid to the file name
    int lng;
    const char * lnom = nom;
    char remote_mach[HOST_NAME_MAX];
    if (FGFDT[entry].attr.scratch) {
        if (strstr(lnom, "/")) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: / is illegal in scratch file name, specified name was %s\n",__func__,lnom);
            return -1;
        }
        int pid = getpid();
        char *tmpdir = getenv("TMPDIR");
        if (tmpdir == NULL) {
            Lib_Log(APP_LIBRMN,APP_WARNING,"%s: TMPDIR environment variable is not defined, /tmp is used\n",__func__);
            tmpdir = "/tmp";
        }
        // Espace tampon supplementaire
        lng = strlen(lnom) + strlen(tmpdir) + 10 + 3 + 128;
        if ((FGFDT[entry].file_name = malloc(lng)) == NULL) {
            Lib_Log(APP_LIBRMN,APP_FATAL,"%s: can't allocate memory for file name\n",__func__);
            perror("c_fnom");
            exit(1);
        }
        sprintf(FGFDT[entry].file_name, "%s/%d_%s", tmpdir, pid, lnom);
    } else {
        // Convert file name to lower case unless it contains a mix of upper case / lower case
        lng = strlen(lnom);
        FGFDT[entry].file_name = malloc(lng + 1);
        if ((FGFDT[entry].attr.remote) && (strchr(lnom, ':'))) {
            if (FGFDT[entry].attr.rnd) {
                char *pos2p = strchr(lnom, ':');
                if (pos2p != NULL) {
                    strncpy(remote_mach, lnom, pos2p - lnom);
                    remote_mach[pos2p - lnom] = '\0';
                    lnom = ++pos2p;
                    Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: remote_mach=%s file name=%s\n",__func__,remote_mach,lnom);
                    lng = strlen(lnom);
                }
            } else {
                // Code to remote copy the file on local machine and change file name
                FGFDT[entry].attr.remote = 0;
                // File becomes read only
                FGFDT[entry].attr.read_only = 0;
            }
        } else {
            FGFDT[entry].attr.remote = 0;
        }
        const char *cptr1 = lnom;
        if (lnom[0] == '@') {
            // Name is of the form @some_file_name
            // Skip the @, scan later under
            cptr1++;
            // CMCCONST & ARMNLIB if not local file
            lng--;
        }
        if (lnom[0] == '%') {
            // Name is of the form %[%@]some_pipe_ file
            // Skip the %
            cptr1++;
            lng--;
            FGFDT[entry].attr.pipe = 1;
        }
        if (lnom[0] == '+') {
            // Name is of the form +some_file_name
            // skip the +, do not convert to lowercase
            cptr1++;
            lng--;
        } 
        strncpy(FGFDT[entry].file_name, cptr1, lng);
        FGFDT[entry].file_name[lng] = '\0';
    }


    // Verify for a cmcarc type of file (filename@subfilename)
    char *cmcarc = strchr(FGFDT[entry].file_name, '@');
    if (cmcarc && !(FGFDT[entry].attr.remote)) {
        FGFDT[entry].subname = malloc(lng + 1);
        strcpy(FGFDT[entry].subname, cmcarc + 1);
        *cmcarc = '\0';
        /* file must exist and be read/only */
        FGFDT[entry].attr.old = 1;
        FGFDT[entry].attr.read_only = 1;
    } else {
        FGFDT[entry].subname = NULL;
    }

    // Check for @file (@filename)
    if (lnom[0] == '@') {
        char filename[PATH_MAX];
        strcpy(filename, FGFDT[entry].file_name);

        if (access(filename, F_OK) == -1) {
            /* no local file */
            sprintf(filename, "%s/%s", CMCCONST, FGFDT[entry].file_name);
            if (access(filename, F_OK)  == -1) {
                /* not under CMCCONST */
                sprintf(filename, "%s/data/%s", ARMNLIB, FGFDT[entry].file_name);

                if (access(filename, F_OK)  == -1) {
                    /* not under ARMNLIB either */
                    return -1;
                }
            }
        }
        free(FGFDT[entry].file_name);
        FGFDT[entry].file_name = malloc(strlen(filename) + 10);
        strcpy(FGFDT[entry].file_name, filename);
        lng = strlen(filename);
     }

    if ((FGFDT[entry].attr.old || FGFDT[entry].attr.read_only) && ! FGFDT[entry].attr.remote) {
        if (!f77name(existe)(FGFDT[entry].file_name, (F2Cl) strlen(FGFDT[entry].file_name))) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: file %s should exist and does not\n",__func__,FGFDT[entry].file_name);
            c_fclos(liun);
            return -1;
        }
    }

    // FORTRAN files must be opened by a FORTRAN module
    int ier = 0;
    if (FGFDT[entry].attr.ftn) {
        int32_t iun77 = liun;
        int32_t lrec77 = lrec;
        int32_t rndflag77 = FGFDT[entry].attr.rnd;
        int32_t unfflag77 = FGFDT[entry].attr.unf;
        // lmult is no longer used by qqqf7op, but the argument was kept for backward compatibility
        int32_t lmult = 42;
        ier = open64(FGFDT[entry].file_name, O_RDONLY);
        if (ier <= 0) {
            FGFDT[entry].file_size = -1;
            FGFDT[entry].eff_file_size = -1;
        } else {
            off64_t dimm = lseek64(ier, 0, SEEK_END);
            FGFDT[entry].file_size = dimm / sizeof(uint32_t);
            FGFDT[entry].eff_file_size = dimm / sizeof(uint32_t);
            close(ier);
        }
        ier = f77name(qqqf7op)(&iun77, FGFDT[entry].file_name, &lrec77, &rndflag77, &unfflag77, &lmult, (F2Cl) lng);
    } else if (FGFDT[entry].attr.stream || FGFDT[entry].attr.std || FGFDT[entry].attr.burp || FGFDT[entry].attr.wa ||
               (FGFDT[entry].attr.rnd && !FGFDT[entry].attr.ftn) ) {
        ier = c_waopen2(liun);
        /* will be set by waopen */
        FGFDT[entry].attr.wa = 0;
    }

    if (FGFDT[entry].attr.remote) ier = fnom_rem_connect(entry, remote_mach);

    if (ier == 0) FGFDT[entry].open_flag = 1;
    if (ier < 0) c_fclos(liun);
    return ier < 0 ? -1 : 0;
}


int32_t f77name(fnom)(
    int32_t * const iun,
    const char * const nom,
    const char * const ftype,
    const int32_t * const flrec,
    F2Cl l1,
    F2Cl l2
) {
    int liun = *iun;
    char filename[PATH_MAX + 1];
    char filetype[FNOM_TYPE_MAX + 1];

    int lng = (l1 <= PATH_MAX) ? l1 : PATH_MAX;

    // copy filename into a C string
    strncpy(filename, nom, lng);
    filename[lng] = '\0';

    while ((filename[lng-1] == ' ') && (lng > 1)) {
        // strip trailing blanks
        lng--;
        filename[lng] = '\0';
    }

    lng = (l2 <= FNOM_TYPE_MAX) ? l2 : FNOM_TYPE_MAX;
    //  copy file type into a C string
    strncpy(filetype, ftype, lng);
    filetype[lng] = '\0';

    while ((filetype[lng-1] == ' ') && (lng > 1)) {
        // strip trailing blanks
        lng--;
        filetype[lng] = '\0';
    }

    int tmp = c_fnom(&liun, filename, filetype, *flrec);
    if (*iun == 0) *iun = liun;
    return tmp;
}



//! \brief Close a file
//! \return 0 on success, non-zero otherwise
int c_fclos(
    //! Unit number of the file to close
    const int iun
) {
    if ((iun == 6) && (stdoutflag)) return 0;
    if ((iun == 5) && (stdinflag)) return 0;

    int entry = find_file_entry("c_fclos", iun);
    if (entry < 0) return entry;

    int ier = 0;
    if (FGFDT[entry].open_flag) {
        if (FGFDT[entry].attr.ftn) {
            int32_t iun77 = iun;
            ier = f77name(ftnclos)(&iun77);
        } else {
            ier = close(FGFDT[entry].fd);
        }
    }

    reset_file_entry(entry);
    return ier;
}

//! \copydoc c_fclos
int32_t f77name(fclos)(
    const int32_t * const fiun
) {
    return c_fclos(*fiun);
}


//! Generate unit number
//! \return Valid unit number
static int c_qqqfscr(
    //! File attributes (see FNOM)
    const char * const type
) {
    int start;
    int iun = -1;
    if (strstr(type, "FTN") || strstr(type, "ftn") || strstr(type, "D77") || strstr(type, "d77")) {
        start = 99;
    } else {
        start = 999;
    }
    for (int j = start; j > 10; j--) {
        int inused = 0;
        for (int i = 0; i < MAXFILES; i++) {
            if (FGFDT[i].iun == j) {
                inused = 1;
                break;
            }
        }
        if (! inused) {
            iun = j;
            break;
        }
    }
    return iun;
}


//! Retreive file name, type and size
//! \return 0 on success, negative number otherwise
int32_t f77name(qqqfnom)(
    //! [in] Unit number
    const int32_t * const iun,
    //! [out] File name(blank padded to have a total length of l1)
    char * const nom,
    //! [out] File type (blank padded to have a total length of l2)
    char * const type,
    //! [out] File size
    int32_t * const flrec,
    //! [in] File name length
    F2Cl l1,
    //! [in] File type length
    F2Cl l2
) {
    int entry = find_file_entry("qqqfnom", *iun);
    if (entry < 0) return entry;

    strncpy(nom, FGFDT[entry].file_name, l1);
    for (int j = strlen(FGFDT[entry].file_name); j < l1; j++) {
       nom[j] = ' ';
    }
    strncpy(type, FGFDT[entry].file_type, l2);
    for (int j = strlen(FGFDT[entry].file_type); j < l2; j++) {
        type[j] = ' ';
    }
    *flrec = FGFDT[entry].lrec;
    return 0;
}


//! Close file
//! \return 0 on success, non-zero on otherwise
static int qqcclos(
    //! [in] Index of the file in the master file table
    const int indf
) {
    int lfd = FGFDT[indf].fd;

    int ind = 0;
    while ((wafile[ind].file_desc != lfd) && (ind < MAXWAFILES)) {
        ind++;
    }
    if (ind == MAXWAFILES) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: file is not open, fd=%d, name=%s\n",__func__,lfd,FGFDT[indf].file_name);
        return 1;
    }

    if (FGFDT[indf].attr.remote) {
        int demande[5];

        int *s_ID = &(demande[0]);
        int *addr = &(demande[1]);
        int *nw = &(demande[2]);
        int *RW_mode = &(demande[3]);
        int *checksum = &(demande[4]);
        *s_ID = 0xBABE;
        *addr = 0;
        *nw = 0;
        *RW_mode = 3;  /* close request */
        *checksum = *s_ID ^ *addr ^ *nw ^ *RW_mode;
        check_swap_records(demande, 5, sizeof(int));
        int nc = write_stream(FGFDT[indf].fd, (char *)demande, 5 * sizeof(int));
        if (nc == 0) {
            Lib_Log(APP_LIBRMN,APP_INFO,"%s: socket wrote to server OK\n",__func__);
            fflush(stdout);
        } else {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: socket wrote only %d bytes to server\n",__func__,nc);
        }
    } else {
        if (WA_PAGE_SIZE != 0) {
            wa_pages_flush(ind);
            if (wafile[ind].nb_page_in_use != 0) {
               Lib_Log(APP_LIBRMN,APP_ERROR,"%s: nb_page_in_use = %d\n",__func__,wafile[ind].nb_page_in_use);
            }
            FGFDT[indf].file_size = 0;
            FGFDT[indf].eff_file_size = 0;
            wafile[ind].nb_page_in_use = 0;
            Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: fermeture du fichier ind=%d, fd=%d\n",__func__,ind,lfd);
        }
    }
    wafile[ind].file_desc = -1;
    FGFDT[indf].fd = -1;
    FGFDT[indf].open_flag = 0;
    close(lfd);
    return 0;
}


//! Open a word addressable file
//! \return 0 on success, non-zero otherwise
int c_waopen2(
    //! [in] Unit number
    const int iun
) {
    int entry = 0;
    while (entry < MAXFILES && FGFDT[entry].iun != iun) {
        entry++;
    }
    if (entry == MAXFILES) {
        // iun not found; search again for an available entry
        entry=0;
        while (entry < MAXFILES && FGFDT[entry].iun != 0) {
            entry++;
        }
        if (entry == MAXFILES) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: file table is full\n",__func__);
            return -1;
        } else {
            FGFDT[entry].iun = iun;
        }

        // file is not associated with fnom, file name is set to Wafileiun
        FGFDT[entry].file_name = malloc(10);
        sprintf(FGFDT[entry].file_name, "%s%d", "Wafile", iun);
        FGFDT[entry].attr.wa = 1;
        FGFDT[entry].attr.rnd = 1;
    } else {
        if (FGFDT[entry].attr.rnd == 0) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: waopen needs a file with the RND or WA type\n",__func__);
            return -1;
        }
        if (FGFDT[entry].open_flag) {
            if (FGFDT[entry].attr.wa == 1){
                // fnom opened the file but does not set wa flag
                Lib_Log(APP_LIBRMN,APP_ERROR,"%s: unit %d already open as %\n",__func__,iun,FGFDT[entry].file_name);
            }
            FGFDT[entry].attr.wa = 1;
            return FGFDT[entry].fd;
        }
    }

    int ier = qqcopen(entry);
    if (ier >= 0) {
        FGFDT[entry].open_flag = 1;
        FGFDT[entry].attr.wa = 1;
        FGFDT[entry].attr.rnd = 1;
    }
    return ier;
}


//! \copydoc c_waopen2
int32_t f77name(waopen2)(
    const int32_t * const iun
) {
    return c_waopen2(*iun);
}


//! Open a word addressable file
//! Quit program on error
void c_waopen(
    //! [in] Unit number
    const int iun
) {
    if (c_waopen2(iun) <= 0) {
        exit(1);
    }
}


//! \copydoc c_waopen
void f77name(waopen)(
    const int32_t * const iun
) {
    c_waopen(*iun);
}


//! Close a word addressable file
//! \return 0 on success, non-zero otherwise
int c_waclos2(
    //! [in] Unit number
    const int iun
) {
    int entry = find_file_entry("c_waclos", iun);
    if (entry < 0) return entry;

    if (! FGFDT[entry].open_flag) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: unit %d is not open\n",__func__,iun);
        return -1;
        }

    int ier = qqcclos(entry);
    FGFDT[entry].open_flag = 0;
    FGFDT[entry].attr.wa = 0;
    return ier;
}


//! \copydoc c_waclos2
int32_t f77name(waclos2)(
    const int32_t * const iun
) {
   return c_waclos2(*iun);
}

//! Close a word addressable file
//! \deprecated Use c_waclos2 instead
void c_waclos(
    //! [in] Unit number
    const int iun
) {
    c_waclos2(iun);
}


//! \copydoc c_waclos
//! \deprecated Use waclos2 instead
void f77name(waclos)(
    const int32_t * const iun
) {
   c_waclos2(*iun);
}


//! Write into a word adressable file
//! \return Number of words written on success, negative number otherwise
int c_wawrit2(
    //! [in] Unit number
    const int iun,
    //! [in] Buffer containing the data to write
    const void * const buf,
    //! [in] Offset where to write in the file
    const unsigned int offset,
    //! [in] Number of words to write
    const int nwords
) {
#define WA_HOLE 2048
    uint32_t scrap[WA_HOLE];
    uint32_t *bufswap = (uint32_t *) buf;

    int entry = find_file_entry("c_wawrit", iun);
    if (entry < 0) return entry;

    if (! FGFDT[entry].open_flag) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: unit %d is not open\n",__func__,iun);
        return -1;
    }
    if ( FGFDT[entry].attr.read_only != 0 ) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: unit %d , file= %s is READ ONLY\n",__func__,iun,FGFDT[entry].file_name);
        return -1;
    }
    if ( offset > FGFDT[entry].file_size + WA_HOLE ) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s:  attempt to write beyond EOF+%d\n\tunit = %d, adr=%u > file_size=%d\n\tfilename=%s\n",__func__,WA_HOLE,iun,offset,FGFDT[entry].file_size,FGFDT[entry].file_name);
        exit(1);
    }
    if ( offset > FGFDT[entry].file_size + 1 ){
        qqcwawr(scrap, FGFDT[entry].file_size + 1, offset - FGFDT[entry].file_size, entry);
    }
    if (*little_endian) swap_buffer_endianness(bufswap, nwords);
    qqcwawr((uint32_t *)buf, offset, nwords, entry);
    if (*little_endian) swap_buffer_endianness(bufswap, nwords);

    return  nwords > 0 ? nwords : 0;
}


//! \copydoc c_wawrit2
int32_t f77name(wawrit2)(
    const int32_t * const iun,
    const void * const buf,
    const uint32_t * const offset,
    const int32_t * const nwords
) {
#if defined (ALL64)
    if ( offset > 0 ) {
        return c_wawrit2(*iun, buf, (2 * (*offset)) - 1, *nwords * 2);
    } else {
        return c_wawrit2(*iun, buf, *offset, *nwords);
    }
#else
    return c_wawrit2(*iun, buf, *offset, *nwords);
#endif
}


//! Write into a word adressable file
void c_wawrit(
    //! [in] Unit number
    const int iun,
    //! [in] Buffer containing the data to write
    const void * const buf,
    //! [in] Offset where to write in the file
    const unsigned int offset,
    //! [in] Number of words to write
    const int nwords
) {
    c_wawrit2(iun, buf, offset, nwords);
}


//! \copydoc c_wawrit
void f77name(wawrit)(
    const int32_t * const iun,
    const void * const buf,
    const uint32_t * const offset,
    const int32_t * const nwords
) {
    f77name(wawrit2)(iun, buf, offset, nwords);
}


//! Read from word adressable file
//! \return Number of entries read on success, negative error code otherwise
int c_waread2(
    //! [in] Unit number of the file from which to read
    const int iun,
    //! [in] Buffer that will contain the data read
    void *buf,
    //! [in] Offset from the beginning of the file where to read
    const unsigned int offset,
    //! [in] Number of word to read
    const int nwords
) {
    uint32_t *bufswap = (uint32_t *) buf;

    int entry = find_file_entry("c_waread", iun);
    if (entry < 0) return entry;

    if (! FGFDT[entry].open_flag) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: unit %d is not open\n",__func__,iun);
        return -1;
    }

    if ( offset > FGFDT[entry].eff_file_size + 2 ) return -2;

    if ( FGFDT[entry].eff_file_size == 0 ) return 0;

    int lnwords = nwords;
    if ( offset + lnwords - 1 > FGFDT[entry].eff_file_size ) {
        lnwords -= (offset + lnwords - 1 - FGFDT[entry].eff_file_size);
    }
    if ( lnwords == 0 ) return 0;
    qqcward((uint32_t *)buf, offset, lnwords, entry);
    if (*little_endian) swap_buffer_endianness(bufswap, lnwords);
    return lnwords;
}


//! \copydoc c_waread2
int32_t f77name(waread2)(
    const int32_t * const iun,
    void * const buf,
    const uint32_t * const offset,
    const int32_t * const nwords
) {
#if defined (ALL64)
    if ( adr > 0 ) {
        return c_waread2(*iun, buf, (2 * (*offset)) - 1, *nwords * 2);
    } else {
        return c_waread2(*iun, buf, *offset, *nwords);
    }
#else
    return c_waread2(*iun, buf, *offset, *nwords);
#endif
}


//! Read from word adressable file
void c_waread(
    //! [in] Unit number of the file from which to read
    const int iun,
    //! [in] Buffer that will contain the data read
    void * const buf,
    //! [in] Offset from the beginning of the file where to read
    const unsigned int offset,
    //! [in] Number of word to read
    const int nwords
) {
    if (c_waread2(iun, buf, offset, nwords) == -2) {
        int entry = find_file_entry("c_waread", iun);
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: attempt to read beyond EOF, of file %s, addr = %u, EOF = %d\n",__func__,FGFDT[entry].file_name,offset,FGFDT[entry].eff_file_size);
    }
}


//! \copydoc c_waread
void f77name(waread)(
    const int32_t * const iun,
    void * const buf,
    const uint32_t * const offset,
    const int32_t * const nwords
) {
    f77name(waread2)(iun, buf, offset, nwords);
}


//! Get file size
//! \return File size in words
int32_t c_wasize(
    //! [in] Unit number
    const int iun
) {
   int entry = find_file_entry("c_wasize", iun);
   if (entry < 0) return entry;

   uint32_t nwords;
   if (! FGFDT[entry].open_flag) {
      qqcopen(entry);
      nwords = FGFDT[entry].eff_file_size;
      qqcclos(entry);
   } else {
      nwords = FGFDT[entry].eff_file_size;
   }

   return nwords;
}


//! Get file size in Fortran words
//! \return File size in Fortran words
int32_t f77name(wasize)(
    //! [in] Unit number
    const int32_t * const iun
) {
#if defined (ALL64)
    return c_wasize(*iun) / 2;
#else
    return c_wasize(*iun);
#endif
}


//! Get file size in kilobytes
//! \return File size in kilobytes, or negative number on error
int32_t c_numblks(
    //! [in] Unit number
    const int iun
) {
   int nwords = c_wasize(iun);
   if (nwords < 0) return nwords;
   int wordPerKb = 1024 / sizeof(uint32_t);
   return (nwords + wordPerKb - 1) / wordPerKb;
}


//! \copydoc c_numblks
int32_t f77name(numblks)(
    const int32_t * const iun
) {
    return c_numblks(*iun);
}


//! Check if file exists
//!  \return 1 if file exists, 0 otherwise
int32_t f77name(existe)(
    //! [in] File path
    const char * const nom,
    //! [in] File path length
    F2Cl llng
) {
    F2Cl lng = llng;
    char filename[PATH_MAX + 1];

    int l2 = (lng <= PATH_MAX) ? lng : PATH_MAX;
    strncpy(filename, nom, l2);
    filename[l2] = '\0';

    while ((filename[l2-1] == ' ') && (l2 > 1)) {
        l2--;
        filename[l2] = '\0';
    }

    if (access(filename, F_OK) == -1) {
        /* file does not exist */
        return 0;
    } else {
        /* file exists */
        return 1;
    }
}


// Open a direct access file
void c_openda(
    //! [in] Unit number
    const int iun
) {
    c_waopen(iun);
}


//! \copydoc c_openda
void f77name(openda)(
    const int32_t * const iun
) {
    c_waopen(*iun);
}

//! Closes a direct access file.
void c_closda(
    //! [in] Unit number
    const int iun
) {
    c_waclos(iun);
}


//! \copydoc c_closda
void f77name(closda)(
    const int32_t * const iun
) {
   c_closda(*iun);
}


//! Check that I/O is done
void c_checda(
    //! [in] Unit number
    const int iun
) {
    for (int *pt = dastat; pt < &dastat[MAXWAFILES]; pt++) {
        if (*pt == iun) {
            *pt = 0;
            break;
        }
    }
}


//! \copydoc c_checda
void f77name(checda)(
    const int32_t * const iun)
{
    c_checda(*iun);
}


//! Read from a direct acces file
void c_readda(
    //! [in] Unit number
    const int iun,
    //! [out] Buffer where the data read will be placed
    int * const buf,
    //! [in] Number of words to read
    const int nwords,
    //! [in] Word to start from
    const int offset
) {
    int *pt = dastat;
    while (pt < &dastat[MAXWAFILES] && *pt != iun) {
       pt++;
    }

    if ( pt <= &dastat[MAXWAFILES] ) {
        if (*pt == iun) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: consecutive calls to readda without call to checda, iun=%d\n",__func__,iun);
            return;
        }
    }
    c_waread(iun, buf, (offset - 1) * blkSize + 1, nwords * blkSize);
    for (pt = dastat; pt < &dastat[MAXWAFILES]; pt++) {
        if ( *pt == 0 ) break ;
        if (  pt >= &dastat[MAXWAFILES] ) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: recompiler avec MAXWAFILES++\n",__func__);
            return;
        }
    }
    *pt = iun;
}


//! \copydoc c_readda
void f77name(readda)(
    const int32_t * const iun,
    int32_t * const buf,
    const int32_t * const nwords,
    const int32_t * const offset
) {
   int save = blkSize;
   blkSize = blkSize * (sizeof(int32_t) / sizeof(uint32_t));
   c_readda(*iun, buf, *nwords, *offset);
   blkSize = save;
}


//! Write to a direct access file
void c_writda(
    //! [in] Unit number
    const int iun,
    //! [in] Buffer containing the data to be written
    const int * const buf,
    //! [in] Number of words to write
    const int nwords,
    //! [in] Word to start from
    const int offset
) {
    int *pt = dastat;
    while (pt < &dastat[MAXWAFILES] && *pt != iun) {
        pt++;
    }
    if ( pt <= &dastat[MAXWAFILES] ) {
        if (*pt == iun) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: consecutive calls to writda without call to checda, iun=%d\n",__func__,iun);
            return;
        }
    }
    c_wawrit(iun, buf, (offset - 1) * blkSize + 1, nwords * blkSize);
    for (pt = dastat; pt < &dastat[MAXWAFILES]; pt++) {
        if( *pt == 0 ) break ;
        if (  pt >= &dastat[MAXWAFILES] ) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: recompiler avec MAXWAFILES++\n",__func__);
            return;
        }
    }
    *pt = iun;
}


//! \copydoc c_writda
void f77name(writda)(
    const int32_t * const iun,
    const int32_t * const buf,
    const int32_t * const nwords,
    const int32_t * const offset
) {
    int save = blkSize;
    blkSize = blkSize * (sizeof(int32_t) / sizeof(uint32_t));
    c_writda(*iun, buf, *nwords, *offset);
    blkSize = save;
}


//! Get file descriptor associated to a unit number
//! \return File descriptor or negative number on error
int c_getfdsc(
    //! [in] Unit number
    const int iun
) {
    int entry = find_file_entry("c_getfdsc", iun);
    if (entry < 0) return entry;

    if (! FGFDT[entry].attr.stream) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: unit %d does not have the STREAM attribute\n",__func__,iun);
        return -1;
    }
    if (! FGFDT[entry].open_flag) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: unit %d is not open\n",__func__,iun);
        return -1;
    }

    return FGFDT[entry].fd;
}


//! \copydoc c_getfdsc
int32_t f77name(getfdsc)(
    const int32_t * const iun
) {
    return c_getfdsc((int) *iun);
}


//! Seriously, why is this here!?  It does absolutely nothing, but a
//! symbol will be created which means it will overshadow any other
//! proper implementation that might exist elsewhere
void c_socket_open()
{
}
//! Open a stream
void c_sqopen(
    //! [in] Unit number
    const int iun
) {
    int entry = find_file_entry("c_sqopen", iun);
    if (entry < 0) return;

    if (FGFDT[entry].attr.pipe) {
        if ((FGFDT[entry].file_name[0] == '@') || (FGFDT[entry].file_name[0] == '%')) {
            c_socket_open();
        } else {
            if ((FGFDT[entry].file_name[1] == '0') || (FGFDT[entry].file_name[1] == '1')) {
                sscanf(&(FGFDT[entry].file_name[1]), "%d", &(FGFDT[entry].fd));
            } else {
                FGFDT[entry].file_name++;
                c_waopen(iun);
            }
        }
    } else {
        c_waopen(iun);
    }
}


//! \copydoc c_sqopen
void f77name(sqopen)(
    const int32_t * const iun
) {
    c_sqopen((int) *iun);
}


//! Close a stream
void c_sqclos(
    //! [in] Unit number
    const int iun
) {
    int entry = find_file_entry("c_sqclos", iun);
    if (entry < 0) return;

    if (FGFDT[entry].attr.wa == 1) {
        c_waclos(iun);
    }
}


//! \copydoc c_sqclos
void f77name(sqclos)(
    const int32_t * const iun) {
    c_sqclos((int) *iun);
}

//! Rewind a stream
void c_sqrew(
    //! [in] Unit number
    const int iun
) {
    int entry = find_file_entry("c_sqrew", iun);
    if (entry < 0) return;

    if (FGFDT[entry].attr.pipe) return;

    int fd = c_getfdsc(iun);
    if (fd <= 0) return;
    lseek64(fd, 0, SEEK_SET);
}


//! \copydoc c_sqrew
void f77name(sqrew)(
    const int32_t * const iun
) {
    c_sqrew((int) *iun);
}


//! Go to the end of stream(information)
void c_sqeoi(
    //! [in] Unit number
    const int iun
) {
    int entry = find_file_entry("c_sqeoi", iun);
    if (entry < 0) return;

    if (FGFDT[entry].attr.pipe) return;

    int fd = c_getfdsc(iun);
    if (fd <= 0) return;
    lseek64(fd, 0, SEEK_END);
}


//! [in] \copydoc c_sqeoi
void f77name(sqeoi)(
    const int32_t * const iun
) {
    c_sqeoi((int) *iun);
}


//! Read a certain number of words from a stream
//! \return Number of words read on success, 0 or negative on error
int c_sqgetw(
    //! [in] Unit number
    const int iun,
    //! [out] Buffer to hold the data read
    uint32_t * const buf,
    //! [in] Number of words to read
    const int nwords
) {
    int alu = 0;
    int nlu = 1;
    int alire = nwords * sizeof(uint32_t);

    int fd = c_getfdsc(iun);
    if (fd <= 0) return fd;

    uint32_t *pbuf = buf;
    while (alire && (nlu > 0)) {
        nlu = read(fd, pbuf, alire);
        alire -= nlu;
        alu += nlu;
        pbuf += (nlu / sizeof(uint32_t));
    }
    return (alire == 0) ? alu / sizeof(uint32_t) : -1;
}


int32_t f77name(sqgetw)(
    const int32_t * const iun,
    uint32_t * const buf,
    const int32_t * const nwords
) {
    return c_sqgetw((int) *iun, (uint32_t *) buf, *nwords);
}


//! Write in a stream
int c_sqputw(
    //! [in] Unit number
    const int iun,
    //! [in] Buf from which to read the data to be written
    const uint32_t * const buf,
    //! [in] Number of words to write
    const int nwords
) {
    int aecrit = 0;
    int aecrire = sizeof(uint32_t) * nwords;
    int necrit = 1;
    int fd = c_getfdsc(iun);
    if (fd <= 0) return fd;

    const uint32_t *pbuf = buf;
    while (aecrire && (necrit > 0)) {
        necrit = write(fd, pbuf, aecrire);
        aecrire -= necrit;
        aecrit += necrit;
        pbuf += (necrit / sizeof(uint32_t));
    }
    return (aecrire == 0) ? necrit / sizeof(uint32_t) : -1;
}


//! \copydoc c_sqputw
int32_t f77name(sqputw)(
    const int32_t * const iun,
    const uint32_t * const buf,
    const int32_t * const nwords
) {
    return c_sqputw((int) *iun, buf, *nwords );
}


//! Read bytes from a file
//! \return The number of characters read on success, 0 or negative on error
int c_sqgets(
    //! [in] Unit number of the file from which to read
    const int iun,
    //! [out] Pointer to where the data read will be placed
    char * const buf,
    //! [in] Number of bytes to read
    const int nchar
) {
    int fd = c_getfdsc(iun);
    if (fd <= 0) return fd;
    int nlu = read(fd, buf, nchar);
    return  (nlu > 0) ? nlu : -1;
}

//! Read bytes from a file.
//! Reads the smallest of nchar or the size of the Fortran string bytes
//! \return The number of characters read on success, 0 or negative on error
int32_t f77name(sqgets)(
    //! [in] Unit number of the file from which to read
    const int32_t * const iun,
    //! [out] Pointer to where the data read will be placed
    char * const buf,
    //! [in] Number of bytes to read
    const int32_t * const nchar,
    //! [in] Size of buf if it's a Fortran string
    F2Cl llbuf
) {
    if (llbuf >= *nchar) {
        return c_sqgets(*iun, buf, *nchar);
    } else {
        return c_sqgets(*iun, buf, llbuf);
    }
}


//! Write bytes to a file
int c_sqputs(
    //! [in] Unit number of the file into which to write
    const int iun,
    //! [in] Buffer containing the data to write
    const char * const buf,
    //! [in] Number of bytes to write
    const int nchar
) {
    int fd = c_getfdsc(iun);
    if (fd <= 0) return fd;
    int nlu = write(fd, buf, nchar);
    return (nlu > 0) ? nlu : -1;
}


//! \copydoc c_sqputs
int32_t f77name(sqputs)(
    const int32_t *iun,
    const char * const buf,
    const int32_t * const nchar,
    F2Cl llbuf
) {
    if (llbuf >= *nchar) {
        return c_sqputs(*iun, buf , *nchar);
    } else {
        return c_sqputs(*iun, buf , llbuf);
    }
}


//! Get rid of the least useful page
static void scrap_page(
    //! [in] Index of the first file
    const int ind0,
    //! [in] Index of the last file
    const int ind1
) {
    int fl0 = ind0;
    int pg0 = 0;
    int age0 = 0x7fffffff;
    int found_a_page = 0;
    Lib_Log(APP_LIBRMN,APP_DEBUG,"%s:ind0=%d, ind1=%d\n",__func__,ind0,ind1);

    // Trouver la page la moins utile
    for (int j = ind0; j <= ind1; j++) {
        for (int i = 0; i < wafile[j].nb_page_in_use; i++) {
             Lib_Log(APP_LIBRMN,APP_EXTRA,"%s: j=%d, i=%d age0=%d\n",__func__,j,i,wafile[j].page[i].access_count);
             if (wafile[j].page[i].access_count < age0) {
                age0 = wafile[j].page[i].access_count;
                found_a_page = 1;
                pg0 = i;
                fl0 = j;
            }
        }
    }
    if (found_a_page == 0) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: cannot find a page to scrap\n",__func__);
        exit(1);
    }
    Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: fl0=%d, pg0=%d, age0=%d\n",__func__,fl0,pg0,age0);

    // Réécrire la page si ce n'est pas une page read-only
    if (wafile[fl0].page[pg0].touch_flag) {
        int nm = wafile[fl0].page[pg0].walast - wafile[fl0].page[pg0].wa0 + 1;
        wseek(wafile[fl0].file_desc, wafile[fl0].page[pg0].wa0 - 1, SEEK_SET);
        int ier = write(wafile[fl0].file_desc, wafile[fl0].page[pg0].page_adr, sizeof(uint32_t) * nm);
        if (ier != sizeof(uint32_t) * nm) {
            Lib_Log(APP_LIBRMN,APP_FATAL,"%s: cannot write page, fd=%d\n",__func__,wafile[fl0].file_desc);
            Lib_Log(APP_LIBRMN,APP_FATAL,"%s: trying to write %d words buffer=%p, fileadr=%d\n",__func__,nm,(void *)wafile[fl0].page[pg0].page_adr,wafile[fl0].page[pg0].wa0-1);
            Lib_Log(APP_LIBRMN,APP_FATAL,"%s: ier=%d,fl0=%d,ind0=%d,ind1=%d\n",__func__,ier,fl0,ind0,ind1);
            perror("FATAL WA ERROR");
            exit(1);
        }
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: ecriture disque adr=%d, nmots=%d\n",__func__,wafile[fl0].page[pg0].wa0,nm);
    }
    wafile[fl0].nb_page_in_use--;
    free_list[++nfree] = wafile[fl0].page[pg0].page_adr;
    if (pg0 != wafile[fl0].nb_page_in_use) {
        memcpy(&wafile[fl0].page[pg0], &wafile[fl0].page[pg0+1], sizeof(PAGEINFO) * (wafile[fl0].nb_page_in_use - pg0));
    }
    pg0 = wafile[fl0].nb_page_in_use;
    wafile[fl0].page[pg0].wa0 = 0;
    wafile[fl0].page[pg0].walast = 0;
    wafile[fl0].page[pg0].access_count = 0;
    wafile[fl0].page[pg0].last_access = 0;
    wafile[fl0].page[pg0].touch_flag = 0;
}


//! Update age and access count of pages
static void process_decay() {
    for (int j = 0; j < MAXWAFILES; j++) {
        for (int i = 0; i < wafile[j].nb_page_in_use; i++) {
            wafile[j].page[i].access_count = decay(wafile[j].page[i].access_count);
            wafile[j].page[i].last_access++;
        }
    }
}


//! Get a new page
static void get_new_page(
    //! [in] Index of the file into which to get a new page
    const int fileIdx
) {
    if (wafile[fileIdx].nb_page_in_use >= WA_PAGE_NB) {
        scrap_page(fileIdx, fileIdx);
        if (wafile[fileIdx].nb_page_in_use >= WA_PAGE_NB) {
            Lib_Log(APP_LIBRMN,APP_FATAL,"%s: no page left !??\n",__func__);
            exit(1);
        }
    }

    if (nfree < 0) {
        if (global_count < WA_PAGE_LIMIT) {
            global_count++;
            free_list[++nfree] = (uint32_t *) malloc(WA_PAGE_SIZE * sizeof(uint32_t));
            if (free_list[nfree] == NULL) {
                Lib_Log(APP_LIBRMN,APP_FATAL,"%s: can't allocate (not enough memory)\n",__func__);
                exit(1);
            }
            Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: nfree=%d malloc=%p\n",__func__,nfree,(void *)free_list[nfree]);
        }
    }
    if (nfree < 0) {
        scrap_page(0, MAXWAFILES - 1);
    }

    if (nfree < 0) {
        Lib_Log(APP_LIBRMN,APP_FATAL,"%s: o page left !??\n",__func__);
        exit(1);
    }

    int pg0 = wafile[fileIdx].nb_page_in_use++;

    wafile[fileIdx].page[pg0].page_adr = free_list[nfree--];
    wafile[fileIdx].page[pg0].wa0 = 0;
    wafile[fileIdx].page[pg0].walast = 0;
    wafile[fileIdx].page[pg0].access_count = 0;
    wafile[fileIdx].page[pg0].last_access = 0;
    wafile[fileIdx].page[pg0].touch_flag = 0;
    Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: pg0=%d, page_adr=%d\n",__func__,pg0,wafile[fileIdx].page[pg0].page_adr);
}


//! Get rid of all the pages of a word addressable file
static void wa_pages_flush(
    //! [in] Index of the wafile
    const int fileIdx
) {
    while (wafile[fileIdx].nb_page_in_use > 0) {
        scrap_page(fileIdx, fileIdx);
    }
}


//! Get the position of the start of the data of a subfile in a CMCARC file
//! \return Position of the start of the data of a subfile in a CMCARC file
static long long filepos(
    //! [in] Index of the subfile in the master file table
    const int indf
) {
    char sign[25];

    typedef struct {
        unsigned char ntotal[4];
        unsigned char ndata[4];
        char code;
        char header[MAX_NAME];
    } HEADER_CMCARC;

    HEADER_CMCARC *cmcarc_file;

    lseek64(FGFDT[indf].fd, 0, SEEK_SET);
    int nblu = read(FGFDT[indf].fd, sign, 8);
    if (strncmp(sign, CMCARC_SIGN, 8) != 0) {
        int version = 0;
        nblu = read(FGFDT[indf].fd, &sign[8], 17);
        if (strncmp(&sign[9], CMCARC_SIGN, 8) == 0) {
            // skip to beginning of next file
            version = 4;
        } else if (strncmp(&sign[17], CMCARC_SIGN_V5, 8) == 0) {
            version = 5;
        } else {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: %s is not a CMCARC type file\n",__func__,FGFDT[indf].file_name);
            return -1;
        }

        cmcarc_file = (HEADER_CMCARC *) &sign[0];
        unsigned int nt = (cmcarc_file->ntotal[0] << 24) |
            (cmcarc_file->ntotal[1] << 16) |
            (cmcarc_file->ntotal[2] <<  8) |
            (cmcarc_file->ntotal[3]);

        unsigned int nd = (cmcarc_file->ndata[0] << 24) |
            (cmcarc_file->ndata[1] << 16) |
            (cmcarc_file->ndata[2] <<  8) |
            (cmcarc_file->ndata[3]);

        if (version == 5) {
            nt = nd;
        } else {
            if (nd != 0) {
                Lib_Log(APP_LIBRMN,APP_ERROR,"%s: %s is a CMCARC file but nd=%d\n",__func__,FGFDT[indf].file_name,nd);
                return -1;
            }
        }
        int lng = (nt * 8) - 25;
        if (lseek64(FGFDT[indf].fd, lng, SEEK_CUR) == -1) {
            return -1;
        }
    }

    //! \warning File scope variable!
    subfile_length = 0;

    int found = 0;
    int tail_offset;
    int64_t nt64, nd64, lng64, nblu64;
    unsigned int nt, nd;
    do {
        // lire nt et nd
        nblu = read(FGFDT[indf].fd, &cmcarc, 8);
        if (nblu != 8) return -2;

        nt = (cmcarc.ntc[0] << 24) |
            (cmcarc.ntc[1] << 16) |
            (cmcarc.ntc[2] <<  8) |
            (cmcarc.ntc[3]);

        nd = (cmcarc.ndc[0] << 24) |
            (cmcarc.ndc[1] << 16) |
            (cmcarc.ndc[2] <<  8) |
            (cmcarc.ndc[3]);

        if (nt >= nd + 4) {
            nt64 = nt;
            nd64 = nd;
            lng64 = (nt64 - nd64 - 2) * 8;
            tail_offset = 1;
        } else {
            tail_offset = 2;
            nt64 = nt;
            nt64 = (nt64 << 32) | nd;
            read(FGFDT[indf].fd, &cmcarc, 8);
            nd64 = cmcarc.ntc[0];
            nd64 = (nd64 << 8) | cmcarc.ntc[1];
            nd64 = (nd64 << 8) | cmcarc.ntc[2];
            nd64 = (nd64 << 8) | cmcarc.ntc[3];
            nd64 = (nd64 << 8) | cmcarc.ndc[0];
            nd64 = (nd64 << 8) | cmcarc.ndc[1];
            nd64 = (nd64 << 8) | cmcarc.ndc[2];
            nd64 = (nd64 << 8) | cmcarc.ndc[3];
            lng64 = (nt64 - nd64 - 4) * 8;
            if (nt64 < nd64 + 6) {
                Lib_Log(APP_LIBRMN,APP_ERROR,"%s: %s is a CMCARC file but nt=%ld nd=%ld\n",__func__,FGFDT[indf].file_name,nt64,nd64);
                return -1;
            }
        }
        nblu64 = read(FGFDT[indf].fd, cmcarc.cmcarc_name, lng64);
        if (nblu64 != lng64) return -3;
        if (strcmp(FGFDT[indf].subname, &cmcarc.cmcarc_name[1]) == 0) {
            found = 1;
        } else {
            // sauter les donnees
            lng64 = (nd64 + tail_offset) * 8;
            if (lseek64(FGFDT[indf].fd, lng64, SEEK_CUR) == -1) {
                return -1;
            }
        }
    } while(!found);

    subfile_length = (nd * 8) / sizeof(uint32_t);
    off64_t pos64 = lseek(FGFDT[indf].fd, 0, SEEK_END);
    return pos64 / sizeof(uint32_t);
}


//! Open a non-Fortran file (active part of c_waopen2)
//! \return File descriptor on success, negative number otherwise
static int qqcopen(
    //! [in] Index of the subfile in the master file table
    const int indf
) {
    if (! init) {
        char * waConfig = getenv("WA_CONFIG");
        int nset = 0;
        int n1, n2, n3, n4;
        if (waConfig != NULL) {
            nset = sscanf(waConfig, "%d %d %d %d", &n1, &n2, &n3, &n4);
        }

        switch (nset) {
            case 4:
            case 3:
                WA_PAGE_LIMIT = n3;

            case 2:
                WA_PAGE_NB = n2;

            case 1:
                WA_PAGE_SIZE = n1 * 1024 * (sizeof(int32_t) / sizeof(uint32_t));
                break;

            default:
                WA_PAGE_SIZE = 0;
        }

        WA_PAGE_NB = (WA_PAGE_NB < MAXPAGES) ? WA_PAGE_NB : MAXPAGES;

        if (WA_PAGE_LIMIT == 0) {
            WA_PAGE_LIMIT = WA_PAGE_NB * MAXWAFILES;
        }
        if (WA_PAGE_SIZE > 0) {
            Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: WA_PAGE_SZ=%ld Bytes WA_PAGE_NB=%d WA_PAGE_LIMIT=%d\n",__func__,WA_PAGE_SIZE*sizeof(uint32_t),WA_PAGE_NB,WA_PAGE_LIMIT);
        }
        for (int ind = 0; ind < MAXWAFILES; ind++) {
            wafile[ind].file_desc = -1;
            wafile[ind].nb_page_in_use = 0;
            wafile[ind].offset = 0;
        }
        //! \warning File scope variable
        init = 1;
    }

    if (FGFDT[indf].attr.remote) {
        // file will be open by fnom_rem_connect
        return 0;
    }

    FGFDT[indf].fd = -1;
    int ind = 0;
    while ((wafile[ind].file_desc != -1) && (ind < MAXWAFILES)) {
        ind++;
    }
    if (ind == MAXWAFILES) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: too many open files\n",__func__);
        return -1;
    }

    FILE* fd;
    if (FGFDT[indf].subname) {
        // cmcarc file
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s:  opening subfile %s from file %s\n",__func__,FGFDT[indf].subname,FGFDT[indf].file_name);
        FGFDT[indf].attr.read_only = 1;
        fd = open64(FGFDT[indf].file_name, O_RDONLY);
        if (fd == -1) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: cannot open file %s\n",__func__,FGFDT[indf].file_name);
            return -1;
        }
        wafile[ind].file_desc = fd;
        FGFDT[indf].fd = fd;
        if ((wafile[ind].offset = filepos(indf)) <= 0) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: subfile %s not found in %s\n",__func__, FGFDT[indf].subname,FGFDT[indf].file_name);
            return -1;
        }
        FGFDT[indf].open_flag = 1;
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: subfile found at position %llu\n",__func__,wafile[ind].offset);
    } else {
        // Not a cmcarc file
        char *errmsg = "";
        if (access(FGFDT[indf].file_name, F_OK) == -1) {
            if (errno == ENOENT) {
                // Create new file
                fd = open64(FGFDT[indf].file_name, O_RDWR | O_CREAT,
                            S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
                FGFDT[indf].attr.read_only = 0;
                errmsg = "cannot create file";
            }
        } else {
            if (! FGFDT[indf].attr.read_only) {
                fd = open64(FGFDT[indf].file_name, O_RDWR);
                if (fd == -1) {
                    if (!FGFDT[indf].attr.write_mode) {
                        FGFDT[indf].attr.read_only = 1;
                        fd = open64(FGFDT[indf].file_name, O_RDONLY);
                        errmsg = "cannot open file";
                    } else {
                        errmsg = "cannot open in write mode";
                    }
                }
            } else if (FGFDT[indf].attr.read_only) {
                fd = open64(FGFDT[indf].file_name, O_RDONLY);
                errmsg = "cannot open file";
            }
        }
        if (fd == -1) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: %s filename=(%s) !\n",__func__,errmsg,FGFDT[indf].file_name);
            return -1;
        }
        wafile[ind].file_desc = fd;
        FGFDT[indf].fd = fd;
        FGFDT[indf].open_flag = 1;
    }

    off64_t dim = lseek64(fd, 0, SEEK_END);
    FGFDT[indf].file_size = dim / sizeof(uint32_t);
    FGFDT[indf].eff_file_size = dim / sizeof(uint32_t);
    dim = 0;
    dim = lseek64(fd, dim, SEEK_SET);
    if (subfile_length > 0) {
        FGFDT[indf].eff_file_size = subfile_length;
    }
    subfile_length = 0;

    if (WA_PAGE_SIZE != 0) {
        for (int i = 0; i < WA_PAGE_NB; i++) {
            wafile[ind].page[i].page_adr = NULL;
            wafile[ind].page[i].wa0 = 0;
            wafile[ind].page[i].walast = 0;
            wafile[ind].page[i].access_count = 0;
            wafile[ind].page[i].last_access = 0;
            wafile[ind].page[i].touch_flag = 0;
        }
        wafile[ind].nb_page_in_use = 0;
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: ouverture du fichier %s ind=%d, fd=%d longueur=%lld Bytes\n",__func__,FGFDT[indf].file_name,ind,fd,dim);
    }
    return fd;
}


//! Print wa control table
void f77name(d_wafdt)()
{
    Lib_Log(APP_LIBRMN,APP_ALWAYS,"%s: DUMP OF WA CONTROL TABLE\n",__func__);
    for (int i = 0; i < MAXWAFILES; i++) {
        if (wafile[i].file_desc != -1) {
            Lib_Log(APP_LIBRMN,APP_VERBATIM,"waindex=%d, fd=%d, npages=%d, offset=%lld\n",
                i, wafile[i].file_desc, wafile[i].nb_page_in_use, wafile[i].offset);
        }
    }
}


//! Read a word adressable file page
static void wa_page_read(
    //! [in] Descriptor of the file from which to read
    const int fd,
    //! [out] Buffer where to place the data read
    uint32_t * const buf,
    //! [in] Offset from beginning of file
    const unsigned int adr,
    //! [in] Number of words to read
    const int nmots,
    //! [in] Index of the file in the master file table
    const int indf
) {
    process_decay();

    // Trouver l'index du fichier
    int fileIdx = 0;
    while ((wafile[fileIdx].file_desc != fd) && (fileIdx < MAXWAFILES)) {
        fileIdx++;
    }
    Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: requete adr=%u, nmots=%d ind=%de\n",__func__,adr,nmots,fileIdx);
    if (fileIdx == MAXWAFILES) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: file is not open\n",__func__);
        exit(1);
    }

    // Trouver la page qui contient [adr, adr+nmots]
    int found = 0;
    int pageIdx = 0;
    while ((! found) && (pageIdx < wafile[fileIdx].nb_page_in_use)) {
        if ((adr >= wafile[fileIdx].page[pageIdx].wa0) && (adr + nmots <= wafile[fileIdx].page[pageIdx].wa0 + WA_PAGE_SIZE)) {
            found = 1;
        } else {
            pageIdx++;
        }
    }

    if (! found) {
        // Obtenir une nouvelle page
        get_new_page(fileIdx);
        pageIdx = wafile[fileIdx].nb_page_in_use - 1;
        int wa0 = adr - (adr % WA_PAGE_SIZE) + 1;
        wafile[fileIdx].page[pageIdx].wa0 = (wa0 > 1) ? wa0 : 1;
        wafile[fileIdx].page[pageIdx].access_count++;
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: obtention d'une page %d\n",__func__,pageIdx);

        wseek(fd, wafile[fileIdx].page[pageIdx].wa0 - 1, SEEK_SET);
        uint32_t readbytes;
        if (WA_PAGE_SIZE + wafile[fileIdx].page[pageIdx].wa0 > FGFDT[indf].file_size) {
            readbytes = sizeof(uint32_t) * (FGFDT[indf].file_size + 1 - wafile[fileIdx].page[pageIdx].wa0);
        } else {
            readbytes = sizeof(uint32_t) * WA_PAGE_SIZE;
        }
        int nbytes = read(fd, wafile[fileIdx].page[pageIdx].page_adr, sizeof(uint32_t) * WA_PAGE_SIZE);
        if ( nbytes < readbytes ) {
            Lib_Log(APP_LIBRMN,APP_FATAL,"%s: cannot read page from file %d,fd=%d, tried to get %ld bytes, got %d\n",__func__,fileIdx,sizeof(uint32_t)*WA_PAGE_SIZE,nbytes);
            perror("WA_PAGE_READ");
            exit(1);
        }
        if (nbytes < sizeof(uint32_t) * WA_PAGE_SIZE) {
            uint32_t lnmots = WA_PAGE_SIZE - (nbytes / sizeof(uint32_t));
            arrayZero(wafile[fileIdx].page[pageIdx].page_adr + (nbytes / sizeof(uint32_t)), lnmots);
        }
        wafile[fileIdx].page[pageIdx].walast = wafile[fileIdx].page[pageIdx].wa0 + nbytes / sizeof(uint32_t) - 1;
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: lecture disque adr=%d\n",__func__,wafile[fileIdx].page[pageIdx].wa0);
    }

    // Copier la section désirée dans buf
    int offset = adr - wafile[fileIdx].page[pageIdx].wa0;
    arrayCopy(wafile[fileIdx].page[pageIdx].page_adr + offset, buf, nmots);
    wafile[fileIdx].page[pageIdx].last_access = 0;
    wafile[fileIdx].page[pageIdx].access_count = new_age_rd(wafile[fileIdx].page[pageIdx].access_count);

    // Vérification des croisements possibles des pages
    for (int j = 0; j < wafile[fileIdx].nb_page_in_use; j++) {
        for (int i = 0; i < wafile[fileIdx].nb_page_in_use; i++) {
            if (j != i) {
                if ((wafile[fileIdx].page[j].wa0 >= wafile[fileIdx].page[pageIdx].wa0) &&
                    (wafile[fileIdx].page[j].wa0 <= wafile[fileIdx].page[pageIdx].wa0 + WA_PAGE_SIZE - 1)) {
                    Lib_Log(APP_LIBRMN,APP_FATAL,"%s: overlapping pages i=%d, page[j].wa0=%d, page[i].wa0=%d, page[i].wa0+WA_PAGE_SIZE=%d\n",__func__,i,wafile[fileIdx].page[j].wa0,wafile[fileIdx].page[pageIdx].wa0,wafile[fileIdx].page[pageIdx].wa0+WA_PAGE_SIZE-1);
                    f77name(tracebck)();
                    exit(1);
                }
            }
        }
    }
}


//! Right justify 8 bits characters
uint32_t f77name(hrjust) (
    //! [in,out] Input string
    uint32_t *str,
    //! [in] Number of characters
    const int32_t * const ncar
) {
   int sc = 8 * ( sizeof(int32_t) - *ncar );
   return sc <= 0 ? *str : (*str) >> sc;
}


//! Left justify 8 bit characters
uint32_t f77name(hljust) (
    //! [in,out] Input string
    uint32_t *str,
    //! [in] Number of characters
    const int32_t * const ncar
) {
   int sc = 8 * ( sizeof(int32_t) - *ncar );
   return sc <= 0 ? *str : (*str) << sc;
}


//! Write a wa page
static void wa_page_write(
    //! [in] File descriptor
    const int fd,
    //! [in] Buffer containing the data to write
    const uint32_t * const buf,
    //! [in] Offset where to write in the file
    const unsigned int offset,
    //! [in] Number of words to write
    const int nmots,
    //! [in] Index of the wafile in the master file table
    int indf
) {
    process_decay();

    int ind = 0;
    while ((wafile[ind].file_desc != fd) && (ind < MAXWAFILES)) {
        ind++;
    }
    Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: requete adr=%u, nmots=%d ind=%d\n",__func__, offset,nmots,ind);
    if (ind == MAXWAFILES) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: file is not open\n",__func__);
        exit(1);
    }

    // Search for the page containing [offset, offset+nmots]
    int found = 0;
    int pageIdx = 0;
    while((! found) && (pageIdx < wafile[ind].nb_page_in_use)) {
        if ((offset >= wafile[ind].page[pageIdx].wa0) && (offset + nmots <= wafile[ind].page[pageIdx].wa0 + WA_PAGE_SIZE)) {
            found = 1;
        } else {
            pageIdx++;
        }
    }

    if (! found) {
        get_new_page(ind);
        pageIdx = wafile[ind].nb_page_in_use - 1;
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: obtention d'une page %d\n",__func__,pageIdx);
        int wa0 = offset - (offset % WA_PAGE_SIZE) + 1;
        wafile[ind].page[pageIdx].wa0 = (wa0 > 1) ? wa0 : 1;
        wafile[ind].page[pageIdx].access_count++;

        // si la requete d'ecriture ne couvre pas la page en entier, ou
        // si la requete ne part pas du debut de la page pour se terminer
        // a la fin de fichier ou plus, alors il faut relire la page en question
        if ((offset > wafile[ind].page[pageIdx].wa0) ||
            ((offset + nmots != wafile[ind].page[pageIdx].wa0 + WA_PAGE_SIZE) &&
            (offset + nmots < FGFDT[indf].file_size))) {

            wseek(fd, wafile[ind].page[pageIdx].wa0 - 1, SEEK_SET);
            int readbytes;
            if (WA_PAGE_SIZE + wafile[ind].page[pageIdx].wa0 > FGFDT[indf].file_size) {
                readbytes = sizeof(uint32_t) * (FGFDT[indf].file_size + 1 - wafile[ind].page[pageIdx].wa0);
            } else {
                readbytes = sizeof(uint32_t) * WA_PAGE_SIZE;
            }
            int nbytes = read(fd, wafile[ind].page[pageIdx].page_adr, readbytes);
            if ( nbytes < readbytes ) {
                Lib_Log(APP_LIBRMN,APP_ERROR,"%s: cannot read page on file %s\n",__func__,FGFDT[indf].file_name);
                Lib_Log(APP_LIBRMN,APP_ERROR,"%s: tried to get %d bytes, got %d. WA_PAGE_SIZE=%d wa0=%d file_size=%d\n",__func__,readbytes,nbytes,WA_PAGE_SIZE,wafile[ind].page[pageIdx].wa0,FGFDT[indf].file_size);
                perror("WA_PAGE_WRITE");
                exit(1);
            }
            if (nbytes < sizeof(uint32_t) * WA_PAGE_SIZE) {
                int lnmots = WA_PAGE_SIZE - (nbytes / sizeof(uint32_t));
                arrayZero(wafile[ind].page[pageIdx].page_adr + (nbytes / sizeof(uint32_t)), lnmots);
            }
            wafile[ind].page[pageIdx].walast = wafile[ind].page[pageIdx].wa0 + nbytes / sizeof(uint32_t) - 1;
            Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: relecture disque de la page %d a l'adresse %d\n",__func__,pageIdx,wafile[ind].page[pageIdx].wa0);
        }
    }

    // Copier dans la page la section buf
    int loffset = offset - wafile[ind].page[pageIdx].wa0;
    arrayCopy(buf, wafile[ind].page[pageIdx].page_adr + loffset, nmots);
    wafile[ind].page[pageIdx].last_access = 0;
    wafile[ind].page[pageIdx].access_count = new_age_wr(wafile[ind].page[pageIdx].access_count);
    wafile[ind].page[pageIdx].touch_flag = 1;
    if (offset > FGFDT[indf].file_size + 1) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: ind = %d, adr=%u > file_size=%d filename=%s\n",__func__,ind,offset,FGFDT[indf].file_size,FGFDT[indf].file_name);
        exit(1);
    }

    if (offset + nmots - 1 > FGFDT[indf].file_size) {
        FGFDT[indf].file_size = offset + nmots - 1;
        FGFDT[indf].eff_file_size = offset + nmots - 1;
    }
    if (wafile[ind].page[pageIdx].walast < offset + nmots - 1) {
        wafile[ind].page[pageIdx].walast = offset + nmots - 1;
    }

    for (int j = 0; j < wafile[ind].nb_page_in_use; j++) {
        for (int i = 0; i < wafile[ind].nb_page_in_use; i++) {
            if (j != i) {
                if ((wafile[ind].page[j].wa0 >= wafile[ind].page[i].wa0) &&
                           (wafile[ind].page[j].wa0 <= wafile[ind].page[i].wa0 + WA_PAGE_SIZE - 1)) {
                    Lib_Log(APP_LIBRMN,APP_FATAL,"%s: overlapping pages i=%d page[j].wa0 =%d, page[i].wa0 =%d, page[i].wa0+WA_PAGE_SIZE =%\n",__func__,i,wafile[ind].page[j].wa0,wafile[ind].page[i].wa0,wafile[ind].page[i].wa0+WA_PAGE_SIZE-1);
                    f77name(tracebck)();
                    exit(1);
                }
            }
        }
    }
}


//! Write in a word adressable file
static void qqcwawr(
    //! [in] Buffer contraining the data to write
    const uint32_t * const buf,
    //! [in] File adressable in words
    const unsigned int wadr,
    //! [in] Number of words to write
    const int nwords,
    //! [in] Index of the file in the master file table
    const int indf
) {
    int lfd = FGFDT[indf].fd;

    int ind = 0;
    while ((wafile[ind].file_desc != lfd) && (ind < MAXWAFILES)) {
        ind++;
    }
    if (ind == MAXWAFILES) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: filename=%s, fd=%d not found in table\n",__func__,FGFDT[indf].file_name,lfd);
        exit(1);
    }

    long long ladr = wadr;
    if (ladr != 0) {
        ladr += wafile[ind].offset;
    }

    if (FGFDT[indf].attr.read_only) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: no write permission for file %s\n",__func__,FGFDT[indf].file_name);
        exit(1);
    }

    if (FGFDT[indf].attr.remote) {
        int demande[5];

        int *s_ID = &(demande[0]);
        int *addr = &(demande[1]);
        int *nw = &(demande[2]);
        int *RW_mode = &(demande[3]);
        int *checksum = &(demande[4]);
        *s_ID = 0xBABE;
        *addr = ladr;
        *nw = nwords;
        *RW_mode = 2;  /* write request */
        *checksum = *s_ID ^ *addr ^ *nw ^ *RW_mode;
        check_swap_records(demande, 5, sizeof(int));
        int nc = write_stream(FGFDT[indf].fd, (const char *)demande, 5 * sizeof(int));
        if (nc != 0) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: socket wrote only %i bytes to server\n",__func__,nc);
        }
        int nelm = write_stream(FGFDT[indf].fd, (const char *)buf, nwords * sizeof(int));
#if defined (DEBUG)
        if (nelm == 0) Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: socket wrote %d bytes\n",__func__,nwords*sizeof(int));
#endif
        if (ladr + nwords - 1 > FGFDT[indf].file_size) {
            FGFDT[indf].file_size = ladr + nwords - 1;
            FGFDT[indf].eff_file_size = ladr + nwords - 1;
        }
    } else {
        // File is local
        if ((WA_PAGE_SIZE == 0) || (ladr == 0)) {
            if (ladr != 0) wseek(lfd, ladr - 1, SEEK_SET);
            int nwritten = write(lfd, buf, sizeof(uint32_t) * nwords);
            if (nwritten != sizeof(uint32_t) * nwords) {
                if (errno == 14) {
                    Lib_Log(APP_LIBRMN,APP_ERROR,"%s: write error for file %s, filename=%s, buf=%p adr=%u, nmots=%d, nwritten=%d, errno=%d\n",__func__,FGFDT[indf].file_name,FGFDT[indf].file_name,buf,ladr,nwords,nwritten,errno);
                    Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Contactez un membre de la section informatique de RPN / Seek support from RPN informatic section\n",__func__);
                    /*            memorymap(1); */
                    perror("qqcwawr");
                    exit(1);
                }
                if (nwritten >= 0) {
                    int togo = (nwords * sizeof(uint32_t)) - nwritten;
                    nwritten = write(lfd, buf, togo);
                    Lib_Log(APP_LIBRMN,APP_ERROR,"%s: multiple write attempt of file %s last write=%d bytes, total needed=%ld bytes\n",__func__,FGFDT[indf].file_name,togo,nwords*sizeof(uint32_t));
                    if (nwritten != togo) {
                        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: write error for file %s, filename=%s, buf=%p adr=%u, nmots=%d, nwritten=%d, errno=%d\n",__func__,FGFDT[indf].file_name,FGFDT[indf].file_name,buf,ladr,nwords,nwritten,errno);
                        perror("qqcwawr");
                        exit(1);
                    }
                } else {
                    Lib_Log(APP_LIBRMN,APP_ERROR,"%s: write error or file not open for write, filename=%s, buf=%p adr=%u, nmots=%d, nwritten=%d, errno=%d\n",__func__,FGFDT[indf].file_name,buf,ladr,nwords,nwritten,errno);
                    perror("qqcwawr");
                    exit(1);
                }
            }
            if (ladr + nwords - 1 > FGFDT[indf].file_size) {
                FGFDT[indf].file_size = ladr + nwords - 1;
                FGFDT[indf].eff_file_size = ladr + nwords - 1;
            }
        } else {
            int lng = nwords;
            int adr0 = ladr;
            int offset = 0;
            int lastadr = (adr0 + WA_PAGE_SIZE -1)/WA_PAGE_SIZE * WA_PAGE_SIZE;
            while (lng > 0) {
                if (lng > lastadr - adr0) {
                    int l = lastadr - adr0 + 1;
                    wa_page_write(lfd, buf + offset, adr0, l, indf);
                    offset = offset + l;
                    adr0 = adr0 + l;
                    lng = lng - l;
                    lastadr = (adr0 + WA_PAGE_SIZE -1) / WA_PAGE_SIZE * WA_PAGE_SIZE;
                } else {
                    wa_page_write(lfd, buf + offset, adr0, lng, indf);
                    lng = 0;
                }
            }
        }
    }
}


//! Read from a word adressable file. (Active part of c_waread2)
static void qqcward(
    //! [out]
    uint32_t * const buf,
    //! [in] Offset in file in words
    const unsigned int woffset,
    //! [in] Number of words to read
    const int lnmots,
    //! [in] Index of the file in the master file table
    const int indf
) {
    int lfd = FGFDT[indf].fd;
    long long ladr = woffset;

    int ind = 0;
    while ((wafile[ind].file_desc != lfd) && (ind < MAXWAFILES)) ind++;
    if (ind == MAXWAFILES) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: fd=%d not found in table\n",__func__,lfd);
        exit(1);
    }
    if (FGFDT[indf].attr.remote) {
        const int sock_comm_ID = 0xBABE;
        int demande[5];

        int *s_ID = &(demande[0]);
        int *addr = &(demande[1]);
        int *nw = &(demande[2]);
        int *RW_mode = &(demande[3]);
        int *checksum = &(demande[4]);
        *s_ID = sock_comm_ID;
        *addr = ladr;
        *nw = lnmots;
        *RW_mode = 1;  /* read request */
        *checksum = *s_ID ^ *addr ^ *nw ^ *RW_mode;
        check_swap_records(demande, 5, sizeof(int));
        int nc = write_stream(FGFDT[indf].fd, (const char *)demande, 5 * sizeof(int));
        if (nc != 0) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: socket wrote only %d bytes to server\n",__func__,nc);
        }
        int nelm = read_stream(FGFDT[indf].fd, (char *)buf, lnmots * sizeof(int));
#       if defined (DEBUG)
            Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: read %d bytes\n",__func__,nelm);
#       endif
    } else {
        if (ladr != 0) {
            ladr += wafile[ind].offset;
        }

        if ((WA_PAGE_SIZE == 0) || (ladr == 0)) {
            if (ladr != 0) {
                wseek(lfd, ladr - 1, SEEK_SET);
            }
            int reste = read(lfd, buf, sizeof(uint32_t) * lnmots);
            if (reste != sizeof(uint32_t) * lnmots) {
                Lib_Log(APP_LIBRMN,APP_ERROR,"%s: tried to read %d words, only read %d, wafile[ind].offset=%d ladr=%Ld\n",__func__,sizeof(uint32_t)*lnmots,reste,wafile[ind].offset,ladr);
                f77name(tracebck)();
                exit(1);
            }
        } else {
            int lng = lnmots;
            int adr0 = ladr;
            int offset = 0;
            int l;
            int lastadr = (adr0 + WA_PAGE_SIZE -1) / WA_PAGE_SIZE * WA_PAGE_SIZE;
            while (lng > 0) {
                if (lng > lastadr-adr0) {
                    l = lastadr - adr0 + 1;
                    wa_page_read(lfd, buf + offset, adr0, l, indf);
                    offset = offset + l;
                    adr0 = adr0 + l;
                    lng = lng - l;
                    lastadr = (adr0 + WA_PAGE_SIZE - 1) / WA_PAGE_SIZE * WA_PAGE_SIZE;
                } else {
                    wa_page_read(lfd, buf + offset, adr0, lng, indf);
                    lng = 0;
                }
            }
        }
    }
}


//! Establish socket connection with host server
//! \return 0 on success, negative otherwise
static int fnom_rem_connect(
    //! [in] Index of the file in the master file table
    const int ind,
    //! [in] Remote host name
    const char * const remote_host
) {
    int server_port = -1;
    char cbuf[1024];
    int fserver = bind_to_localport(&server_port, cbuf, sizeof(cbuf)-1);
    listen(fserver, 5);
    Lib_Log(APP_LIBRMN,APP_INFO,"%s: bound to #%s#\n",__func__,cbuf);

    char pbuf[1024];
    Lib_Log(APP_LIBRMN,APP_INFO,"%s: echo wa_server %s %s @%s | ssh %s 'bash --login 1>/dev/null 2>/dev/null'\n",__func__,FGFDT[ind].file_name,(FGFDT[ind].attr.read_only == 1)?"R/O":"R/W",cbuf,remote_host);

    char remote_command[1024];
    Lib_Log(APP_LIBRMN,APP_INFO,"%s: r.remote_wa_server %s %s %s %s\n",__func__,FGFDT[ind].file_name,(FGFDT[ind].attr.read_only==1)?"R/O":"R/W",cbuf,remote_host);
    Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: commande passee =%s\n",__func__,remote_command);
    system(remote_command);

    fd_set rfds;
    FD_ZERO(&rfds);
    FD_SET(fserver, &rfds);
    struct timeval tv;
    tv.tv_sec = 5;
    tv.tv_usec = 0;

    int fclient = -1;
    int indx = 0;
    int isel = select(fserver + 1, &rfds, NULL, NULL, &tv);
    if (isel) {
        fclient = accept_from_sock(fserver);
        Lib_Log(APP_LIBRMN,APP_INFO,"%s: connected to server\n",__func__);
        FGFDT[ind].fd = -1;
        while ((wafile[indx].file_desc != -1) && (indx < MAXWAFILES)) {
            indx++;
        }
        if (indx == MAXWAFILES) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: too many open files\n",__func__);
            return -1;
        }

        int demande[5];
        int *s_ID = &(demande[0]);
        int *addr = &(demande[1]);
        int *nw = &(demande[2]);
        int *RW_mode = &(demande[3]);
        int *checksum = &(demande[4]);
        *s_ID = 0xBABE;
        *addr = 0;
        *nw = 0;
        *RW_mode = 4;  /* wasize request */
        *checksum = *s_ID ^ *addr ^ *RW_mode;
        check_swap_records(demande, 5, sizeof(int));
        int nc = write_stream(fclient, (const char *)demande, 5*sizeof(int));

        if (nc == 0) {
            Lib_Log(APP_LIBRMN,APP_INFO,"%s: wrote to server OK\n",__func__);
        } else {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: wrote only %d bytes to server\n",__func__,nc);
            close(fclient);
            return -1;
        }

        demande[0] = 0; demande[1] = 0; demande[2] = 0; demande[3] = 0; demande[4] = 0;
        nc = read_stream(fclient, demande, 5 * sizeof(int));
        if (nc !=  5 * sizeof(int)) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: read only %d bytes from server\n",__func__,nc);
            close(fclient);
            return -1;
        }

        check_swap_records(demande, 5, sizeof(int));
        int new_checksum = *s_ID ^ *addr ^ *RW_mode;
        if (new_checksum != *checksum) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: invalid checksum=%X not %X\n",__func__,new_checksum,checksum);
            close(fclient);
            return -1;
        }
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: wasize=%d\n",__func__,*nw);
        FGFDT[ind].file_size = *nw;
        FGFDT[ind].eff_file_size = *nw;
    } else {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: cannot connect to server\n",__func__);
        return -1;
    }

    wafile[indx].file_desc = fclient;
    FGFDT[ind].fd = fclient;
    FGFDT[ind].open_flag = 1;
    return 0;
}


//! Copy words from one array to another
static void arrayCopy(
    //! Pointer to the source array
    const uint32_t * const src,
    //! Pointer to the destination array
    uint32_t * const dest,
    //! Number of elements to copy
    const int nwords
) {
    for (int i = 0; i < nwords; i++) {
        dest[i] = src[i];
    }
}


//! Zero words of an array
static void arrayZero(
    //! Pointer to the array
    uint32_t * const dest,
    //! Number of words to zero
    const int nwords
) {
    for (int i = 0; i < nwords; i++) {
        dest[i] = 0;
    }
}

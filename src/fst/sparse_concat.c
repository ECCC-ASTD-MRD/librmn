//! \author  M. Valin,   Recherche en Prevision Numerique
//! \date 2024
//! \file sparse_concat.c
//! \brief Append a set of potentially sparse files to another potentially sparse file, or copy a sparse file

#define _LARGEFILE64_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

#include <App.h>
#include "rmn/sparse_concat.h"

//! Copy nbytes from fdi into fdo
//! \return 0 if copy successful, anything else if not
size_t CopyFileData(
    const int fdi,  //!< Input file descriptor
    const int fdo,  //!< Output file descriptor
    const size_t nbytes   //!< Number of bytes to copy
) {
    char buf[1024*1024*8];  // 8vi MB buffer for copying data
    size_t remaining = nbytes;
    while(remaining > 0){
        size_t nb = (remaining < sizeof(buf)) ? remaining : sizeof(buf);
        const ssize_t nr = read(fdi, buf, nb);
        const ssize_t nw = write(fdo, buf, nr);
        remaining -= nw;
        if(nr != nw) return 0xffffffffffffffff;
    }
    return remaining;
}

//! Copy fdi into fdo
//! \return Final size of output file
size_t SparseConcatFd(
    const int fdi,    //!< Input file descriptor
    const int fdo,    //!< Output file descriptor
    const int diag    //!< If non zero, fdo is ignored and fdi hole/data map is printed
) {
    off_t hole, cur_i, szo, cur_o;

    const off_t szi = lseek(fdi, 0L, SEEK_END);                // input file size
    szo = 0;
    if(diag == 0) szo = lseek(fdo, 0L, SEEK_END);  // output file size (only if diag == 0)

    hole = cur_i = lseek(fdi, 0L, SEEK_SET);
    while (hole < szi) {
        hole = lseek(fdi, cur_i, SEEK_HOLE);         // look for a hole
        if (diag && (hole - cur_i) > 0) Lib_Log(APP_LIBFST, APP_VERBATIM, "data at %12ld, %12ld bytes\n", cur_i, hole - cur_i);
        lseek(fdi, cur_i, SEEK_SET);
        if (diag == 0) CopyFileData(fdi, fdo, hole - cur_i);           // copy from current position up to hole
        if (hole < szi) {
            cur_i = lseek(fdi, hole, SEEK_DATA);
            if (diag) {
                Lib_Log(APP_LIBFST, APP_VERBATIM, "hole at %12ld, %12ld bytes \n", hole, cur_i - hole);
            } else {
                cur_o = lseek(fdo, cur_i - hole, SEEK_CUR);   // make a hole in output file
            }
        }
    }
    if (diag == 0) szo = lseek(fdo, 0L, SEEK_END);                   // return final size of output
    close(fdi);
    close(fdo);
    return szo;
}

//! Copy file name2 at end of name1.
//! If diag is true, name2 is ignored and name1 hole/data map is printed
//! \return Final size of output file. 0 if error
size_t SparseConcatFile(
    const char *name1,    //!< Output file name
    const char *name2,    //!< Input file name
    const int diag        //!< If non zero, name2 is ignored and a map of name1 is produced
) {
    int fdi, fdo ;

    if (diag == 0) Lib_Log(APP_LIBFST, APP_INFO, "%s: appending %s to %s\n", __func__, name2, name1);
    fdi = 0 ;
    if (diag == 0) fdi = open(name2, O_RDONLY | O_LARGEFILE);  // input file
    if (fdi < 0) return 0;

    if (diag) {
        fdo = open(name1, O_RDONLY | O_LARGEFILE);  // open as input file
        fdi = fdo;
    } else {
        fdo = open(name1, O_RDWR | O_CREAT | O_LARGEFILE, 0777);
    }
    if (fdo < 0) return 0;

    return SparseConcatFd(fdi, fdo, diag);
}

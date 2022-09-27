/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2007  Environnement Canada
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
/*
!-------------------------------------------------------------------
! Dependencies:
! This 'package' make use of RPN' FTN2C package
! to link Fortran and C s/r
!
! The interfaces to fortran code are defined in clib_interface.cdk
!-------------------------------------------------------------------
! Description
! This module is an interface for Fortran to some C STD fonctions
! Description of C STD functions can be found at:
! http://www.opengroup.org/onlinepubs/007908799/headix.html
!
! stdlib.h: standard library definitions
!   char *getenv(const char *name);
!   int putenv(char *string);
!   char *realpath(const char *file_name, char *resolved_name);
!   #int system(const char *command);
!      Use F90 command instead
!   #void qsort(void *base, size_t nel, size_t width, int (*compar)(const void *, const void *));
! stdio.h: standard buffered input/output
!   int remove(const char *path);
!   int rename(const char *old, const char *new);
! unistd.h: standard symbolic constants and types
!   #int access(const char *path, int amode);
!   #replaced by:
!     clib_fileexist : true if path exist
!     clib_isreadok  : true if path is readable
!     clib_iswriteok : true if path is writable
!     clib_isexecok  : true is path is executable/searchable
!   int chdir(const char *path);
!   char *getcwd(char *buf, size_t size);
!   #use getcwd because getwd is considerd unsafe by the compiler
!   #char *getwd(char *path_name);
!   int getuid();
!   #int getopt(int argc, char * const argv[], const char *optstring);
!   #int link(const char *path1, const char *path2);
!   int rmdir(const char *path);
!   int symlink(const char *path1, const char *path2);
!   int unlink(const char *path);
!
! sys/stat.h - data returned by the stat() function
!   int mkdir(const char *path, mode_t mode);
!   #int stat(const char *path, struct stat *buf);
!   #replaced by:
!     clib_isdir
!     clib_islink
!     clib_ispipe
!     clib_isfile
!     clib_mtime
!     clib_size
!     clib_stat
!   # might develep on top with filetype from rmnlib
!
! libgen.h: http://www.opengroup.org/onlinepubs/007908799/xsh/libgen.h.html
!   char *basename(char *path);  this one is emulated, because of IRIX
!   char *dirname(char *path);  this one is emulated, because of IRIX
!
! glob.h: http://www.opengroup.org/onlinepubs/007908799/xsh/glob.h.html
!   int glob(const char *pattern, int flags,
!            int(*errfunc)(const char *epath, int errno),
!            glob_t *pglob);
!   void globfree(glob_t *pglob);
!
! ctype.h
!   int toupper(int c);
!   int tolower(int c);
!   int isalnum(int c);
!   int isalpha(int c);
!   int isblank(int c);
!   int isdigit(int c);
!   int islower(int c);
!   int ispunct(int c);
!   int isspace(int c);
!   int isupper(int c);
!   int isxdigit(int c);
!
! [Pending functions]
! #time.h
! #dirent.h
! #ftw.h - file tree traversal
! #sys/resource.h - definitions for XSI resource operations
! #ulimit.h - ulimit commands
!===================================================================*/

/* be ready for files > 2 GB by using long offsets */
#define _FILE_OFFSET_BITS 64
#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <sys/types.h> /* for mkdir & stat */
#include <sys/stat.h>
#include <unistd.h>

#include <libgen.h>
// #include <glob.h>
#include <sys/param.h> /* for MAXPATHLEN = PATH_MAX */
#include <alloca.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include <rmn/rpnmacros.h>
#include <rmn/ftn2c_helper.h>

#define CLIB_OK    1
#define CLIB_ERROR -1

#define CLIB_F77NAME(a) f77_name(a##_schhide)


//! Get the value of an environment variable
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_getenv)(
    //! [in] Name of the environment variable to retrieve
    F77_CHARACTER *name,
    //! [out] Value of the environment variable
    F77_CHARACTER *value,
    //! [in] Maximum length of name
    F2Cl name_length,
    //! [in] Maximum length of value
    F2Cl value_length
) {
    // Translate to C strings
    int name_c_len = 1 + name_length;
    char * name_c = (char *)alloca((size_t)(name_c_len * sizeof(char)));
    if (!name_c || FTN2C_FSTR2CSTR(name, name_c, name_length, name_c_len) < 0) {
        return CLIB_ERROR;
    }

    // Call C function
    char * value_c = getenv(name_c);

    /* Translate Back to Fortran strings */
    F77_INTEGER status = CLIB_ERROR;
    if (value_c &&
        FTN2C_CSTR2FSTR(value_c, value, strlen(value_c)+1, value_length)>=0) {
        status = CLIB_OK;
    } else {
        char *defStr = " ";
        FTN2C_CSTR2FSTR(defStr, value, 2, value_length);
    }
    return status;
}


//! Set environment variable
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_putenv)(
    //! [in] Name of the environment variable to set and value in the form of "name=value"
    F77_CHARACTER *name_value,
    //! [in] Maximum length of name_value
    F2Cl name_value_length
) {
    // Translate to C strings
    int name_c_len = 1 + name_value_length;
    char * name_value_c = (char *)malloc((size_t)(name_c_len*sizeof(char)));
    if (!name_value_c || FTN2C_FSTR2CSTR(name_value, name_value_c, name_value_length, name_c_len) < 0) {
        return CLIB_ERROR;
    }

    /* Call C function */
    if (putenv(name_value_c)) {
        return CLIB_ERROR;
    } else {
        return CLIB_OK;
    }
    // WARNING: the name_value_c point should not be freed; putenv copy only the pointer, not its content
}


//! Get absolute path name without symbolic links
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_realpath)(
    //! [in] Path to resolve
    F77_CHARACTER *fnamein,
    //! [out] Resolved path (absolute)
    F77_CHARACTER *fnameout,
    //! [in] Maximum length of fnamein
    F2Cl fnamein_length,
    //! [in] Maximum length of fnameout
    F2Cl fnameout_length
) {
    // Translate to C strings
    char fnamein_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(fnamein, fnamein_c, fnamein_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    char * fnameout_c = realpath(fnamein_c, NULL);

    // Translate Back to Fortran strings
    F77_INTEGER status = CLIB_ERROR;
    if (fnameout_c &&
        FTN2C_CSTR2FSTR(fnameout_c, fnameout, strlen(fnameout_c) + 1, fnameout_length) >= 0) {
        status = CLIB_OK;
    } else {
        FTN2C_CSTR2FSTR(" ", fnameout, 2, fnameout_length);
    }
    free(fnameout_c);
    return status;
}


//! Get the path pointed by a symbolic link
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_readlink)(
    //! [in] Path to resolve
    F77_CHARACTER *fnamein,
    //! [out] Path pointed by the symbolic link
    F77_CHARACTER *fnameout,
    //! [in] Maximum length of fnamein
    F2Cl fnamein_length,
    //! [in] Maximum length of fnameout
    F2Cl fnameout_length
) {
    // Translate to C strings
    char fnamein_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(fnamein, fnamein_c, fnamein_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    char fnameout2_c[MAXPATHLEN] = "";
    ssize_t nc = readlink(fnamein_c, fnameout2_c, MAXPATHLEN-1) ;
    if (nc < 0) perror("clib_readlink");
    if (nc > 0) fnameout2_c[nc] = '\0';
    // Translate Back to Fortran strings
    F77_INTEGER status = CLIB_ERROR;
    if ( (nc > 0) &&
            (FTN2C_CSTR2FSTR(fnameout2_c, fnameout, nc, fnameout_length) >= 0)  ) {
        status = CLIB_OK;
    } else {
        char *defStr = " ";
        FTN2C_CSTR2FSTR(defStr, fnameout, 2, fnameout_length);
    }
    return status;
}


//! Delete a file of diretory.  If the path designates a directory, it must be empty.
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_remove)(
    //! [in] Path to delete
    F77_CHARACTER *path,
    //! [in] Maximum length of path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    if (remove(path_c)) {
        return CLIB_ERROR;
    } else {
        return CLIB_OK;
    }
}


//! Rename file or folder
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_rename)(
    F77_CHARACTER *pathold,
    F77_CHARACTER *pathnew,
    F2Cl pathold_length,
    F2Cl pathnew_length
) {
    // Translate to C strings
    char pathold_c[MAXPATHLEN];
    char pathnew_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(pathold, pathold_c, pathold_length, MAXPATHLEN) < 0 ||
        FTN2C_FSTR2CSTR(pathnew, pathnew_c, pathnew_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    if (rename(pathold_c, pathnew_c)) {
        return CLIB_ERROR;
    } else {
        return CLIB_OK;
    }
}


//! Check if path exists
//! \return CLIB_OK if an entry corresponds to the provided path, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_fileexist)(
    //! [in] Path of the to check
    F77_CHARACTER *path,
    //! [in] Maximum length of path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    if (access(path_c, F_OK)) {
        return CLIB_ERROR;
    } else {
        return CLIB_OK;
    }
}

//! Check if the path is readable
//! \return CLIB_OK if the provided path is readable, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_isreadok)(
    //! [in] Path of the to check
    F77_CHARACTER *path,
    //! [in] Maximum length of path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    if (access(path_c, R_OK)) {
        return CLIB_ERROR;
    } else {
        return CLIB_OK;
    }
}


//! Check if the path is writable
//! \return CLIB_OK if the provided path is writable, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_iswriteok)(
    //! [in] Path of the to check
    F77_CHARACTER *path,
    //! [in] Maximum length of path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    if (access(path_c, W_OK)) {
        return CLIB_ERROR;
    } else {
        return CLIB_OK;
    }
}

//! Check if the path is executable (searchable for directories)
//! \return CLIB_OK if the provided path is executable, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_isexecok)(
    //! [in] Path of the to check
    F77_CHARACTER *path,
    //! [in] Maximum length of path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    /* Call C function */
    if (access(path_c, X_OK)) {
        return CLIB_ERROR;
    } else {
        return CLIB_OK;
    }
}

//! Change working directory
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_chdir)(
    //! [in] Path to change to
    F77_CHARACTER *path,
    //! [in] Maximum length of path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    /* Call C function */
    if (chdir(path_c)) {
        return CLIB_ERROR;
    } else {
        return CLIB_OK;
    }
}

//! Get the current directory path
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_getcwd)(
    //! [out] Path of the current directory
    F77_CHARACTER *path,
    //! [in] Maximum length of path
    F2Cl path_length
) {
    F77_INTEGER status = CLIB_ERROR;

    char path_c[MAXPATHLEN];
    if (getcwd(path_c, (size_t)MAXPATHLEN*sizeof(char)) &&
        FTN2C_CSTR2FSTR(path_c, path, MAXPATHLEN, path_length) >=0) {
        status = CLIB_OK;
    } else {
        char *defStr = " ";
        FTN2C_CSTR2FSTR(defStr, path, 2, path_length);
    }
    return status;
}


//! Get user id
//! \return CLIB_OK
F77_INTEGER CLIB_F77NAME(clib_getuid)(
    //! [out] User id
    F77_INTEGER *uid
) {
    *uid = getuid();
    return CLIB_OK;
}


//! Remove empty directory
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_rmdir)(
    //! [in] Path of the directory to remove
    F77_CHARACTER *path,
    //! [in] Maximum length of path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    if (rmdir(path_c)) {
        return CLIB_ERROR;
    } else {
        return CLIB_OK;
    }
}


//! Create a symbolic link
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_symlink)(
    //! [in] Existing path to link to
    F77_CHARACTER *pathold,
    //! [in] Path of the symbolic link to be created
    F77_CHARACTER *pathnew,
    //! [in] Maximum length of pathold
    F2Cl pathold_length,
    //! [in] Maximum length of pathnew
    F2Cl pathnew_length
) {
    // Translate to C strings
    char pathold_c[MAXPATHLEN];
    char pathnew_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(pathold, pathold_c, pathold_length, MAXPATHLEN) < 0 ||
        FTN2C_FSTR2CSTR(pathnew, pathnew_c, pathnew_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    if (symlink(pathold_c, pathnew_c)) {
        return CLIB_ERROR;
    } else {
        return CLIB_OK;
    }
}


//! Remove a file
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_unlink)(
    //! [in] Path of the file to remove
    F77_CHARACTER *path,
    //! [in] Maximum length of the path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    if (unlink(path_c)) {
        return CLIB_ERROR;
    } else {
        return CLIB_OK;
    }
}


//! Create a directory
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_mkdir)(
    //! [in] Path of the directory to create
    F77_CHARACTER *path,
    //! [in] Maximum length of the path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    if (mkdir(path_c, (mode_t)0755)) {
        return CLIB_ERROR;
    } else {
        return CLIB_OK;
    }
}


//! Check if the path is a directory
//! \return CLIB_OK if the path is a directory, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_isdir)(
    //! [in] Path to check
    F77_CHARACTER *path,
    //! [in] Maximum length of the path
    F2Cl path_length
) {
    F77_INTEGER status;

    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    struct stat buf;
    if (!stat(path_c, &buf) && S_ISDIR(buf.st_mode)) {
        return CLIB_OK;
    } else {
        return CLIB_ERROR;
    }
}

//! Check if the path is a link
//! \return CLIB_OK if the path is a link, CLIB_ERROR otherwise.
F77_INTEGER CLIB_F77NAME(clib_islink)(
    //! [in] Path to check
    F77_CHARACTER *path,
    //! [in] Maximum length of the path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    struct stat buf;
    if (!lstat(path_c, &buf) && S_ISLNK(buf.st_mode)) {
        return CLIB_OK;
    } else  {
        return CLIB_ERROR;
    }
}

//! Check if the path is a fifo
//! \return CLIB_OK if the path is a fifo, CLIB_ERROR otherwise.
F77_INTEGER CLIB_F77NAME(clib_isfifo)(
    //! [in] Path to check
    F77_CHARACTER *path,
    //! [in] Maximum length of the path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    struct stat buf;
    if (!stat(path_c, &buf) && S_ISFIFO(buf.st_mode)) {
        return CLIB_OK;
    } else {
        return CLIB_ERROR;
    }
}


//! Check if the path is a regular file
//! \return CLIB_OK if the path is a regular file, CLIB_ERROR otherwise.
F77_INTEGER CLIB_F77NAME(clib_isfile)(
    //! [in] Path to check
    F77_CHARACTER *path,
    //! [in] Maximum length of the path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    struct stat buf;
    if (!stat(path_c, &buf) && S_ISREG(buf.st_mode)) {
        return CLIB_OK;
    } else {
        return CLIB_ERROR;
    }
}


//! Get file size in bytes
//! \return File size in bytes on succes, CLIB_ERROR otherwise
F77_INTEGER8 CLIB_F77NAME(clib_size)(
    //! [in] Path of the file for which to get the size
    F77_CHARACTER *path,
    //! [in] Maximum length of the path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    struct stat buf;
    if (!stat(path_c, &buf)) {
        return buf.st_size;
    } else {
        return CLIB_ERROR;
    }
}


//! Get the latest modification date/time
//! \return Latest modification date/time on success, CLIB_ERROR otherwise.
F77_INTEGER CLIB_F77NAME(clib_mtime)(
    //! [in] Path for which to get the latest modification date/time
    F77_CHARACTER *path,
    //! [in] Maximum length of the path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    struct stat buf;
    if (!stat(path_c, &buf)) {
        return buf.st_mtime;
    } else {
        return CLIB_ERROR;
    }
}


//! Get file stats as an array of 64 bit numbers
//! \return CLIB_OK on succes, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_stat)(
    //! [in] Path for which to get the stats
    F77_CHARACTER *path,
    //! [out] Array of 64 bits where to store the stats (13 entries)
    int64_t *table,
    //! [in] Maximum length of path
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    struct stat buf;
    if ( !stat(path_c, &buf) ) {
        table[0] = buf.st_dev;
        table[1] = buf.st_ino;
        table[2] = buf.st_mode;
        table[3] = buf.st_nlink;
        table[4] = buf.st_uid;
        table[5] = buf.st_gid;
        table[6] = buf.st_rdev;
        table[7] = buf.st_size;
        table[8] = buf.st_blksize;
        table[9] = buf.st_blocks;
        table[10] = buf.st_atime;
        table[11] = buf.st_mtime;
        table[12] = buf.st_ctime;
        return CLIB_OK;
    } else {
        return CLIB_ERROR;
    }
}


//! Get the basename
//! \return CLIB_OK on succes, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_basename)(
    //! [in] Path for which to retrieve the basename
    F77_CHARACTER *path,
    //! [out] Basename
    F77_CHARACTER *mybasename,
    //! [in] Maximum length of path
    F2Cl path_length,
    //! [in] Maximum length of mybasename
    F2Cl mybasename_length
) {
    char *defStr = " ";
    // fill destination with blanks, in case ...
    FTN2C_CSTR2FSTR(defStr, mybasename, 1, mybasename_length);
    mybasename[0] = '/';
    int lpath = path_length - 1;
    if ( lpath == 1 && path[0] == '/' ) {
        return CLIB_OK;
    }
    // scan backwards to find last '/' in path
    while (lpath > 0 && path[lpath] != '/') {
        lpath--;
    }
    F77_INTEGER status = FTN2C_CSTR2FSTR(path + lpath + 1, mybasename, path_length - lpath - 1, mybasename_length);
    return status < 0 ? CLIB_ERROR : CLIB_OK;
}


//! Get the dirname from a path
//! \return CLIB_OK on succes, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_dirname)(
    //! [in] Path from which to get the direname
    F77_CHARACTER *path,
    //! [out] Dirname
    F77_CHARACTER *mydirname,
    //! [in] Maximum length of path
    F2Cl path_length,
    //! [in] Maximum length of mydirename
    F2Cl mydirname_length
) {
    char *defStr = " ";
    // fill destination with blanks, in case ...
    FTN2C_CSTR2FSTR(defStr, mydirname, 1, mydirname_length);
    int lpath = path_length - 1;
    // scan backwards to find last '/' in path
    while (lpath > 0 && path[lpath] != '/') {
        lpath--;
    }

    if (lpath == 0) {
        if (path[0] != '/') {
            // no '/' found, return . as dirname
            mydirname[0] = '.'; 
            return(CLIB_OK) ;
        } else {
            // Path starts with / and has no other /
            return CLIB_ERROR;
        }
    }
    F77_INTEGER status = FTN2C_CSTR2FSTR(path, mydirname, lpath, mydirname_length);
    return  status < 0 ? CLIB_ERROR : CLIB_OK ;
}


#if defined(NO_CLIB_INTERFACE2)
//! Get list of files mathcing a gloc pattern in the current directory
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_glob)(
    F77_CHARACTER *filelist,
    F77_INTEGER *nfiles,
    F77_CHARACTER *pattern,
    F77_INTEGER *maxnfiles,
    F2Cl filelist_length,
    F2Cl pattern_length
) {
    // Translate to C strings
    char pattern_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(pattern, pattern_c, pattern_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    *nfiles = 0;
    F77_INTEGER status = CLIB_ERROR;
    glob_t globbuf;
    if (!glob(pattern_c, GLOB_NOSORT, NULL, &globbuf)) {
        if ((F77_INTEGER)globbuf.gl_pathc <= *maxnfiles) {
            *nfiles = (F77_INTEGER)globbuf.gl_pathc;
            if (FTN2C_CSTR2FSTR_A(globbuf.gl_pathv, filelist, MAXPATHLEN, (int)filelist_length, (int)*nfiles) >= 0) {
                status = CLIB_OK;
            }
        }
    }
    globfree(&globbuf);
    return status;
}
#endif


//! Convert string to lower case in place
//! \return CLIB_OK
F77_INTEGER CLIB_F77NAME(clib_tolower)(
    //! [in,out] String to convert
    F77_CHARACTER *str,
    //! [in] Maximum length of str
    F2Cl str_length
) {
    for (int i = 0; i < str_length; i++) {
        str[i] = (F77_CHARACTER)tolower((int)str[i]);
    }
    return CLIB_OK;
}


//! Convert string to upper case in place
//! \return CLIB_OK
F77_INTEGER CLIB_F77NAME(clib_toupper)(
    //! [in,out] String to convert
    F77_CHARACTER *str,
    //! [in] Maximum length of str
    F2Cl str_length
) {
    for (int i = 0; i < str_length; i++) {
        str[i] = (F77_CHARACTER)toupper((int)str[i]);
    }
    return CLIB_OK;
}


//! Check if the string is a number
//! \return CLIB_OK if it is, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_isalnum)(
    //! [in,out] String to evaluate
    F77_CHARACTER *str,
    //! [in] Maximum length of str
    F2Cl str_length
) {
    return isalnum((int)str[0]) ? CLIB_OK : CLIB_ERROR;
}


//! Check if the string is a alpha numeric
//! \return CLIB_OK if it is, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_isalpha)(
    //! [in,out] String to evaluate
    F77_CHARACTER *str,
    //! [in] Maximum length of str
    F2Cl str_length
) {
    return isalpha((int)str[0]) ? CLIB_OK : CLIB_ERROR;
}


//! Check if the string is just blanks
//! \return CLIB_OK if it is, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_isblank)(
    //! [in,out] String to evaluate
    F77_CHARACTER *str,
    //! [in] Maximum length of str
    F2Cl str_length
) {
    return isblank((int)str[0]) ? CLIB_OK : CLIB_ERROR;
}


//! Check if the string is a digit
//! \return CLIB_OK if it is, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_isdigit)(
    //! [in,out] String to evaluate
    F77_CHARACTER *str,
    //! [in] Maximum length of str
    F2Cl str_length
) {
    return isdigit((int)str[0]) ? CLIB_OK : CLIB_ERROR;
}


//! Check if the string is lower case
//! \return CLIB_OK if it is, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_islower)(
    //! [in,out] String to evaluate
    F77_CHARACTER *str,
    //! [in] Maximum length of str
    F2Cl str_length
) {
    return islower((int)str[0]) ? CLIB_OK : CLIB_ERROR;
}


//! Check for any printable character which is not a space or an alphanumeric character
//! \return CLIB_OK if it is, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_ispunct)(
    //! [in,out] String to evaluate
    F77_CHARACTER *str,
    //! [in] Maximum length of str
    F2Cl str_length
) {
    return ispunct((int)str[0]) ? CLIB_OK : CLIB_ERROR;
}


//! Check for if the string only contains white-space characters
//! \return CLIB_OK if it is, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_isspace)(
    //! [in,out] String to evaluate
    F77_CHARACTER *str,
    //! [in] Maximum length of str
    F2Cl str_length
) {
    return isspace((int)str[0]) ? CLIB_OK : CLIB_ERROR;
}


//! Check if the string is upper case
//! \return CLIB_OK if it is, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_isupper)(
    //! [in,out] String to evaluate
    F77_CHARACTER *str,
    //! [in] Maximum length of str
    F2Cl str_length
) {
    return isupper((int)str[0]) ? CLIB_OK : CLIB_ERROR;
}


//! Check if the string contains only hexadecimal characters
//! \return CLIB_OK if it is, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_isxdigit)(
    //! [in,out] String to evaluate
    F77_CHARACTER *str,
    //! [in] Maximum length of str
    F2Cl str_length
) {
    return isxdigit((int)str[0]) ? CLIB_OK : CLIB_ERROR;
}


//! Create directory hierarchy (like mkdir -p)
// Source: http://niallohiggins.com/2009/01/08/mkpath-mkdir-p-alike-in-c-for-unix/
int mkpath(char *s, mode_t mode){
    char *q, *r = NULL, *path = NULL, *up = NULL;
    int rv;

    rv = -1;
    if (strcmp(s, ".") == 0 || strcmp(s, "/") == 0)
            return 0;

    if ((path = strdup(s)) == NULL)
            exit(1);

    if ((q = strdup(s)) == NULL)
            exit(1);

    if ((r = dirname(q)) == NULL)
            goto out;

    if ((up = strdup(r)) == NULL)
            exit(1);

    if ((mkpath(up, mode) == -1) && (errno != EEXIST))
            goto out;

    if ((mkdir(path, mode) == -1) && (errno != EEXIST))
            rv = -1;
    else
            rv = 0;

out:
    if (up != NULL)
            free(up);
    free(q);
    free(path);

    return rv;
}


//! Create directory hierarchy
//! \return CLIB_OK on success, CLIB_ERROR otherwise
F77_INTEGER CLIB_F77NAME(clib_mkdir_r)(
    F77_CHARACTER *path,
    F2Cl path_length
) {
    // Translate to C strings
    char path_c[MAXPATHLEN];
    if (FTN2C_FSTR2CSTR(path, path_c, path_length, MAXPATHLEN) < 0){
        return CLIB_ERROR;
    }

    /* Call C function */
    if (mkpath((char *) &path_c, (mode_t)0755)) {
        return CLIB_ERROR;
    } else {
        return CLIB_OK;
    }
}

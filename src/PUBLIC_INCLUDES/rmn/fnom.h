#ifndef FNOM_H
#define FNOM_H

#include <stdint.h>

#include "rpnmacros.h"

#define MAXFILES 1024

//! \todo Rename this type to something more specific.  Is it used by client apps?
typedef struct {
    unsigned int
        stream:1,
        std:1,
        burp:1,
        rnd:1,
        wa:1,
        ftn:1,
        unf:1,
        read_only:1,
        old:1,
        scratch:1,
        notpaged:1,
        pipe:1,
        write_mode:1,
        remote:1,
        padding:18;
} attributs;


//! \todo Rename this type to something more specific.  Is it used by client apps?
typedef struct {
    //! Complete file name
    char *file_name;
    //! Sub file name for cmcarc files
    char *subname;
    //! File type and options
    char * file_type;
    //! fnom unit number
    int32_t iun;
    //! C file descriptor
    int32_t fd;
    //! File size in words
    int32_t file_size;
    //! effective file size in words
    int32_t eff_file_size;
    //! Record length when appliable
    int32_t lrec;
    //! Open/close flag
    int32_t open_flag;
    attributs attr;
} general_file_info;

#if defined(FNOM_OWNER)
//! Fnom General File Desc Table
general_file_info FGFDT[MAXFILES];
#else
extern general_file_info FGFDT[MAXFILES];
#endif


int c_fretour(
    const int iun
);
int32_t f77name(fretour)(
    const int32_t * const fiun
);

void f77name(d_fgfdt)();

int c_fnom(
    int * const iun,
    const char * const nom,
    const char * const type,
    const int lrec
);
int32_t f77name(fnom)(
    int32_t * const iun,
    const char * const nom,
    const char * const type,
    const int32_t * const flrec,
    F2Cl l1,
    F2Cl l2
);

int c_fclos(const int iun);
int32_t f77name(fclos)(const int32_t * const fiun);

int32_t f77name(qqqfnom)(
    const int32_t * const iun,
    char * const nom,
    char * const type,
    int32_t * const flrec,
    F2Cl l1,
    F2Cl l2
);

int c_waopen2(const int iun);
int32_t f77name(waopen2)(const int32_t * const iun);

void c_waopen(const int iun);
void f77name(waopen)(const int32_t * const iun);

int c_waclos2(const int iun);
int32_t f77name(waclos2)(const int32_t * const iun);

void c_waclos(const int iun);
void f77name(waclos)(const int32_t * const iun);

int c_wawrit2(
    const int iun,
    const void * const buf,
    const unsigned int offset,
    const int nwords
);
int32_t f77name(wawrit2)(
    const int32_t * const iun,
    const void * const buf,
    const uint32_t * const offset,
    const int32_t * const nwords
);

void c_wawrit(
    const int iun,
    const void * const buf,
    const unsigned int offset,
    const int nwords
);
void f77name(wawrit)(
    const int32_t * const iun,
    const void * const buf,
    const uint32_t * const offset,
    const int32_t * const nwords
);

int c_waread2(
    const int iun,
    void *buf,
    const unsigned int offset,
    const int nwords
);
int32_t f77name(waread2)(
    const int32_t * const iun,
    void * const buf,
    const uint32_t * const offset,
    const int32_t * const nwords
);

void c_waread(
    const int iun,
    void * const buf,
    const unsigned int offset,
    const int nwords
);
void f77name(waread)(
    const int32_t * const iun,
    void * const buf,
    const uint32_t * const offset,
    const int32_t * const nwords
);

int32_t c_wasize(const int iun);
int32_t f77name(wasize)(const int32_t * const iun);

int32_t c_numblks(const int iun);
int32_t f77name(numblks)(const int32_t * const iun);

int32_t f77name(existe)(const char * const nom, F2Cl llng);

void c_openda(const int iun);
void f77name(openda)(const int32_t * const iun);

void c_closda(const int iun);
void f77name(closda)(const int32_t * const iun);

void c_checda(const int iun);
void f77name(checda)(const int32_t * const iun);

void c_readda(
    const int iun,
    int * const buf,
    const int nwords,
    const int offset
);
void f77name(readda)(
    const int32_t * const iun,
    int32_t * const buf,
    const int32_t * const nwords,
    const int32_t * const offset
);

void c_writda(
    const int iun,
    const int * const buf,
    const int nwords,
    const int offset
);
void f77name(writda)(
    const int32_t * const iun,
    const int32_t * const buf,
    const int32_t * const nwords,
    const int32_t * const offset
);

int c_getfdsc(const int iun);
int32_t f77name(getfdsc)(const int32_t * const iun);

void c_sqopen(const int iun);
void f77name(sqopen)(const int32_t * const iun);

void c_sqclos(const int iun);
void f77name(sqclos)(const int32_t * const iun);

void c_sqrew(const int iun);
void f77name(sqrew)(const int32_t * const iun);

void c_sqeoi(const int iun);
void f77name(sqeoi)(const int32_t * const iun);

int c_sqgetw(
    const int iun,
    uint32_t * const buf,
    const int nwords
);
int32_t f77name(sqgetw)(
    const int32_t * const iun,
    uint32_t * const buf,
    const int32_t * const nwords
);

int c_sqputw(
    const int iun,
    const uint32_t * const buf,
    const int nwords
);
int32_t f77name(sqputw)(
    const int32_t * const iun,
    const uint32_t * const buf,
    const int32_t * const nwords
);

int c_sqgets(
    const int iun,
    char * const buf,
    const int nchar
);
int32_t f77name(sqgets)(
    const int32_t * const iun,
    char * const buf,
    const int32_t * const nchar,
    F2Cl llbuf
);

int c_sqputs(
    const int iun,
    const char * const buf,
    const int nchar
);
int32_t f77name(sqputs)(
    const int32_t *iun,
    const char * const buf,
    const int32_t * const nchar,
    F2Cl llbuf
);

void f77name(d_wafdt)();

uint32_t f77name(hrjust) (uint32_t *str, const int32_t * const ncar);

uint32_t f77name(hljust) (uint32_t *str, const int32_t * const ncar);

#endif

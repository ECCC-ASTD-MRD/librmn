#include <stdint.h>

#define MAXFILES 1024
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

typedef struct {
    //! Complete file name
    char *file_name;
    //! Sub file name for cmcarc files
    char *subname;
    //! File type and options
    char * file_type;
    //! fnom unit number
    INT_32 iun;
    //! C file descriptor
    INT_32 fd;
    //! File size in words
    INT_32 file_size;
    //! effective file size in words
    INT_32 eff_file_size;
    //! Record length when appliable
    INT_32 lrec;
    //! Open/close flag
    INT_32 open_flag;
    attributs attr;
} general_file_info;
#if defined(FNOM_OWNER)
general_file_info Fnom_General_File_Desc_Table[MAXFILES];
#else
extern general_file_info Fnom_General_File_Desc_Table[MAXFILES];
#endif
#define FGFDT Fnom_General_File_Desc_Table

int c_fretour(int iun);
int32_t f77name(fretour)(int32_t *fiun);
void f77name(d_fgfdt)();
int c_fnom(int *iun, char *nom, char *type, int lrec);
int32_t f77name(fnom)(int32_t *iun, char *nom, char *type, int32_t *flrec, F2Cl l1, F2Cl l2);
int c_fclos(int iun);
int32_t f77name(fclos)(int32_t *fiun);
int32_t f77name(qqqfnom)(int32_t *iun, char *nom, char *type, int32_t *flrec, F2Cl l1, F2Cl l2);
void c_waopen(int iun);
int c_waopen2(int iun);
int32_t f77name(waopen2)(int32_t *fiun);
void f77name(waopen)(int32_t *fiun);
void c_waclos(int iun);
int c_waclos2(int iun);
int32_t f77name(waclos2)(int32_t *fiun);
void f77name(waclos)(int32_t *fiun);
void c_wawrit(int iun, void *buf, unsigned int adr, int nmots);
int c_wawrit2(int iun, void *buf, unsigned int adr, int nmots);
void f77name(wawrit)(int32_t *fiun, void *buf, uint32_t *fadr, int32_t *fnmots);
int32_t f77name(wawrit2)(int32_t *fiun, void *buf, uint32_t *fadr, int32_t *fnmots);
void c_waread(int iun, void *buf, unsigned int adr, int nmots);
int c_waread2(int iun, void *buf, unsigned int adr, int nmots);
void f77name(waread)(int32_t *fiun, void *buf, uint32_t *fadr, int32_t *fnmots);
int32_t f77name(waread2)(int32_t *fiun, void *buf, uint32_t *fadr, int32_t *fnmots);
INT_32 c_wasize(int iun);
int32_t f77name(wasize)(int32_t *fiun);
INT_32 c_numblks(int iun);
int32_t f77name(numblks)(int32_t *fiun);
int32_t f77name(existe)(char *nom, F2Cl lng);
void c_openda(int iun);
void f77name(openda)(int32_t *iun);
void c_closda(int iun);
void f77name(closda)(int32_t *iun);
void c_checda(int iun);
void f77name(checda)(int32_t *iun);
void c_readda(int iun, int *bufptr, int ns, int is);
void f77name(readda)(int32_t *iun, int32_t *bufptr, int32_t *ns, int32_t *is);
void c_writda(int iun, int *bufptr, int ns, int is);
void f77name(writda)(int32_t *iun, int32_t *bufptr, int32_t *ns, int32_t *is);
int c_getfdsc(int iun);
int32_t f77name(getfdsc)( int32_t *iun);
void c_sqopen(int iun);
void f77name(sqopen)(int32_t *iun);
void c_sqclos(int iun);
void f77name(sqclos)(int32_t *iun);
void c_sqrew(int iun);
void f77name(sqrew)(int32_t *iun);
void c_sqeoi(int iun);
void f77name(sqeoi)(int32_t *iun);
int c_sqgetw(int iun, uint32_t *bufptr, int nmots);
int32_t f77name(sqgetw)(int32_t *iun, int32_t *bufptr, int32_t *nmots);
int c_sqputw(int iun, uint32_t *bufptr, int nmots);
int32_t f77name(sqputw)(int32_t *iun, int32_t *bufptr, int32_t *nmots);
int c_sqgets(int iun, char *bufptr, int nchar);
int32_t f77name(sqgets)(int32_t *iun, char  *bufptr, int32_t *nchar, F2Cl lbuf);
int c_sqputs(int iun, char *bufptr, int nchar);
int32_t f77name(sqputs)(int32_t *iun, char  *bufptr, int32_t *nchar, F2Cl lbuf);
void f77name(d_wafdt)();
uint32_t f77name(hrjust) (uint32_t *moth, int32_t *ncar);
uint32_t f77name(hljust) (uint32_t *moth, int32_t *ncar);
unsigned INT_32 f77name(check_host_id)();

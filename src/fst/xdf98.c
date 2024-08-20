/*! @file xdf98.c
    @brief EXtensible Directory File: a data bank system software

    @author M. Lepine
    @author M. Valin
    @author J. Caveen
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include <unistd.h>
#include <sys/types.h>

#include <pthread.h>

#define XDF_OWNER
#include "xdf98.h"

#include <App.h>
#include <armn_compress.h>
#include "base/base.h"
#include "burp98.h"
#include "fst98_internal.h"
#include "primitives/fnom_internal.h"
#include "qstdir.h"
#include "rmn/excdes_new.h"

static pthread_mutex_t xdf_mutex = PTHREAD_MUTEX_INITIALIZER;

static int endian_int = 1;
static char *little_endian = (char *)&endian_int;
static int req_no = 0;
static int init_package_done = 0;

// prototypes declarations
static int get_free_index();
static void init_file(file_table_entry* const entry, const int index);
static int32_t scan_random(int file_index);
static int32_t add_dir_page(int file_index, int wflag);
static int32_t rewind_file(int file_index, int handle);
static int create_new_xdf(int index, int iun, word_2 *pri, int npri,
                          word_2 *aux, int naux, char *appl);
static uint32_t next_match(int file_index);
static void build_gen_prim_keys(uint32_t *buf, uint32_t *keys, uint32_t *mask,
                                uint32_t *mskkeys, int index, int mode);
static void build_gen_info_keys(uint32_t * const buf, uint32_t * const keys, const int index, const int mode);

const int ABSOLUTE_MAX_XDF_FILES = 1024;
file_table_entry_ptr* file_table = NULL;
int MAX_XDF_FILES = 0;

static const intptr_t XDF_RESERVED = 1;
static const int XDF_IUN_AVAILABLE = -1;
static const int XDF_IUN_RESERVED = -2;

//! Check whether the given handle belongs to an open XDF file
int32_t c_xdf_handle_in_file(const int32_t handle) {
    const int32_t index = INDEX_FROM_HANDLE(handle);
    return ((file_table[index] != NULL) && (file_table[index]->iun >= 0));
}

//! Initialize the XDF API
//! Uses the xdf_mutex
//! \return 
int initialize_xdf() {
    
    if (MAX_XDF_FILES > 0) return MAX_XDF_FILES; // xdf already initialized
    if (MAX_FNOM_FILES <= 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Cannot initialize XDF if fnom has never been called/initialized\n", __func__);
        return MAX_FNOM_FILES;
    }

    // --- START critical region ---
    pthread_mutex_lock(&xdf_mutex);

    if (MAX_XDF_FILES > 0) goto unlock; // Check again after locking, in case someone else was initializing

    const int num_files = MAX_FNOM_FILES > ABSOLUTE_MAX_XDF_FILES ? ABSOLUTE_MAX_XDF_FILES : MAX_FNOM_FILES;
    file_table = (file_table_entry_ptr*) calloc(num_files, sizeof(file_table_entry_ptr));
    if (file_table == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to allocate space for file table (%d files)\n",
                __func__, num_files);
        goto unlock;
    }

    MAX_XDF_FILES = num_files; // Signal to other threads that the API is initialized

    // Init first entry to start with file index = 1. Is that really useful? We need to have the API initialized to call the get_free_index() function.
    int ind = get_free_index();
    file_table[ind]->iun = 123456789;

unlock:
    pthread_mutex_unlock(&xdf_mutex);
    // --- END critical region ---

    return MAX_XDF_FILES;
}

//! Check XDF file for corruption
//! @return              Valid(0) or not(-1)
int c_xdfcheck(
    //! [in] filePath Path to the file
    const char * const filePath
) {
    file_header header;
    uint32_t tmp;
    uint32_t *buf_ptr = (uint32_t *) &header;
    FILE *fd = fopen(filePath, "r");

    if (!fd) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: Cannot open file\n",__func__);
        return(ERR_NO_FILE);
    }

    // Read the header
    int num_records = fread(buf_ptr, sizeof(header), 1, fd);

    // Get file size
    fseek(fd, 0L, SEEK_END);
    long file_size = ftell(fd);
    fseek(fd, 0L, SEEK_SET);

    fclose(fd);

    // Flip bytes in each 32-bit word (16 of them)
    for(int i = 0 ; i < 16 ; i++) {
        tmp = buf_ptr[i] ;
        buf_ptr[i] = (tmp << 24) | (tmp >> 24) | ((tmp & 0xFF0000) >> 8) | ((tmp & 0xFF00) << 8) ;
    }

    if ((size_t)header.fsiz * 8 != file_size) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: File size does not match header. Expected size: %d bytes. Actual size: %d\n",__func__,header.fsiz*8,file_size);
        return(ERR_DAMAGED);
    }

    if (header.idtyp != 0) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: Wrong header ID type (%d), should be %d\n",__func__,header.idtyp,0);
        return(ERR_DAMAGED);
    }

    return 0;
}


//! Add a directory page to file file_index
//! \return 0 on success.  Can be ERR_DIR_FULL or ERR_MEM_FULL in case of error.
static int32_t add_dir_page(
    //! [in] File index in file table
    int file_index,
    //! [in] Write flag. In case of a new file, or a file extenxion the directory page has to be written on disk
    int wflag
) {
    register file_table_entry *fte;
    int page_size, i, wdlng;
    full_dir_page *page;

    // check if file exists
    if( (fte = file_table[file_index]) == NULL) {
        return (ERR_NO_FILE);
    }

    // check if we can add a directory page, and if memory is available
    if(fte->npages >= MAX_DIR_PAGES) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: Too many records, no more directory pages available\n",__func__);
        return(ERR_DIR_FULL);
    }
    // primary_len is given in unit of 64bits, hence the multiplication by 8
    page_size = sizeof(full_dir_page) + 8 * (fte->primary_len) * ENTRIES_PER_PAGE;

    // allocate directory page , link it into the directory page chain
    if ( (page = (page_ptr) malloc(page_size)) == NULL ) {
        return (ERR_MEM_FULL);
    }
    fte->dir_page[fte->npages] = page;
    (fte->dir_page[fte->npages])->next_page = NULL;
    if( fte->npages == 0) {
        // First page has no predecessor
        (fte->dir_page[fte->npages])->prev_page = NULL;
    } else {
        // Update succesor to preceding page
        (fte->dir_page[fte->npages])->prev_page = fte->dir_page[fte->npages-1];
        (fte->dir_page[fte->npages-1])->next_page = fte->dir_page[fte->npages];
    }

    // Initialize directory header and directory page entries
    page->modified = 0;
    page->true_file_index = file_index;
    page->dir.idtyp = 0;
    wdlng = ENTRIES_PER_PAGE *fte->primary_len + 4;
    page->dir.lng = wdlng;
    page->dir.addr = WDTO64(fte->nxtadr -1) +1;
    page->dir.reserved1 = 0;
    page->dir.reserved2 = 0;
    page->dir.nxt_addr = 0;
    page->dir.nent = 0;
    page->dir.chksum = 0;
    for (i = 0; i <= fte->primary_len*ENTRIES_PER_PAGE; i++) {
        page->dir.entry[i] = 0;
    }
    if (wflag) {
        // Write directory page to file
        if (fte->npages != 0) {
            // First page has no predecessor
            (fte->dir_page[fte->npages-1])->dir.nxt_addr = page->dir.addr;
        }
        c_wawrit(fte->iun, &page->dir, fte->nxtadr, W64TOWD(wdlng));
        fte->nxtadr += W64TOWD(wdlng);
        fte->header->fsiz = WDTO64(fte->nxtadr -1);
        fte->header->nbd++;
        fte->header->plst = page->dir.addr;
        // Checksum has to be computed
        page->modified = 1;
    }
    fte->npages++;
    return 0;
}


//! Calculates an address from an handle for a sequential file
//! \return Corresponding address
static int32_t address_from_handle(
    //! [in] (cluster:2, address:22, file_index:8)
    int handle,
    //! [in] Pointer to xdf file information structure
    file_table_entry *fte
) {
    int addr = (ADDRESS_FROM_HNDL(handle) << (2 * CLUSTER_FROM_HANDLE(handle)));
    if (fte->fstd_vintage_89) {
        addr = (addr * 15);
    }
    addr = W64TOwd(addr) + 1;
    return addr;
}


//! Pack fstd info keys into buffer or get info keys from buffer depending on mode argument
//! @deprecated This function does absolutely nothing
void build_fstd_info_keys(
    //! [inout] Buffer to contain the keys
    uint32_t *buf,
    //! [inout] Info keys
    uint32_t *keys,
    //! [in] File index in file table
    int index,
    //! [in] if mode = WMODE, write to buffer otherwise get keys from buffer.
    int mode
) {
}


//! Pack fstd primary keys into buffer or get primary keys from buffer depending on mode argument.
void build_fstd_prim_keys(
    //! [inout] Buffer to contain the keys
    uint32_t *buf,
    //! [inout] Primary keys
    uint32_t *keys,
    //! [out] Search mask
    uint32_t *mask,
    //! [in] Unpacked masks
    uint32_t *mskkeys,
    //! [in] File index in file table
    int index,
    //! [in] If mode = WMODE, write to buffer otherwise get keys from buffer
    int mode
) {
    file_header *fh;
    int i, wi, sc, rmask, key, wfirst, wlast;

    // Skip first 64 bit header
    buf += W64TOWD(1);

    // First 64 bits not part of the search mask
    mask[0] = 0;
    if (W64TOWD(1) > 1) mask[1] = 0;

    // Skip first 64 bit header
    mask += W64TOWD(1);

    fh = file_table[index]->header;

    if (mode == WMODE) {
        // Write keys to buffer
        for (i = 0; i < W64TOWD(fh->lprm -1); i++) {
            buf[i] = keys[i];
            mask[i] = mskkeys[i];
        }
    } else {
        for (i = 0; i < W64TOWD(fh->lprm -1); i++) {
            keys[i] = buf[i];
        }
    }
}


//! Pack generic info keys into buffer or get info keys from buffer depending on mode argument.
static void build_gen_info_keys(
    //! [inout] Buffer to contain the keys
    uint32_t *buf,
    //! [inout] Info keys
    uint32_t *keys,
    //! [in] File index in file table
    int index,
    //! [in] Ff mode = WMODE, write to buffer otherwise get keys from buffer
    int mode
) {
    //! \todo Shouldn't this comme from somewhere else?
    const int bitmot = 32;

    file_header * fh = file_table[index]->header;

    if (mode == WMODE) {
        // Write keys to buffer
        for (int i = 0; i < fh->naux; i++) {
            if (keys[i] != -1) {
                int wi = fh->keys[i+fh->nprm].bit1 / bitmot;
                int sc = (bitmot-1) - (fh->keys[i+fh->nprm].bit1 % bitmot);
                int rmask = -1 << (fh->keys[i+fh->nprm].lcle);
                rmask = ~(rmask << 1);
                // equivalent of << lcle+1 and covers 32 bit case
                int key = keys[i];
                if ((fh->keys[i+fh->nprm].tcle /32) > 0)
                key = key & (~((key & 0x40404040) >> 1));
                // Clear bits
                buf[wi] = buf[wi] & (~(rmask << sc));
                buf[wi] = buf[wi] | ((key & rmask) << sc);
            }
        }
    } else {
        for (int i = 0; i < fh->naux; i++) {
            int wi = fh->keys[i+fh->nprm].bit1 / bitmot;
            int sc = (bitmot-1) - (fh->keys[i+fh->nprm].bit1 % bitmot);
            int rmask = -1 << (fh->keys[i+fh->nprm].lcle);
            rmask = ~(rmask << 1);
            keys[i] = (buf[wi] >> sc) & rmask;
        }
    }
}


//! Pack generic primary keys into buffer or get primary keys from buffer depending on mode argument.
static void build_gen_prim_keys(
    //! [inout] Buffer to contain the keys
    uint32_t *buf,
    //! [inout] Primary keys
    uint32_t *keys,
    //! [out] Search mask
    uint32_t *mask,
    //! [in] Unpacked masks
    uint32_t *mskkeys,
    //! [in] File index in file table
    int index,
    //! [in] if this is set to WMODE, write to buffer otherwise get keys from buffer
    int mode
) {
    const int bitmot = 32;

    // Skip first 64 bit header
    buf += 2;

    // First 64 bits not part of the search mask
    mask[0] = 0;
    mask[1] = 0;

    // Skip first 64 bit header
    mask += 2;

    file_header * fh = file_table[index]->header;

    if (mode == WMODE) {
        // Write keys to buffer
        int wfirst = fh->keys[0].bit1 / bitmot;
        int wlast = fh->keys[fh->nprm-1].bit1 / bitmot;
        for (int i = wfirst; i <= wlast; i++) {
            mask[i] = 0;
        }

        for (int i = 0; i < fh->nprm; i++) {
            if (keys[i] != -1) {
                int wi = fh->keys[i].bit1 / bitmot;
                int sc = (bitmot-1) - (fh->keys[i].bit1 % bitmot);
                int rmask = -1 << (fh->keys[i].lcle);
                rmask = ~(rmask << 1);
                /* equivalent of << lcle+1 and covers 32 bit case */
                int key = keys[i];
                if ((fh->keys[i].tcle /32) > 0) {
                    key = key & (~((key & 0x40404040) >> 1));
                }
                // clear bits
                buf[wi] = buf[wi] & (~(rmask << sc));
                buf[wi] = buf[wi] | ((key & rmask) << sc);
                // clear bits
                mask[wi] = mask[wi] & (~(rmask << sc));
                mask[wi] = mask[wi] | (rmask << sc);
            }
        }
    } else {
        for (int i = 0; i < fh->nprm; i++) {
            int wi = fh->keys[i].bit1 / bitmot;
            int sc = (bitmot-1) - (fh->keys[i].bit1 % bitmot);
            int rmask = -1 << (fh->keys[i].lcle);
            rmask = ~(rmask << 1);
            keys[i] = (buf[wi] >> sc) & rmask;
        }
    }
}


//! Establishes diagnosis and statistics for a xdf file
int c_qdfdiag(
    //! [in] unit number associated to the file
    int iun
) {
#define swap_4(mot) { register uint32_t tmp =(uint32_t)mot; \
   mot = (tmp>>24) | (tmp<<24) | ((tmp>>8)&0xFF00) | ((tmp&0xFF00)<<8); }

    int index_fnom = get_fnom_index(iun);
    if (index_fnom < 0) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not connected with fnom\n",__func__);
        return(ERR_NO_FNOM);
    }

    int wasopen = 0;
    int index = file_index_xdf(iun);
    file_record header64;
    file_header *fh;
    if (index == ERR_NO_FILE) {
        // open file and read file header
        c_waopen(iun);
        c_waread(iun, &header64, 1, W64TOWD(2));
        if (header64.data[0] != 'XDF0' && header64.data[0] !='xdf0') {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not XDF type\n",__func__);
            return(ERR_NOT_XDF);
        }
        if ((fh = malloc(header64.lng * 8)) == NULL) {
            Lib_Log(APP_LIBFST,APP_FATAL,"%s: memory is full\n",__func__);
            return(ERR_MEM_FULL);
        }
        c_waread(iun, fh, 1, W64TOWD(header64.lng));
    } else {
        // file is open, file_table[index]->header already contains the required info
        wasopen = 1;
        fh = file_table[index]->header;
    }
    int nw = c_wasize(iun);
    char vers[5];
    if (*little_endian) {
        int  ct = fh->vrsn ;
        swap_4(ct);
        strncpy(vers, (char *)&(ct), 4);
    } else {
        strncpy(vers, (char *)&(fh->vrsn), 4);
    }
    char appl[5];
    if (*little_endian) {
        int  ct = fh->sign ;
        swap_4(ct);
        strncpy(appl, (char *)&(ct), 4);
    } else {
       strncpy(appl, (char *)&(fh->sign), 4);
    }

    vers[4] = '\0';
    appl[4] = '\0';
    int readpos = 1 + W64TOwd(header64.lng);
    int eofile = 0;
    int nrec_tot = 0;
    int nrec_act = 0;
    int nrec_eff = 0;
    int ndirect = 0;
    int leplusgros = 0;
    xdf_record_header header;
    while (! eofile) {
        c_waread(iun, &header, readpos, W64TOwd(1));
        int addr = W64TOwd(header.addr-1) + 1;

        if (addr == readpos) {
            if (header.lng < W64TOwd(1)) {
                Lib_Log(APP_LIBFST,APP_FATAL,"%s: Invalid record length=%d, addr=%d\n",__func__,header.lng,addr);
                return(ERR_BAD_LEN);
            }
            if (header.idtyp == 0) {
                ndirect++;
            } else {
                if (header.idtyp == 255) {
                    nrec_eff++;
                } else {
                    nrec_act++;
                }
                nrec_tot++;
                leplusgros = (header.lng > leplusgros) ? header.lng : leplusgros;
            }
            readpos += W64TOwd(header.lng);
            if (readpos > nw) {
                eofile = 1;
            }
        } else {
            eofile = 1;
        }
    }

    int thesame = ((fh->nxtn == nrec_tot) && (fh->nrec == nrec_act) &&
                  ((fh->nxtn - fh->nrec) == nrec_eff) &&
                  (fh->nbig == leplusgros) && (fh->nbd == ndirect));

    fprintf(stdout, "\nStatistics from file header for %s\n", FGFDT[index_fnom].file_name);
    fprintf(stdout, "\t file size (64 bit units)        %d\n", fh->fsiz);
    fprintf(stdout, "\t number of rewrites              %d\n", fh->nrwr);
    fprintf(stdout, "\t number of extensions            %d\n", fh->nxtn);
    fprintf(stdout, "\t number of directory pages       %d\n", fh->nbd);
    fprintf(stdout, "\t last directory page address     %d\n", fh->plst);
    fprintf(stdout, "\t size of biggest record          %d\n", fh->nbig);
    fprintf(stdout, "\t number erasures                 %d\n", fh->neff);
    fprintf(stdout, "\t number of valid records         %d\n", fh->nrec);
    fprintf(stdout, "\t XDF version                     %s\n", vers);
    fprintf(stdout, "\t application signature           %s\n", appl);

    if (! thesame) {
        fprintf(stdout, "\n **** This file has been damaged ****\n");
        fprintf(stdout, "\nStatistics from file scan\n");
        fprintf(stdout, "\t number of extensions            %d\n", nrec_tot);
        fprintf(stdout, "\t number of directory pages       %d\n", ndirect);
        fprintf(stdout, "\t size of biggest record          %d\n", leplusgros);
        fprintf(stdout, "\t number erasures                 %d\n", nrec_eff);
        fprintf(stdout, "\t number of valid records         %d\n", nrec_act);
    } else {
        fprintf(stdout, "\n **** This file is OK ****\n");
    }

    if (! wasopen) {
        c_waclos(iun);
    }
    free(fh);
    if (! thesame) {
        return ERR_DAMAGED;
    } else {
        return 0;
    }
}


//! Change the application signature of a file.
//
//! This is used for example in conjuncture with burp files when the table used is not the official one.
int c_qdfmsig(
    //! [in] unit number associated to the file
    const int iun,
    //! [in] new application signature
    char *newappl
) {
    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not connected with fnom\n",__func__);
        return(ERR_NO_FNOM);
    }

    int index = file_index_xdf(iun);
    if (index == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not open\n",__func__);
        return(ERR_NO_FILE);
    }

    file_header * fh = file_table[index]->header;
    fh->sign = newappl[0] << 24 | newappl[1] << 16 | newappl[2] << 8 | newappl[3];
    return 0;
}


//! Add to the buffer element elem of length nbits ending a bit dernit.
//! \return Always 0
int c_qdfput(
    //! [inout] Buffer to contain the element to add
    uint32_t *buf,
    //! [in] Element to add
    int elem,
    //! [in] Position of last bit in buf
    int derbit,
    //! [in] Length in bit of element to add
    int nbits
) {
    const int bitmot = 32;

    int wi = derbit / bitmot;
    int sc = (bitmot-1) - (derbit % bitmot);
    int msk = ~(-1 << nbits);
    buf[wi] = buf[wi] & (~(msk << sc));
    buf[wi] = buf[wi] | ((elem & msk) << sc);
    return 0;
}


//! Restores a file to its original length (the one before a task that has prematurely terminated while doing appends)
int c_qdfrstr(
    //! [in] Input unit number associated to the file
    int inp,
    //! [in] Output unit number associated to the file
    int outp
) {
#define Buflen 8192
    int buffer[Buflen];

    int index_fnom = get_fnom_index(inp);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file (unit=%d) is not connected with fnom\n",__func__,inp);
        return(ERR_NO_FNOM);
    }

    index_fnom = get_fnom_index(outp);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file (unit=%d) is not connected with fnom\n",__func__,outp);
        return(ERR_NO_FNOM);
    }

    // Open file and read file header
    c_waopen(inp);
    file_record header64;
    c_waread(inp, &header64, 1, W64TOWD(2));
    if (header64.data[0] != 'XDF0' && header64.data[0] !='xdf0') {
        Lib_Log(APP_LIBFST,APP_FATAL,"%s: file is not XDF type\n",__func__);
        return(ERR_NOT_XDF);
    }
    int lng = W64TOWD(header64.lng);
    int nw = c_wasize(inp);

    if (lng > nw) {
        Lib_Log(APP_LIBFST,APP_FATAL,"%s: Invalid header file length=%d\n",__func__,header64.lng);
        return(ERR_BAD_LEN);
    }
    file_header fh;
    c_waread(inp, &fh, 1, W64TOWD(header64.lng));
    c_waopen(outp);

    // Reset read/write flag to zero
    fh.rwflg = 0;
    c_wawrit(outp, &fh, 1, W64TOwd(header64.lng));
    int rwpos = 1 + W64TOwd(header64.lng);

    lng = W64TOwd(fh.fsiz - header64.lng);
    int alire = (lng < Buflen) ? lng : Buflen;
    while (alire > 0) {
        for (int i = 0; i < Buflen; i++) {
            buffer[i] = 0;
        }
        c_waread(inp, &buffer, rwpos, alire);
        c_wawrit(outp, &buffer, rwpos, alire);
        lng -= alire;
        rwpos += alire;
        alire = (lng < Buflen) ? lng : Buflen;
    }

    c_waclos(inp);
    c_waclos(outp);

    return 0;
}

void* xdf_set_file_filter(const int iun, void* const new_filter) {
    const int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not connected with fnom\n",__func__);
        return NULL;
    }

    const int index = file_index_xdf(iun);
    if (index == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not open\n",__func__);
        return NULL;
    }

    file_table_entry * const f = file_table[index];

    void* old_file_filter = f->file_filter;
    f->file_filter = new_filter;
    return old_file_filter;
}

//! Add to the end of the record contained in buf, nelm*nbits bits from donnees.
int c_xdfadd(
    //! [inout] Buffer to contain the record
    uint32_t *buffer,
    //! [in] Data bits to add to buffer
    uint32_t *donnees,
    //! [in] Number of elements
    int nelm,
    //! [in] Number of bits per element, must be a multiple of 64
    int nbits,
    //! [in] Data type of elements to add
    int datyp
) {
    if (((datyp == 3) || (datyp == 5)) && (nbits != 8)) {
        Lib_Log(APP_LIBFST,APP_FATAL,"%s: nbits must be 8 for datyp %d\n",__func__,datyp);
        return(ERR_BAD_DATYP);
    }

    int nbwords = (nelm * nbits + 63) / 64;
    nbwords = W64TOWD(nbwords);

    buffer_interface_ptr buf = (buffer_interface_ptr) buffer;
    int index_word = buf->nbits / (sizeof(uint32_t) * 8);

    if ((index_word + nbwords - 1) > buf->nwords) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: buffer not big enough for insertion\n",__func__);
        return(ERR_BAD_DIM);
    }

    switch (datyp) {
        case 0:
            // transparent mode
        case 3:
        case 6:
        case 8:
            for (int i = 0; i < nbwords; i++) {
                buf->data[index_word+i] = donnees[i];
            }
            break;

        case 7:
        case 9:
            if (*little_endian) {
                for (int i = 0; i < nbwords; i+=2) {
                    buf->data[index_word+i] = donnees[i+1];
                    buf->data[index_word+i+1] = donnees[i];
                }
            } else {
                for (int i = 0; i < nbwords; i++) {
                    buf->data[index_word+i] = donnees[i];
                }
            }
            break;

        case 5:
            // Upper char only
            for (int i = 0; i < nbwords; i++) {
                buf->data[index_word+i] = upper_case_word(donnees[i]);
            }
            break;

        case 2:
            compact_integer(donnees, (void *) NULL, &(buf->data[index_word]), nelm, nbits, 0, xdf_stride, 1);
            break;

        case 4:
            compact_integer(donnees, (void *) NULL, &(buf->data[index_word]), nelm, nbits, 0, xdf_stride, 3);
            break;

        default:
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid datyp=%d\n",__func__,datyp);
            return(ERR_BAD_DATYP);

    } /* end switch */

    buf->nbits += nbwords * sizeof(uint32_t) * 8;
    return 0;
}


//! Truncate a file
int c_secateur(
    //! Path of the file
    char *filePath,
    //! File size to set (in bytes)
    int size
) {
    Lib_Log(APP_LIBFST,APP_TRIVIAL,"%s: Truncating %s to \t %d Bytes\n",__func__,filePath,size);

    int ier = truncate(filePath, size);
    if (ier == -1) perror("secateur");
    return ier;
}


//! Truncate a file
int32_t f77name(secateur)(
    //! Path of the file to truncate
    char *filePath,
    //! File size to set (in bytes)
    int32_t *f_size,
    F2Cl l1
) {
    int ier = c_secateur(filePath, *f_size);
    return (int32_t) ier;
}


//! Pack key descriptors into 2 different 32 bit wide elements.
int c_xdfcle(
    //! [in] Name of the key (max 4 char)
    const char *keyname,
    //! [in] Last right bit of key in record
    const int bit1,
    //! [in] Key length in bits
    const int lkey,
    //! [in] Type of key (0 : unsigned int, 1..4 : ascii char)
    const int tkey,
    //! [out] First element to contain the keyname
    int *desc1,
    //! [out] Second element to contain the bit position length and type
    int *desc2
) {
    int i, bitpos, rmask, shift_count;
    int bitmot = 32;

    *desc1 = 0; *desc2 = 0;

    for(i=0; (i < 4 && keyname[i]); i++) {
        *desc1 = (*desc1 <<8) | (keyname[i] & 0xff);
    }

    while (i++ < 4) {
        *desc1 = (*desc1 <<8) | ' ';
    }

    bitpos = BPBIT1 - (64-bitmot);
    shift_count = (bitmot-1) - bitpos & (bitmot-1);
    rmask = -1 >> (bitmot -LBIT1);
    *desc2 = *desc2 | ((bit1 & rmask) << shift_count);

    bitpos = BPLCLE - (64-bitmot);
    shift_count = (bitmot-1) - bitpos & (bitmot-1);
    rmask = -1 >> (bitmot -LLCLE);
    *desc2 = *desc2 | (((lkey -1) & rmask) << shift_count);

    bitpos = BPTCLE - (64-bitmot);
    shift_count = (bitmot-1) - bitpos & (bitmot-1);
    rmask = -1 >> (bitmot -LTCLE);
    *desc2 = *desc2 | ((tkey & rmask) << shift_count);

    return 0;
}


//! Closes the XDF file. Rewrites file header, computes directory checksum and rewrites directory pages if modified.
int c_xdfcls(
    //! [in] Unit number associated to the file
    int iun
) {
    int index_fnom = get_fnom_index(iun);
    if (index_fnom < 0) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not connected with fnom\n",__func__);
        return(ERR_NO_FNOM);
    }

    int index = file_index_xdf(iun);
    if (index == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not open\n",__func__);
        return(ERR_NO_FILE);
    }

    file_table_entry * fte = file_table[index];

    if ((fte->header->rwflg != RDMODE) && (!FGFDT[index_fnom].attr.read_only)) {
        // Rewrite file header
        c_wawrit(iun, fte->header, 1, W64TOWD(fte->header->lng));
    }

    if (fte->modified) {
        // File has been modified rewrite dir. pages
        for (int i = 0; i < fte->header->nbd; i++) {
            if (fte->dir_page[i]->modified) {
                // Reset idtyp entries to original value if modified by scan_dir_page
                int width = W64TOWD(fte->primary_len);
                uint32_t *entry = (fte->dir_page[i])->dir.entry;
                for (int j = 0; j < (fte->dir_page[i])->dir.nent; j++) {
                    xdf_record_header * rec = (xdf_record_header *) entry;
                    if ((rec->idtyp | 0x80) == 254) {
                        rec->idtyp = 255;
                        c_wawrit(iun, rec, W64TOWD(rec->addr - 1) + 1, W64TOWD(1));
                    }
                    rec->idtyp = ((rec->idtyp | 0x80) == 255) ? 255 : (rec->idtyp & 0x7f);
                    entry += width;
                }
                // Compute checksum and rewrite page
                int lng64 = fte->primary_len * ENTRIES_PER_PAGE + 4;
                xdf_dir_page * curpage = &((fte->dir_page[i])->dir);
                uint32_t checksum = curpage->chksum;
                uint32_t *check32 = (uint32_t *) curpage;
                for (int j = 4; j < W64TOWD(lng64); j++) {
                    checksum ^= check32[j];
                }
                curpage->chksum = checksum;
                c_wawrit(iun, curpage, W64TOWD(curpage->addr - 1) + 1, W64TOWD(lng64));
                fte->dir_page[i]->modified = 0;
            } // end if page modified
         } // end for i
        if (fte->xdf_seq) {
            int trunc_to;
            trunc_to = FGFDT[index_fnom].file_size * sizeof(uint32_t);
            c_secateur(FGFDT[index_fnom].file_name, trunc_to);
        }
        fte->modified = 0;
    } // end if file modified

    if (!xdf_checkpoint) {
        if ((fte->header->rwflg != RDMODE) && (!FGFDT[index_fnom].attr.read_only)) {
            // Rewrite file header
            fte->header->rwflg = 0;
            c_wawrit(iun, fte->header, 1, W64TOWD(fte->header->lng));
        }

        c_waclos(iun);

        // Free allocated pages
        for (int i = 0; i < fte->npages; i++) {
            free(fte->dir_page[i]);
        }

        // Reset file informations
        init_file(fte, index);
    } else {
        xdf_checkpoint = 0;
    }

    return 0;
}

//! Retrieve nelm elements from buf starting at bit position bitpos.
int c_xdfcut(
    //! [inout] Buffer to contain the modified record
    void *buffer,
    //! [in] Starting bit position for cutting
    int bitpos,
    //! [in] Number of elements to cut from buf
    int nelm,
    //! [in] Number of bits kept per element
    int nbits,
    //! [in] Data type
    int datyp
) {
   int nbwords, index_word, last_ind, i;
   buffer_interface_ptr buf = (buffer_interface_ptr) buffer;

   if ((bitpos % 64) != 0) {
      Lib_Log(APP_LIBFST,APP_FATAL,"%s: bitpos must be a multiple of 64\n",__func__);
      return(ERR_BAD_ADDR);
   }

   if (((datyp == 3) || (datyp == 5)) && (nbits != 8)) {
      Lib_Log(APP_LIBFST,APP_FATAL,"%s: nbits must be 8 for datyp %d\n",__func__,datyp);
      return(ERR_BAD_DATYP);
   }

   nbwords = (nelm * nbits + 63) / 64;
   nbwords = W64TOWD(nbwords);

   index_word = buf->data_index + (bitpos / (sizeof(uint32_t) * 8));

   last_ind = buf->record_index + (buf->nbits / (sizeof(uint32_t) *8));

   // Move buffer content nbwords to the left
   if (last_ind != index_word) {
      for (i=index_word; i <= last_ind-nbwords; i++) {
         buf->data[i] = buf->data[i+nbwords];
      }
      for( ; i <= last_ind; i++) {
         buf->data[i] = 0 ;
      }
   }

   buf->nbits -= nbwords * sizeof(uint32_t) * 8;
   return 0;
}


//! Delete record referenced by handle.
//
//! Deleted record are marked as idtyp = X111111X (X = don't care bits)
//! and will be marked as idtyp=255 upon closing of the file.
//! \return 0 on success, a negative number if there's an error
int c_xdfdel(
    //! [in] File index, page number and record number to record
    const int handle
) {
    int index = INDEX_FROM_HANDLE(handle);
    int page_number = PAGENO_FROM_HANDLE(handle);
    int record_number = RECORD_FROM_HANDLE(handle);

    // validate index, page number and record number

    if ((index >= MAX_XDF_FILES) || (file_table[index] == NULL) || (file_table[index]->iun < 0)) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle, invalid file index\n",__func__);
        return(ERR_BAD_HNDL);
    }

    file_table_entry *fte = file_table[index];

    if ((fte->header->rwflg == RDMODE) || (fte->header->rwflg == APPEND)) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is open in read or append mode only\n",__func__);
        return(ERR_RDONLY);
    }

    if (fte->cur_info->attr.read_only) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is read only\n",__func__);
        return(ERR_RDONLY);
    }

    int idtyp;
    int addr;
    file_record *record;
    xdf_record_header header;
    page_ptr target_page;
    if (! fte->xdf_seq) {
        if (page_number < fte->npages) {
            // Page is in current file
            target_page = fte->dir_page[page_number];
        } else {
            // Page is in a link file
            if (fte->link == -1) {
                Lib_Log(APP_LIBFST,APP_ERROR,"%s: page number=%d > last page=%d and file not linked\n",__func__,page_number,fte->npages-1);
                return(ERR_BAD_PAGENO);
            }
            target_page = fte->dir_page[fte->npages-1];
            for (int i = 0; (i <= (page_number - fte->npages)) && target_page; i++) {
                target_page = (target_page)->next_page;
            }
            if (target_page == NULL) {
                Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle, invalid page number\n",__func__);
                return(ERR_BAD_PAGENO);
            }
        }

        if (record_number > target_page->dir.nent) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle, invalid record number\n",__func__);
            return(ERR_BAD_HNDL);
        }

        uint32_t *rec = target_page->dir.entry + record_number * W64TOWD(fte->primary_len);
        record = (file_record *) rec;

        idtyp = record->idtyp;
    } else {
        // xdf sequential
        addr = address_from_handle(handle, fte);
        c_waread(fte->iun, &header, addr, W64TOWD(1));
        idtyp = header.idtyp;
    }

    if (idtyp == 0) {
        Lib_Log(APP_LIBFST,APP_WARNING,"%s: special record idtyp=0\n",__func__);
        return(ERR_SPECIAL);
    }

    if ((idtyp & 0x7E) == 0x7E) {
        Lib_Log(APP_LIBFST,APP_WARNING,"%s: record already deleted\n",__func__);
        return(ERR_DELETED);
    }

    if (! fte->xdf_seq) {
        // update directory entry
        record->idtyp = 254;
        target_page->modified = 1;
    } else {
        // xdf sequential
        // deleted
        header.idtyp = 255;
        c_wawrit(fte->iun, &header, addr, W64TOWD(1));
    }

    // Update file header
    fte->header->neff++;
    fte->header->nrec--;

    fte->modified = 1;
    return 0;
}


//! Obtain record referenced by handle in buf
int c_xdfget2(
    //! [in] File index, page number and record number to record
    const int handle,
    // [out] Buffer to contain record
    buffer_interface_ptr buf,
    int * const aux_ptr
) {
    int index = INDEX_FROM_HANDLE(handle);
    int page_number = PAGENO_FROM_HANDLE(handle);
    int record_number = RECORD_FROM_HANDLE(handle);

    // validate index, page number and record number
    if ((index >= MAX_XDF_FILES) || (file_table[index] == NULL) || (file_table[index]->iun < 0)) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle, invalid file index\n",__func__);
        return(ERR_BAD_HNDL);
    }

    file_table_entry *fte = file_table[index];

    file_record *record;
    if (! fte->xdf_seq) {
        // Page is in current file
        page_ptr target_page;
        if (page_number < fte->npages) {
            target_page = fte->dir_page[page_number];
        } else {
            // page is in a link file
            if (fte->link == -1) {
                Lib_Log(APP_LIBFST,APP_ERROR,"%s: page number=%d > last page=%d and file not linked\n",__func__,page_number,fte->npages-1);
                return(ERR_BAD_PAGENO);
            }
            target_page = fte->dir_page[fte->npages-1];
            for (int i = 0; (i <= (page_number - fte->npages)) && target_page; i++) {
                target_page = (target_page)->next_page;
            }
            if (target_page == NULL) {
                Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle, invalid page number\n",__func__);
                return(ERR_BAD_PAGENO);
            }
            fte = file_table[target_page->true_file_index];
        }

        if (record_number > target_page->dir.nent) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle, invalid record number\n",__func__);
            return(ERR_BAD_HNDL);
        }

        uint32_t *rec = target_page->dir.entry + record_number * W64TOWD(fte->primary_len);
        record = (file_record *) rec;
    } else {
        if (! fte->valid_pos) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: no valid file position for sequential file\n",__func__);
            return(ERR_NO_POS);
        }
        record = (file_record *) fte->head_keys;
        if (address_from_handle(handle, fte) != W64TOWD(record->addr - 1) + 1) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle, invalid address=%d record address=%d\n",__func__,address_from_handle(handle,fte),W64TOWD(record->addr-1)+1);
            return(ERR_BAD_HNDL);
        }
    }

    int idtyp = record->idtyp;
    int addr = record->addr;
    int lng = record->lng;
    int lngw = W64TOWD(lng);

    if (idtyp == 0) {
        Lib_Log(APP_LIBFST,APP_WARNING,"%s: special record idtyp=0\n",__func__);
        return(ERR_SPECIAL);
    }

    if ((idtyp & 0x7E) == 0x7E) {
        Lib_Log(APP_LIBFST,APP_WARNING,"%s: deleted record\n",__func__);
        return(ERR_DELETED);
    }

    int nw = buf->nwords;
    int offset = 0;
    if (nw < 0) {
        if (buf->nbits != -1) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: dimension of buf is invalid = %d\n",__func__,nw);
            return(ERR_BAD_DIM);
        }
        // data only, no directory entry in record
        nw = -nw;
        if (! fte->fstd_vintage_89) {
            offset = W64TOWD(fte->primary_len + fte->info_len);
        } else {
            if (fte->xdf_seq) {
                // old standard sequential
                offset = 30;
            }
        }
    }
    if (lngw > (nw - RECADDR +1)) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: dimension of buf (%d) < record size (%d)\n",__func__,nw,lngw);
        return(ERR_BAD_DIM);
    }

    buf->nbits = lngw * 8 * sizeof(uint32_t);
    buf->record_index = RECADDR;
    buf->data_index = buf->record_index + W64TOWD(fte->primary_len + fte->info_len);
    buf->iun = fte->iun;
    buf->aux_index = buf->record_index + W64TOWD(fte->primary_len);

    if (aux_ptr != NULL) {
        *aux_ptr = 0;
        *(aux_ptr + 1) = 0;
    }
    if ( (aux_ptr != NULL) && (!fte->fstd_vintage_89) && (!fte->xdf_seq) ) {
        c_waread(buf->iun, aux_ptr, W64TOWD(addr-1) + 1 + W64TOWD(fte->primary_len), W64TOWD(fte->info_len));
    }

    for(int i = 0; i < lngw; i++) {
        buf->data[i] = 0;
    }

    int nread = c_waread2(buf->iun, &(buf->data), W64TOWD(addr - 1) + 1 + offset, lngw - offset);
    if (nread != lngw-offset) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: short read, truncated record, asking for %d, got %d\n",__func__,lngw-offset,nread);
        return(ERR_SHORT_READ);
    }

    return 0;
}


//! Obtain record referenced by handle in buf
int c_xdfget(
    //! [in] file index, page number and record number to record
    const int handle,
    //! [out]  buffer to contain record
    buffer_interface_ptr buf
) {
    return c_xdfget2(handle, buf, NULL);
}


//! Get different options settings values.
//! \return 0 on success. ERR_BAD_OPT if the option name is unknown
int c_xdfgop(
    //! [in] Name of option to get
    char *optname,
    //! [out] Value of option if type is character
    char *optc,
    //! [out] Value of option if type is integer
    int *optv
) {
    Lib_Log(APP_LIBFST,APP_INFO,"%s: Theses options are deprecated\n",__func__);
    return 0;
}


//! Get the descriptive parameters of the record in buf
//! \return 0 on success, error code otherwise
int c_xdfhdr(
    //! [in] Buffer that contains the record
    buffer_interface_ptr buf,
    //! [out] Record starting address
    int *addr,
    //! [out] Length of the record in 64 bit units
    int *lng,
    //! [out] Record id type
    int *idtyp,
    //! [out] Primary keys
    uint32_t *primk,
    //! [in] Number of primary keys
    int nprim,
    //! [out] Info keys
    uint32_t *info,
    //! [in] Number of info keys
    int ninfo
) {
    file_record *record = (file_record *) buf->data;

    *idtyp = record->idtyp;
    *addr = record->addr;
    *lng = record->lng;

    int index = file_index_xdf(buf->iun);
    if (index == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not open\n",__func__);
        return(ERR_NO_FILE);
    }

    file_table_entry *fte = file_table[index];

    max_dir_keys argument_not_used;
    uint32_t *mskkeys = NULL;
    fte->build_primary(buf->data, primk, argument_not_used, mskkeys, index, RDMODE);
    if (ninfo > 0) {
        fte->build_info(buf->data + W64TOWD(fte->primary_len), info, index, RDMODE);
    }

    return 0;
}


//! Prints the statistics associated to a file as given by xdfsta
int c_xdfimp(
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Statistics of the file
    uint32_t *stat,
    //! [in] Number of statistics to print
    int nstat,
    //! [in] Primary keys
    word_2 *pri,
    //! [in] Auxiliary keys
    word_2 *aux,
    //! [in] Software version
    char *vers,
    //! [in] Application signature
    char *appl
) {
    int i, ind, temp;
    key_descriptor * kdp;
    uint32_t wtemp[2];
    char nomcle[5];

    ind = get_fnom_index(iun);
    if (ind == -1) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not connected with fnom\n",__func__);
        return(ERR_NO_FNOM);
    }

    fprintf(stdout, "\n  Statistiques pour le fichier %d, Nom %s\n", iun, FGFDT[ind].file_name);
    fprintf(stdout, "  Taille du fichier \t\t\t %d\n", stat[0]);
    fprintf(stdout, "  Nombre de reecriture \t\t\t %d\n", stat[1]);
    fprintf(stdout, "  Nombre d'extensions \t\t\t %d\n", stat[2]);
    fprintf(stdout, "  Nombre de pages de Repertoires \t %d\n", stat[3]);
    fprintf(stdout, "  Addr. derniere page de repertoire \t %d\n", stat[4]);
    fprintf(stdout, "  Taille du plus gros enregistrement \t %d\n", stat[5]);
    fprintf(stdout, "  Nombre d'enregistrements effaces \t %d\n", stat[10]);
    fprintf(stdout, "  Nombre d'enregistrements valides \t %d\n", stat[11]);
    fprintf(stdout, "  Version du progiciel XDF \t\t %s\n", vers);
    fprintf(stdout, "  Nom de l'application \t\t\t %s\n", appl);

    fprintf(stdout, "\n  Definition des cles primaires \n\n");
    fprintf(stdout, " \tNom \tBit1 \tLongueur \tType \n\n");

    for (i = 0; i < stat[6]; i++) {
        wtemp[0] = pri[i].wd1;
        wtemp[1] = pri[i].wd2;
        kdp = (key_descriptor *) &wtemp;
        temp = kdp->ncle;
        nomcle[0] = temp >> 24 & 0xff;
        nomcle[1] = temp >> 16 & 0xff;
        nomcle[2] = temp >>  8 & 0xff;
        nomcle[3] = temp & 0xff;
        nomcle[4] = '\0';
        fprintf(stdout, " \t%s \t%d \t%d \t\t%d \n", nomcle, kdp->bit1, kdp->lcle + 1, kdp->tcle);
    }

    if (stat[8] > 0) {
        fprintf(stdout, "\n  Definition des cles auxiliaires \n\n");
        fprintf(stdout, " \tNom \tBit1 \tLongueur \tType \n\n");
        for (i = 0; i < stat[8]; i++) {
            wtemp[0] = aux[i].wd1;
            wtemp[1] = aux[i].wd2;
            kdp = (key_descriptor *) &wtemp;
            temp = kdp->ncle;
            nomcle[0] = temp >> 24 & 0xff;
            nomcle[1] = temp >> 16 & 0xff;
            nomcle[2] = temp >>  8 & 0xff;
            nomcle[3] = temp & 0xff;
            nomcle[4] = '\0';
            fprintf(stdout, " \t%s \t%d \t%d \t\t%d \n", nomcle, kdp->bit1, kdp->lcle + 1, kdp->tcle);
        }
    }

   return 0;
}

//! Initialize the keys in buffer.
int c_xdfini(
    //! [in] Unit number associated to the file
    int iun,
    //! [inout] Buffer to contain the record (buf(0) contains dim of buf)
    buffer_interface_ptr buf,
    //! [in] Id type
    int idtyp,
    //! [in] Primary keys
    uint32_t *keys,
    //! [in] Number of primary keys
    int nkeys,
    //! [in] Info keys
    uint32_t *info,
    //! [in] Number of info keys
    int ninfo
) {
    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not connected with fnom\n",__func__);
        return(ERR_NO_FNOM);
    }

    int index = file_index_xdf(iun);
    if (index == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not open\n",__func__);
        return(ERR_NO_FILE);
    }

    uint32_t *buffer = (uint32_t *) buf;
    for (int i = 1; i < buf->nwords; i++) {
        buffer[i] = 0;
    }

    if ((idtyp < 1) || (idtyp > 126)) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid idtyp=%d, must be between 1 and 126\n",__func__,idtyp);
        return(ERR_BAD_DATYP);
    }

    buf->record_index = RECADDR;
    buf->iun = iun;

    file_record *record = (file_record *) buf->data;
    record->idtyp = idtyp;

    file_table_entry *fte = file_table[index];
    buf->data_index = buf->record_index + W64TOWD(fte->primary_len + fte->info_len);
    buf->nbits = (fte->primary_len + fte->info_len) * 64;
    buf->aux_index = buf->record_index + W64TOWD(fte->primary_len);

    max_dir_keys mask;
    uint32_t *mskkeys = NULL;
    if (nkeys > 0) fte->build_primary(buf->data, keys, mask, mskkeys, index, WMODE);
    if (ninfo > 0) fte->build_info(buf->data + W64TOWD(fte->primary_len), info, index, WMODE);

   return 0;
}


//! Insert content of donnees into buf. nelm elements are inserted into buf starting at position bitpos.
//! \return 0 on success, error code otherwise
int c_xdfins(
    //! [inout] Buffer to contain the modified record
    uint32_t *buffer,
    //! [in] Data bits to add
    uint32_t *donnees,
    //! [in] Bit position of insertion into buf
    int bitpos,
    //! [in] Number of elements to add into buf
    int nelm,
    //! [in] Number of bits kept per element
    int nbits,
    //! [in] Data type
    int datyp
) {
    int nbwords, index_word, last_ind, i, mode;
    buffer_interface_ptr buf = (buffer_interface_ptr) buffer;
    int  ier;

    if ((bitpos % 64) != 0) {
        Lib_Log(APP_LIBFST,APP_FATAL,"%s: bitpos must be a multiple of 64\n",__func__);
        return(ERR_BAD_ADDR);
    }

    if (((datyp == 3) || (datyp == 5)) && (nbits != 8)) {
        Lib_Log(APP_LIBFST,APP_FATAL,"%s: nbits must be 8 for datyp %d\n",__func__,datyp);
        return(ERR_BAD_DATYP);
    }

    nbwords = (nelm * nbits + 63) / 64;
    nbwords = W64TOWD(nbwords);

    index_word = buf->data_index + (bitpos / (sizeof(uint32_t) * 8));

    last_ind = buf->record_index + (buf->nbits / (sizeof(uint32_t) *8));

    if ((last_ind + nbwords - 1) > buf->nwords) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: buffer not big enough for insertion\n",__func__);
        return(ERR_BAD_DIM);
    }

    // Move buffer content nbwords to the right for insertion
    if (last_ind != index_word) {
        for (i = last_ind; i >= index_word; i--) {
            buf->data[i + nbwords] = buf->data[i];
        }
    }


    // insert data
    switch (datyp) {
        case 0:
            // transparent mode
        case 3:
        case 6:
        case 8:
            for (i = 0; i < nbwords; i++) {
                buf->data[index_word + i] = donnees[i];
            }
            break;

        case 7:
        case 9:
            if (*little_endian) {
                for (i=0; i < nbwords; i += 2) {
                    buf->data[index_word + i] = donnees[i + 1];
                    buf->data[index_word + i + 1] = donnees[i];
                }
            } else {
                for (i = 0; i < nbwords; i++) {
                    buf->data[index_word + i] = donnees[i];
                }
            }
            break;

        case 5:
            // Upper char only
            for (i = 0; i < nbwords; i++) {
                buf->data[index_word+i] = upper_case_word(donnees[i]);
            }
            break;

        case 2:
            mode = 1;
            ier = compact_integer(donnees, (void *) NULL, &(buf->data[index_word]), nelm, nbits, 0, xdf_stride, mode);
            break;

        case 4:
            mode = 3;
            ier = compact_integer(donnees, (void *) NULL, &(buf->data[index_word]), nelm, nbits, 0, xdf_stride, mode);
         break;

        default:
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid datyp=%d\n",__func__,datyp);
            return(ERR_BAD_DATYP);

    } /* end switch */

    buf->nbits += nbwords * sizeof(uint32_t) * 8;
    return 0;
}


//! Links the list of random files together for record search purpose.
//! \return 0 on success, error code otherwise
int c_xdflnk(
    //! List of files indexes for the files to be linked
    int *liste,
    //! Number of files to be linked
    int nbLinkFiles
) {
    int index_fnom = get_fnom_index(liste[0]);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not connected with fnom\n",__func__);
        return(ERR_NO_FNOM);
    }

    int index = file_index_xdf(liste[0]);
    if (index == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not open\n",__func__);
        return(ERR_NO_FILE);
    }

    file_table_entry *fte = file_table[index];
    for (int i = 1; i < nbLinkFiles; i++) {
        if ((index_fnom = get_fnom_index(liste[i])) == -1) {
           Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not connected with fnom\n",__func__);
           return(ERR_NO_FNOM);
        }
        int indnext = file_index_xdf(liste[i]);
        if (indnext == ERR_NO_FILE) {
           Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not open\n",__func__);
           return(ERR_NO_FILE);
        }
        Lib_Log(APP_LIBFST,APP_DEBUG,"%s: xdflink %d avec %d\n",__func__,liste[i-1],liste[i]);

        file_table_entry *fnext = file_table[indnext];
        fte->link = indnext;
        (fte->dir_page[fte->npages-1])->next_page = fnext->dir_page[0];
        index = indnext;
        fte = file_table[index];
    }

    return 0;
}


//! Find the position of the record as described by the given primary keys.
//
//! The search begins from the record pointed by handle.  If handle is 0,
//! the search is from beginning of file. If handle is -1, the search
//! begins from the current position. If a specific key has the value of
//! -1, this key will not used as a selection criteria.
//! If nprim is -1, the last selection criterias will be used for the
//! search. Upon completion a "pointer" to the record (handle) is returned.
int c_xdfloc(
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Handle to the starting search position
    int handle,
    //! [in] Primary search keys
    uint32_t *primk,
    //! [in] Number search primary search keys
    int nprim
) {
  uint32_t *mskkeys = NULL;

    return c_xdfloc2(iun, handle, primk, nprim, mskkeys);
}


//! Find the position of the record as described by the given primary keys.
//
//! The search begins from the record pointed by handle.  If handle is 0,
//! the search is from beginning of file. If handle is -1, the search
//! begins from the current position. If a specific key has the value of
//! -1, this key will not used as a selection criteria.
//! If nprim is -1, the last selection criterias will be used for the
//! search. Upon completion a "pointer" to the record (handle) is returned.
//! \return Record handle if found or error code
int c_xdfloc2(
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Handle to the starting search position
    int handle,
    //! [in] Primary search keys
    uint32_t *primk,
    //! [in] Number search primary search keys
    int nprim,
    //! [in] Search mask
    uint32_t *mskkeys
) {
    int index = file_index_xdf(iun);
    if (index == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not open, iun=%d\n",__func__,iun);
        return(ERR_NO_FILE);
    }
    file_table_entry *fte = file_table[index];

    int was_allocated = 0;
    if ((mskkeys == NULL) && (nprim != 0)) {
        mskkeys = malloc(nprim * sizeof(uint32_t));
        was_allocated = 1;
        for (int i = 0; i < nprim; i++) {
            if (*primk == -1) {
                *mskkeys = 0;
            } else {
                *mskkeys = -1;
            }
        }
    }

    int record;
    int pageno;
    if (handle > 0) {
        // search begins from handle given position
        int index_h = INDEX_FROM_HANDLE(handle);
        // Search from next record
        record = RECORD_FROM_HANDLE(handle) + 1;
        pageno = PAGENO_FROM_HANDLE(handle);
        if (index_h != index) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle=%d or iun=%d\n",__func__,handle,iun);
            return(ERR_BAD_HNDL);
         }
        if (fte->xdf_seq) {
            fte->cur_addr = address_from_handle(handle, fte);
            xdf_record_header header;
            if (fte->fstd_vintage_89) {
                seq_dir_keys seq_entry;
                c_waread(iun, &seq_entry, fte->cur_addr, sizeof(seq_entry) / sizeof(uint32_t));
                header.lng = ((seq_entry.lng + 3) >> 2) + 15;
            } else {
                c_waread(iun, &header, fte->cur_addr, W64TOWD(1));
            }
            fte->cur_addr += W64TOWD(header.lng);
        }
    } else if (handle == 0) {
        //! Search from beginning of file
        record = 0;
        pageno = 0;
        fte->cur_pageno = -1;
        if (fte->xdf_seq) {
            fte->cur_addr = fte->seq_bof;
        }
    } else if (handle == -1) {
        //! Search from current position
        if (((fte->cur_entry == NULL) || (fte->cur_pageno == -1)) && (! fte->xdf_seq)) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: current file position is invalid\n",__func__);
            return(ERR_NO_POS);
        }
    } else {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle\n",__func__);
        return(ERR_BAD_HNDL);
    }

    // If nprim == 0 keep same search target
    if (nprim) {
        fte->build_primary(fte->target, primk, fte->cur_mask, mskkeys, index, WMODE);
        uint32_t *pmask = (uint32_t *) fte->cur_mask;
        uint32_t *psmask = (uint32_t *) fte->srch_mask;
        for (int i = 0; i < W64TOWD(fte->primary_len); i++, pmask++, psmask++) {
            *pmask &= *psmask;
        }
        fte->valid_target = 1;
    }

    if ((handle != -1) && (! fte->xdf_seq)) {
        if (pageno != fte->cur_pageno) {
            if (pageno < fte->npages) {
                // Page is in current file
                fte->cur_dir_page = fte->dir_page[pageno];
                fte->cur_pageno = pageno;
            } else {
                // Page is in a link file
                if (fte->link == -1) {
                    Lib_Log(APP_LIBFST,APP_ERROR,"%s: page number=%d > last page=%d and file not linked\n",__func__,pageno,fte->npages-1);
                    fte->cur_entry = NULL;
                    return(ERR_BAD_PAGENO);
                }
                fte->cur_dir_page = fte->dir_page[fte->npages - 1];
                fte->cur_pageno = fte->npages - 1;
                for (int i = 0; (i <= pageno - fte->cur_pageno) && (fte->cur_dir_page); i++) {
                    fte->cur_dir_page = (fte->cur_dir_page)->next_page;
                    fte->cur_pageno++;
                }
                if (fte->cur_dir_page == NULL) {
                    Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle, invalid page number\n",__func__);
                    fte->cur_entry = NULL;
                    return(ERR_BAD_PAGENO);
                }
            }
        } else {
            // Just to make sure
            fte->cur_dir_page = fte->dir_page[fte->cur_pageno];
        }
        fte->cur_entry = (fte->cur_dir_page)->dir.entry + record * W64TOWD(fte->primary_len);
        fte->page_record = record;
    }

    if (((fte->cur_entry == NULL) || (fte->cur_pageno == -1)) && (! fte->xdf_seq)) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: no valid current file position\n",__func__);
        return(ERR_NO_POS);
    }

    if (! fte->valid_target) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: no valid current search target\n",__func__);
        return(ERR_NO_TARGET);
    }

    int new_handle = next_match(index);
    if (was_allocated) free(mskkeys);
    return new_handle;
}


//! Open an XDF file.
//! \return Number of records in the file, -1 if error.
int c_xdfopn(
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Open mode (READ, WRITE, R-W, CREATE, APPEND)
    char *mode,
    //! [in] Primary keys
    word_2 *pri,
    //! [in] Number of primary keys
    int npri,
    //! [in] Info keys
    word_2 *aux,
    //! [in] Number of info keys
    int naux,
    //! [in] Application signature
    char *appl
) {
    int32_t f_datev;
    double nhours;
    int deet, npas, i_nhours, run, datexx;
    uint32_t STDR_sign = 'S' << 24 | 'T' << 16 | 'D' << 8 | 'R';
    uint32_t STDS_sign = 'S' << 24 | 'T' << 16 | 'D' << 8 | 'S';

    if (initialize_xdf() <= 0) return -1; // Error message will already be printed

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Opening file with iun %d\n", __func__, iun);

    if ((iun <= 0) || (iun >= MAX_FNOM_FILES)) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid unit number=%d\n",__func__,iun);
        return(ERR_BAD_UNIT);
    }

    if (file_index_xdf(iun) != ERR_NO_FILE) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file (unit=%d) is already open\n",__func__,iun);
        return(ERR_FILE_OPN);
    }

    int index = get_free_index();
    if (index < 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to assign slot for opening XDF file with iun %d\n", __func__, iun);
        return index;
    }

    Lib_Log(APP_LIBFST, APP_EXTRA, "%s: Assigning unit %d to entry index %d\n", __func__, iun, index);
    file_table[index]->iun = iun;
    file_table[index]->file_index = index;

    int index_fnom = get_fnom_index(iun);
    if (index_fnom < 0) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file (unit=%d) is not connected with fnom\n",__func__,iun);
        return(ERR_NO_FNOM);
    }

    file_table_entry *fte = file_table[index];
    fte->cur_info = &FGFDT[index_fnom];

    if (fte->cur_info->file_type == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file type is NULL. iun = %d, index_fnom = %d, index = %d\n",
                __func__, iun, index_fnom, index);
        return -1;
    }

    if (! fte->cur_info->attr.rnd) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file must be random\n file in error: %s\n",__func__,FGFDT[index_fnom].file_name);
        return(-1);
    }

    if (strstr(appl, "BRP0")) fte->cur_info->attr.burp = 1;
    if (strstr(appl, "STD"))  fte->cur_info->attr.std = 1;

    if (fte->cur_info->attr.burp) {
        fte->build_primary = (fn_b_p *) build_burp_prim_keys;
        fte->build_info = build_burp_info_keys;
    } else {
        if (fte->cur_info->attr.std) {
            fte->build_primary = (fn_b_p *) build_fstd_prim_keys;
            fte->build_info = build_fstd_info_keys;
            fte->file_filter = C_fst_match_req;
        } else {
            fte->build_primary = (fn_b_p *) build_gen_prim_keys;
            fte->build_info = build_gen_info_keys;
        }
    }

    if ((strstr(fte->cur_info->file_type, "SEQ")) || (strstr(fte->cur_info->file_type, "seq"))) {
        fte->xdf_seq = 1;
        // at least one seq file is opened, limit number of xdf files is now 128
        STDSEQ_opened = 1;
        if (index > 127) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: while opening std/seq file, limit of 128 opened file reached\n",__func__);
            return(-1);
        }
    }

    Lib_Log(APP_LIBFST,APP_DEBUG,"%s: c_xdfopn fte->xdf_seq=%d\n",__func__,fte->xdf_seq);

    if (strstr(mode, "CREATE") || strstr(mode, "create")) {

        // Create new xdf file
        c_waopen(iun);
        int ier = create_new_xdf(index, iun, pri, npri, aux, naux, appl);
        if (! fte->xdf_seq) {
            add_dir_page(index, WMODE);
        } else {
            fte->cur_addr = fte->nxtadr;
            fte->header->fsiz = WDTO64(fte->nxtadr -1);
        }
        fte->header->rwflg = CREATE;
        fte->modified = 1;
        return ier;
    }

    // File exist, read directory pages
    if (! FGFDT[index_fnom].open_flag) {
        c_waopen(iun);
    }

    int nrec = 0;
    {
        int wdaddress = 1;
        file_record header64;
        uint32_t *check32, checksum;
        int header_seq[30];
        int lng;
        stdf_struct_RND header_rnd;
        rnd_dir_keys *directory;
        stdf_dir_keys *stdf_entry;
        xdf_dir_page *curpage;

        c_waread(iun, &header64, wdaddress, W64TOWD(2));
        if (header64.data[0] == 'XDF0' || header64.data[0] == 'xdf0') {
            if ((fte->header = calloc(1,header64.lng * 8)) == NULL) {
                Lib_Log(APP_LIBFST,APP_FATAL,"%s: memory is full\n",__func__);
                return(ERR_MEM_FULL);
            }

            // Read file header
            int wdlng_header = W64TOWD(header64.lng);
            c_waread(iun, fte->header, wdaddress, wdlng_header);
            fte->primary_len = fte->header->lprm;
            fte->info_len = fte->header->laux;
            // nxtadr = fsiz +1
            fte->nxtadr = W64TOWD(fte->header->fsiz) + 1;
            wdaddress += wdlng_header;
            if (fte->cur_info->attr.std) {
                if ((fte->header->sign != STDR_sign) && (fte->header->sign != STDS_sign)) {
                    Lib_Log(APP_LIBFST,APP_FATAL,"%s: %s is not a standard file\n",__func__,FGFDT[index_fnom].file_name);
                    return(ERR_WRONG_FTYPE);
                }
            }
            if (strstr(mode, "READ") || strstr(mode, "read")) {
                fte->header->rwflg = RDMODE;
            } else {
                if (fte->header->rwflg != RDMODE) {
                    Lib_Log(APP_LIBFST,APP_FATAL,"%s: file (unit=%d) currently used by another application in write mode\n",__func__,iun);
                    return(ERR_STILL_OPN);
                }

                if (strstr(mode, "WRITE") || strstr(mode, "write")) {
                    fte->header->rwflg = WMODE;
                } else if (strstr(mode, "R-W") || strstr(mode, "r-w")) {
                    fte->header->rwflg = RWMODE;
                } else if (strstr(mode, "APPEND") || strstr(mode, "append")) {
                    fte->header->rwflg = APPEND;
                }
            }

            if (fte->header->nbd == 0) {
                if ( (fte->cur_info->attr.std) && (header64.data[1] == 'STDR' || header64.data[1] == 'stdr') ) {
                    Lib_Log(APP_LIBFST,APP_ERROR,"%s: File probably damaged\n file in error %s\n",__func__,FGFDT[index_fnom].file_name);
                    return( ERR_BAD_DIR);
                } else {
                    fte->xdf_seq = 1;
                }
            } else {
                if (fte->xdf_seq == 1) {
                    // Random file opened in seqential mode
                    fte->header->rwflg = RDMODE;
                }
            }

            Lib_Log(APP_LIBFST,APP_DEBUG,"%s: fichier existe fte->xdf_seq=%d\n",__func__,fte->xdf_seq);

            if (! fte->xdf_seq) {
                // Read directory pages and compute checksum
                for (int i = 0; i < fte->header->nbd; i++) {
                    add_dir_page(index, RDMODE);
                    int lng64 = fte->primary_len * ENTRIES_PER_PAGE + 4;
                    curpage = &((fte->dir_page[fte->npages-1])->dir);
                    c_waread(iun, curpage, wdaddress, W64TOWD(lng64));
                    checksum = 0;
                    check32 = (uint32_t *) curpage;
                    for (int j = 4; j < W64TOWD(lng64); j++) {
                        checksum ^= check32[j];
                    }
                    if (checksum != 0) {
                        Lib_Log(APP_LIBFST,APP_ERROR,"%s: incorrect checksum in page %d, directory is probably damaged\n file in error %s\n",__func__,i,FGFDT[index_fnom].file_name);
                    }
                    wdaddress = W64TOWD(curpage->nxt_addr - 1) +1;
                    if (((wdaddress == 0) && (i != fte->header->nbd-1)) || (wdaddress > FGFDT[index_fnom].file_size)) {
                        Lib_Log(APP_LIBFST,APP_ERROR,"%s: number of directory pages is incorrect\n file in error %s\n",__func__,FGFDT[index_fnom].file_name);
                        return(ERR_BAD_DIR);
                    }
                    nrec += curpage->nent;
                } /* end for */

                fte->nrecords = nrec;
            } else {
                // File is xdf sequential, position address to first record
                fte->cur_addr = wdaddress;
                fte->seq_bof = wdaddress;
            }
        } else {
            // Signature != XDF0
            check32 = (uint32_t *) &header64;
            if (*check32 == STDF_RND_SIGN) {
                // Old random standard file
                fte->cur_info->attr.read_only = 1;
                fte->fstd_vintage_89 = 1;
                lng = sizeof(header_rnd) / sizeof(uint32_t);
                c_waread(iun, &header_rnd, wdaddress, lng);
                wdaddress += lng;
                if ((directory = calloc(header_rnd.nutil, sizeof(uint32_t) * sizeof(rnd_dir_keys))) == NULL) {
                    Lib_Log(APP_LIBFST,APP_FATAL,"%s: memory is full\n",__func__);
                    return(ERR_MEM_FULL);
                }
                lng = header_rnd.nutil * sizeof(rnd_dir_keys) / sizeof(uint32_t);
                c_waread(iun, directory, wdaddress, lng);
                create_new_xdf(index, iun, (word_2 *)&stdfkeys, 16, aux, 0, "STDF");
                add_dir_page(index, RDMODE);
                fte->cur_dir_page = fte->dir_page[fte->npages-1];
                for (int i = 0; i < header_rnd.nutil; i++) {
                    if (fte->cur_dir_page->dir.nent >= ENTRIES_PER_PAGE) {
                        fte->nrecords += fte->page_nrecords;
                        add_dir_page(index, RDMODE);
                        fte->cur_dir_page = fte->dir_page[fte->npages-1];
                    }
                    fte->cur_entry = fte->cur_dir_page->dir.entry + fte->cur_dir_page->dir.nent * W64TOWD(fte->primary_len);
                    stdf_entry = (stdf_dir_keys *) fte->cur_entry;
                    fte->page_nrecords = ++fte->cur_dir_page->dir.nent;
                    if (! directory[i].dltf) {
                        stdf_entry->deleted = 0;
                        stdf_entry->select = 1;
                        stdf_entry->lng = (directory[i].lng + 3) >> 2;
                        stdf_entry->addr = (directory[i].swa >> 2) +1;
                        stdf_entry->deet = directory[i].deet;
                        stdf_entry->nbits = directory[i].nbits;
                        stdf_entry->ni = directory[i].ni;
                        stdf_entry->gtyp = directory[i].grtyp;
                        stdf_entry->nj = directory[i].nj;
                        stdf_entry->datyp = directory[i].datyp;
                        stdf_entry->nk = directory[i].nk;
                        stdf_entry->ubc = 0;
                        stdf_entry->npas = (directory[i].npas2 << 16) | directory[i].npas1;
                        stdf_entry->pad7 = 0;
                        stdf_entry->ig4 = directory[i].ig4;
                        stdf_entry->ig2a = 0;
                        stdf_entry->ig1 = directory[i].ig1;
                        stdf_entry->ig2b = directory[i].ig2 >> 8;
                        stdf_entry->ig3 = directory[i].ig3;
                        stdf_entry->ig2c = directory[i].ig2 & 0xff;
                        stdf_entry->etik15 =
                            (ascii6(directory[i].etiq14 >> 24) << 24) |
                            (ascii6((directory[i].etiq14 >> 16) & 0xff) << 18) |
                            (ascii6((directory[i].etiq14 >>  8) & 0xff) << 12) |
                            (ascii6((directory[i].etiq14      ) & 0xff) <<  6) |
                            (ascii6((directory[i].etiq56 >>  8) & 0xff));
                        stdf_entry->pad1 = 0;
                        stdf_entry->etik6a =
                            (ascii6((directory[i].etiq56      ) & 0xff) << 24) |
                            (ascii6((directory[i].etiq78 >>  8) & 0xff) << 18) |
                            (ascii6((directory[i].etiq78      ) & 0xff) << 12);
                        stdf_entry->pad2 = 0;
                        stdf_entry->etikbc = 0;
                        stdf_entry->typvar = ascii6(directory[i].typvar) << 6;
                        stdf_entry->pad3 = 0;
                        stdf_entry->nomvar =
                            (ascii6((directory[i].nomvar >>  8) & 0xff) << 18) |
                            (ascii6((directory[i].nomvar      ) & 0xff) << 12);
                        stdf_entry->dasiz = 0;
                        stdf_entry->ip1 = directory[i].ip1;
                        stdf_entry->levtyp = 0;
                        stdf_entry->ip2 = directory[i].ip2;
                        stdf_entry->pad5 = 0;
                        stdf_entry->ip3 = directory[i].ip3;
                        stdf_entry->pad6 = 0;
                        stdf_entry->date_stamp = directory[i].date;
                        deet = stdf_entry->deet;
                        npas = stdf_entry->npas;
                        if (((deet*npas) % 3600) != 0) {
                            // Recompute datev to take care of rounding used with 1989 version
                            // de-octalise the date_stamp
                            run = stdf_entry->date_stamp & 0x7;
                            datexx = (stdf_entry->date_stamp >> 3) * 10 + run;

                            f_datev = (int32_t) datexx;
                            i_nhours = (deet*npas - ((deet*npas+1800)/3600)*3600);
                            nhours = (double) (i_nhours / 3600.0);
                            f77name(incdatr)(&f_datev, &f_datev, &nhours);
                            datexx = (int) f_datev;
                            // re-octalise the date_stamp
                            stdf_entry->date_stamp = 8 * (datexx/10) + (datexx % 10);
                        }
                    } else {
                        stdf_entry->deleted = 1;
                    }
                } /* end for */
                fte->nrecords += fte->page_nrecords;
                free(directory);
            } else {
                // Sequential
                c_waread(iun, &header_seq, 1, 30);
                if (header_seq[896/32] == STDF_SEQ_SIGN) {
                    // old sequential stdf
                    fte->cur_info->attr.read_only = 1;
                    fte->fstd_vintage_89 = 1;
                    fte->xdf_seq = 1;
                    create_new_xdf(index, iun, (word_2 *)&stdfkeys, 16, aux, 0, "STDF");
                    fte->cur_addr = 1;
                    fte->seq_bof = 1;
                    return 0;
                } else {
                    Lib_Log(APP_LIBFST,APP_FATAL,"%s: file is not XDF type or old standard random type\n",__func__);
                    return(ERR_NOT_XDF);
                }
            }
        }
    }
    return fte->header->nrec;
}


//! Set different options in xdf.
int c_xdfopt(
    //! [in] Name of option to set
    char *optname,
    //! [in] Value of option if type is character
    char *optc,
    //! [in] Value of option if type is integer
    int optv
) {
    if (strstr(optname, "ERRTOLR") || strstr(optname, "errtolr")) {
    } else if (strstr(optname, "MSGLVL") || strstr(optname, "msglvl")) {
    } else if (strstr(optname, "STRIPING") || strstr(optname, "striping")) {
        xdf_nsplit = optv;
    } else {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid option name: %s\n",__func__,optname);
        return(ERR_BAD_OPT);
    }
    return 0;
}


//! Get the descriptive parameters of the record pointed by handle
//! \return 0 on success, error code otherwise
int c_xdfprm(
    //! [in] Buffer that contains the record
    int handle,
    //! [out] Record starting address
    int *addr,
    //! [out] Length of the record in 64 bit units
    int *lng,
    //! [out] Record id type
    int *idtyp,
    //! [out] Primary keys
    uint32_t *primk,
    //! [in] Number of primary keys
    int nprim
) {
    int index = INDEX_FROM_HANDLE(handle);
    int page_number = PAGENO_FROM_HANDLE(handle);
    int record_number = RECORD_FROM_HANDLE(handle);

    // Validate index, page number and record number
    if ((file_table[index] == NULL) || (file_table[index]->iun < 0)) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle, invalid file index\n",__func__);
        return(ERR_BAD_HNDL);
    }

    file_table_entry * fte = file_table[index];

    file_record *record;
    if (! fte->xdf_seq) {
        page_ptr target_page;
        if (page_number < fte->npages) {
            // Page is in current file
            target_page = fte->dir_page[page_number];
        } else {
            // Page is in a link file
            if (fte->link == -1) {
                Lib_Log(APP_LIBFST,APP_ERROR,"%s: page number=%d > last page=%d and file not linked\n",__func__,page_number,fte->npages-1);
                return(ERR_BAD_PAGENO);
            }
            target_page = fte->dir_page[fte->npages-1];
            for (int i = 0; (i <= (page_number - fte->npages)) && target_page; i++) {
                target_page = (target_page)->next_page;
            }
            if (target_page == NULL) {
                Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle, invalid page number\n",__func__);
                return(ERR_BAD_PAGENO);
            }
        }

        if (record_number > target_page->dir.nent) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle, invalid record number\n",__func__);
            return(ERR_BAD_HNDL);
        }

        uint32_t * rec = target_page->dir.entry + record_number * W64TOWD(fte->primary_len);
        record = (file_record *) rec;
    } else {
        if (! fte->valid_pos) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: no valid file position for sequential file\n",__func__);
            return(ERR_NO_POS);
        }
        record = (file_record *) fte->head_keys;
        if (address_from_handle(handle, fte) != W64TOWD(record->addr - 1) + 1) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle=%d, invalid address=%d record address=%d\n",__func__,handle,address_from_handle(handle,fte),W64TOWD(record->addr-1)+1);
            return(ERR_BAD_HNDL);
        }
    }

    *idtyp = record->idtyp;
    *addr = record->addr;
    *lng = record->lng;

    uint32_t *mskkeys = NULL;
    max_dir_keys argument_not_used;
    fte->build_primary((uint32_t *) record, primk, argument_not_used, mskkeys, index, RDMODE);

    return 0;
}


//! Write record (from buf) to xdf file.
//
//! If handle is not 0, rewrite record referenced by handle.
//! If handle is negative, the record will be append to end of file.
//! If handle is 0, the record is added at the end of file.
//! \return 0 on success, error code otherwise
int c_xdfput(
    //! [in] Unit number of the file to be written
    const int iun,
    //! [in] File index, page number and record number to record
    int handle,
    //! [in] Buffer to contain record
    buffer_interface_ptr buf)
{
    int index_fnom = get_fnom_index(iun);
    int index_from_buf = file_index_xdf(buf->iun);
    if (index_from_buf == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: record not properly initialized\n",__func__);
        return(ERR_BAD_INIT);
    }

    int index_from_iun = file_index_xdf(iun);
    if (index_from_iun == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid iun (%d)\n",__func__,iun);
        return(ERR_BAD_UNIT);
    }

    if ((buf->nbits & 0x3f) != 0) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: buf->nbits is not a multiple of 64 bits\n",__func__);
        return(ERR_BAD_ADDR);
    }

    file_table_entry *fte = file_table[index_from_iun];
    if ((fte->header->rwflg == RDMODE) || (FGFDT[index_fnom].attr.read_only)) {
        // Read only mode
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is open in read only mode or no write permission\n",__func__);
        return(ERR_NO_WRITE);
    }

    if ((handle != 0) && (fte->header->rwflg == APPEND)) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is open in append mode only\n",__func__);
        return(ERR_NO_WRITE);
    }

    int write_addr;
    int write_to_end = (handle <= 0) ? 1 : 0;
    int nwords = buf->nbits / (8 * sizeof(uint32_t));
    int index = index_from_iun;
    file_record *record;

    if (handle != 0) {
        if (handle < 0 ) handle = -handle;

        index = INDEX_FROM_HANDLE(handle);
        int page_number = PAGENO_FROM_HANDLE(handle);
        int record_number = RECORD_FROM_HANDLE(handle);

        // Validate index, page number and record number
        if (index != index_from_iun) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: iun and handle do not match\n",__func__);
            return(ERR_BAD_HNDL);
        }

        // Make sure that files are the same type
        if (index_from_iun != index_from_buf) {
            file_table_entry * f_buf = file_table[index_from_buf];
            if ((fte->header->nprm != f_buf->header->nprm) ||
                (fte->header->lprm != f_buf->header->lprm) ||
                (fte->header->naux != f_buf->header->naux) ||
                (fte->header->laux != f_buf->header->laux)) {
                Lib_Log(APP_LIBFST,APP_ERROR,"%s: source and destination files are different type of file\n",__func__);
                return(ERR_NOT_COMP);
            }
        }

        if (! fte->xdf_seq) {
            if (page_number < fte->npages) {
                // Page is in current file
                fte->cur_dir_page = fte->dir_page[page_number];
            } else {
                // Page is in a link file
                if (fte->link == -1) {
                    Lib_Log(APP_LIBFST,APP_ERROR,"%s: page number=%d > last page=%d and file not linked\n",__func__,page_number,fte->npages-1);
                    return(ERR_BAD_PAGENO);
                }
                fte->cur_dir_page = fte->dir_page[fte->npages-1];
                for (int i = 0; ((i <= page_number - fte->npages) && fte->cur_dir_page); i++) {
                    fte->cur_dir_page = (fte->cur_dir_page)->next_page;
                }
                if (fte->cur_dir_page == NULL) {
                    Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle, invalid page number\n",__func__);
                    return(ERR_BAD_PAGENO);
                }
            }

            if (record_number > fte->cur_dir_page->dir.nent) {
                Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid handle, invalid record number\n",__func__);
                return(ERR_BAD_HNDL);
            }

            fte->cur_entry = fte->cur_dir_page->dir.entry + record_number * W64TOWD(fte->primary_len);

            record = (file_record *) fte->cur_entry;
        } else {
            // file is xdf sequential
            if (handle > 0) {
                // Rewrite record
                int addr = address_from_handle(handle, fte);
                c_waread(iun, fte->head_keys, addr, MAX_PRIMARY_LNG);
                record = (file_record *) fte->head_keys;
            }
        }

        if (record->idtyp == 0) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: special record idtyp=0\n",__func__);
            return(ERR_SPECIAL);
        }

        if ((record->idtyp & 0x7E) == 0x7E) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: deleted record\n",__func__);
            return(ERR_DELETED);
        }

        int lng = record->lng;
        int lngw = W64TOWD(lng);
        if (lngw != nwords) {
            // enforce rewrite to end of file
            write_to_end = 1;
        } else {
            write_addr = W64TOWD(record->addr - 1) + 1;
        }

    } // end if handle != 0

    if ((write_to_end) && (! fte->xdf_seq)) {
        fte->cur_dir_page = fte->dir_page[fte->npages - 1];
        fte->cur_pageno = fte->npages - 1;
        if (fte->cur_dir_page->dir.nent >= ENTRIES_PER_PAGE) {
            fte->cur_dir_page->modified = 1;
            int err = add_dir_page(index, WMODE);
            if (err < 0) return err;
            fte->cur_dir_page = fte->dir_page[fte->npages-1];
            fte->cur_entry = fte->cur_dir_page->dir.entry;
        } else {
            fte->cur_entry = fte->cur_dir_page->dir.entry + fte->cur_dir_page->dir.nent * W64TOWD(fte->primary_len);
        }
        fte->page_nrecords = fte->cur_dir_page->dir.nent++;
        write_addr = fte->nxtadr;
    }

    if ((write_to_end) && (fte->xdf_seq)) write_addr = fte->cur_addr;

    if (handle != 0) {
        // Rewrite, delete old record
        int err = c_xdfdel(handle);
        if (err < 0) return err;
    }

    // update record header
    file_record * bufrec = (file_record *) buf->data;
    bufrec->addr = WDTO64(write_addr - 1) + 1;
    if (fte->xdf_seq) {
        int next_cluster_addr = fte->cur_addr -1 + nwords + W64TOwd(2);
        int cluster_size;
        if ((next_cluster_addr >> 18) >= 512) {
            cluster_size = 128;
        } else if ((next_cluster_addr >> 18) >= 128) {
            cluster_size = 32;
        } else if ((next_cluster_addr >> 18) >= 32) {
            cluster_size = 8;
        } else {
            cluster_size = 2;
        }
        next_cluster_addr = ((next_cluster_addr + cluster_size - 1) / cluster_size) * cluster_size;
        nwords = next_cluster_addr - fte->cur_addr - W64TOwd(2) + 1;
    }
    bufrec->lng = WDTO64(nwords);

    // Write record to file
    c_wawrit(iun, &(buf->data), write_addr, nwords);
    if (fte->xdf_seq) {
        fte->cur_addr += nwords;
    }

    if (! fte->xdf_seq) {
        // Update directory entry
        // Get keys
        max_dir_keys primk, argument_not_used, mskkeys;
        fte->build_primary(buf->data, primk, argument_not_used, mskkeys, index, RDMODE);
        fte->build_primary(fte->cur_entry, primk, argument_not_used, mskkeys, index, WMODE);
        record = (file_record *) fte->cur_entry;
        record->idtyp = bufrec->idtyp;
        record->addr = WDTO64(write_addr - 1) + 1;
        record->lng = WDTO64(nwords);
    }

    // Update file header
    fte->header->nrec++;
    if (write_to_end) {
        fte->header->nxtn++;
        fte->header->fsiz += WDTO64(nwords);
        fte->nxtadr = W64TOWD(fte->header->fsiz) + 1;
        fte->header->nbig = (WDTO64(nwords) > fte->header->nbig) ? WDTO64(nwords) : fte->header->nbig;
        if (fte->xdf_seq) {
            // Add postfix and eof marker
            xdf_record_header header64;
            postfix_seq postfix;
            postfix.idtyp = 0;
            postfix.lng = 2;
            postfix.addr = -1;
            postfix.prev_idtyp = bufrec->idtyp;
            postfix.prev_lng = bufrec->lng;
            postfix.prev_addr = WDTO64(write_addr -1) + 1;
            c_wawrit(iun, &postfix, fte->cur_addr, W64TOWD(2));
            fte->cur_addr += W64TOWD(2);
            // 112 + 15, 15 means EOF
            header64.idtyp = 127;
            header64.lng = 1;
            header64.addr = WDTO64(fte->cur_addr - 1) + 1;
            fte->nxtadr = fte->cur_addr;
            c_wawrit(iun, &header64, fte->cur_addr, W64TOWD(1));
            FGFDT[index_fnom].file_size = fte->nxtadr + W64TOWD(1) - 1;
        }
    }
    if (handle != 0) {
        fte->header->nrwr++;
    }

    fte->modified = 1;
    if (! fte->xdf_seq) {
        fte->cur_dir_page->modified = 1;
    }
    return 0;
}


//! Replace the content of the provided buffer
//! \return 0 on success, error code otherwise
int c_xdfrep(
    //! [inout] Buffer to contain the modified record
    uint32_t *buffer,
    //! [in] Replacement data bits
    uint32_t *donnees,
    //! [in] Bit position of replacement in buf
    int bitpos,
    //! [in] Number of elements to replace in buf
    int nelm,
    //! [in] Number of bits kept per element
    int nbits,
    //! [in] Data type
    int datyp
) {
    int nbwords, index_word, last_ind, i, mode;
    buffer_interface_ptr buf = (buffer_interface_ptr) buffer;
    int ier;

    if ((bitpos % 64) != 0) {
        Lib_Log(APP_LIBFST,APP_FATAL,"%s: bitpos must be a multiple of 64\n",__func__);
        return(ERR_BAD_ADDR);
    }

    if (((datyp == 3) || (datyp == 5)) && (nbits != 8)) {
        Lib_Log(APP_LIBFST,APP_FATAL,"%s: nbits must be 8 for datyp %d\n",__func__,datyp);
        return(ERR_BAD_DATYP);
    }

    nbwords = (nelm * nbits + 63) / 64;
    nbwords = W64TOWD(nbwords);

    index_word = buf->data_index + (bitpos / (sizeof(uint32_t) * 8));

    last_ind = buf->record_index + (buf->nbits / (sizeof(uint32_t) *8));

    if ((index_word + nbwords - 1) > buf->nwords) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: buffer not big enough for replacemen\n",__func__);
        return(ERR_BAD_DIM);
    }

    // initialize region to 0
    for (i = 0; i < nbwords; i++){
        buf->data[index_word+i] = 0;
    }

    // insert data
    switch (datyp) {
        case 0:
            // Transparent mode
        case 3:
        case 6:
        case 8:
            for (i=0; i < nbwords; i++) {
                buf->data[index_word+i] = donnees[i];
            }
            break;

        case 7:
        case 9:

            if (*little_endian) {
                for (i = 0; i < nbwords; i += 2) {
                    buf->data[index_word + i] = donnees[i + 1];
                    buf->data[index_word + i + 1] = donnees[i];
                }
            } else {
                for (i = 0; i < nbwords; i++) {
                    buf->data[index_word + i] = donnees[i];
                }
            }
            break;

        case 5:
            // Upper char only
            for (i = 0; i < nbwords; i++) {
                buf->data[index_word + i] = upper_case_word(donnees[i]);
            }
            break;

        case 2:
            mode = 1;
            ier = compact_integer(donnees, (void *) NULL, &(buf->data[index_word]), nelm, nbits, 0, xdf_stride, mode);
            break;

        case 4:
            mode = 3;
            ier = compact_integer(donnees, (void *) NULL, &(buf->data[index_word]), nelm, nbits, 0, xdf_stride, mode);
            break;

        default:
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid datyp=%d\n",__func__,datyp);
            return(ERR_BAD_DATYP);
    }

   return 0;
}


//! Get the statistics associated to a file.
//! \return 0 on success, error code otherwise
int c_xdfsta(
    //! [in] Unit number associated to the file
    int iun,
    //! [out] Statistics of the file
    uint32_t *stat,
    //! [in] Number of statistics to print
    int nstat,
    //! [out] Primary keys
    word_2 *pri,
    //! [in] Number of primary keys
    int npri,
    //! [out] Auxiliary keys
    word_2 *aux,
    //! [in] Number of auxiliary keys
    int naux,
    //! [out] Software version
    char *vers,
    //! [out] Application signature
    char *appl
) {
    int index, index_fnom, ier, wasopen = 0, i, nn;
    file_header *fh;
    file_record header64;

    index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not connected with fnom\n",__func__);
        return(ERR_NO_FNOM);
    }

    if ((index = file_index_xdf(iun)) == ERR_NO_FILE) {
        // Open file and read file header
        c_waopen(iun);
        c_waread(iun, &header64, 1, W64TOWD(2));
        if (header64.data[0] != 'XDF0' && header64.data[0] != 'xdf0') {
            Lib_Log(APP_LIBFST,APP_FATAL,"%s: file is not XDF type\n",__func__);
            return(ERR_NOT_XDF);
        }
        if ((fh = malloc(header64.lng * 8)) == NULL) {
            Lib_Log(APP_LIBFST,APP_FATAL,"%s: memory is full\n",__func__);
        }
        c_waread(iun, fh, 1, W64TOWD(header64.lng));
    } else {
        // File is open, file_table[index]->header already contains the required info
        wasopen = 1;
        fh = file_table[index]->header;
    }

   switch (nstat) {
        case 12: stat[11] = fh->nrec;
        case 11: stat[10] = fh->neff;
        case 10: stat[9] = fh->laux;
        case  9: stat[8] = fh->naux;
        case  8: stat[7] = fh->lprm;
        case  7: stat[6] = fh->nprm;
        case  6: stat[5] = fh->nbig;
        case  5: stat[4] = fh->plst;
        case  4: stat[3] = fh->nbd;
        case  3: stat[2] = fh->nxtn;
        case  2: stat[1] = fh->nrwr;
        case  1: stat[0] = fh->fsiz;
        case  0:
            vers[0] = fh->vrsn >> 24 & 0xff;
            vers[1] = fh->vrsn >> 16 & 0xff;
            vers[2] = fh->vrsn >>  8 & 0xff;
            vers[3] = fh->vrsn & 0xff;
            vers[4] = '\0';
            appl[0] = fh->sign >> 24 & 0xff;
            appl[1] = fh->sign >> 16 & 0xff;
            appl[2] = fh->sign >>  8 & 0xff;
            appl[3] = fh->sign & 0xff;
            appl[4] = '\0';
            break;
        default:
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: wrong number of stat nstat=%d\n",__func__,nstat);
            return(ERR_BAD_NSTAT);
    }

    // Primary keys description
    nn = 0;
    npri = (npri < fh->nprm) ? npri : fh->nprm;
    for (i = 0; i < npri; i++, nn++) {
        pri[i].wd1 = fh->keys[nn].ncle;
        pri[i].wd2 = (fh->keys[nn].bit1 << 19) | (fh->keys[nn].lcle << 14) |
                     (fh->keys[nn].tcle << 8) | (fh->keys[nn].reserved);
    }

    // Auxiliary keys description
    naux = (naux < fh->naux) ? naux : fh->naux;
    for (i = 0; i < naux; i++, nn++) {
        aux[i].wd1 = fh->keys[nn].ncle;
        aux[i].wd2 = (fh->keys[nn].bit1 << 19) | (fh->keys[nn].lcle << 14) |
                     (fh->keys[nn].tcle << 8) | (fh->keys[nn].reserved);
    }

    if (! wasopen) c_waclos(iun);
    return 0;
}


//! Unlinks the list of random files previously linked by c_xdflnk.
//! \return 0 on success, error code otherwise
int c_xdfunl(
    //! [in] Unit number associated to the file
    int *liste,
    //! [in] Number of files to be unlinked
    int nbLinkedFiles
) {
    for (int i = 0; i < nbLinkedFiles; i++) {
        int index_fnom = get_fnom_index(liste[i]);
        if (index_fnom  == -1) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not connected with fnom\n",__func__);
            return(ERR_NO_FNOM);
        }
        int index = file_index_xdf(liste[i]);
        if (index == ERR_NO_FILE) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not open\n",__func__);
            return(ERR_NO_FILE);
        }
        file_table_entry *fte = file_table[index];
        fte->link = -1;
        (fte->dir_page[fte->npages-1])->next_page = NULL;
    }

    return 0;
}


//! Update primary keys and info keys in buffer.
//
//! If a key value is -1, it is not updated
//! \return 0 on success, error code otherwise
int c_xdfupd(
    //! [in] Unit number associated to the file
    int iun,
    //! [inout] Buffer to contain the modified record
    buffer_interface_ptr buf,
    //! [in] Record type
    int idtyp,
    //! [in] List of primary keys
    uint32_t *keys,
    //! [in] Number of primary keys
    int nkeys,
    //! [in] List of secondary keys
    uint32_t *info,
    //! [in] Number of secondary keys
    int ninfo
) {
    int index_fnom = get_fnom_index(iun);
    if (index_fnom == -1) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not connected with fnom\n",__func__);
        return(ERR_NO_FNOM);
    }

    int index = file_index_xdf(iun);
    if (index == ERR_NO_FILE) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: file is not open\n",__func__);
        return(ERR_NO_FILE);
    }

    if (((idtyp < 1) && (idtyp != -1)) || (idtyp > 126)) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid idtyp=%d, must be between 1 and 126 or -1\n",__func__,idtyp);
        return(ERR_BAD_DATYP);
    }

    buf->iun = iun;

    file_record *record = (file_record *) buf->data;
    if (idtyp > -1) record->idtyp = idtyp;

    file_table_entry *fte = file_table[index];

    max_dir_keys argument_not_used;
    uint32_t *mskkeys = NULL;
    if (nkeys > 0) fte->build_primary(buf->data, keys, argument_not_used, mskkeys, index, WMODE);
    if (ninfo > 0) fte->build_info(buf->data + W64TOWD(fte->primary_len), info, index, WMODE);

    return 0;
}


//! Create or update a destination file containing all the valid records of the source file.
//!
//! If one of the two files is not opened it will be open and close at the end except for the destination file
//! which has to be close by the user.
//! \return 0 on success, error code otherwise
int c_xdfuse(
    //! [in] Unit number associated to the source file
    int src_unit,
    //! [in] Unit number associated to the destination file
    int dest_unit
) {
    int index_fnom_src = get_fnom_index(src_unit);
    if (index_fnom_src == -1) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: source file is not connected with fnom\n",__func__);
        return(ERR_NO_FNOM);
    }

    int index_fnom_dest = get_fnom_index(dest_unit);
    if (index_fnom_dest == -1) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: destination file is not connected with fnom\n",__func__);
        return(ERR_NO_FNOM);
    }

    // Process destination file
    int index_dest = file_index_xdf(dest_unit);
    static int stat[MAX_STAT];
    static word_2 primk[MAX_KEYS];
    static word_2 info[MAX_KEYS];
    char vers[5];
    char appl[5];
    if (index_dest == ERR_NO_FILE) {
        if (FGFDT[index_fnom_dest].file_size > 0) {
            // Destination file exists
            int err = c_xdfsta(dest_unit, (uint32_t *)&stat, MAX_STAT, primk, MAX_KEYS, info, MAX_KEYS, vers, appl);
            if (err < 0) return err;
            err = c_xdfopn(dest_unit, "APPEND", primk, stat[6], info, stat[8], appl);
            if (err < 0) return err;
        } else {
            int err = c_xdfsta(src_unit, (uint32_t *)&stat, MAX_STAT, primk, MAX_KEYS, info, MAX_KEYS, vers, appl);
            if (err < 0) return err;
            for (int i = 0; i < xdf_nsplit; i++) {
                err = c_xdfopn(dest_unit + i, "CREATE", primk, stat[6], info, stat[8], appl);
                if (err < 0) return err;
            }
        }
    } else {
        // file is open
        int err = c_xdfsta(dest_unit, (uint32_t *)&stat, MAX_STAT, primk, MAX_KEYS, info, MAX_KEYS, vers, appl);
        if (err < 0) return err;
    }
    int nprim = stat[6];
    int ninfo = stat[8];

    // Process source file
    int index_src = file_index_xdf(src_unit);
    int close_src = 0;
    word_2 src_primk[MAX_KEYS];
    word_2 src_info[MAX_KEYS];
    if (index_src == ERR_NO_FILE) {
        close_src = 1;
        int err = c_xdfopn(src_unit, "READ", src_primk, 0, src_info, 0, appl);
        if (err < 0) return err;
    }

    int src_stat[MAX_STAT];
    int err = c_xdfsta(src_unit, (uint32_t *)&src_stat, MAX_STAT, src_primk, MAX_KEYS, src_info, MAX_KEYS, vers, appl);
    if (err < 0) return err;

    // Make sure that source file and destination file are the same type
    int match = 1;
    for (int i = 6; i <= 9; i++) {
        match = match && (stat[i] == src_stat[i]);
    }
    for (int i = 0; i < nprim; i++) {
        match = match && (primk[i].wd1 == src_primk[i].wd1)
                      && (primk[i].wd2 == src_primk[i].wd2);
    }
    for (int i = 0; i < ninfo; i++) {
        match = match && (info[i].wd1 == src_info[i].wd1)
                      && (info[i].wd2 == src_info[i].wd2);
    }

    if (! match) {
      if (close_src)
        err = c_xdfcls(src_unit);
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: incompatible source (unit %d) and destination (unit %d) files\n",__func__,src_unit,dest_unit);
        return(ERR_NOT_COMP);
    }
    if (Lib_LogLevel(APP_LIBFST,NULL)>=APP_INFO) {
        err = c_xdfimp(src_unit, (uint32_t *)&src_stat, src_stat[6], src_primk, src_info, vers, appl);
    }

    // Copy source file to destination
    {
        xdf_record_header header;
        buffer_interface_ptr buf;
        int readpos = 1, nbrec = 0, idtyp, lng, addr, nbwords, file_size, nomore;
        int maxmem, initial_mem = 5000, last_word;

        if ((file_size = FGFDT[index_fnom_src].file_size) > 0) {
            nomore = 0;
        } else {
            nomore = 1;
        }

        maxmem = WDTO64(initial_mem + RECADDR);

        if ((buf = malloc(maxmem * 8)) == NULL) {
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: malloc can't allocate, no more memory availabl\n",__func__);
            return(ERR_MEM_FULL);
        }

        buf->nwords = initial_mem + RECADDR;
        buf->data_index = RECADDR;
        buf->iun = src_unit;

        while (! nomore) {
            c_waread(src_unit , &header, readpos, W64TOWD(1));
            addr = (header.addr == 0) ? 1 : header.addr;
            idtyp = header.idtyp;
            lng = header.lng;
            if (W64TOWD(addr-1)+1 == readpos) {
                // Valid address
                if ((idtyp > 0) && (idtyp < 127)) {
                    // valid regular record
                    if (lng + 100 + RECADDR > maxmem) {
                        // Ajout de 100 elements pour couvrir le debut du buffer buf
                        maxmem = lng + 100 + RECADDR;
                        if ((buf = realloc(buf, maxmem * 8)) == NULL) {
                            Lib_Log(APP_LIBFST,APP_ERROR,"%s: malloc can't allocate, no more memory available\n",__func__);
                            return(ERR_MEM_FULL);
                        }
                        buf->nwords = W64TOWD(maxmem) + RECADDR;
                        }  /* end if lng > maxmem */
                    nbwords = W64TOWD(lng);
                    last_word = readpos + nbwords - 1;
                    if (last_word <= file_size) {
                        buf->nbits = nbwords * sizeof(uint32_t) * 8;
                        for (int i = 0; i < nbwords; i++) {
                            buf->data[i] = 0;
                        }
                        c_waread(src_unit, &(buf->data), readpos, nbwords);
                        if (xdf_nsplit > 1) {
                            err = c_xdfput(dest_unit + (nbrec%xdf_nsplit), 0, buf);
                        } else {
                            err = c_xdfput(dest_unit, 0, buf);
                        }
                        nbrec++;
                    }
                } // end valid regular record
                readpos += W64TOWD(lng);
                if (readpos >= file_size) nomore = 1;
            } // end valid address
            else {
                nomore = 1;
            }
        } // end while
        free(buf);
        Lib_Log(APP_LIBFST,APP_INFO,"%s: copy of %d records from unit %d to unit %d\n",nbrec,src_unit,dest_unit);
    } // end copy files block

   if (close_src) err = c_xdfcls(src_unit);

   return 0;
}

//! Extract a portion of the record contained in buf and copy it into vector donnees.
//! \return 0 on success, error code otherwise
int c_xdfxtr(
    //! [in] Buffer to contain the modified record
    uint32_t *buffer,
    //! [out] Data bits to get
    uint32_t *donnees,
    //! [in] Bit position of starting extraction
    int bitpos,
    //! [in] Number of elements to extract into buf
    int nelm,
    //! [in] Number of bits kept per element
    int nbits,
    //! [in] Data type
    int datyp
) {
    int nbwords, index_word, last_ind, i, mode;
    buffer_interface_ptr buf = (buffer_interface_ptr) buffer;
    int ier;

    if ((bitpos % 64) != 0) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: bitpos must be a multiple of 64\n",__func__);
        return(ERR_BAD_ADDR);
    }

    if (((datyp == 3) || (datyp == 5)) && (nbits != 8)) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: nbits must be 8 for datyp %d\n",__func__,datyp);
        return(ERR_BAD_DATYP);
    }

    nbwords = (nelm * nbits + 63) / 64;
    nbwords = W64TOWD(nbwords);

    index_word = buf->data_index + (bitpos / (sizeof(uint32_t) * 8));

    // extract data
    switch (datyp) {
        case 0:
        case 3:
        case 5:
        case 6:
        case 8:
            for (i = 0; i < nbwords; i++)
                donnees[i] = buf->data[index_word+i];
            break;

        case 7:
        case 9:
            if (*little_endian) {
                for (i = 0; i < nbwords; i += 2) {
                    donnees[i+1] = buf->data[index_word+i];
                    donnees[i] = buf->data[index_word+i+1];
                }
            } else {
                for (i = 0; i < nbwords; i++) {
                    donnees[i] = buf->data[index_word+i];
                }
            }
            break;

        case 2:
            mode = 2;
            ier = compact_integer(donnees, (void *) NULL, &(buf->data[index_word]), nelm, nbits, 0, xdf_stride, mode);
            break;

        case 4:
            mode = 4;
            ier = compact_integer(donnees, (void *) NULL, &(buf->data[index_word]), nelm, nbits, 0, xdf_stride, mode);
            break;

        default:
            Lib_Log(APP_LIBFST,APP_ERROR,"%s: invalid datyp=%d\n",__func__,datyp);
            return(ERR_BAD_DATYP);
    } // End switch (datyp)

    return 0;
}


//! Create a new XDF file.
//! \return 0 on success, error code otherwise
static int create_new_xdf(
    //! [in] File index in table
    int index,
    //! [in] Unit number associated to the file
    int iun,
    //! [in] Primary keys
    word_2 *pri,
    //! [in] Number of primary keys
    int npri,
    //! [in] Auxiliary keys
    word_2 *aux,
    //! [in] Number of auxiliary keys
    int naux,
    //! [in] Application signature
    char *appl
) {
    // FIXME: No validation of user provided naux, npri befoe allocation
    // Why no allocate sizeof(file_header) for the header?
    // FIXME: According to the definition qstdir.h, the size of, file_table[index]->header, which is of type file_header, is static!

    file_header *file;
    int ikle = 0, lprm = 0, laux = 0;
    int lng_header = naux + npri + 512 / 64;

    if ((file_table[index]->header = malloc(lng_header * 8)) == NULL) {
        Lib_Log(APP_LIBFST,APP_FATAL,"%s: memory is full\n",__func__);
        return(ERR_MEM_FULL);
    }
    file = file_table[index]->header;
    file->vrsn = 'X' << 24 | 'D' << 16 | 'F' << 8 | '0';
    file->sign = appl[0] << 24 | appl[1] << 16 | appl[2] << 8 | appl[3];
    file->idtyp = 0;
    // keys + fixed part of 512 bits
    file->lng = lng_header;
    file->addr = 0;
    // keys + fixed part of 512 bits
    file->fsiz = lng_header;
    file->nrwr = 0;
    file->nxtn = 0;
    file->nbd = 0;
    file->plst = 0;
    file->nbig = 0;
    file->nprm = npri;
    file->naux = naux;
    file->neff = 0;
    file->nrec = 0;
    file->rwflg = 0;
    file->reserved = 0;

   {
        int i = 0 , bit1 = 0, lcle = 0, tcle = 0;
        while (npri--) {
            file->keys[ikle].ncle = pri[i].wd1;
            bit1 = pri[i].wd2 >> 19;
            lcle = 0x1F & (pri[i].wd2 >> 14);
            tcle = 0x3F  & (pri[i].wd2 >> 8);
            file->keys[ikle].bit1 = bit1;
            file->keys[ikle].lcle = lcle;
            file->keys[ikle].tcle = tcle;
            file->keys[ikle].reserved = 0;
            lprm += lcle;
            i++;
            ikle++;
        }
   }

   /* primary keys + 64 bit header */
   lprm = (lprm + 63) / 64 + 1;
   file->lprm = lprm;
   file_table[index]->primary_len = lprm;
   {
       int i=0 , bit1=0, lcle=0, tcle=0;
        while (naux--) {
            file->keys[ikle].ncle=aux[i].wd1;
            bit1=aux[i].wd2 >> 19;
            lcle=0x1F & (aux[i].wd2 >> 14);
            tcle=0x3F  & (aux[i].wd2 >> 8);
            file->keys[ikle].bit1=bit1;
            file->keys[ikle].lcle=lcle;
            file->keys[ikle].tcle=tcle;
            file->keys[ikle].reserved=0;
            laux += lcle;
            i++; ikle++;
        }
   }
   laux = (laux + 63) / 64;
   file->laux = laux;
   file_table[index]->info_len = laux;
   if (! file_table[index]->cur_info->attr.read_only) {
        {
            int unit = iun, waddress = 1, nwords = file->fsiz * 8 / sizeof(uint32_t);
            c_wawrit(unit, file_table[index]->header, waddress, nwords);
            file_table[index]->nxtadr += nwords;
        }
   }
   return 0;
}

// //! Find position of file iun in file table.
// //! \return Index of the unit number in the file table or ERR_NO_FILE if not found
int file_index_xdf(
    //! [in] Unit number associated to the file
    const int iun
) {
    for (int i = 0; i < MAX_XDF_FILES; i++) {
        const file_table_entry* entry = file_table[i];
        if (entry != NULL && entry != (void*)XDF_RESERVED) { // Non-NULL and not reserved
            if (entry->iun == iun) {
                return i;
            }
        }
    }
    return ERR_NO_FILE;
}

//! Find a free position in file table and initialize file attributes.
//! \return Free position index or negative error code
static int get_free_index()
{
    int nlimite;

    if (STDSEQ_opened == 1) {
        nlimite = 128;
    } else {
        nlimite = MAX_XDF_FILES;
    }


    for (int i = 0; i < nlimite; i++) {
        file_table_entry* entry = file_table[i];
        if (entry == NULL) {
            if (__sync_bool_compare_and_swap(&(file_table[i]), NULL, (void*)XDF_RESERVED)) { // Reserve slot
                entry = (file_table_entry*) malloc(sizeof(file_table_entry));
                if (entry == NULL) {
                    Lib_Log(APP_LIBFST, APP_FATAL, "%s: can't allocate file_table_entry\n", __func__);
                    return(ERR_MEM_FULL);
                }

                entry->file_index = -1; // assure first time use of index i
                init_file(entry, i);
                entry->iun = XDF_IUN_RESERVED; // Set as "reserved" for the purpose of this function
                file_table[i] = entry;
                return i;
            }
        } else if (entry != (void*)XDF_RESERVED) {
            // Entry has been initialized, but not reserved
            if (entry->iun == XDF_IUN_AVAILABLE) {
                if (__sync_bool_compare_and_swap(&(entry->iun), XDF_IUN_AVAILABLE, XDF_IUN_RESERVED)) {
                    return i;
                }
            }
         }
    }
    Lib_Log(APP_LIBFST,APP_FATAL,"%s: xdf file table is full\n",__func__);
    return(ERR_FTAB_FULL);
}


//! Initialize a file table entry.
static void init_file(
    //! [in,out] Entry to initialize
    file_table_entry* const entry,
    //! [in] Index of this entry in the file table
    const int index
) {
    int j;

    for (j = 1; j < MAX_DIR_PAGES; j++) {
        entry->dir_page[j] = NULL;
    }
    entry->cur_dir_page = NULL;
    entry->build_primary = NULL;
    entry->build_info = NULL;

    entry->scan_file = NULL;
    entry->file_filter = NULL;
    entry->cur_entry = NULL;
    if ((entry->file_index == index) && (entry->header != NULL)) {
        // Reuse file
        free(entry->header);
    }
    entry->header = NULL;
    entry->nxtadr = 1;
    entry->primary_len = 0;
    entry->info_len = 0;
    entry->link = -1;
    entry->file_index = index;
    entry->modified = 0;
    entry->npages = 0;
    entry->nrecords = 0;
    entry->cur_pageno = -1;
    entry->page_record = 0;
    entry->page_nrecords = 0;
    entry->file_version = 0;
    entry->valid_target = 0;
    entry->xdf_seq = 0;
    entry->valid_pos = 0;
    entry->cur_addr = -1;
    entry->seq_bof = 1;
    entry->fstd_vintage_89 = 0;
    for (j = 0; j < MAX_SECONDARY_LNG; j++) {
        entry->info_keys[j] = 0;
    }
    for (j = 0; j < MAX_PRIMARY_LNG; j++) {
        entry->head_keys[j] = 0;
        entry->cur_keys[j] = 0;
        entry->target[j] = 0;
        entry->srch_mask[j] = -1;
        entry->cur_mask[j] = -1;
    }

    // Do this one last. It marks the slot as available
    entry->iun = XDF_IUN_AVAILABLE;
}


//! Calculates an handle for a sequential file from address and index
//! \return Sequential file handle
static int32_t make_seq_handle(
    //! [in] Address (in units of 32bit)
    int address,
    //! [in] File index in table
    int file_index,
    //! [in] Pointer to xdf file information structure
    file_table_entry *fte
) {
    static int MB512 = 0x4000000, MB128 = 0x1000000, MB32 = 0x400000;
    // MB512 , MB128 and MB32 are represented in 64 bit unit

    // 32 bit word to 64 bit unit address
    address = (address - 1) >> 1;
    int cluster;
    int addr;
    if (fte->fstd_vintage_89) {
        cluster = 0;
        address = address / 15;
        addr = address;
    } else {
        if (address >= MB512) {
            cluster = 3;
            addr = address >> 6;
        } else {
            if (address >= MB128) {
                cluster = 2;
                addr = address >> 4;
            } else {
                if (address >= MB32) {
                    cluster = 1;
                    addr = address >> 2;
                } else {
                    cluster = 0;
                    addr = address;
                }
            }
        }
    }

    return MAKE_SEQ_HANDLE(cluster, addr, file_index);
}


//! Find the next record that matches the current search criterias.
//! \return Handle of the next matching record or error code
static uint32_t next_match(
    //! [in] index of file in the file table
    int file_index
) {
    int record_in_page, page_no, match;
    int end_of_file, nw, addr_match;
    uint32_t *entry, *search, *mask, handle;
    stdf_dir_keys *stds, *stdm, *stde;
    seq_dir_keys *seq_entry;
    xdf_record_header *header;
    int32_t f_datev;
    double nhours;
    int deet, npas, i_nhours, run, datexx;

    // check if file exists
    register file_table_entry *fte = file_table[file_index];
    if (fte  == NULL) return ERR_NO_FILE;

    int iun = file_table[file_index]->iun;
    int width = W64TOWD(fte->primary_len);
    int found = 0;
    fte->valid_pos = 0;

    if (! fte->xdf_seq) {
        // Check if there is a valid current page
        // We consider an entry as "not found" if we have already reached the end of the file
        if (fte->cur_dir_page == NULL) return (ERR_NOT_FOUND);
        if (fte->cur_entry == NULL) return (ERR_NO_POS);
        if (fte->cur_entry - (fte->cur_dir_page)->dir.entry != fte->page_record *W64TOWD(fte->primary_len)) {
            return ERR_NO_POS;
        }

        fte->page_nrecords = (fte->cur_dir_page)->dir.nent;
        while (! found) {
            if (fte->page_record >= fte->page_nrecords ) {
                // No more records in page
                fte->page_record = 0;
                fte->page_nrecords = 0;
                // Position to next dir page
                fte->cur_dir_page = (fte->cur_dir_page)->next_page;
                if (fte->cur_dir_page == NULL) {
                    // No more pages, end of file
                    break;
                }
                fte->cur_pageno++;
                fte->page_nrecords = (fte->cur_dir_page)->dir.nent;
                fte->cur_entry = (fte->cur_dir_page)->dir.entry;
            } else {
                for (int j = fte->page_record; (j < fte->page_nrecords) && !found; j++) {
                    entry = fte->cur_entry;
                    header = (xdf_record_header *) entry;
                    if (header->idtyp < 127) {
                        search = (uint32_t *) fte->target;
                        mask = (uint32_t *) fte->cur_mask;
                        stde = (stdf_dir_keys *) entry;
                        stdm = (stdf_dir_keys *) mask;
                        stds = (stdf_dir_keys *) search;
                        match = 0;
                        for (int i = 0; i < width; i++, mask++, search++, entry++) {
                            match |= (((*entry) ^ (*search)) & (*mask));
                        }
                        found = (match == 0);
                        if ( (fte->file_filter != NULL) && found ) {
                            // No need to call filter if not 'found'
                            handle= MAKE_RND_HANDLE( fte->cur_pageno, fte->page_record, fte->file_index );
                            found = found && fte->file_filter(handle);
                        }
                    }
                    // Position to next record for next search
                    fte->page_record++;
                    fte->cur_entry += width;
                }
            }
        } // end while
    } else {
        while (! found) {
            nw = c_waread2(iun, fte->head_keys, fte->cur_addr, width);
            header = (xdf_record_header *) fte->head_keys;
            if ((header->idtyp >= 112) || (nw < width)) {
                if ((header->idtyp >= 112) && (header->idtyp < 127)) {
                    fte->cur_addr += W64TOWD(1);
                }
                end_of_file = 1;
                break;
            }
            if (fte->fstd_vintage_89) {
                /* old sequential standard */
                if ((stde = malloc(sizeof(stdf_dir_keys))) == NULL) {
                    Lib_Log(APP_LIBFST,APP_FATAL,"%s: memory is full\n",__func__);
                    return(ERR_MEM_FULL);
                }
                seq_entry = (seq_dir_keys *) fte->head_keys;
                if (seq_entry->dltf) {
                    fte->cur_addr += W64TOWD(((seq_entry->lng + 3) >> 2)+15);
                    continue;
                }
                if (seq_entry->eof > 0) {
                    header->idtyp = 112 + seq_entry->eof;
                    header->lng = 1;
                    end_of_file = 1;
                    break;
                }
                stde->deleted = 0;
                stde->select = 1;
                stde->lng = ((seq_entry->lng + 3) >> 2) + 15;
                stde->addr = (seq_entry->swa >> 2) +1;
                stde->deet = seq_entry->deet;
                stde->nbits = seq_entry->nbits;
                stde->ni = seq_entry->ni;
                stde->gtyp = seq_entry->grtyp;
                stde->nj = seq_entry->nj;
                stde->datyp = seq_entry->datyp;
                stde->nk = seq_entry->nk;
                stde->ubc = 0;
                stde->npas = (seq_entry->npas2 << 16) |
                seq_entry->npas1;
                stde->pad7 = 0;
                stde->ig4 = seq_entry->ig4;
                stde->ig2a = 0;
                stde->ig1 = seq_entry->ig1;
                stde->ig2b = seq_entry->ig2 >> 8;
                stde->ig3 = seq_entry->ig3;
                stde->ig2c = seq_entry->ig2 & 0xff;
                stde->etik15 =
                    (ascii6(seq_entry->etiq14 >> 24) << 24) |
                    (ascii6((seq_entry->etiq14 >> 16) & 0xff) << 18) |
                    (ascii6((seq_entry->etiq14 >>  8) & 0xff) << 12) |
                    (ascii6((seq_entry->etiq14      ) & 0xff) <<  6) |
                    (ascii6((seq_entry->etiq56 >>  8) & 0xff));
                stde->pad1 = 0;
                stde->etik6a =
                    (ascii6((seq_entry->etiq56      ) & 0xff) << 24) |
                    (ascii6((seq_entry->etiq78 >>  8) & 0xff) << 18) |
                    (ascii6((seq_entry->etiq78      ) & 0xff) << 12);
                stde->pad2 = 0;
                stde->etikbc = 0;
                stde->typvar = ascii6(seq_entry->typvar) << 6;
                stde->pad3 = 0;
                stde->nomvar =
                    (ascii6((seq_entry->nomvar >>  8) & 0xff) << 18) |
                    (ascii6((seq_entry->nomvar      ) & 0xff) << 12);
                stde->dasiz = 0;
                stde->ip1 = seq_entry->ip1;
                stde->levtyp = 0;
                stde->ip2 = seq_entry->ip2;
                stde->pad5 = 0;
                stde->ip3 = seq_entry->ip3;
                stde->pad6 = 0;
                stde->date_stamp = seq_entry->date;
                deet = stde->deet;
                npas = stde->npas;
                if (((deet*npas) % 3600) != 0) {
                    // Recompute datev to take care of rounding used with 1989 version de-octalise the date_stamp
                    run = stde->date_stamp & 0x7;
                    datexx = (stde->date_stamp >> 3) * 10 + run;

                    f_datev = (int32_t) datexx;
                    i_nhours = (deet*npas - ((deet*npas+1800)/3600)*3600);
                    nhours = (double) (i_nhours / 3600.0);
                    f77name(incdatr)(&f_datev, &f_datev, &nhours);
                    // Re-octalise the date_stamp
                    datexx = (int) f_datev;
                    stde->date_stamp = 8 * (datexx/10) + (datexx % 10);
                }

                entry = (uint32_t *) stde;
                search = (uint32_t *) fte->head_keys;
                for (int i = 0; i < width; i++, entry++, search++) {
                    *search = *entry;
                }
                free(stde);
            } // end if fstd_vintage_89

            if ((header->idtyp < 1) || (header->idtyp > 127)) {
                fte->cur_addr += W64TOWD(header->lng);
                continue;
            }
            entry = (uint32_t *) fte->head_keys;
            search = (uint32_t *) fte->target;
            mask = (uint32_t *) fte->cur_mask;
            match = 0;
            for (int i = 0; i < width; i++, mask++, search++, entry++) {
                match |= (((*entry) ^ (*search)) & (*mask));
            }
            found = (match == 0);

            if (found) {
                fte->valid_pos = 1;
                addr_match = fte->cur_addr;
            }
            // Position to next record
            fte->cur_addr += W64TOWD(header->lng);
            if (! fte->fstd_vintage_89) {
                // Skip postfix
                fte->cur_addr += W64TOWD(2);
            }
        }
    }

    if (! found) return ERR_NOT_FOUND;

    if (! fte->xdf_seq) {
        Lib_Log(APP_LIBFST,APP_DEBUG,"%s: Record found at page# %d, record# %d\n",__func__,fte->cur_pageno,fte->page_record-1);
        handle = MAKE_RND_HANDLE(fte->cur_pageno, fte->page_record-1, fte->file_index);
    } else {
        Lib_Log(APP_LIBFST,APP_DEBUG,"%s: Record found at address %d\n",__func__,addr_match);
        stde = (stdf_dir_keys *) fte->head_keys;
        handle = make_seq_handle(addr_match, fte->file_index, fte);
    }
    return handle;
}


// Everything after this point has been moved from if_xdf98.h
// This is the only place where it was used


int32_t  f77name(qdfdiag)(int32_t *f_iun)
{
    int iun = *f_iun, ier;

    return (int32_t) c_qdfdiag(iun);
}


int32_t f77name(qdfind)(int32_t *iun)
{
    int ind = file_index_xdf(*iun);
    ind = (ind != ERR_NO_FILE) ? ind : 9999;

    return (int32_t) ind;
}


int32_t f77name(qdfmsig)(int32_t *fiun, char *appl, F2Cl l1)
{
    int iun = *fiun;
    char c_appl[257];

    int lng = (l1 <= 256) ? l1 : 256;
    strncpy(c_appl, appl, lng);
    c_appl[lng] = '\0';

    return c_qdfmsig(iun, c_appl);
}


int32_t f77name(qdfput)(uint32_t *buf, int32_t *felem, int32_t *fderbit,
            int32_t *fnbits)
{
    int elem = *felem, nbits = *fnbits, derbit = *fderbit;

    return (int32_t) c_qdfput(buf, elem, derbit, nbits);
}

int32_t  f77name(qdfrstr)(int32_t *f_inp, int32_t *f_outp)
{
    int inp = *f_inp, outp = *f_outp;

    return (int32_t) c_qdfrstr(inp, outp);
}

//! Set file position, if handle=-1, rewind file
static int32_t rewind_file(
    int file_index,
    int handle
) {
    // check if file exists
    register file_table_entry *fte = file_table[file_index];
    if ( fte == NULL) return ERR_NO_FILE;

    if (handle == -1) {
        fte->cur_dir_page = fte->dir_page[0];
        fte->cur_entry = (fte->cur_dir_page)->dir.entry;
        fte->page_nrecords = (fte->cur_dir_page)->dir.nent;
        fte->page_record = 0;
    } else {
        int file_index2 = INDEX_FROM_HANDLE(handle);
        register file_table_entry *fte2 = file_table[file_index2];
        if (file_index != file_index2) {
            // check if file2 exists and is linked to file
            if (fte2 == NULL) return ERR_NO_FILE;
            fte2 = fte;
            int linked = fte2->link;
            while ((linked > 0) && (linked != file_index2)) {
                fte2 = file_table[linked];
                linked = fte2->link;
            }
            if (linked != file_index2) return ERR_BAD_LINK;
        }
        fte2 = file_table[file_index2];
        fte->cur_dir_page = fte2->dir_page[PAGENO_FROM_HANDLE(handle)];
        fte->page_record = RECORD_FROM_HANDLE(handle);
        fte->page_nrecords = (fte->cur_dir_page)->dir.nent;
        fte->cur_entry = (fte->cur_dir_page)->dir.entry + (fte->page_record)*(fte->primary_len);
    }
    return 0;
}

int32_t f77name(xdfadd)(uint32_t *buf, uint32_t *donnees,
                        int32_t *fnelm, int32_t *fnbits, int32_t *fdatyp)
{
   int nelm = *fnelm, nbits = *fnbits, datyp = *fdatyp;
   int ier = c_xdfadd(buf, donnees, nelm, nbits, datyp);

   return (int32_t) ier;
}

int32_t f77name(xdfcle)(char *fkeyname, int32_t *fbit1, int32_t *flkey,
            int32_t *ftkey, int32_t *fdesc1, int32_t *fdesc2, F2Cl l1)
{
    char keyname[5] = {' ', ' ', ' ', ' ', '\0'};

    int lng = (l1 <= 4) ? l1 : 4;
    strncpy(keyname, fkeyname, lng);

    int desc1, desc2;
    int lkey = *flkey, tkey = *ftkey, bit1 = *fbit1;
    int err = c_xdfcle(keyname, bit1, lkey, tkey, &desc1, &desc2);

    *fdesc1 = (int32_t) desc1;
    *fdesc2 = (int32_t) desc2;

    return err;
}


int32_t f77name(xdfcls)(int32_t *fiun) {
    int iun = *fiun;
    return c_xdfcls(iun);
}


int32_t f77name(xdfcut)(uint32_t *buf,
            int32_t *fbitpos, int32_t *fnelm,
            int32_t *fnbits, int32_t *fdatyp
) {
    int nelm = *fnelm, nbits = *fnbits, datyp = *fdatyp, bitpos = *fbitpos;
    return (int32_t)c_xdfcut(buf, bitpos, nelm, nbits, datyp);
}


int32_t f77name(xdfdel)(int32_t *fhandle) {
    int handle = *fhandle;
    return (int32_t) c_xdfdel(handle);
}


int32_t f77name(xdfget)(int32_t *fhandle, uint32_t *buf) {
    int handle = *fhandle;
    return (int32_t) c_xdfget(handle, (buffer_interface_ptr) buf);
}


int32_t f77name(xdfgop)(char *foptname, char *foptc, int32_t *foptv, F2Cl ll1, F2Cl ll2) {
    int optv, err, l1=ll1, l2=ll2;
    char optname[257], optc[257];

    l1 = (l1 <= 256) ? l1 : 256;
    strncpy(optname, foptname, l1);
    optname[l1] = '\0';

    err = c_xdfgop(optname, optc, &optv);

    l2 = (l2 <= 256) ? l2 : 256;
    strncpy(foptc, optc, l2);

    *foptv = (int32_t) optv;
    return (int32_t) err;
}


int32_t f77name(xdfhdr)(uint32_t *buf, int32_t *addr, int32_t *lng,
                        int32_t *idtyp, int32_t *primk, int32_t *fnprim,
            int32_t *info, int32_t *fninfo)
{
    int nprim = *fnprim, ninfo = *fninfo;
    int l_addr, l_lng, l_idtyp;
    uint32_t l_primk[MAX_KEYS];
    uint32_t l_info[MAX_KEYS];

    int ier = c_xdfhdr((buffer_interface_ptr)buf, &l_addr, &l_lng, &l_idtyp, l_primk, nprim, l_info, ninfo);

    *addr =  (int32_t) l_addr;
    *lng =   (int32_t) l_lng;
    *idtyp = (int32_t) l_idtyp;

    if ((nprim > MAX_KEYS) || (ninfo >MAX_KEYS)) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: nprim=%d or ninfo=%d > MAX_KEYS must recompile\n",__func__,nprim,ninfo);
        return ERR_OUT_RANGE;
    }

    for (int i = 0; i < nprim; i++) {
        primk[i] = (int32_t) l_primk[i];
    }

    for (int i = 0; i < ninfo; i++) {
        info[i] = (int32_t) l_info[i];
    }

    return (int32_t) ier;
}


int32_t f77name(xdfimp)(int32_t *fiun, int32_t *stat, int32_t *fnstat,
                    ftnword_2 *pri, ftnword_2 *aux,
                    char *vers, char *appl, F2Cl l1, F2Cl l2)
{
    int iun = *fiun, nstat = *fnstat;
    char c_vers[257], c_appl[257];
    word_2 primk[MAX_KEYS], infok[MAX_KEYS];
    uint32_t lstat[12];

    int lng = (l1 <= 256) ? l1 : 256;
    strncpy(c_vers, vers, lng);
    c_vers[lng] = '\0';

    lng = (l2 <= 256) ? l2 : 256;
    strncpy(c_appl, appl, lng);
    c_appl[lng] = '\0';

    return (int32_t) c_xdfimp(iun, (uint32_t*)stat, nstat, (word_2 *)pri, (word_2 *)aux, c_vers, c_appl);
}


int32_t f77name(xdfini)(int32_t *fiun, uint32_t *buf, int32_t *fidtyp,
            int32_t *keys, int32_t *fnkeys, int32_t *info,int32_t *fninfo)
{
   int iun = *fiun, idtyp = *fidtyp, nkeys = *fnkeys, ninfo = *fninfo;

   return (int32_t) c_xdfini(iun, (buffer_interface_ptr)buf, idtyp, (uint32_t*)keys, nkeys, (uint32_t*)info, ninfo);
}


int32_t f77name(xdfins)(uint32_t *buf, uint32_t *donnees,
                        int32_t *fbitpos, int32_t *fnelm,
            int32_t *fnbits, int32_t *fdatyp)
{
    int nelm = *fnelm, nbits = *fnbits, datyp = *fdatyp, bitpos = *fbitpos;

    return (int32_t) c_xdfins(buf, donnees, bitpos, nelm, nbits, datyp);
}


int32_t f77name(xdflnk)(int32_t *liste, int32_t *fn)
{
    int n = *fn;

    return (int32_t) c_xdflnk(liste, n);
}


int32_t f77name(xdfloc)(int32_t *fiun, int32_t *fhandle, int32_t *primk,
            int32_t *fnprim)
{
    int iun = *fiun;
    int nprim = *fnprim;
    int handle = *fhandle;
    uint32_t l_primk[MAX_KEYS];

    if (nprim > MAX_KEYS) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: nprim=%d > MAX_KEYS must recompile\n",__func__,nprim);
        return(ERR_OUT_RANGE);
    }
    for (int i = 0; i < nprim; i++) {
        l_primk[i] = primk[i];
    }

    return (int32_t) c_xdfloc(iun, handle, l_primk, nprim);
}


int32_t f77name(xdfopn)(int32_t *fiun, char *mode,
            ftnword_2 *pri, int32_t *fnpri,
            ftnword_2 *aux, int32_t *fnaux,
            char *appl, F2Cl l1, F2Cl l2)
{
    int iun = *fiun, npri = *fnpri, naux = *fnaux;
    char c_mode[257];
    char c_appl[257];
    word_2 primk[MAX_KEYS], infok[MAX_KEYS];

    int lng = (l1 <= 256) ? l1 : 256;
    strncpy(c_mode, mode, lng);
    c_mode[lng] = '\0';

    lng = (l2 <= 256) ? l2 : 256;
    strncpy(c_appl, appl, lng);
    c_appl[lng] = '\0';

    if ((npri > MAX_KEYS) || (naux >MAX_KEYS)) {
        Lib_Log(APP_LIBFST,APP_ERROR,"%s: npri=%d or naux=%d > MAX_KEYS must recompil\n",__func__,npri,naux);
        return(ERR_OUT_RANGE);
    }
    for (int i=0; i < npri; i++) {
        primk[i].wd1 = pri[i].wd1;
        primk[i].wd2 = pri[i].wd2;
    }
    for (int i=0; i < naux; i++) {
        infok[i].wd1 = aux[i].wd1;
        infok[i].wd2 = aux[i].wd2;
    }

    return (int32_t) c_xdfopn(iun, c_mode, primk, npri, infok, naux, c_appl);
}


int32_t f77name(xdfopt)(char *foptname, char *foptc, int32_t *foptv,
            F2Cl ll1, F2Cl ll2)
{
    int optv = *foptv, l1=ll1, l2=ll2;
    char optname[257], optc[257];

    l1 = (l1 <= 256) ? l1 : 256;
    strncpy(optname, foptname, l1);
    optname[l1] = '\0';

    l2 = (l2 <= 256) ? l2 : 256;
    strncpy(optc, foptc, l2);
    optc[l2] = '\0';

    return (int32_t) c_xdfopt(optname, optc, optv);
}


int32_t f77name(xdfprm)(int32_t *fhandle, int32_t *addr, int32_t *lng,
                        int32_t *idtyp, int32_t *primk, int32_t *fnprim)
{
    int nprim = *fnprim;
    int handle = *fhandle;
    int l_addr, l_lng, l_idtyp;
    uint32_t l_primk[MAX_KEYS];

    int ier = c_xdfprm(handle, &l_addr, &l_lng, &l_idtyp, l_primk, nprim);
    *addr =  (int32_t) l_addr;
    *lng =   (int32_t) l_lng;
    *idtyp = (int32_t) l_idtyp;

    for (int i=0; i < nprim; i++) {
        primk[i] = (int32_t) l_primk[i];
    }

    return (int32_t) ier;
}


int32_t f77name(xdfput)(int32_t *fiun, int32_t *fhandle,
            uint32_t *buf)
{
    int handle = *fhandle;
    int iun = *fiun;

    return (int32_t) c_xdfput(iun, handle, (buffer_interface_ptr)buf);
}


int32_t f77name(xdfrep)(uint32_t *buf, uint32_t *donnees,
                        int32_t *fbitpos, int32_t *fnelm,
            int32_t *fnbits, int32_t *fdatyp)
{
    int nelm = *fnelm, nbits = *fnbits, datyp = *fdatyp, bitpos = *fbitpos;

    return (int32_t) c_xdfrep(buf, donnees, bitpos, nelm, nbits, datyp);
}


int32_t f77name(xdfsta)(int32_t *fiun, int32_t *stat, int32_t *fnstat,
            ftnword_2 *pri, int32_t *fnpri,
            ftnword_2 *aux, int32_t *fnaux,
            char *vers, char *appl, F2Cl l1, F2Cl l2)
{
   int iun = *fiun, npri = *fnpri, naux = *fnaux, nstat = *fnstat;
   char c_vers[257], c_appl[257];

   int ier = c_xdfsta(iun, (uint32_t*)stat, nstat, (word_2 *)pri, npri, (word_2 *)aux, naux, c_vers, c_appl);

   int lng = (l1 <= 256) ? l1 : 256;
   c_vers[lng] = '\0';
   strncpy(vers, c_vers, lng);

   lng = (l2 <= 256) ? l2 : 256;
   c_appl[lng] = '\0';
   strncpy(appl, c_appl, lng);

   return (int32_t) ier;
}


int32_t f77name(xdfupd)(int32_t *fiun, uint32_t *buf, int32_t *fidtyp,
            int32_t *keys, int32_t *fnkeys,
            int32_t *info, int32_t *fninfo)
{
    int iun = *fiun, idtyp = *fidtyp, nkeys = *fnkeys, ninfo = *fninfo;

    return (int32_t) c_xdfupd(iun, (buffer_interface_ptr)buf, idtyp, (uint32_t*)keys, nkeys, (uint32_t*)info, ninfo);
}


int32_t f77name(xdfuse)(int32_t *fsrc_unit, int32_t *fdest_unit)
{
    int src_unit = *fsrc_unit, dest_unit = *fdest_unit;

    return (int32_t) c_xdfuse(src_unit, dest_unit);
}


int32_t f77name(xdfxtr)(uint32_t *buf, uint32_t *donnees,
                        int32_t *fbitpos, int32_t *fnelm,
            int32_t *fnbits, int32_t *fdatyp)
{
    int nelm = *fnelm, nbits = *fnbits, datyp = *fdatyp, bitpos = *fbitpos;

    return (int32_t) c_xdfxtr(buf, donnees, bitpos, nelm, nbits, datyp);
}

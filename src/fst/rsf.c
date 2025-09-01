//! \file
//! RSF file format main implementation

#include "rsf_internal.h"

#include <pthread.h>

static pthread_mutex_t rsf_global_mutex = PTHREAD_MUTEX_INITIALIZER;

// =================================  table of pointers to rsf files (slots) =================================
static RSF_File ** rsf_files = NULL;      //!< Global table of pointers to RSF files (slot table)
static size_t max_rsf_files_open = 0;  //!< Size of the list of open RSF files

//!> Number of RSF-reserved 32-bit words with respect to version 
static const uint8_t num_rsf_reserved[] = {
    RSF_META_RESERVED_V0,
    RSF_META_RESERVED_V1,
};

static void RSF_Finalize(void);
static int32_t RSF_File_lock(const int32_t fd, const int do_lock);
static int32_t RSF_Ensure_new_segment(RSF_File *fp, const uint64_t min_segment_size);
static rsf_rec_type RSF_Read_record(RSF_File* fp, const uint64_t address, void* dest, const size_t num_bytes);
static int32_t RSF_Close_compact_segment(RSF_File* fp);


//! Initialize the RSF library:
//!  - Allocate global table of pointers (slot table) to rsf files if not already done
//!  - Assign a function to run at program exit (to clean up the table and close any open file)
//! Uses the RSF global mutex
//! \return table address if successful, NULL if table cannot be allocated
static void* RSF_Initialize(void) {
    if (rsf_files != NULL) return rsf_files;                    // slot table already allocated

    if (sizeof(num_rsf_reserved) <= RSF_VERSION_COUNT) {
        Lib_Log(APP_LIBFST, APP_FATAL,
                "%s: The number of entries in 'num_rsf_reserved' (%d) does not match RSF version count +1 (%d)\n",
                __func__, sizeof(num_rsf_reserved), RSF_VERSION_COUNT); 
        return NULL;
    }

    // --- START critical region ---
    pthread_mutex_lock(&rsf_global_mutex);

    if (atexit(RSF_Finalize) != 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to register function with atexit()\n", __func__);
        goto unlock;
    }

    if (rsf_files == NULL) { // Check again after locking
        max_rsf_files_open = get_max_open_files();
        max_rsf_files_open = (max_rsf_files_open <= 131072) ? max_rsf_files_open : 131072; // never more than 128K files
        rsf_files = calloc(max_rsf_files_open, sizeof(RSF_File *));       // allocate zero filled table for max number of allowed files
    }

unlock:
    pthread_mutex_unlock(&rsf_global_mutex);
    // --- END critical region ---

    return rsf_files;
}

//! Look through list of files to find any that has been left open, and close them.
static void RSF_Finalize(void) {
    if (rsf_files != NULL) {
        for (int i = 0; i < max_rsf_files_open; i++) {
            if (rsf_files[i] != NULL) {
                RSF_File* fp = rsf_files[i];
                // Only print warning for files open for writing
                if ((fp->mode & RSF_RW) == RSF_RW) {
                    Lib_Log(APP_LIBFST, APP_WARNING, "%s: File \"%s\" is still open, so we will close it now to avoid "
                            "corruption.\n",
                            __func__, fp->name);
                }
                RSF_Close_file((RSF_handle){.p = fp});
            }
        }
    }
}

//! Find a free slot in global slot table and set it to p
//! \return Slot number if successful, -1 if table is full
static int32_t RSF_Set_file_slot(
    RSF_File * const fp //!< [in] Pointer to an RSF_file structure
) {
    if (rsf_files == NULL) rsf_files = RSF_Initialize();  // first time around, allocate table
    if (rsf_files == NULL) return -1;

    for (int i = 0; i < max_rsf_files_open; i++) {
        if (rsf_files[i] == NULL) {
            // Assign number atomically
            if (__sync_val_compare_and_swap(&(rsf_files[i]), NULL, fp) == NULL) {
                Lib_Log(APP_LIBFST, APP_EXTRA, "%s: RSF file table slot %d assigned, p = %p\n", __func__, i, fp);
                return i;              // slot number
            }
        }
    }
    return -1;     // pointer not found or table full
}

//! Remove existing pointer p from global slot table
//! *Uses the global RSF mutex*
//! \return former slot number if successful , -1 if error
static int32_t RSF_Purge_file_slot(
    void *p //!< pointer to RSF_file structure
) {
    if (rsf_files == NULL) return -1;  // no file table

    for (int i = 0; i < max_rsf_files_open; i++) {
        if (rsf_files[i] == p) {
            // --- START critical region ---
            pthread_mutex_lock(&rsf_global_mutex);
            if (rsf_files[i] == p) { // Check again
                rsf_files[i] = (void *) NULL;
                Lib_Log(APP_LIBFST, APP_EXTRA, "%s: RSF file table slot %d freed, p = %p\n", __func__, i, p);
            }
            pthread_mutex_unlock(&rsf_global_mutex);
            // --- END critical region ---
            return i; // slot number
        }
    }
    return -1;   // not found
}

// =================================  utility functions =================================

// 32 <-> 64 bit key converter code will need to be adjusted so that 32 bit keys can be recognized
// as XDF keys or RSF keys
// XDF key > -1 is a valid search criterion
// a negative value is invalid
//
// current choice below (to be revised as needed)
// 1 1 20 10 ? (sign / 1 / index / slot ) (30 bit effective key, 1024 files, up to 1M records)
// slot number : lower 10 bits
// index       : bits 10->29
// XDF marker  : bit 30 (XDF key if 0, RSF key if 1)
// invalid     : bit 31 (nonzero = invalid key)
//
// XDF HANDLE
//    sign page_no record_index file_index
//      1     12         9         10
// #define MAKE_RND_HANDLE(pageno,recno,file_index) ((file_index &0x3FF) | ((recno & 0x1FF)<<10) | ((pageno & 0xFFF)<<19))
// (it will be assumed that page number will not exceed 2047 (11 bits), i.e. bit 30 will be 0 for a valid XDF handle)
// in the case of a valid XDF key, recno is expected to be <= 255
//

//! Extract file slot from a 64-bit record key (it's in the upper 32 bits).
static inline uint32_t key64_to_file_slot(int64_t key64) {
    return key64 >> 32;
}

//! Extract record index from a 64-bit record key (it's in the lower 32 bits).
static inline int32_t key64_to_index(const int64_t key64) {
    return ((int32_t)(key64 & 0xFFFFFFFFl)) - 1;
}

int32_t RSF_Key64_to_index(const int64_t key64) {
    return key64_to_index(key64);
}

static inline int64_t make_key(
    const int32_t slot,      //!< starts at 0
    const int32_t record_id  //!< starts at 0
) {
    return ((int64_t)(slot + 1) << 32) + record_id + 1;
}

int64_t RSF_Make_key(
    const int32_t slot,     //!< starts at 0
    const int32_t record_id //!< Starts at 0
) {
    return make_key(slot, record_id);
}

//! Generate a 32 bit record key (sign:1, one:1 , index : 20, slot:10)
//! from a 64 bit key (slot:32, index:32)
//! code may have to be added to deal properly with negative key64 values
//! code may be added to check that slot and index make sense
//! \return A 32-bit key, or a large negative value in case of error
int32_t RSF_Key32(int64_t key64){
    const uint32_t index = key64_to_index(key64);
    if (index > 0xFFFFF)                 // ERROR, index larger than 20 bits
        return (1 << 31);                 // return huge negative value

    const uint32_t slot = key64_to_file_slot(key64);
    if (slot > 0x3FF)                    // ERROR, slot number larger than 10 bits
        return (1 << 31);                // return huge negative value

    return ((index + 1) << 10) | slot | (1 << 30);  // index, slot, and bit 30 set to 1 to indicate RSF handle
}

//! Check whether the given 32-bit key is a valid RSF key and whether it _could_ be a valid XDF key
//! \return 1 for a valid 32 bit RSF key
//! \return 0 for a possibly valid 32 bit XDF key
//! \return -1 if invalid 32 bit key
int32_t RSF_Key32_type(int32_t key32){
    if (key32 < 0) return BAD_KEY32;
    return (key32 >> 30) ? RSF_KEY32 : XDF_KEY32;
}

//! \copydoc key64_to_file_slot
uint32_t RSF_Key64_to_file_slot(int64_t key64) {
    return key64_to_file_slot(key64);
}

//! generate a 64 bit record key  (slot:32, index:32)
//! from a 32 bit RSF key (sign:1, one:1 , index : 20, slot:10)
//! code may have to be added to deal properly with negative key32 values
//! code may be added to check that slot and index make sense
//! an error should be returned if bit 30 is 0, i.e. ((key32 >> 30) & 1) == 0
int64_t RSF_Key64(int32_t key32){
    uint64_t key64 = (key32 & 0x3FF);          // slot number (10 bits)
    key64 <<= 32;                              // shift to proper position
    key64 |= ((key32 >> 10) & 0xFFFFF);        // add 20 bit record index
    return key64;
}

//! Retrieve a handle to the file to which the given key belongs
RSF_handle RSF_Key32_to_handle(int32_t key32) {
    RSF_handle h;
    const int64_t  key64 = RSF_Key64(key32);
    const uint32_t slot  = key64_to_file_slot(key64);
    h.p = rsf_files[slot - 1];
    return h;
}

//! Retrieve a handle to the file to which the given key belongs
RSF_handle RSF_Key64_to_handle(int64_t key64) {
    RSF_handle h;
    const uint32_t slot = key64_to_file_slot(key64);
    h.p = rsf_files[slot - 1];
    return h;
}

//! Whether the given file has an open segment
static inline int has_open_segment(const RSF_File* fp) {
    return fp->next_write > 0;
}

//! Whether the given file has sparse segments
static inline int is_segment_sparse(const RSF_File* fp) {
    return fp->seg_max != 0;
}

//! Check if given file points to a valid RSF_File structure
//! \return Slot number + 1 if valid structure, otherwise return 0
static uint32_t RSF_Is_file_valid(
    //! Pointer to RSF_File structure
    const RSF_File * const fp
) {
    if (fp == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: file handle is NULL\n", __func__);
        return 0;                   // fp is a NULL pointer
    }

    if (fp->version != RSF_VERSION) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Looks like the version token (%d) is wrong (should be %d)."
                " This pointer does not point to an actual RSF_File struct.\n", __func__, fp->version, RSF_VERSION);
        return 0;
    }

    if (fp->fd < 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid fd < 0 (%d)\n", __func__, fp->fd);
        return 0;                   // file is not open, ERROR
    }

    // check validity of fp->slot
    if ((fp->slot < 0) || (fp->slot >= max_rsf_files_open)) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: slot number found in file handle is invalid\n", __func__);
        return 0;                   // not in file handle table
    }
    if (fp != rsf_files[fp->slot] ) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: inconsistent slot data %p %p, slot = %d\n",
                __func__, fp, rsf_files[fp->slot], fp->slot);
        return 0;                   // inconsistent slot
    }
    return fp->slot + 1;
}

//! Verify that the application code given when opening the file matches the one from the
//! file on disk (given SOS)
//! \return 0 if they match, -1 if they don't
static inline int check_application_code(
    const RSF_File * const fp,
    const start_of_segment * const sos
) {
    for (int i = 0; i < 4; i++) {
        if (fp->appl_code[i] != sos->sig1[4+i]) {
            Lib_Log(APP_LIBFST, APP_INFO, "%s: Given app code (%c%c%c%c) does not match the one in file (%c%c%c%c)\n",
                    __func__, fp->appl_code[0], fp->appl_code[1], fp->appl_code[2], fp->appl_code[3],
                    sos->sig1[4], sos->sig1[5], sos->sig1[6], sos->sig1[7]);
        return -1;
        }
    }
    return 0;
}

// =================================  directory management =================================

// the directory is managed using
//  - an array of pointers (one per record) into the chained list of metadata contents blocks
//  - a chained list of metadata contents blocks
// if/when the array of pointers gets full, it is reallocated with a larger size
// if/when a metadata contents block is full, a new block is created and chained into the list

// - create a new directory metadata contents block
// - insert it into the block list associated with RSF_File pointer fp (insertion at the beginning of the list)
// the block will be able to contain at least min_block bytes
// (VARIABLE length metadata directories)
// fp        pointer to RSF file control structure
// min_block minimum size for the block
// return    address of the block (NULL in case of error)
static directory_block *RSF_Add_vdir_block(
    RSF_File * const fp,
    uint32_t min_block
) {
    size_t sz = sizeof(directory_block) + ( (min_block > DIR_BLOCK_SIZE) ? min_block : DIR_BLOCK_SIZE );
    directory_block * const dd = (directory_block *) calloc(sz, sizeof(char));         // allocate a block filled with nulls
    if (dd == NULL) return NULL;            // malloc failed
    dd->next = fp->dirblocks;             // next block pointer -> start of current list
    dd->cur = dd->entries;                // beginning of entries storage area (insertion point)
    dd->top = (uint8_t *) dd + sz;                     // last usable addess in block is dd->top -1
    fp->dirblocks = dd;                   // new start of blocks list
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: added block of size %ld, next = %p, fp->dir = %p\n",
            __func__, sz, dd->next, fp->dirblocks);
    return dd;
}

// manage metadata directory structures associated with file fp
// make sure that there is room for at least one more entry
// fp    pointer to RSF file control structure
static void RSF_Vdir_setup(RSF_File *fp){
    if (fp->dirblocks == NULL) {                                       // first time around
        RSF_Add_vdir_block(fp, 0);                                     // create first entries block (default size)
        fp->vdir_size = DISK_VDIR_BASE_SIZE;                           // record size of an empty directory
    }
    if (fp->vdir_used >= fp->vdir_slots) {                             // table is full, reallocate with a larger size
        fp->vdir_slots += DIR_SLOTS_INCREMENT;                         // increase number of slots
        // reallocate vdir table with a larger size
        fp->vdir = (vdir_entry **) realloc(fp->vdir, fp->vdir_slots * sizeof(void *));
        for(int i = fp->vdir_used; i < fp->vdir_slots; i++) {
            fp->vdir[i] = NULL;                                          // nullify new slots
        }
    }
}

//! add a new VARIABLE length metadata entry into memory directory (for a new record)
//! \return index key of record, -1 in case of error (upper 32 bits, file slot number, lower 32 bits record number)
//!         both slot number and record number in origin 1, so 0 would be an invalid record index key
static int64_t RSF_Add_vdir_entry(
    RSF_File * const fp,           //!< pointer to RSF file control structure
    const uint32_t * const meta,   //!< pointer to metadata array (32 bit elements)
    uint32_t mlr,           //!< record (lower 16 bits) and directory (upper 16) metadata lengths
    uint64_t wa,            //!< byte address of record in file
    uint64_t rl,            //!< record length in bytes (should always be a multiple of 4)
    uint32_t element_size,  //!<
    uint64_t entry_offset   //!< byte address of directory entry in file
) {
    // fprintf(stderr, "RSF_Add_vdir_entry: meta data length = 0x%x, record length %d, elem size %d\n", mlr, rl, element_size);

    if (fp == NULL) return -1;           // invalid file struct pointer
    int64_t slot = RSF_Is_file_valid(fp);
    if (slot == 0) return -1;             // something not O.K. with fp if RSF_Is_file_valid returned 0
    slot <<= 32;                          // file slot number (origin 1)
    RSF_Vdir_setup(fp);
    directory_block * dd = fp->dirblocks; // current directory entries block

    uint32_t mld = DIR_ML(mlr);          // length of directory metadata
    mlr = REC_ML(mlr);                  // length of record metadata

    // needed size for entry to be added
    int needed = sizeof(vdir_entry) +        // base size for entry to be added
                 mld * sizeof(uint32_t);    // directory metadata size
    if (dd->top - dd->cur < needed) {     // add a new block if no room for entry in current block
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: need %d, have %ld, allocating a new block\n",
                __func__, needed, dd->top - dd->cur);
        dd = RSF_Add_vdir_block(fp, needed);
    }
    if (dd == NULL) return -1;          // new block creation failed
    vdir_entry * const entry = (vdir_entry *) dd->cur;     // pointer to insertion point in block
    RSF_64_to_32(entry->wa, wa);        // record address in file
    RSF_64_to_32(entry->rl, rl);        // record length
    RSF_64_to_32(entry->entry_offset, entry_offset);
    entry->ml = DRML_32(mld, mlr);      // composite entry with directory and record metadata length
    entry->dul = element_size;          // data element size
    for (int i = 0; i < mld; i++) {
        entry->meta[i] = meta[i];         // copy directory metadata
    }
    dd->cur += needed;                  // update insertion point

    uint8_t version;
    rsf_rec_class rec_class;
    rsf_rec_type rec_type;
    extract_meta0(meta[0], &version, &rec_class, &rec_type);
    if (rec_type == RT_DEL) fp->num_deleted_records++;

    fp->vdir[fp->vdir_used] = entry;     // enter pointer to entry into vdir array
    fp->vdir_used++;                     // update number of directory entries in use
    fp->vdir_size += needed;             // update worst case directory size (used to compute directory record size)
    int64_t index = fp->vdir_used;               // update number of entries in use
    index |= slot;                       // record key (file slot , record index) (both ORIGIN 1)
    return index;
}


//! Retrieve the contents of a record memory directory entry using key (file slot , record index) (both ORIGIN 1)
//! \return length of directory metadata in 32 bit units, -1 if error
static int32_t RSF_Get_vdir_entry(
    //! Pointer to RSF file control structure
    RSF_File * const fp,
    //! [in] Record key from RSF_Add_vdir_entry, RSF_Lookup, RSF_Scan_vdir, etc ...
    int64_t key,
    //! [out] Record address in file (bytes)
    uint64_t * const wa,
    //! [out] Record length (bytes)
    uint64_t * const rl,
    //! [out] Pointer to memory directory metadata for record
    uint32_t ** const meta
) {
    *wa = 0;           // precondition for failure
    *rl = 0;
    *meta = NULL;

    const int32_t slot = RSF_Is_file_valid(fp);

    if ( ! slot ) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid file struct pointer\n", __func__);
        return -1;
    }
    if ( slot != (key >> 32) ) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: inconsistent slot in key\n", __func__);
        return -1;
    }
    int32_t indx = (key & 0x7FFFFFFF) - 1;
    if ( indx  >= fp->vdir_used ) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid record number\n", __func__);
        return -1;
    }

    vdir_entry * const ventry = fp->vdir[indx];
    *wa = RSF_32_to_64(ventry->wa);   // file address
    *rl = RSF_32_to_64(ventry->rl);   // record length
    *meta = &(ventry->meta[0]);       // pointer to metadata
    return DIR_ML(ventry->ml);        // return directory metadata length
}


//! scan vdir (VARIABLE length metadata) of file fp to find a record where (metadata & mask)  matches (criteria & mask)
//! criteria, mask are arrays of 32 bit items (like metadata)
//! if a matching function has been associated with the file, it is used insteaad of the default match function
//!
//! \return  key for record file slot(index) in upper 32 bits, record index in lower 32 bits (both in origin 1)
//!          a negative number in case of error (or no match?)
static int64_t RSF_Scan_vdir(
    //!> Handle to open file
    RSF_File *fp,
    //!> Start scan one position after position described by key0
    //!> (if key0 == 0, start from beginning of file directory)
    int64_t  key0,
    //!> What the user is looking for [???]
    uint32_t *criteria,
    //!> 0 bits in mask imply don't care conditions for those bits [???]
    uint32_t *mask,
    //!> Length of both criteria and mask arrays
    uint32_t lcrit,
    //!> [in,out] Address of record in file if found (untouched if no match)
    uint64_t *wa,
    //!> [in,out] Record length if found (untouched if no match)
    uint64_t *rl
) {
    // uint32_t mask0 = 0xFFFFFFFF;   // default mask0 is all bits active
    uint32_t mask0 = (~0);   // default mask0 is all bits active
    int reject_a_priori;

    uint8_t crit_version = 0;
    rsf_rec_class crit_class = RC_NULL;
    rsf_rec_type crit_type = RT_NULL;

    const int64_t slot = RSF_Is_file_valid(fp);
    if (!slot) {
        // something wrong with fp
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: key = %16.16lx, invalid file reference\n", __func__, key0);
        return ERR_NO_FILE;
    }

    if ( (key0 != 0) && (key64_to_file_slot(key0) != slot) ) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: key = %16.16lx, inconsistent file slot\n", __func__, key0);
        // slot in key different from file table slot
        return ERR_BAD_UNIT;
    }

    // slot is origin 1 (zero is invalid)
    int64_t key = (slot << 32);                      // move to upper 32 bits
    if ( key0 < -1 ) key0 = 0;                // first record position for this file
    int index = key0 & 0x7FFFFFFF;               // starting ordinal for search (one more than what key0 points to)
    if (index >= fp->vdir_used) {
        Lib_Log(APP_LIBFST, APP_TRIVIAL, "%s: key = %16.16lx, beyond last record\n", __func__, key0);
        return ERR_NOT_FOUND;
    }

    if (criteria == NULL){                     // no criteria specified, anything matches
        lcrit = 0;
    } else {                                    // criteria were specified
        // mask[0] == 0 will deactivate any record type / class based selection
        mask0 = mask ? mask[0] : mask0;                  // set mask0 to default if mask is NULL

        uint8_t mask_version;
        rsf_rec_class mask_class;
        rsf_rec_type mask_type;
        extract_meta0(mask0, &mask_version, &mask_class, &mask_type);
        extract_meta0(criteria[0], &crit_version, &crit_class, &crit_type);

        crit_type &= mask_type;            // low level record type match target

        // check for valid type for a data record (RT_DATA, RT_XDAT, RT_FILE, RT_CUSTOM->RT_DEL-1 are valid record types)
        // if record type is not a valid selection criterium , selection will ignore it
        // crit_type == 0 means no selection based upon record type (except for deleted records, which we always ignore)
        if (crit_type >= RT_DEL || crit_type == RT_SOS || crit_type == RT_EOS || crit_type == RT_VDIR) crit_type = RT_NULL;

        if (crit_class == 0) crit_class = fp->rec_class;     // if 0, use default class for file
        crit_class &= mask_class;                           // apply mask to get final low level record class match target
        // crit_class == 0 at this point means record class will not be used for selection
    }

    // get metadata match function associated to this file, if none associated, use default function
    RSF_Match_fn * scan_match = (fp->matchfn != NULL) ? fp->matchfn : &RSF_Default_match;
    //   scan_match = fp->matchfn;                // get metadata match function associated to this file
    //   if(scan_match == NULL)
    //     scan_match = &RSF_Default_match;       // no function associated, use default function

    if (App_LogLevel(NULL) >= APP_EXTRA) {
        char buffer[lcrit * 10 + 1024];
        char* ptr = buffer;
        for (int i = 0; i < lcrit; i++) {
            ptr += snprintf(ptr, 10, "%.8x ", criteria[i]);
            if (i % 8 == 7) ptr += snprintf(ptr, 2, "\n");
        }
        Lib_Log(APP_LIBFST, APP_EXTRA, "%s: Criteria = \n%s\n", __func__, buffer);

        ptr = buffer;
        for (int i = 0; i < lcrit; i++) {
            ptr += snprintf(ptr, 10, "%.8x ", mask[i]);
            if (i % 8 == 7) ptr += snprintf(ptr, 2, "\n");
        }
        Lib_Log(APP_LIBFST, APP_EXTRA, "%s: Mask = \n%s\n", __func__, buffer);
    }

    vdir_entry * ventry = NULL;
    for (; index < fp->vdir_used; index++ ) { // loop over records in directory starting from requested position
        Lib_Log(APP_LIBFST, APP_EXTRA, "%s: Looking at record with key 0x%x\n", __func__, key + index + 1);
        ventry = fp->vdir[index];              // get entry
        if (lcrit == 0) goto MATCH;             // no criteria specified, everything matches
        const uint32_t * const meta = ventry->meta;                   // entry metadata from directory

        uint8_t rec_version;
        rsf_rec_class rec_class;
        rsf_rec_type rec_type;
        extract_meta0(meta[0], &rec_version, &rec_class, &rec_type);

        // the first element of meta, mask, criteria is pre-processed here, and sent to matching function
        reject_a_priori = ((crit_type != 0) && (crit_type != rec_type)) || (rec_type == RT_DEL);    // record type mismatch ?
        //     if( reject_a_priori )      continue;    // record type mismatch

        reject_a_priori = reject_a_priori | ( (crit_class != 0) && ( (crit_class & rec_class) == 0 ) );   // class mismatch ?
        //     if( reject_a_priori ) continue;   // class mismatch, no bit in common

        int dir_meta = DIR_ML(ventry->ml);                    // length of directory metadata
        // more criteria than metadata or less criteria than metadata will be dealt with by the matching function
        // the first element of criteria, meta, mask (if applicable) will be ignored
        // dir_meta is the number of metadata elements
        // NOTE: we may want to pass reject_a_priori and all criteria, mask and directory metadata
        // to the matching function in case it makes use of it
        // therefore NOT BUMPING pointers and NOT DECREASING sizes
        if ((*scan_match)(criteria, meta, mask, lcrit, dir_meta, reject_a_priori) == 1 ) {   // do we have a match ?
            goto MATCH;
        }
        // if((*scan_match)(criteria+1, meta+1, mask ? mask+1 : NULL, lcrit-1, dir_meta-1, reject_a_priori) == 1 ){   // do we have a match ?
        //   goto MATCH;
        // }
    }

    // falling through, return an invalid key
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: key = %16.16lx, no match found\n", __func__, key0);
    return ERR_NOT_FOUND;

MATCH:
    // upper 32 bits of key contain the file "slot" number (origin 1)
    key = key + index + 1;               // add record number (origin 1) to key
    *wa = RSF_32_to_64(ventry->wa);      // address of record in file
    *rl = RSF_32_to_64(ventry->rl);      // record length
    Lib_Log(APP_LIBFST, APP_EXTRA, "%s: SUCCESS, key = %16.16lx\n\n", __func__, key);
    return key;                          // return key value containing file "slot" and record index
}

//! read file directory (all segments) into memory directory
//! this function reads both FIXED and VARIABLE metadata directories
//! \return number of records found in segment directories (-1 upon error)
static int32_t RSF_Read_directory(
    RSF_File *fp //!< pointer to file control struct
) {
    if (fp->dir_read > 0) {  // redundant call, directory already read
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: directory ALREADY READ %d entries\n", __func__, fp->dir_read);
        return fp->dir_read;
    }

    int32_t entries = 0;
    off_t segment_offset = 0;                                             // first segment at beginning of file
    int32_t num_directories = 0;
    int32_t num_segments = 0;
    while (1) {                                                 // loop over segments
        lseek(fp->fd, segment_offset , SEEK_SET);              // go to start of segment

        start_of_segment sos;
        ssize_t nc = read(fp->fd, &sos, sizeof(start_of_segment));     // try to read start of segment record
        if (nc < sizeof(start_of_segment)) break;               // end of file reached, done
        num_segments++;                                        // bump segment count

        if (RSF_Rl_sor(sos.head, RT_SOS) == 0) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid start of segment, %d entries, %d segments\n", __func__, "", entries, num_segments);
            return -1;
        }

        const uint64_t segment_size = RSF_32_to_64(sos.sseg);
        uint64_t sos_seg = RSF_32_to_64(sos.seg);                      // would be 0 for a sparse segment
        if (segment_size == 0) break;                               // open, compact segment, this is the last segment
        // remember free space in non open sparse segments
        if (sos_seg == 0 && sos.head.rlm == 0) {                 // ONLY process sparse segments NOT currently open for write
            if (fp->sparse_segs == NULL) {                          // sparse segments table not allocated yet
                Lib_Log(APP_LIBFST, APP_DEBUG, "%s: allocating sparse_segs table\n", __func__);
                fp->sparse_segs = malloc(1024 * sizeof(sparse_entry));
                fp->sparse_table_size = 1024;
                fp->sparse_table_used = 0;
            }
            fp->sparse_segs[fp->sparse_table_used].base = segment_offset;
            fp->sparse_segs[fp->sparse_table_used].size = segment_size - sizeof(start_of_segment);
            Lib_Log(APP_LIBFST, APP_DEBUG, "%s: segment %d sparse at %12.12lx, space available = %ld, rlm = %d\n",
                    __func__, num_segments-1, fp->sparse_segs[fp->sparse_table_used].base,
                    fp->sparse_segs[fp->sparse_table_used].size, sos.head.rlm);
            if (fp->sparse_table_used < fp->sparse_table_size - 1) fp->sparse_table_used++;  // do not overflow table
        }
        const uint64_t vdir_offset  = RSF_32_to_64(sos.vdir);  // offset of vdir within segment
        const uint64_t vdir_size    = RSF_32_to_64(sos.vdirs); // vdir size from start of record

        Lib_Log(APP_LIBFST, APP_EXTRA, "%s: segment %d (size %ld) dir offset: %ld (size %ld)\n",
                __func__, num_segments - 1, segment_size, vdir_offset, vdir_size);

        if(vdir_size > 0 && vdir_offset > 0) {                     // non empty VARIABLE metadata segment
        int32_t l_entries = 0;
        num_directories++;
        disk_vdir * vdir = (disk_vdir *) malloc(vdir_size);              // allocate space to read segment directory

        const rsf_rec_type read_status = RSF_Read_record(fp, segment_offset + vdir_offset, vdir, vdir_size);

        if (read_status == RT_INVALID) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to read entire directory, only %lu / %lu bytes\n",
                    __func__, nc, vdir_size);
            return -1;
        }

        if (read_status != RT_VDIR) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid record type for directory. Looks like this file is corrupted.\n", __func__);
            if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_DEBUG)
            print_start_of_record(&vdir->sor);
            return -1;
        }

        char * e = (char *) &(vdir->entry[0]);
        uint64_t entry_offset = segment_offset + vdir_offset + sizeof(disk_vdir);
        for(int i = 0; i < vdir->entries; i++){
            l_entries++;
            entries++;
            const vdir_entry* ventry = (const vdir_entry *) e;
            const uint64_t wa = RSF_32_to_64(ventry->wa) + segment_offset;
            const uint64_t rl = RSF_32_to_64(ventry->rl);
            const uint32_t* meta = &(ventry->meta[0]);
            RSF_Add_vdir_entry(fp, meta, ventry->ml, wa, rl, ventry->dul, entry_offset);

            const int meta_length = DIR_ML(ventry->ml);
            const uint64_t entry_size = sizeof(vdir_entry) + meta_length * sizeof(uint32_t);
            entry_offset += entry_size;
            e += entry_size;
        }
        if (vdir) free(vdir);                                 // free memory used to read segment directory from file
            vdir = NULL;                                         // to avoid a potential double free
            Lib_Log(APP_LIBFST, APP_DEBUG, "%s: found %d entries in segment %d\n", __func__, l_entries, num_segments-1);
        }
        segment_offset += segment_size;                        // offset of the start of the next segment
    }  // while(1) loop over segments
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: directory entries = %d, num_segments = %d, num_directories = %d \n",
            __func__, entries, num_segments, num_directories);
    fp->dir_read = entries;
    return entries;                                          // return number of records found in segment directories
}

//! Compute the size of the (combined) in-memory directory record. This is the sum of what is
//! on disk and what was written since (but not yet committed to disk).
//! \return Directory record size associated with file associated with fp
static size_t RSF_Vdir_record_size(const RSF_File * const fp){
    size_t dir_rec_size = sizeof(disk_vdir) + sizeof(end_of_record);                      // fixed part + end_of_record
    for (int32_t i = fp->dir_read; i < fp->vdir_used; i++){                                // add entries (wa/rl/ml/metadata)
        int32_t ml = DIR_ML(fp->vdir[i]->ml);
        dir_rec_size += ( sizeof(vdir_entry) + ml * sizeof(uint32_t) );  // fixed part + metadata
    }
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: dir rec size = %ld %ld\n", __func__, dir_rec_size, fp->vdir_size);
    return dir_rec_size;
}

//! Write directory to file from memory directory (variable length metadata).
//! It is NOT assumed that the file is correctly positioned.
//! Thread safe using the given file's mutex.
//! \return Directory size
static int64_t RSF_Write_vdir(RSF_File * const fp){
    int32_t slot = RSF_Is_file_valid(fp);
    if ( ! slot ) return 0;             // something wrong with fp
    if (fp->dir_read >= fp->vdir_used) return 0;               // nothing to write

    if (fp->vdir == NULL) return 0;
    size_t dir_rec_size = RSF_Vdir_record_size(fp);          // size of directory record to be written
    uint8_t * const p = malloc(dir_rec_size);
    if ( p == NULL ) return 0;      // allocation failed

    disk_vdir * const vdir = (disk_vdir *) p;
    end_of_record * const eorp = (end_of_record *) (p + dir_rec_size - sizeof(end_of_record));  // point to eor at end of record

    vdir->sor = (start_of_record)SOR;                 // Initialize to default (to avoid uninitialized memory)
    vdir->sor.rt = RT_VDIR;                           // adjust start of record
    vdir->sor.rlm = 1;                                // indicates variable length metadata
    RSF_64_to_32(vdir->sor.rl, dir_rec_size);         // record length
    vdir->sor.zr = ZR_SOR;                            // SOR marker
    vdir->entries = fp->vdir_used - fp->dir_read;     // number of directory entries in record

    *eorp = (end_of_record)EOR;                       // Initialize to default (to avoid uninitialized memory)
    eorp->rt = RT_VDIR;                               // adjust end or record
    eorp->rlm = 1;                                    // indicates variable length metadata
    RSF_64_to_32(eorp->rl, dir_rec_size);             // record length
    eorp->zr = ZR_EOR;                                // EOR marker

    uint8_t * e = (uint8_t *) &(vdir->entry[0]);                // start of directory metadata portion

    // do not start at entry # 0, but entry # fp->dir_read (only write entries from "active" segment)
    // when "fusing" segments, fp->dir_read will be reset to 0
    Lib_Log(APP_LIBFST, APP_DEBUG,
            "%s: skipping %d records, segment base = %lx, dir_rec_size = %ld, dir_read = %d, vdir_used = %d\n",
            __func__, fp->dir_read, fp->seg_base, dir_rec_size, fp->dir_read, fp->vdir_used);

    for (int i = fp->dir_read; i < fp->vdir_used; i++){             // fill from in memory directory
        vdir_entry * const entry = (vdir_entry *) e;
        int32_t ml = DIR_ML(fp->vdir[i]->ml);
        size_t dir_entry_size = sizeof(vdir_entry) + ml * sizeof(uint32_t);
        memcpy(entry, fp->vdir[i], dir_entry_size);   // copy entry from memory directory
        uint64_t wa64 = RSF_32_to_64(entry->wa);               // adjust wa64 (file address -> offset in segment)
        wa64 -= fp->seg_base;
        RSF_64_to_32(entry->wa, wa64);
        e += dir_entry_size;
    }

    // --- START critical region ---
    RSF_Multithread_lock(fp);
    lseek(fp->fd, fp->next_write , SEEK_SET);        // set position after last write (SOS or DATA record)
    ssize_t n_written = write(fp->fd, vdir, dir_rec_size);   // write directory record
    fp->next_write += n_written;                     // update last write position
    fp->last_op = OP_WRITE;                          // last operation was a write
    const off_t next_write = fp->next_write; // Save for printing
    RSF_Multithread_unlock(fp);
    // --- END critical region ---

    Lib_Log(APP_LIBFST, APP_EXTRA, "%s: fp->next_write = %lx, vdir record size = %ld \n",
            __func__, next_write, dir_rec_size);

    if (n_written != dir_rec_size) {                   // everything written ?
        dir_rec_size = 0;                              // error, return 0 as directory size
    }

    if (p) free(p);
    return dir_rec_size;
}


// =================================  user callable rsf file functions =================================

//! Write to disk everything that's only in memory and close the segment so that it can be complete and someone else
//! can read it
//! \return Result of individual operations: 0 or positive on success, negative on failure
int64_t RSF_Checkpoint(RSF_handle h) {
    RSF_File* fp = (RSF_File*) h.p;
    if (!RSF_Is_file_valid(fp)) return -1;           // something not O.K. with fp
    if ((fp->mode & RSF_RO) == RSF_RO) return 1; // Read-only, nothing to do

    if (fp->seg_max == 0) {
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: compact segment\n", __func__);
        if (RSF_Close_compact_segment(fp) != 1) return -1;
    }
    else {
        Lib_Log(APP_LIBFST, APP_ALWAYS, "%s: sparse segment\n", __func__);
        if (RSF_Switch_sparse_segment(h, 0) != 1) return -1;
    }

    return 1;
}

// default directory match function (user overridable)
//
// match the first ncrit words of criteria and meta where mask has bits set to 1
// where mask has bits set to 0, a match is assumed
// if mask == NULL, a direct binary criteria vs meta match is used, as if mask was all 1s
// criteria and mask MUST have the same dimension : ncrit
// meta dimension nmeta may be larger than ncrit (only the first ncrit words will be considered)
// ncrit > nmeta has an undefined behavior and considered as a NO MATCH for now
// return : 1 in case of match 0,  in case of no match
int32_t RSF_Default_match(
    uint32_t * criteria,
    const uint32_t * meta,
    const uint32_t * mask,
    int ncrit,
    int nmeta,
    int reject_a_priori
) {
    // NOTE : first item in criteria / meta / mask no longer skipped  by caller
    //        it is ignored here, and reject_a_priori is used as is reflects
    //        the condition derived from the first element
    //        the USER SUPPLIED replacement for this function may act otherwise
    if (reject_a_priori) return 0; // the user supplied function might act differently and re-analize criteria[0] meta[0] mask[0]
    ncrit--;
    nmeta--;
    criteria++;
    meta++;
    if (mask != NULL) mask++;

    if (ncrit > nmeta) return 0;  // too many criteria, no match
    if (ncrit <= 0) return 1;    // no criteria, we assume a match

    if (mask != NULL) {           // caller supplied a mask
        if (App_LogLevel(NULL) >= APP_EXTRA) {
            char buffer[1024 + ncrit * 35];
            char* ptr = buffer;

            int count = 0;
            for (int i = 0; i < ncrit; i++) {
                if (mask[i] != 0) {
                    ptr += snprintf(ptr, 34, "%3d: %.8x|%.8x|%.8x  ", i, meta[i], criteria[i], mask[i]);
                    count++;
                }
                if ((count-1) % 4 == 3) {
                    ptr += snprintf(ptr, 2, "\n");
                }
            }
            Lib_Log(APP_LIBFST, APP_EXTRA, "%s: Record | Criterion | Mask \n%s\n", __func__, buffer);
        }
        for (int i = 0; i < ncrit; i++) {
            if ( (criteria[i] & mask[i]) != (meta[i] & mask[i]) ) {
                Lib_Log(APP_LIBFST, APP_EXTRA, "%s: MISMATCH at %d, criteria = %8.8x, meta = %8.8x, mask = %8.8x, ncrit = %d, nmeta = %d\n",
                        __func__, i, criteria[i], meta[i], mask[i], ncrit, nmeta);
                return 0;  // mismatch, no need to go any further
            }
        }
    } else {
        // caller did not supply a mask
        for (int i = 0; i < ncrit; i++) {
            if ( criteria[i] != meta[i] ) return 0;  // mismatch, no need to go any further
        }
    }
    Lib_Log(APP_LIBFST, APP_EXTRA, "%s: ncrit = %d, MATCH O.K.\n", __func__, ncrit);
    return 1;   // if we get here, there is a match
}

// same as RSF_Default_match but ignores mask
int32_t RSF_Base_match(uint32_t *criteria, uint32_t *meta, uint32_t *mask, int ncrit, int nmeta, int reject_a_priori)
{
    return RSF_Default_match(criteria, meta, NULL, ncrit, nmeta, reject_a_priori);
}

// Check for a valid data record structure
//! \return
//! | Value | Meaning                   |
//! | ----: | :------------------------ |
//! |     0 | Record is valid           |
//! |     1 | SOR is wrong              |
//! |     2 | Meta - SOR is wrong       |
//! |     3 | Meta size is inconsistent |
//! |     4 | Data would go beyond EOR  |
int32_t RSF_Valid_record(const RSF_record * const rec) {
    const void * const recptr = (void *) rec;
    if ( rec->sor != (void *) rec->d ) return 1;
    if ( ((char*)(rec->sor) + sizeof(start_of_record)) != (void *) rec->meta) return 2;
    if ( ((char*)(rec->data) - (char*)(rec->meta)) != sizeof(int32_t) * rec->rec_meta ) return 3;
    int64_t rsz = (rec->rsz >= 0) ? rec->rsz : -(rec->rsz); // use absolute value of rsz
    if ( (char *)&rec->d[rec->max_data] > ( (char*)recptr + rsz - sizeof(end_of_record) ) ) return 4;
    return 0;
}

//! Create a data record structure that can accomodate up to [max_data] bytes of payload
//! and up to [rec_meta] 32-bit metadata items (rec_meta == 0 means use fp->meta_dim).
//! If t is NULL, allocate the space for this record. In that case, the caller is responsible
//! for releasing (free) the allocated space when no longer needed.
//! If t is non-NULL, the record will be created from the memory pointed to by it.
//! szt indicates the size of the array t and must be sufficiently large to accomodate the record.
//! If t is non-NULL and szt == 0, it must be a previously created "record" large enough to
//! accomodate max_data and rec_meta
//!
//! This record structure will be used by RSF_Get, RSF_Put.
//!
//! This record is reusable as long as the allocated size can accomodate [max_data] bytes of payload
//! (this is why the allocated size is kept in the struct).
//!
//! If the szt field in the struct is negative, the memory was not allocated by RSF_New_record
//!
//! \return A pointer to the allocated data record
RSF_record *RSF_New_record(
    //!> Handle to the open file for which we want to create a record.
    RSF_handle handle,
    //!> Max size (in 32-bit units) of record metadata. Use rec_meta from file if too small.
    int32_t rec_meta,
    //!> Max size (in 32-bit units) of record metadata. Will be bounded by [rec_meta].
    //!> Use dir_meta from file if too small.
    int32_t dir_meta,
    //!> Type of record (RT_NULL or 0 for default -> data)
    const uint8_t rec_type,
    //!> Max size (in bytes) of data that the record can hold.
    const size_t max_data,
    //!> [optional] Array that will hold the record.
    void * const t,
    //!> [optional] Size of the [t] array. If 0, [t] must be a previously created record.
    int64_t szt
) {
    RSF_File * const fp = (RSF_File *) handle.p;
    RSF_record * rec;
    void *p;

    if ( ! RSF_Is_file_valid(fp) ) return NULL;
    // calculate space needed for the requested "record"
    if (rec_meta < fp->rec_meta) rec_meta = fp->rec_meta;
    if (dir_meta < fp->dir_meta) dir_meta = fp->dir_meta;
    if (dir_meta > rec_meta)     dir_meta = rec_meta;

    const size_t record_size = RSF_Record_size(rec_meta, max_data);

    if (t != NULL){
        // caller supplied space
        p = t;                                             // use caller supplied space
        rec = (RSF_record *) p;                              // pointer to record structure
        if (szt == 0) {                                      // passing a previously allocated record
            if ( RSF_Valid_record(rec) != 0 ) return NULL;      // inconsistent info in structure
            szt = (rec->rsz >= 0) ? rec->rsz : -(rec->rsz);        // get space size from existing record
        } else {
            if(szt < 0) return NULL;                         // szt MUST be positive for caller supplied space
            rec->rsz = -szt;                                   // caller allocated space, szt stored as a negative value
        }
        if (szt < record_size) return NULL;                 // not enough space to build record
    } else {                                                // create a new record
        p = malloc( record_size + sizeof(RSF_record) );    // allocated record + overhead
        if (p == NULL) return NULL;                         // malloc failed
        rec = (RSF_record *) p;                              // pointer to record structure
        rec->rsz = record_size + sizeof(RSF_record);         // allocated memory size (positive, as it is allocated here)

    }  // if(t != NULL)

    p = (char*)p + sizeof(RSF_record);   // skip overhead. p now points to data record part (SOR)

    rec->rec_class = fp->rec_class;
    rec->rec_type = (rec_type != RT_NULL ) ? rec_type : DEFAULT_RECORD_TYPE;
    if (rec->rec_type > RT_DEL) {
        Lib_Log(APP_LIBFST, APP_WARNING, "%s: Record type (%d) does not seem valid, defaulting to %s (%d)\n",
                __func__, rec->rec_type, rt_to_str(DEFAULT_RECORD_TYPE), DEFAULT_RECORD_TYPE);
    }

    rec->rec_meta = rec_meta;       // metadata sizes in 32 bit units (metadata filling is not tracked)
    rec->dir_meta = dir_meta;
    rec->elem_size = 0;                                 // unspecified data element length
    rec->meta = (uint32_t *) ((char*)p + sizeof(start_of_record));                                                // points to metadata
    bzero(rec->meta, sizeof(uint32_t) * rec_meta);      // set metadata to 0
    rec->max_data  = max_data;                          // max data payload for this record
    rec->data_size = 0;                                 // no data in record yet
    rec->data = (void *)  ((char*)p + sizeof(start_of_record) + sizeof(uint32_t) *  rec_meta);                    // points to data payload
    start_of_record * const sor = (start_of_record *) p;
    rec->sor = sor;
    sor->zr = ZR_SOR; sor->rt = rec->rec_type; sor->rlm = rec_meta; sor->rlmd = dir_meta; sor->dul = 0;
    RSF_64_to_32(sor->rl, record_size); // provisional sor, assuming full record

    end_of_record * const eor = (end_of_record *) ((char*)p + record_size - sizeof(end_of_record));
    rec->eor  = eor;
    eor->zr = ZR_EOR; eor->rt = sor->rt; eor->rlm = rec_meta;
    RSF_64_to_32(eor->rl, record_size); // provisional eor, assuming full record

    // rec-> rsz already set, > 0 if allocated by RSF_New_record, < 0 otherwise

    return rec; // return address of record
}

// store metadata into record allocated by RSF_New_record
int32_t RSF_Record_add_meta(
    RSF_record * const rec,      //!< [in,out] Handle to record being updated
    const uint32_t * const meta,     //!< [in]     The new metadata
    int32_t rec_meta,   //!< [in]     Size of the metadata (32-bit elements)
    int32_t dir_meta,   //!< [in]     Size of directory metadata (32-bit elements, must hold within record meta)
    uint32_t elem_size  //!< [in]     Size in bytes of data elements stored in the record. Must be in [1, 2, 4, 8]
){
    if (rec_meta != rec->rec_meta) return 0;               // inconsistemt record metadata size
    if (dir_meta > rec_meta) dir_meta = rec_meta;        // dir_meta is bounded by rec_meta
    rec->rec_meta = rec_meta;
    rec->dir_meta = dir_meta;
    rec->elem_size = elem_size;
    for (int i = 0; i < rec_meta; i++) rec->meta[i] = meta[i];  // copy metadata
    return rec_meta;
}

// add data to payload in record allocated by RSF_New_record
// returns how much free space remains available (-1 if data_bytes was too large)
// r points to a "record" allocated/initialized by RSF_New_record
// data points to data to be added to current record payload
// data_bytes is size in bytes of added data
int64_t RSF_Record_add_bytes(
    RSF_record * const rec,
    const void * const data,
    size_t data_bytes
) {
    start_of_record * const sor = (start_of_record *) rec->sor;

    if (sor->dul != DT_08 && sor->dul != 0)          return -1;     // element size should be bytes
    if (data_bytes <= 0)                             return -1;     // invalid size
    if ( (rec->data_size + data_bytes) > rec->max_data ) return -1; // data to insert too large
    sor->dul = DT_08;
    memcpy((char*)(rec->data) + rec->data_size, data, data_bytes ); // add data to current payload
    rec->data_size = rec->data_size + data_bytes;                   // update payload size
    return rec->max_data - rec->data_size;                          //!< \return free space remaining
}

//! similar to RSF_Record_add_bytes
//! \return Free space remaining in the record
int64_t RSF_Record_add_elements(
    RSF_record * const rec,              //!< [in, out] Handle to record to update
    const void * const data,                 //!< [in]      The data to add
    size_t num_data_elements,   //!< [in]      How many elements we are putting there
    int data_element_size       //!< [in]      Size of each data element in bytes (can do 1, 2, 4 or 8)
) {
    start_of_record * const sor = (start_of_record *) rec->sor;

    if (num_data_elements <=0 || data_element_size <= 0) return -1; // invalid data element size or number of elements
    if (data_element_size >2 &&
        data_element_size != 4 &&
        data_element_size != 8) return -1;                      // invalid data element size
    if (sor->dul == 0) sor->dul = data_element_size;            // dul not initialized, set to data_element_size
    if (sor->dul != data_element_size)               return -1; // data element size chage
    size_t data_bytes = num_data_elements * data_element_size;      // size in bytes of data to add
    if ( (rec->data_size + data_bytes) > rec->max_data ) return -1; // data to insert too large
    memcpy((char*)(rec->data) + rec->data_size, data, data_bytes);  // add data to current payload
    rec->data_size = rec->data_size + data_bytes;                   // update payload size
    return rec->max_data -rec->data_size;                           // free space remaining
}

//! Set internal data of the given record to indicate that it contains a certain number of elements
//! _BE CAREFUL. It should match the data that actually is in the record._
int64_t RSF_Record_set_num_elements(
    RSF_record *rec,        //!< [in, out] Handle to record to update
    size_t num_elements,  //!< [in]      How many elements there are (should be)
    int element_size      //!< [in]      Size of the elements (in bytes)
) {
  // Validate element size input
  if (element_size >2 && element_size != 4 && element_size != 8) return -1;

  // Check or set element size in SOR (must not change)
  start_of_record *sor = (start_of_record *) rec->sor;
  if (sor->dul == 0) sor->dul = element_size;
  if (sor->dul != element_size) return -1;

  // Check and set number of element (can't be larger than space in record)
  const size_t num_bytes = num_elements * element_size;
  if (num_bytes > rec->max_data) return -1;
  rec->data_size = num_bytes;
  return num_bytes;
}

// free dynamic record allocated by RSF_New_record
void RSF_Free_record(RSF_record * const rec){
    if (rec->rsz > 0) {
#ifdef DEBUG_RSF
        fprintf(stderr, "RSF_Free_record DEBUG: freeing memory at %16.16p\n", rec);
#endif
        free(rec);    // only if allocated by RSF_New_record
    }
}

// space available for more data in record allocated by RSF_New_record
int64_t RSF_Record_free_space(RSF_record *rec){  // asssuming record payload is "managed"
  return (rec->max_data -rec->data_size);
}

//! Size of record (in bytes) allocated by RSF_New_record.
//! Assuming record payload is "managed".
int64_t RSF_Record_allocated(RSF_record *rec){
  return (rec->rsz >= 0) ? rec->rsz : -(rec->rsz);
}

//! Maximum data payload size in record (in bytes) allocated by RSF_New_record.
//! Asssuming record payload is "managed".
int64_t RSF_Record_max_space(RSF_record *rec){
  return rec->max_data;
}

//! Pointer to data payload in record allocated by RSF_New_record.
//! Asssuming record payload is "managed".
void *RSF_Record_data(RSF_record *rec){
  return rec->data;
}

//! Current size of data payload in record (in bytes) allocated by RSF_New_record.
//! Asssuming record payload is "managed".
uint64_t RSF_Record_data_size(RSF_record *rec){
  return rec->data_size;
}

// pointer to metadata in record allocated by RSF_New_record
void *RSF_Record_meta(RSF_record *rec){
  return rec->meta;
}

//! Size of metadata (32-bit elements) in record allocated by RSF_New_record.
uint32_t RSF_Record_meta_size(RSF_record *rec){
  return rec->rec_meta;
}

// adjust record created with RSF_New_record (make it ready to write)
// the sor and eor components will be adjusted to properly reflect data payload size
// return size of adjusted record (amount of data to write)
size_t RSF_Adjust_data_record(RSF_handle h, RSF_record *rec){
  RSF_File *fp = (RSF_File *) h.p;
  start_of_record *sor;
  end_of_record   *eor;
  size_t new_size;
  size_t data_bytes;

  if( ! RSF_Is_file_valid(fp) ) return 0L;
  data_bytes = rec->data_size;                           // get data size from record
  if(data_bytes == 0) return 0L;                       // no data in record

  sor = rec->sor;
  if(sor->dul == 0) return 0L;                         // uninitialized data element size
  Lib_Log(APP_LIBFST, APP_EXTRA, "%s: data_bytes = %ld, element size = %d bytes\n", __func__, data_bytes,sor->dul );
  new_size = RSF_Record_size(rec->rec_meta, data_bytes); // properly rounded up size
  eor = (end_of_record*)((char*)(rec->sor) + new_size - sizeof(end_of_record));

  // adjust eor and sor to reflect actual record contents
  sor->zr = ZR_SOR; sor->rt = RT_DATA; sor->rlm = rec->rec_meta; sor->rlmd = rec->dir_meta;
  RSF_64_to_32(sor->rl, new_size);
  eor->zr = ZR_EOR; eor->rt = RT_DATA; eor->rlm = rec->rec_meta;
  RSF_64_to_32(eor->rl, new_size);

  return new_size;   // adjusted size of record
}

// return used space in active segment
// if the handle is invalid, -1 is returned
// mostly used for debug purposes
int64_t RSF_Used_space(RSF_handle h){
  RSF_File *fp = (RSF_File *) h.p;
  uint64_t used;

  if( ! RSF_Is_file_valid(fp) ) return -1;

  used = fp->next_write - fp->seg_base;

  return used;
}

// return available space in active segment (accounting for directory space and other overhead)
// if segment is "compact", an insanely large number is returned
// if the handle is invalid, -1 is returned
// mostly used for debug purposes
int64_t RSF_Available_space(RSF_handle h){
  RSF_File *fp = (RSF_File *) h.p;
  uint64_t used;

  if( ! RSF_Is_file_valid(fp) ) return -1;
  if(fp->seg_max == 0) return 0x7FFFFFFFFFFFFFFFl;           // no practical limit if not a sparse segment

  used = fp->next_write - fp->seg_base +                      // space already used in segment
         fp->vdir_size +                                      // directory current record size (worst case)
         sizeof(end_of_segment) +                             // end of compact segment
         sizeof(start_of_segment) + sizeof(end_of_segment) +  // sparse segment at end (sos + eos)
         ARBITRARY_OVERHEAD;                                 // arbitrary overhead
  return (fp->seg_max - used - sizeof(start_of_record) - sizeof(end_of_record));
}

//! Write a null record, no metadata, sparse data, no entry in directory
//! record_size = data size (not including record overhead)
//! mostly used for debug purposes, error if not enough room in segment
//! \return record size if successful, 0 otherwise
uint64_t RSF_Put_null_record(
    RSF_handle h,       //!< Handle to file where we want to add the null record
    size_t record_size  //!< Size of the "data" the record will contain (it's data section will take that much space)
) {
    RSF_File * const fp = (RSF_File *) h.p;
    start_of_record sor = SOR;      // start of data record
    end_of_record   eor = EOR;      // end of data record

    if ( ! RSF_Is_file_valid(fp) ) return 0;

    if (RSF_Ensure_new_segment(fp, 0) < 0) return 0; // Can't put a null record if we don't have a writeable segment

    // Verify there is enough space in the segment
    uint64_t needed = record_size;
    if (fp->seg_max > 0) {
        int64_t free_space = RSF_Available_space(h);
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: free = %ld, needed = %ld\n", __func__, free_space, needed);
        if ( free_space < needed ) {
            Lib_Log(APP_LIBFST, APP_DEBUG, "%s: segment overflow\n", __func__);
        return 0;
        }
    }

    // Create + commit the record
    needed = needed + sizeof(start_of_record) + sizeof(end_of_record);
    sor.rt = RT_NULL;
    RSF_64_to_32(sor.rl, needed);
    eor.rt = RT_NULL;
    RSF_64_to_32(eor.rl, needed);
    lseek(fp->fd, fp->next_write, SEEK_SET);
    write(fp->fd, &sor, sizeof(start_of_record));
    lseek(fp->fd, record_size, SEEK_CUR);
    write(fp->fd, &eor, sizeof(end_of_record));
    fp->next_write += needed;
    return needed;
}

// write data chunks and metadata or a record structure into a file
// data sizes are specified in bytes
// chunks is an array of pointers to the data chunks
// if an element of chunks is NULL, a gap will be inserted in the file instead of actual data
// chunk_size is an array of chunk sizes (in bytes)
// if not NULL, record is a pointer to a RSF_record struct
// if record is not NULL, chunks, chunk_size, nchunks, element_size, meta, dir_meta and rec_meta are ignored
// meta is a pointer to record metadata
// meta[0] is not really part of the record metadata, it is used to pass optional extra record type information
// rec_meta is the size (in 32 bit elements) of the file metadata (must be >= file metadata default dimension)
// if rec_meta is ZERO, it will be taken from the defauklt file metadata length
// dir_meta is the size (in 32 bit elements) of the directory metadata (must be >= file metadata default dimension)
// if dir_meta is ZERO, it is set to rec_meta
// dir_meta <= rec_meta
// element_size is the length in bytes of the data elements (for endianness management)
// if meta is NULL, record MUST be a pointer to a pre allocated record ( RSF_record ), rec_meta and dir_meta are ignored
// RSF_Adjust_data_record will be called if record is not NULL
// NOTE: do we want to add a datamap array (-1 terminated) when not using a record
int64_t RSF_Put_chunks(RSF_handle h, RSF_record *record,
                      uint32_t *meta, uint32_t rec_meta, uint32_t dir_meta,
                      void **chunks, size_t *chunk_size, int nchunks, int element_size){
    RSF_File * const fp = (RSF_File *) h.p;
    uint64_t record_size, extra;
    int64_t available, desired;
    start_of_record sor = SOR;      // start of data record
    end_of_record   eor = EOR;      // end of data record
    uint32_t meta0, rt0, class0;
    off_t gap;
    size_t data_bytes = 0;
    int i;

    if( ! RSF_Is_file_valid(fp) ) return 0;           // something not O.K. with fp
    if( RSF_Ensure_new_segment(fp, 0) < 0 ) return 0;    // We don't have write access to the file
    if( fp->next_write <= 0) return 0;                // next_write address is not set

    if ( record != NULL ) {                           // using a pre allocated record ?
        if( RSF_Valid_record(record) != 0 ) return 0; // make sure record is valid
        data_bytes = record->data_size;               // take data_bytes from record
        // get dir_meta and rec_meta from record to compute record size
        dir_meta = record->dir_meta;                  // should never be 0
        if(dir_meta == 0) return 0;
        rec_meta = record->rec_meta;                  // should never be 0
        if(rec_meta == 0) return 0;
        // adjust to actual size of data in record structure (metadata assumed already set)
        record_size = RSF_Adjust_data_record(h, record);
        if (record_size != RSF_Record_size(rec_meta, data_bytes)) return 0;
        if (((start_of_record *) record->sor)->dul == 0) return 0;  // uninitialized data element size

    } else {                                           // NOT a preallocated record, use chunk sizes
        for (i = 0; i < nchunks; i++) {
            data_bytes += chunk_size[i];                // sum of chunk sizes
        }
        // metadata stored in directory may be shorter than metadata in file record
        if (rec_meta == 0) rec_meta = fp->rec_meta;    // get default metadata length from file structure
        if (rec_meta < fp->rec_meta) return 0;         // metadata too small, must be at least fp->rec_meta
        if (dir_meta == 0) dir_meta = rec_meta;        // default size of directory metadata (record metadata size)
        if (dir_meta > rec_meta) dir_meta = rec_meta;  // directory metadata size cannot be larger than record metadata size
        record_size = RSF_Record_size(rec_meta, data_bytes);
    }

    Lib_Log(APP_LIBFST, APP_EXTRA, "%s: data_bytes = %ld, record_size = %ld %lx\n", __func__, data_bytes, record_size, record_size);
    // write record if enough room left in segment (always O.K. if compact segment)
    if(fp->seg_max > 0){                                                 // write into a sparse segment
        available = RSF_Available_space(h);           // available space in sparse segment according to current conditions
        desired   = record_size +                      // rounded up size of this record
                    sizeof(vdir_entry) +               // directory space needed for this record
                    rec_meta * sizeof(uint32_t);
        if(desired > available) {
            Lib_Log(APP_LIBFST, APP_INFO, "%s: sparse segment OVERFLOW, switching to new segment\n", __func__);
            extra = desired +                          // extra space needed for this record and associated dir entry
                    NEW_SEGMENT_OVERHEAD;             // extra space needed for closing sparse segment
            RSF_Switch_sparse_segment(h, extra);      // switch to a new segment (minimum size = extra)
        }
    }
    lseek(fp->fd , fp->next_write , SEEK_SET); // position file at fp->next_write
    Lib_Log(APP_LIBFST, APP_EXTRA, "%s: write at %lx\n", __func__, fp->next_write);

    // write record into file
    if( record != NULL){                                              // using a pre allocated, pre filled record structure

        meta = record->meta;                                           // set meta to address of metadata from record
        meta0 = meta[0];                                               // save meta[0]
        rt0 = meta0 & 0xFF;                                            // lower 8 bits
        class0 = meta0 >> 8;                                           // upper 24 bits
        if(rt0 != RT_XDAT)
        if(rt0 < RT_CUSTOM || rt0 >= RT_DEL) rt0 = RT_DATA;          // RT_DATA (normal data record) by default
        if(class0 == 0) class0 = (fp->rec_class & 0xFFFFFF);           // fp->rec_class if unspecified
        meta[0] = (class0 << 8) | ( rt0 & 0xFF);                       // RT + record class
        ((start_of_record *) record->sor)->rt = rt0;                   // update record type in start_of_record
        ((end_of_record *)   record->eor)->rt = rt0;                   // update record type in end_of_record
        // data element size will be taken from record, element_size is ignored
        write(fp->fd, record->sor, record_size);                   // write record into file from record structure

    } else {                                                            // using meta and chunk data

        meta0 = meta[0];                                               // save meta[0]
        rt0 = meta0 & 0xFF;                                            // record type in lower 8 bits
        class0 = meta0 >> 8;                                           // record class in upper 24 bits
        if (rt0 != RT_XDAT)
        if (rt0 < RT_CUSTOM || rt0 >= RT_DEL) rt0 = RT_DATA;          // RT_DATA (normal data record) by default
        if (class0 == 0) class0 = (fp->rec_class & 0xFFFFFF);           // fp->rec_class if unspecified
        meta[0] = (class0 << 8) | ( rt0 & 0xFF);                       // RT + record class
        sor.rlm = DIR_ML(rec_meta);
        sor.rt = rt0;                                                  // update record type in start_of_record
        sor.rlmd = dir_meta;
        sor.dul = element_size;
        sor.ubc = 0;
        RSF_64_to_32(sor.rl, record_size);
        write(fp->fd, &sor, sizeof(start_of_record));             // write start of record
        write(fp->fd, meta, rec_meta * sizeof(uint32_t));         // write metadata

        for (i = 0; i < nchunks; i++) {                               // write data chunks (or create appropriate holes)
            gap = chunk_size[i];                                      // round up the size of the data to write
            if (chunks[i] != NULL) {                                  // data or hole ?
                write(fp->fd, chunks[i], gap);                        // write gap data bytes
            } else {
                lseek(fp->fd, gap, SEEK_CUR);                         // create a hole instead of writing data
            }
        }
        gap = RSF_Round_size(data_bytes) - data_bytes;
        if( gap > 0) lseek(fp->fd, gap, SEEK_CUR);                     // round up write size

        eor.rlm = DIR_ML(rec_meta);
        eor.rt = rt0;                                                  // update record type in end_of_record
        RSF_64_to_32(eor.rl, record_size);
        write(fp->fd, &eor, sizeof(end_of_record));               // write end_of_record
    }

    // update directory in memory
    int64_t slot = RSF_Add_vdir_entry(fp, meta, DRML_32(dir_meta, rec_meta), fp->next_write, record_size,element_size, 0);
    meta[0] = meta0;                                                 // restore meta[0] to original value

    fp->next_write += record_size;         // update fp->next_write and fp->current_pos
    fp->last_op = OP_WRITE;                // last operation was write
    fp->nwritten += 1;                     // update unmber of writes
    // return slot/index for record (0 in case of error)
    return slot;
}

int64_t RSF_Put_bytes_new(RSF_handle h, RSF_record *record,
                      uint32_t *meta, uint32_t rec_meta, uint32_t dir_meta,
                      void *data, size_t data_bytes, int element_size){
  void *chunks = data;
  size_t chunk_size = data_bytes;
  // call RSF_Put_chunks with one chunk of size data_bytes
  return RSF_Put_chunks(h, record, meta, rec_meta, dir_meta, &chunks, &chunk_size, 1, element_size);
}

//! Write data/meta or record into a file. data size is specified in bytes
//! Uses the file's mutex.
//! if meta is NULL, data is a pointer to a pre allocated record ( RSF_record ), rec_meta and dir_meta are ignored
//! RSF_Adjust_data_record may have to be called
//! lower 16 bits of meta_size : length of metadata written to disk
//! upper 16 bits of meta_size : length of metadata written into vdir (0 means same as disk)
//! written into vdir <= written to disk
//! \return Record handle (index) if successful, 0 otherwise
int64_t RSF_Put_bytes(
    RSF_handle h,       //!< [in,out] Handle to the file where to write
    RSF_record *record, //!< [in,out] [Optional] Record to write. If non-NULL, data and meta are ignored.
    uint32_t *meta,     //!< [in,out] [Optional] Pointer to record metadata to write. Ignored if [record] is non-NULL.
    uint32_t rec_meta,  //!< [in]     [Optional] Size of record metadata. Must be >= dir_meta. Ignored if [record] is non-NULL.
    uint32_t dir_meta,  //!< [in]     [Optional] Size of directory metadata. Ignored if [record] is non-NULL.
    void *data,         //!< [in]     [Optional] Data to write to the file. If NULL, a gap is inserted. Ignored if [record] is non-NULL.
    size_t data_bytes,  //!< [in]     [Optional] Size (in bytes) of the data to write. Ignored if [record] is non-NULL.
    int element_size    //!< [in]     [Optional] Size of data elements (in bytes). For endianness management. Ignored if [record] is non-NULL.
) {
    RSF_File *fp = (RSF_File *) h.p;
    uint64_t extra;
    int64_t available, desired;
    start_of_record sor = SOR;      // start of data record
    end_of_record   eor = EOR;      // end of data record
    off_t gap;

    if ( ! RSF_Is_file_valid(fp) ) return 0;          // something not O.K. with fp
    if ( RSF_Ensure_new_segment(fp, 0) < 0 ) return 0; // Don't have write permission

    // metadata stored in directory may be shorter than record metadata
    if ( record != NULL ) {                            // using a pre allocated record ?
        if ( RSF_Valid_record(record) != 0 ) return 0;    // make sure record is valid
        // get dir_meta and rec_meta from record to compute record size
        dir_meta = record->dir_meta;                  // should never be 0
        rec_meta = record->rec_meta;                  // should never be 0
    }

    if (rec_meta == 0) rec_meta = fp->rec_meta;      // get default metadata length from file structure
    if (rec_meta < fp->rec_meta) return 0;           // metadata too small, must be at least fp->rec_meta
    if (dir_meta == 0) dir_meta = rec_meta;          // default size of directory metadata (record metadata size)
    if (dir_meta > rec_meta) dir_meta = rec_meta;    // directory metadata size vannot be larger than record metadata size

    const uint64_t record_size = (record == NULL) ?
                                RSF_Record_size(rec_meta, data_bytes) :
                                RSF_Adjust_data_record(h, record);
    // fprintf(stderr,"RSF_Put_bytes DEBUG : data_bytes = %ld, record_size = %ld %lx\n", data_bytes, record_size, record_size);
    // write record if enough room left in segment (always O.K. if compact segment)
    if (fp->seg_max > 0) {                                                 // write into a sparse segment
        available = RSF_Available_space(h);           // available space in sparse segment according to current conditions
        desired   = record_size +                      // rounded up size of this record
                    sizeof(vdir_entry) +               // directory space needed for this record
                    rec_meta * sizeof(uint32_t);
        if(desired > available) {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: sparse segment OVERFLOW, switching to new segment\n", __func__);
        extra = desired +                          // extra space needed for this record and associated dir entry
                NEW_SEGMENT_OVERHEAD;             // extra space needed for closing sparse segment
        RSF_Switch_sparse_segment(h, extra);      // switch to a new segment (minimum size = extra)
        }
    }

    if (record != NULL) meta = record->meta;
    meta[0] = make_meta0(record->rec_class, record->rec_type);
    meta[1] = 0; // Datamap size

    if (record != NULL && ((start_of_record *) record->sor)->dul == 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Uninitialized data element size\n");
        return 0;
    }

    // --- START CRITICAL REGION --- //
    RSF_Multithread_lock(fp);

    lseek(fp->fd , fp->next_write , SEEK_SET); // position file at fp->next_write

    // write record
    if ( record != NULL) {                                              // using a pre allocated, pre filled record structure
        dir_meta = record->dir_meta;                                   // get metadata sizes from record
        rec_meta = record->rec_meta;
        if (dir_meta == 0) dir_meta = rec_meta;
        if (dir_meta > rec_meta) dir_meta = rec_meta;
        ((start_of_record *) record->sor)->rt = record->rec_type;                   // alter record type in start_of_record
        ((end_of_record *)   record->eor)->rt = record->rec_type;                   // alter record type in end_of_record
        // data element size taken from record, element_size ignored
        write(fp->fd, record->sor, record_size);                  // write record structure to disk
    } else {                                                          // using meta and data
        sor.rlm = DIR_ML(rec_meta);
        sor.rt = record->rec_type;                                                  // alter record type in start_of_record
        sor.rlmd = dir_meta;
        sor.dul = element_size;
        sor.ubc = 0;
        RSF_64_to_32(sor.rl, record_size);
        write(fp->fd, &sor, sizeof(start_of_record));             // write start of record
        write(fp->fd, meta, rec_meta * sizeof(uint32_t));         // write metadata
        gap = RSF_Round_size(data_bytes);                              // round up the size of the data to write
        if (data != NULL) {
            write(fp->fd, data, gap);                               // write data
        } else {
            lseek(fp->fd, gap, SEEK_CUR);                                // create a gap where data would have been written
        }
        eor.rlm = DIR_ML(rec_meta);
        eor.rt = record->rec_type;                                                  // alter record type in end_of_record
        RSF_64_to_32(eor.rl, record_size);
        write(fp->fd, &eor, sizeof(end_of_record));               // write end_of_record
    }

    // update directory in memory
    const int64_t slot = RSF_Add_vdir_entry(fp, meta, DRML_32(dir_meta, rec_meta), fp->next_write, record_size,element_size, 0);

    fp->next_write += record_size;         // update fp->next_write
    fp->last_op = OP_WRITE;                // last operation was write
    fp->nwritten += 1;                     // update unmber of writes

    RSF_Multithread_unlock(fp);
    // --- END CRITICAL REGION --- //

    // return slot/index for record (0 in case of error)
    return slot;
}

// similar to RSF_Put_bytes, write data elements of a specified size into the file
// data_elements     : number of data items to add
// data_element_size : size in bytes of each data element
int64_t RSF_Put_data(RSF_handle h, void *data_record,
                       uint32_t *meta, uint32_t rec_meta, uint32_t dir_meta,
                       void *data, size_t data_elements, int data_element_size){
    if (data_record == NULL) {     // data and metadata
        return RSF_Put_bytes(h, NULL, meta, rec_meta, dir_meta, data, data_elements * data_element_size, data_element_size);
    } else {                     // a pre allocated data record struct
        return RSF_Put_bytes(h, data_record, NULL, 0, 0, NULL, data_elements * data_element_size, data_element_size);
    }
}

//! Write pre allocated data record to file
//! \sa RSF_Put_bytes
//! \return The result of of RSF_Put_bytes (record handle index, or 0 if error)
int64_t RSF_Put_record(
    RSF_handle h,         //!< [in,out] Handle to file where we want to write
    RSF_record *record,   //!< [in]     Pointer to record we want to write
    size_t data_bytes     //!< [in]     Size of the record data (bytes)
) {
    return RSF_Put_bytes(h, record, NULL, 0, 0, NULL, data_bytes, 0);
}

//! Mark the given record as "deleted" in its file
//! Uses the mutex of the corresponding file
//! This function is safe to call from multiple threads on the same file, even on the same record.
//! It is also safe to call from multiple processes on the same file, but *not on the same record*.
//! \return 1 if success, 0 if not
int32_t RSF_Delete_record(
    RSF_handle h,
    const int64_t key
) {
    const RSF_record_info info = RSF_Get_record_info(h, key);
    if (info.wa == 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not get record info from directory\n", __func__);
        return 0;
    }

    RSF_File *fp = (RSF_File *) h.p;
    if ((fp->mode & RSF_RO) == RSF_RO) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: File is open in read-only mode, cannot delete records\n", __func__);
        return 0;
    }

    int32_t status = 0;

    // --- START CRITICAL REGION --- //
    RSF_Multithread_lock(fp);

    {
        // Group start-of-record + reserved metadata to read in 1 go
        struct {
            start_of_record sor;
            uint32_t meta[RSF_META_RESERVED];
        } r;

        const rsf_rec_type read_status = RSF_Read_record(fp, info.wa, &r, sizeof(r));

        if (read_status == RT_INVALID) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Reading start-of-record\n", __func__);
            goto RETURN;
        }

        if (r.sor.rt == RT_DEL) {
            Lib_Log(APP_LIBFST, APP_INFO, "%s: Record at key 0x%x is already deleted\n", __func__, key);
            status = 1;
            goto RETURN;
        }

        // Change the record on disk
        uint8_t version;
        rsf_rec_class record_class;
        rsf_rec_type record_type;
        extract_meta0(r.meta[0], &version, &record_class, &record_type);
        r.sor.rt = RT_DEL;

        // print_start_of_record(&r.sor);

        lseek(fp->fd, info.wa, SEEK_SET);
        const ssize_t nc = write(fp->fd, &r, sizeof(r));
        if (nc != sizeof(r)) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to update start-of-record on disk (size %d, wrote %ld) at position 0x%lx\n",
                    __func__, sizeof(r), nc, info.wa);
            goto RETURN;
        }

        // Change the directory on disk
        const uint32_t index = key64_to_index(key);
        fp->vdir[index]->meta[0] = make_meta0(record_class, RT_DEL);
        const uint32_t entry_offset = RSF_32_to_64(fp->vdir[index]->entry_offset);
        if (entry_offset > 0) {
            lseek(fp->fd, entry_offset, SEEK_SET);
            write(fp->fd, fp->vdir[index], sizeof(vdir_entry) + sizeof(uint32_t));
        }

        fp->num_deleted_records++;
        fp->has_rewritten = 1;

        status = 1;
    }

RETURN:
    RSF_Multithread_unlock(fp);
    // --- END CRITICAL REGION --- //

    return status;
}

//! Overwrite the metadata of an existing record. Changes both the directory and the metadata in the record
//! itself. The change is completed on disk when this function returns.
//! If the new metadata does not fit in the existing space, the function fails.
//! This function is safe to call from multiple threads on the same file, even on the same record.
//! It is also safe to call from multiple processes on the same file, but *not on the same record*.
//! \return 1 on success, 0 on failure
int32_t RSF_Rewrite_record_meta(
  RSF_handle h,           //!< [in] The file to modify
  const int64_t key,      //!< [in] The record to modify
  uint32_t* metadata,     //!< [in] The new metadata
  const size_t num_meta_bytes //!< [in] Size of the new metadata in bytes
) {
    const RSF_record_info info = RSF_Get_record_info(h, key);
    if (info.wa == 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not get record info from directory\n", __func__);
        return 0;
    }

    RSF_File *fp = (RSF_File *) h.p;
    if ((fp->mode & RSF_RO) == RSF_RO) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: File is open in read-only mode, cannot rewrite records\n", __func__);
        return 0;
    }

    if (info.dir_meta * sizeof(uint32_t) < num_meta_bytes) {
        Lib_Log(APP_LIBFST, APP_ERROR,
                "%s: Trying to write metadata (%d bytes) larger than that of existing record (%d bytes)\n",
                __func__, (int)num_meta_bytes, (int)info.dir_meta * sizeof(uint32_t));
        return 0;
    }

    // Check version, to know how many 32-bit words are reserved (these will not be overwritten)
    uint8_t version;
    rsf_rec_class record_class;
    rsf_rec_type record_type;
    extract_meta0(info.meta[0], &version, &record_class, &record_type);

    const uint8_t num_reserved = num_rsf_reserved[version];
    const size_t num_changed_bytes = num_meta_bytes - num_reserved * sizeof(uint32_t);

    int32_t status = 0;

    // --- START CRITICAL REGION --- //
    RSF_Multithread_lock(fp);

    // Change the record on disk
    {
        const off_t write_location = info.wa + sizeof(start_of_record) + num_reserved * sizeof(uint32_t);
        lseek(fp->fd, write_location, SEEK_SET);
        const ssize_t nc = write(fp->fd, metadata + num_reserved, num_changed_bytes);
        if (nc != num_changed_bytes) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to update record meta on disk (size %d, wrote %ld) at position 0x%lx\n",
                    __func__, num_changed_bytes, nc, write_location);
            goto RETURN;
        }
    }

    // Change the directory on disk
    {
        const uint32_t index = key64_to_index(key);
        const uint32_t entry_offset = RSF_32_to_64(fp->vdir[index]->entry_offset);

        if (entry_offset == 0) goto RETURN;

        memcpy(fp->vdir[index]->meta + num_reserved, metadata + num_reserved, num_changed_bytes);
        lseek(fp->fd, entry_offset + sizeof(vdir_entry), SEEK_SET);
        const ssize_t nc = write(fp->fd, fp->vdir[index]->meta, num_meta_bytes);
        
        if (nc != num_meta_bytes) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to update record meta on disk (size %d, wrote %ld) at position 0x%lx\n",
                    __func__, num_meta_bytes, nc, fp->vdir[index]);
            goto RETURN;
        }
    }

    fp->has_rewritten = 1;

    status = 1;

RETURN:
    RSF_Multithread_unlock(fp);
    // --- END CRITICAL REGION --- //

    return status;
}

//! Retrieve file index contained in RSF file and restore it under the name alias
//! \return Index on scuccess, -1 in case of error (meta and meta_size are set to NULL and 0 respectively)
int64_t RSF_Get_file(
    //! [in] File handle
    RSF_handle handle,
    //! [in] Record key from RSF_Add_vdir_entry, RSF_Lookup, RSF_Scan_vdir, etc ...
    int64_t key,
    //! [in] New name. If  NULL, the file's own name will be used
    char *alias,
    //! [out] Metadata pointer
    uint32_t **meta,
    //! [out] Metadata size
    uint32_t *meta_size
) {
    RSF_File *fp = (RSF_File *) handle.p;

    *meta = NULL;
    *meta_size = 0;

    int64_t slot = RSF_Is_file_valid(fp);
    if ( ! slot ) return -1; // something wrong with fp

    uint64_t wa, rl;
    uint32_t * vmeta;
    int32_t ml = RSF_Get_vdir_entry(fp, key, &wa, &rl, &vmeta);
    if (ml == -1) return -1;                        // key not found

    char * temp0 = (char *) vmeta;                               // start of metadata
    char * temp  = temp0 + ml * sizeof(uint32_t);                // end of metadata
    while((temp[ 0] == '\0') && (temp > temp0)) temp --;
    while((temp[-1] != '\0') && (temp > temp0)) temp --;
    temp0 = temp -1;
    uint32_t * tempm = (uint32_t *) temp0;
    uint32_t nmeta = tempm - vmeta;
    uint64_t file_size = RSF_32_to_64(vmeta + nmeta - 2);
    char * filename = (alias != NULL) ? alias : temp;
    //   fprintf(stderr,"RSF_Get_file DEBUG : filename = '%s' [%ld] retrieved as '%s'\n", temp, file_size, filename);

    fp->last_op = OP_READ;

    off_t offset = wa;
    lseek(fp->fd, offset, SEEK_SET);                               // position at start of record
#ifdef DEBUG_RSF
    ssize_t nc =
#endif
    start_of_record sor;
    read(fp->fd, &sor, sizeof(start_of_record)) ;              // read start of record
#ifdef DEBUG_RSF
    fprintf(stderr,"RSF_Get_file DEBUG : rlm = %d\n", sor.rlm);
#endif
    lseek(fp->fd, sor.rlm * sizeof(uint32_t), SEEK_CUR);           // skip metadata
    int fd = open(filename, O_WRONLY | O_CREAT | O_EXCL, 0777);
    if (fd == -1) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: failed to create file '%s'\n", __func__, filename);
        return -1;
    } else {
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: successfully created file '%s'\n", __func__, filename);
    }
    ssize_t towrite = file_size;
    ssize_t nread = 0;
    ssize_t nwritten = 0;
    uint8_t copy_buf[1024*16];
    while (towrite > 0) {
        ssize_t toread = (towrite < sizeof(copy_buf)) ? towrite : sizeof(copy_buf);
        towrite -= toread;
        nread += read(fp->fd, copy_buf, toread);
        nwritten += write(fd, copy_buf, toread);
        // fprintf(stderr,".");
    }
    // fprintf(stderr,"\n");
    // fprintf(stderr,"RSF_Get_file DEBUG : read %ld, written %ld \n", nread, nwritten);
    close(fd);
    if (nread != nwritten) return -1;
    Lib_Log(APP_LIBFST, APP_INFO, "%s: successfully copied image of '%s' into '%s'\n", __func__, temp, filename);

    *meta = vmeta;       // address of directory metadata
    *meta_size = ml;     // directory metadata length
    return key;
}

// store an external file into a RSF file
// the external file will be opened as Read-Only
// meta and meta_size have the same use as for RSF_Put_file
// return key to record
// \todo : use macros to manage metadata sizes
int64_t RSF_Put_file(RSF_handle h, char *filename, uint32_t *meta, uint32_t meta_size){
  RSF_File *fp = (RSF_File *) h.p;
  start_of_record sor = SOR;      // start of data record
  end_of_record   eor = EOR;      // end of data record
  int fd;
  int64_t slot = -1;              // precondition for error
  int64_t index = -1;             // precondition for error
  uint64_t record_size;           // sor + metadata + file + eor
  int32_t vdir_meta;
  off_t file_size0, file_size2;
  int32_t unused;
  size_t name_len = 1024;         // maximum allowed name length
  ssize_t nc, nread, nwritten;
  int i;
  uint32_t meta0;
  int extra_meta;
  struct mmeta{
  uint32_t rl[2];
  char name[];
  } *fmeta;
  uint32_t *dir_meta, *file_meta;
  uint64_t needed, extra;
  uint8_t copy_buf[1024*1024];

  dir_meta = NULL; file_meta = NULL; fmeta = NULL;

  if( ! (slot = RSF_Is_file_valid(fp)) ) goto ERROR; // something wrong with fp
  slot <<= 32;
  if( RSF_Ensure_new_segment(fp, 0) < 0 ) goto ERROR; // Don't have write permission
  if( fp->next_write <= 0) goto ERROR;            // next_write address is not set

  //   vdir_meta = (meta_size >> 16);                  // keep the upper 16 bits for future use
  vdir_meta = DIR_ML(meta_size);                  // directory metadata length
  //   meta_size &= 0xFFFF;                            // only keep the lower 16 bits
  meta_size = REC_ML(meta_size);                  // record metadata length
  fd = open(filename, O_RDONLY);
  // fprintf(stderr,"RSF_Put_file DEBUG : file = '%s', fd = %d\n", filename, fd);
  if(fd < 0) goto ERROR;
  file_size0 = lseek(fd, 0L, SEEK_END);           // get file size
  // file_size2 = ((file_size0 + 3) & (~0x3));
  file_size2 = RSF_Round_size(file_size0);        // file size rounded up to a multiple of 4
  unused = file_size2 - file_size0;               // number of unused bytes
  name_len = strnlen(filename, name_len);         // length of file name

  extra_meta = 2 +                                 // file size (2 x 32 bit integers)
               ((name_len + 5) >> 2);             // name length + 2 rounded up to a multiple of 4 characters
  // fprintf(stderr,"RSF_Put_file DEBUG: name_len = %ld, extra_meta = %d, meta_size = %d\n", name_len, extra_meta, meta_size);
  fmeta = (struct mmeta *) calloc(extra_meta, sizeof(uint32_t));  // extra metadata
  RSF_64_to_32(fmeta->rl, file_size0);                     // record size
  fmeta->name[0] = '\0';
  for(i=0; i<name_len; i++) fmeta->name[i+1] = filename[i];  // copy filename into fmeta->name

  // directory metadata cannot be longer than record metadata
  if(vdir_meta > meta_size) vdir_meta = meta_size;
  dir_meta = (uint32_t *) calloc(vdir_meta + extra_meta, sizeof(uint32_t)); // metadata for in memory directory
  memcpy(dir_meta, meta, vdir_meta * sizeof(uint32_t));        // copy meta[0 -> vdir_meta-1] into dir_meta
  memcpy(dir_meta + vdir_meta, (uint32_t *) fmeta, extra_meta * sizeof(uint32_t));    // copy extra metadata into dir_meta

  file_meta = (uint32_t *) calloc(meta_size + extra_meta, sizeof(uint32_t)); // metadata for record in file
  memcpy(file_meta, meta, meta_size * sizeof(uint32_t));        // copy meta[0 -> meta_size-1] into file_meta
  memcpy(file_meta + meta_size, (uint32_t *) fmeta, extra_meta * sizeof(uint32_t)); // copy extra metadata into file_meta
  // fprintf(stderr,"RSF_Put_file DEBUG : file_meta [");
  // for(i=0; i<vdir_meta + extra_meta; i++) fprintf(stderr," %8.8x", file_meta[i]); fprintf(stderr,"\n");
  record_size = sizeof(start_of_record) +
                meta_size * sizeof(uint32_t) +   // file record metadata size
                extra_meta * sizeof(uint32_t) +  // extra metadata (unrounded file size and file name)
                file_size2 +                     // file size rounded up to a multiple of 4
                sizeof(end_of_record);

  if(fp->seg_max > 0){                             // write into a sparse segment not allowed for now
    extra  = record_size +                         // this record
             sizeof(end_of_segment) +              // end of fixed segment
             sizeof(start_of_segment) +            // new sparse segment (SOS + EOS)
             sizeof(end_of_segment) +
             ARBITRARY_OVERHEAD;                  // arbitrary overhead
    needed = fp->next_write +
             fp->vdir_size +                       // current directory size (worst case)
             sizeof(vdir_entry) +                  // space for this entry in directory
             (meta_size + extra_meta) * sizeof(uint32_t) +
             extra;                               // for this record and segment overhead
    if(needed > fp->seg_max + fp->seg_base) {
      Lib_Log(APP_LIBFST, APP_ERROR, "%s: sparse segment OVERFLOW\n", __func__);
      // switch to a new segment
      RSF_Switch_sparse_segment(h, extra);
      // goto ERROR;
    }
  }

  sor.rt = RT_FILE;
  sor.rlm = meta_size + extra_meta;          // size of record metadata
  sor.rlmd = sor.rlm;                        // size of directory metadata
  sor.ubc = unused << 3;                     // unused bits in data part of record
  sor.dul = 1;                               // record contents is bytes (endianness management)
  RSF_64_to_32(sor.rl, record_size);
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: name = '%s', size = %ld(%ld), vdir_meta = %d, extra_meta = %d, meta_size = %d, record_size = %ld\n",
          __func__, filename, file_size0, file_size2, vdir_meta, extra_meta, meta_size, record_size);
  nc = write(fp->fd, &sor, sizeof(start_of_record));

  meta0 = RT_FILE + (RC_FILE << 8);          // record type and class
  file_meta[0] = meta0;
  meta[0] = meta0;                                 // update caller's meta[0]
  nc = write(fp->fd, file_meta, (meta_size + extra_meta) * sizeof(uint32_t));

  // nc = RSF_Copy_file(fp->fd, fd, file_size);
  // lseek(fp->fd, file_size2, SEEK_CUR);   // will be replaced by RSF_Copy_file
  nread = nwritten = 0;
  lseek(fd, 0l, SEEK_SET);                  // rewind file before copying it
  nc = read(fd, copy_buf, sizeof(copy_buf) );
  while(nc > 0){
    nread += nc;
    if(nread > file_size2) break;           // file being copied is changing size
    nwritten += write(fp->fd, copy_buf, nc);
    nc = read(fd, copy_buf, sizeof(copy_buf) );
  }
  if(nwritten < file_size2){
    nc = write(fp->fd, copy_buf, file_size2 - nwritten);  // pad
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: read %ld bytes, wrote %ld bytes, padded with %ld bytes\n",
            __func__, nread, nwritten, file_size2 - nwritten);
  }

  eor.rt = RT_FILE;
  RSF_64_to_32(eor.rl, record_size);
  eor.rlm = meta_size + extra_meta;
  nc = write(fp->fd, &eor, sizeof(end_of_record));

  close(fd);

  dir_meta[0] = meta0;
  // directory and record metadata lengths will be identical
  index = RSF_Add_vdir_entry(fp, dir_meta, DRML_32(vdir_meta + extra_meta, vdir_meta + extra_meta), fp->next_write, record_size, DT_08, 0); // add to directory

  fp->next_write  = lseek(fp->fd, 0l, SEEK_CUR);
  fp->last_op = OP_WRITE;                // last operation was write
  fp->nwritten += 1;                     // update unmber of writes

ERROR:
  if(fmeta) free(fmeta);
  if(file_meta) free(file_meta);
  if(dir_meta) free(dir_meta);
  return index;
}

//! Get key to record from file fp, matching criteria & mask
//! \return Key to the first record found that matches the criteria/mask, a negative error code if not found or error
//! \sa RSF_Scan_vdir
int64_t RSF_Lookup(
    RSF_handle h,       //!< Handle to file to search
    int64_t key0,       //!< Slot/index where to start searching (beginning, if key0 <= 0)
    uint32_t *criteria, //!< What to look for
    uint32_t *mask,     //!< Mask on the criteria (ignore attribute if bit is 0)
    uint32_t lcrit      //!< How many criteria there are (same as number of masks)
) {
  RSF_File *fp = (RSF_File *) h.p;

  // fprintf(stderr,"in RSF_Lookup key = %16.16lx, crit = %8.8x @%p, mask = %8.8x @%p\n", key0, criteria[0], criteria, mask[0], mask);
  // fprintf(stderr, "          ");
  // for(int i=0; i<lcrit; i++)
  //   fprintf(stderr,"%8.8x ", criteria[i]);
  // fprintf(stderr,"\n          ");
  // for(int i=0; i<lcrit; i++)
  //   fprintf(stderr,"%8.8x ", mask[i]);
  // fprintf(stderr,"\n");
  uint64_t wa, rl;  // wa and rl not sent back to caller
  const int64_t record_key =  RSF_Scan_vdir(fp, key0, criteria, mask, lcrit, &wa, &rl);
  return record_key;
}

static const RSF_record_info info0 = {
    .wa = 0,
    .rl = 0,
    .wa_data = 0,
    .data_size = 0,
    .wa_meta = 0,
    .meta = NULL,
    .fname = NULL,
    .file_size = 0,
    .dir_meta = 0,
    .dir_meta0 = 0,
    .rec_meta = 0,
    .elem_size = 0,
    .rec_type = 0,
};

//! Get information about a record at a specific index within the given file
//! \return a RSF_record_info structure
RSF_record_info RSF_Get_record_info_by_index(
    RSF_handle h,         //!< Pointer to a RSF file
    const uint32_t index  //!< Record index
) {
    RSF_File *fp = (RSF_File *) h.p;

    const int32_t fslot = RSF_Is_file_valid(fp);
    if (fslot == 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid file pointer\n", __func__);
        return info0;
    }

    if (index >= fp->vdir_used) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: that record index does not exist (%d), "
                "there are only %d available records\n",
                __func__, index, fp->vdir_used);
        return info0;
    }

    uint8_t version;
    rsf_rec_class record_class;
    rsf_rec_type record_type;
    extract_meta0(fp->vdir[index]->meta[0], &version, &record_class, &record_type);

    if (record_type == RT_DEL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Trying to get a deleted record (index %d)\n", __func__, index);
        return info0;
    }

    if (version > RSF_VERSION_COUNT) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Trying to get record information for a record "
                "written by a newer version of the library (%d vs %d)\n",
                __func__, version, RSF_VERSION_COUNT);
        return info0;
    }

    RSF_record_info info;
    info.wa = RSF_32_to_64( fp->vdir[index]->wa );     // record address in file
    info.rl = RSF_32_to_64( fp->vdir[index]->rl );     // record size
    info.elem_size = fp->vdir[index]->dul;
    info.wa_meta = info.wa + sizeof(start_of_record);  // metadata address in file
    info.rec_meta = REC_ML(fp->vdir[index]->ml);       // record metadata length
    info.dir_meta = DIR_ML(fp->vdir[index]->ml);       // directory metadata length
    info.dir_meta0 = info.dir_meta;
    info.meta = fp->vdir[index]->meta;
    info.wa_data = info.wa_meta +                       // data address in file
                    info.rec_meta * sizeof(int32_t);
    info.data_size = info.rl -                          // length of data payload
                    sizeof(start_of_record) -
                    sizeof(end_of_record) -
                    info.rec_meta * sizeof(int32_t);
    info.rec_type = record_type;
    info.rsf_version = version;
    info.fname = NULL;                                 // if not a file container
    info.file_size = 0;                                // if not a file container
    if(record_type == RT_FILE){                         // file container detected
        char *temp0 = (char *) info.meta;
        char *temp = (char *) &info.meta[info.dir_meta];             // end of metadata
        while((temp[ 0] == '\0') && (temp > temp0)) temp--;    // skip trailing nulls
        while((temp[-1] != '\0') && (temp > temp0)) temp--;    // back until null is found
        info.fname = temp;                                     // file name from directory metadata
        info.dir_meta0 = (uint32_t *) temp - info.meta - 2;    // metadata length excluding file name and file length
        info.file_size = info.meta[info.dir_meta0];
        info.file_size <<= 32;
        info.file_size += info.meta[info.dir_meta0+1];
        // fprintf(stderr, "RSF_Get_record_info DEBUG: file container %s detected\n", temp);
    }
    return info;
}

//! Determine whether the given record key is found in the given file
int32_t RSF_Is_record_in_file(RSF_handle h, const int64_t key) {
  const uint32_t record_slot = key64_to_file_slot(key);
  const uint32_t file_slot = RSF_Is_file_valid(h.p);
  return (file_slot == record_slot);
}

//! Get information about a record
//! \return a RSF_record_info structure
RSF_record_info RSF_Get_record_info(
    RSF_handle h, //!< Pointer to a RSF file
    const int64_t key   //!< Record identifier
) {
    RSF_File *fp = (RSF_File *) h.p;

    const int32_t fslot = RSF_Is_file_valid(fp);
    const int32_t slot  = key64_to_file_slot(key);
    if (fslot == 0 || slot != fslot) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: inconsistent file slot (file says %d, record says %d). Key = 0x%x\n",
                __func__, fslot, slot, key);
        return info0;
    }

    // Get information from vdir using index from key (Lower 32 bits of key, starting from 0)
    const uint32_t index = key64_to_index(key);
    return RSF_Get_record_info_by_index(h, index);
}

// USER CALLABLE FUNCTION
// read nelem items of size itemsize into buf from  RSF file at "address"
// return number of items read
// this function assumes that the user got address / nelem*itemsize from the proper RSF functions
ssize_t RSF_Read(RSF_handle h, void *buf, int64_t address, int64_t nelem, int itemsize) {
  RSF_File *fp = (RSF_File *) h.p;
  ssize_t nbytes;
  int32_t fslot = RSF_Is_file_valid(fp);
  size_t count = nelem * itemsize;

  if(fslot == 0) return ERR_NO_FILE;

  lseek(fp->fd, address, SEEK_SET);   // start of data to read in file
  nbytes = read(fp->fd, buf, count);
  fp->last_op = OP_READ;                                // last operation was a read operation

  return nbytes / itemsize;
}

// write nelem items of size itemsize from buf into  RSF file at "address"
// return number of items written
// THIS CODE WOULD BE TOTALLY UNSAFE and is there only as an axample
// ssize_t RSF_Write(RSF_handle h, void *buf, int64_t address, int64_t nelem, int64_t itemsize) {
//   RSF_File *fp = (RSF_File *) h.p;
//   ssize_t nbytes;
//   int32_t fslot = RSF_Is_file_valid(fp);
//   size_t count = nelem * itemsize;
//   off_t offset;
//
//   if(fslot == 0) return ERR_NO_FILE;
//
//   offset = lseek(fp->fd, offset = address, SEEK_SET);   // start of record in file
//   nbytes = write(fp->fd, buf, count);
//   fp->last_op = OP_WRITE;                                // last operation was a write operation
//
//   return nbytes / itemsize;
// }

//! Read file at given address and perform sanity check the start-of-record
//! *This function is not threadsafe*
//! \return Record type (> 0) if everything went fine, negative otherwise
static rsf_rec_type RSF_Read_record(
    RSF_File* fp,           //!< File from which we are reading
    const uint64_t address, //!< Offset within the file where to start reading
    void* dest,             //!< Where the put the content read
    const size_t num_bytes  //!< How many bytes to read
) {
    lseek(fp->fd, address, SEEK_SET);

    const ssize_t nc = read(fp->fd, dest, num_bytes);
    if (nc != num_bytes) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not read record with the specified size %d\n", __func__, num_bytes);
        return RT_INVALID;
    }

    const rsf_rec_type record_type = (rsf_rec_type)RSF_Check_record_type(*(start_of_record*)dest, RT_NULL);
    if (record_type == 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Record does not seem valid\n", __func__);
        return RT_INVALID;
    }

    fp->last_op = OP_READ;         // last operation was a read operation

    return record_type;
}

//! Read the content of the specified record from the file. Option to only read metadata
//! Uses the file's mutex.
//! The caller is responsible for freeing the allocated space. If providing a preallocated space,
//! the caller is responsible for making sure it is large enough to read the entire record.
//! \return a RSF_record_handle if everyting went fine, NULL in case of error
RSF_record *RSF_Get_record(
    RSF_handle h,               //!< Handle to open file in which record is located
    const int64_t key,          //!< from RSF_Lookup, RSF_Scan_vdir
    const int32_t metadata_only,//!< [in] 1 if we only want to read the metadata, 0 otherwise
    void* prealloc_space,       //!< [out] [optional] If non-NULL, space in which the record will be read. *Must be large enough*
    RSF_record_info* info_out   //!< [out] [optional] If non-NULL, put record information here
){
    RSF_record_info info = RSF_Get_record_info(h, key);
    if (info.wa == 0) return NULL; // error detected by RSF_Get_record_info (should be printed already)

    if (info_out != NULL) { *info_out = info; }

    // Determine size to read
    const uint64_t read_size = (metadata_only == 1) ?
        sizeof(start_of_record) + info.rec_meta *sizeof(uint32_t) :
        info.rl;

    void* p = prealloc_space;
    if (p == NULL) p = malloc(read_size + sizeof(RSF_record));

    if (p == NULL) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to allocate memory for record (%d bytes)\n",
                __func__, read_size + sizeof(RSF_record));
        return NULL;
    }

    RSF_File * const fp = (RSF_File *) h.p;

    // --- START CRITICAL REGION --- //
    RSF_Multithread_lock(fp);

    RSF_record* record = (RSF_record *) p;
    const rsf_rec_type read_status = RSF_Read_record(fp, info.wa, record->d, read_size);

    RSF_Multithread_unlock(fp);
    // --- END CRITICAL REGION --- //

    if (read_status == RT_INVALID) {
            Lib_Log(APP_LIBFST, APP_ERROR, "%s: Error while reading record from disk.\n", __func__);
            return NULL;
    }

    // \todo : adjust the record struct using data from record (especially meta_size)
    record->rsz = read_size;
    record->sor = (char*)p + sizeof(RSF_record);

    // Verify that metadata size matches what's in the directory
    const uint32_t rlm = ((start_of_record *)record->sor)->rlm;     // record metadata length from record
    const uint32_t rlmd = ((start_of_record *)record->sor)->rlmd;   // directory metadata length from record
    if (rlm != info.rec_meta || rlmd != info.dir_meta) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Inconsistent metadata length between directory (%d/%d) and record (%d/%d)\n",
                __func__, info.rec_meta, info.dir_meta, rlm, rlmd);
        return NULL;
    }

    record->rec_meta = rlm;
    record->dir_meta = rlmd;
    record->meta = (uint32_t *)((char *)(record->sor) + sizeof(start_of_record));

    if (metadata_only == 1) {
        record->eor = NULL;
        record->data = NULL;
        record->data_size = 0;
        record->max_data = 0;
    } else {
        record->eor = (char*)p + sizeof(RSF_record) + read_size - sizeof(end_of_record);
        record->data = (char*)(record->sor) + sizeof(start_of_record) + sizeof(uint32_t) * rlm;
        record->data_size = (char *)(record->eor) - (char *)(record->data);
        record->max_data = record->data_size;
    }

    return record;
}

uint32_t RSF_Get_num_records(RSF_handle h) {
    const RSF_File * const fp = (RSF_File *) h.p;
    if (! RSF_Is_file_valid(fp) ) return UINT_MAX;
    return fp->vdir_used - fp->num_deleted_records;
}

uint32_t RSF_Get_num_records_at_open(RSF_handle h) {
    const RSF_File * const fp = (RSF_File *) h.p;
    if (! RSF_Is_file_valid(fp) ) return UINT_MAX;
    return fp->dir_read - fp->num_deleted_records;
}

//! Retrieve opening mode of the file (read-only, read-write, fuse, etc.)
int32_t RSF_Get_mode(RSF_handle h) {
    const RSF_File * const fp = (RSF_File *) h.p;
    return (int32_t)fp->mode;
}

void *RSF_Get_next(RSF_handle h) {
    const RSF_File * const fp = (RSF_File *) h.p;
    return (RSF_File *)fp->next;
}

//! Check whether the given handle points to a valid RSF file
//! \return 1 if the file is valid, 0 if not
//! \sa RSF_Is_file_valid
int32_t RSF_Is_handle_valid(
    RSF_handle h //!< The handle to check
) {
    const RSF_File * const fp = (RSF_File *) h.p;
    if ( RSF_Is_file_valid(fp) ) return 1;
    return 0;
}

//! lock/unlock file associated with file descriptor fp->fd
//! lock == 1 : lock, lock == 0 : unlock
static int32_t RSF_File_lock(
    const int32_t fd,   //!< OS file descriptor of the file to (un)lock
    const int do_lock      //!< Whether to lock (1) or unlock (0) the file
){
    struct flock file_lock;
    file_lock.l_whence = SEEK_SET;  // locked area is by position
    file_lock.l_start = 0;          // base of segment to be locked
    file_lock.l_len = 0;            // lock entire file
    file_lock.l_pid = 0;            // We want an "open file description" lock

    int status = -1;
    if (do_lock) {
        // LOCK file
        file_lock.l_type = F_WRLCK; // exclusive write lock
        int status = fcntl(fd, F_OFD_SETLKW, &file_lock); // do not wait
        if (status == 0) Lib_Log(APP_LIBFST, APP_DEBUG, "%s: locked by pid %d\n", __func__, getpid());
    }
    else {
        // UNLOCK file
        file_lock.l_type = F_UNLCK; // release lock
        const int status = fcntl(fd, F_OFD_SETLK, &file_lock);
        if (status == 0) Lib_Log(APP_LIBFST, APP_DEBUG, "%s: released by pid %d\n", __func__, getpid());
    }

    return status;
}

//! Try to lock the given file for writing.
//! This checks whether the file already exists. If not, create a first, empty segment and set the "write" flag on it.
//! If the file already exists, check whether it is available for writing in the requested mode (sparse or compact),
//! before marking the first segment as being written.
//! \return 0 if the file could be successfully locked, -1 if it was already locked by someone else.
static int RSF_Lock_for_write(
    RSF_File *fp  //!< [in,out] File we want to lock
) {
    int return_value = -1;

    // ---------- Lock file ---------- //
    RSF_File_lock(fp->fd, 1);

    // Go to beginning of file and try reading the first start_of_segment
    start_of_segment first_sos = SOS;
    lseek(fp->fd, 0, SEEK_SET);
    const ssize_t num_bytes_read = read(fp->fd, &first_sos, sizeof(start_of_segment));

    if (num_bytes_read == 0) { // File does not exist yet
        fp->is_new = 1;
        // Create an empty first segment
        Lib_Log(APP_LIBFST, APP_TRIVIAL, "%s: Creating a new, empty segment\n", __func__);

        for (int i = 0; i < 4; i++) { // Set application code
            first_sos.sig1[4+i] = fp->appl_code[i];
        }
        first_sos.head.rlm  = fp->seg_max_hint > 0 ? 1 : RSF_EXCLUSIVE_WRITE;
        first_sos.head.rlmd = DIR_ML(fp->dir_meta);
        first_sos.tail.rlm  = 0;
        RSF_64_to_32(first_sos.seg,  sizeof(start_of_segment));
        RSF_64_to_32(first_sos.sseg, sizeof(start_of_segment) + sizeof(end_of_segment));

        end_of_segment first_eos = EOS;
        RSF_64_to_32(first_eos.h.seg,  sizeof(start_of_segment));
        RSF_64_to_32(first_eos.h.sseg, sizeof(start_of_segment) + sizeof(end_of_segment));

        // Commit segment header/footer
        write(fp->fd, &first_sos, sizeof(start_of_segment));
        write(fp->fd, &first_eos, sizeof(end_of_segment));
    } else {
        // Check if anyone else is writing in this file
        if (first_sos.head.rlm == RSF_EXCLUSIVE_WRITE) {
            Lib_Log(APP_LIBFST, APP_WARNING, "%s: file %s is already open for exclusive write.\n", __func__, fp->name);
            goto RETURN;
        }
        else if (first_sos.head.rlm > 0 && fp->seg_max_hint <= 0) {
            Lib_Log(APP_LIBFST, APP_WARNING, "%s: file %s is already open for shared write. "
                    "Cannot lock it for exclusive write\n", __func__, fp->name);
            goto RETURN;
        }

        // Mark first segment as being open for write
        if (fp->seg_max_hint > 0) {
            first_sos.head.rlm += 1;
        } else {
            first_sos.head.rlm = RSF_EXCLUSIVE_WRITE;
        }

        // Commit changes
        lseek(fp->fd, 0, SEEK_SET);
        write(fp->fd, &first_sos, sizeof(start_of_segment));
    }

    lseek(fp->fd, 0, SEEK_SET);
    return_value = 0;
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Successfully locked file %s for writing\n", __func__, fp->name);

RETURN:
    RSF_File_lock(fp->fd, 0);
    // --------- Unlock file ---------- //

    return return_value;
}

//! Create an empty RSF segment : start of segment, empty directory, end of segment
static int32_t RSF_Ensure_new_segment(
    RSF_File *fp,    //!< [in,out] Pointer to file where the segment is being added
    const uint64_t min_segment_size //!< [optional] Minimum size (bytes) of the segment (only for sparse)
) {
    int status = -1;

    // If mode is RSF_RW, we know that this process has already marked the file as being open and reserved
    // for writing, so that flag is the only check needed to go on with creating the new segment
    if ((fp->mode & RSF_RW) != RSF_RW) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: File is not open in write mode\n", __func__);
        return -1;
    }

    RSF_Multithread_lock(fp);

    if (has_open_segment(fp)) {
        // Segment is already created
        status = 0;
        goto RETURN_OMP;
    }

    // --------- Lock file --------- //
    RSF_File_lock(fp->fd, 1);
    {
      // Update first SOS from file, if open in sparse mode (other processes may have modified it)
      if (fp->sos0.head.rlm != RSF_EXCLUSIVE_WRITE) {
          lseek(fp->fd, 0, SEEK_SET);
          read(fp->fd, &fp->sos0, sizeof(start_of_segment));
      }

      // Read last end-of-segment and get start location of new segment
      off_t new_segment_start = lseek(fp->fd, -sizeof(end_of_segment), SEEK_END);
      end_of_segment last_eos;
      read(fp->fd, &last_eos, sizeof(end_of_segment));
      new_segment_start += sizeof(end_of_segment);

      // Check that last EOS (high part) looks valid
      if(RSF_Rl_eosh(last_eos.h) == 0) {
          Lib_Log(APP_LIBFST, APP_ERROR, "%s: bad end of segment in file\n", __func__);
          goto RETURN;
      }

      // Initialize new SOS as a copy of the first SOS
      start_of_segment new_sos = SOS;
      memcpy(&new_sos, &fp->sos0, sizeof(start_of_segment));
      new_sos.head.rlm  = fp->rec_meta;
      new_sos.head.rlmd = fp->dir_meta;

      end_of_segment eos = EOS;
      if (fp->seg_max_hint > 0) {
          // We have a sparse segment

          // Determine sparse segment size from hint, subject to alignment on block boundary
          off_t top = new_segment_start + fp->seg_max_hint - 1;                    // Last address of segment
          off_t align = 1; align <<= SPARSE_BLOCK_ALIGN; align  = align -1;      // Compute alignment pattern
          top |= align;                                                      // Align top address on block boundary
          top = top + 1;
          const uint64_t sparse_size = top - new_segment_start;              // Adjusted size of sparse segment
          const uint64_t sparse_rl = sparse_size - sizeof(start_of_segment); // Sparse segment record length

          // Fill eos and new_sos with proper values (sparse segment size, record length)
          RSF_64_to_32(eos.h.tail.rl, sparse_rl);
          RSF_64_to_32(eos.h.sseg, sparse_size);
          RSF_64_to_32(eos.l.head.rl, sparse_rl);

          // Write end of segment (high part) at the correct position (this will create the "hole" in the sparse file
          lseek(fp->fd, new_segment_start + sparse_size - sizeof(end_of_segment_hi), SEEK_SET);
          write(fp->fd, &eos.h, sizeof(end_of_segment_hi));
          RSF_64_to_32(new_sos.sseg, sparse_size);                       // sseg MUST be non zero in a new sparse segment
          fp->seg_max = sparse_size;

      } else {
          // We have a compact segment

          eos.l.head.rlm = fp->sos0.head.rlmd;
          eos.h.tail.rlm = fp->sos0.head.rlmd;
          RSF_64_to_32(new_sos.sseg, 0L); // Unlimited size
          fp->seg_max = 0;
          fp->seg_max_hint = 0;

      } // if sparse

      RSF_64_to_32(new_sos.vdir, 0L);                                   // no directory, set size and offset to 0;
      RSF_64_to_32(new_sos.vdirs, 0L);
      RSF_64_to_32(new_sos.seg, 0L);                                   // seg = 0 when a segment is open

      // Copy new SOS into RSF_File and write it to disk at end of file
      // memcpy(&(fp->sos1), &new_sos, sizeof(start_of_segment));
      lseek(fp->fd, new_segment_start, SEEK_SET);
      write(fp->fd, &new_sos, sizeof(start_of_segment));

      // Copy new EOS into RSF_File and write it to disk after its SOS (only commit the lower part for sparse files)
      memcpy(&(fp->eos1), &eos, sizeof(end_of_segment));
      write(fp->fd, &eos.l, sizeof(end_of_segment_lo));
      if (fp->seg_max_hint == 0) write(fp->fd, &eos.h, sizeof(end_of_segment_hi));

      fp->seg_base   = new_segment_start;                                     // base offset of active segment
      fp->next_write = new_segment_start + sizeof(start_of_segment);
      Lib_Log(APP_LIBFST, APP_DEBUG, "%s: %s EOF at %lx\n",
              __func__, (fp->seg_max_hint > 0) ? "sparse" : "compact", lseek(fp->fd, 0L, SEEK_END));
      fp->last_op = OP_WRITE;

      status = 0;
      // fprintf(stderr,"after segment create %ld\n", lseek(fp->fd, 0L , SEEK_CUR));
      // system("ls -l demo0.rsf");
    }

RETURN :
    RSF_File_lock(fp->fd, 0);
    // -------- Unlock file -------- //
RETURN_OMP :
    RSF_Multithread_unlock(fp);

    return status;
}

//! Check that the beginning of the file makes sense for an RSF
//! \return 1 if it makes sense, 0 if not
int32_t RSF_Basic_check(const char* filename) {

  int32_t result = 0;
  int32_t fd = open(filename, O_RDONLY);

  if (fd == -1) {
    Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to open file %s\n", __func__, filename);
    return 0;
  }

  size_t segment_offset = 0;

  int num_segments = 0;
  // Check all segments one by one
  while (1) {
    start_of_segment sos;
    lseek(fd, segment_offset, SEEK_SET);
    size_t num_bytes_read = read(fd, &sos, sizeof(start_of_segment));
    if (num_bytes_read < sizeof(start_of_segment)) {
      // Only a problem if it's the first segment
      if (num_segments == 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Unable to read start of first segment of file %s\n", __func__, filename);
        goto END;
      }
      break;
    }

    // Validate start-of-segment record
    if (RSF_Rl_sor(sos.head, RT_SOS) == 0 || RSF_Rl_eor(sos.tail, RT_SOS) == 0 ||
        RSF_Rl_sos(sos) == 0) {
      Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid start of segment for file %s\n", __func__, filename);
      goto END;
    }

    // Check start of directory (if there is one)
    const uint64_t vdir_offset = RSF_32_to_64(sos.vdir);
    const uint64_t vdir_size   = RSF_32_to_64(sos.vdirs);
    if (vdir_size > 0) {
      disk_vdir vdir;
      lseek(fd, segment_offset + vdir_offset, SEEK_SET);
      num_bytes_read = read(fd, &vdir, sizeof(disk_vdir));
      if (num_bytes_read != sizeof(disk_vdir) || RSF_Rl_sor(vdir.sor, RT_VDIR) == 0) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Invalid start of directory for file %s\n", __func__, filename);
        goto END;
      }
    }

    const uint64_t segment_size = RSF_32_to_64(sos.sseg);

    if (segment_size < sizeof(start_of_segment)) {
      Lib_Log(APP_LIBFST, APP_ERROR, "%s: Segment %d not closed properly\n", __func__, num_segments);
      goto END;
    }

    segment_offset += segment_size;
    num_segments++;
  }

  result = 1;

END:
  close(fd);
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: %d segments in file\n", __func__, num_segments);
  return result;
}

//! Open a file (file segment)
//!
//! Even if a file is open in write mode, its previous contents (but no new records) will remain available
//! if the file is open in read mode by another thread/process.
//! A file CANNOT be open in write mode by 2 threads/processes except if the file is sparse
//! (different threads/processe may be writing simultaneously into different segments).
//! A segment is marked as being written into by setting the seg field of the start_of_segment to 0
//! (done under file region lock if supported by the filesystem)
//!
//! \return Handle (pointer to file control structure) if successful, NULL if error
RSF_handle RSF_Open_file(
    //!> [in] Path to the file (will be stored internally as a canonical path)
    const char    *fname,
    //!> [in] RO/RW/AP/... If 0, will be RW if file is writable/creatable, RO otherwise
    const rsf_open_mode_type mode,
    //!> [in] Directory metadata size in 32-bit units
    const int32_t dir_meta_length,
    //!> [in] 4-character application identifier. Chosen by the calling application.
    const char    *appl,
    //!> [in,out] [Optional]
    //!> As output: Segment size from file
    //!> If NULL or *segsizep == 0: ignored
    //!> As input: File is open as 'sparse'
    //!>    Segment size is always a multiple of 1 MB, determined as *segsizep & 0xFFFFFFFFFFF00000
    //!>    Segment N to open is *segsizep & 0xFF
    int64_t *segsizep
) {
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Opening file %s\n", __func__, fname);

  RSF_File *fp = (RSF_File *) malloc(sizeof(RSF_File));   // allocate a new RSF_File structure
  RSF_handle handle;
  char *errmsg = "";

  const int64_t segsize = segsizep ? *segsizep : 0;     // segsize will be 0 if segsizep is NULL

  if(fp == NULL) {
    errmsg = "allocation failed";
    goto ERROR;
  }

  if(segsize < 0) goto ERROR;           // invalid segment size

  RSF_File_init(fp);                    // set safe initial values

  fp->name = realpath(fname, NULL);     // get canonical path

  for (int i = 0; i < 4; i++) fp->appl_code[i] = appl[i];
  fp->dir_meta = DIR_ML(dir_meta_length);
  fp->rec_meta = REC_ML(dir_meta_length);
  fp->seg_max_hint = segsize;

  fp->mode = mode;
  if(fp->mode == 0) fp->mode = RSF_RW;  // automatic mode, try to open for read+write with RO fallback

  // Open file and determine actual mode, based on whether we were able to open the file in the requested mode.
  switch(fp->mode & (RSF_RO | RSF_RW | RSF_AP | RSF_FUSE)){

    case RSF_RO:                         // open for read only
      errmsg = "file not found";
      if( (fp->fd = open(fname, O_RDONLY)) == -1 ) goto ERROR;  // file does not exist or is not readable
      break;

    case RSF_RW:                         // open for read+write, create if it does not exist
      fp->fd = open(fname, O_RDWR | O_CREAT, 0777);
      if(fp->fd == -1){                  // fallback, try to open in read only mode
        fp->mode = RSF_RO;
        errmsg = "file does not exist or is not readable";
        if( (fp->fd = open(fname, O_RDONLY)) == -1 ) goto ERROR;  // file does not exist or is not readable
      }
      // Try to lock the file for writing. If that doesn't work, cancel opening
      // This creates the first segment if it didn't already exist
      if (RSF_Lock_for_write(fp) < 0) {
        close(fp->fd);
        goto ERROR;
      }
      break;

    case RSF_AP:                         // to be added later
      fp->mode = RSF_RW;                    // open existing file for read+write, no fallback
      errmsg = "cannot open in write mode";
      if( (fp->fd = open(fname, O_RDWR, 0777)) == -1) goto ERROR;  // cannot open in write mode
      break;

    case RSF_FUSE:
      fp->fd = open(fname, O_RDWR, 0777);
      fp->mode = RSF_FUSE;

      errmsg = "unable to open file in write mode";
      if (fp->fd == -1) goto ERROR;

      errmsg = "unable to lock for writing ";
      if (RSF_Lock_for_write(fp) < 0) {
        close(fp->fd);
        goto ERROR;
      }
      break;

    default:
      errmsg = "opening mode is not valid";
      goto ERROR;
  }
  errmsg = "";

  RSF_Vdir_setup(fp);

  // Verify that there is something to read in the file
  lseek(fp->fd, fp->seg_base, SEEK_SET);               // first segment of the file
  if (read(fp->fd, &fp->sos0, sizeof(start_of_segment)) < sizeof(start_of_segment)) {
    errmsg = "file is empty";
    close(fp->fd);
    goto ERROR;  // invalid SOS (too short)
  }
  // Lib_Log(APP_LIBFST, APP_DEBUG, "%s: SOS:\n", __func__);
  // print_start_of_segment(&fp->sos0);

  fp->dir_meta = fp->sos0.head.rlmd;
  fp->rec_meta = fp->dir_meta;

  if( RSF_Rl_sor(fp->sos0.head, RT_SOS) == 0 ) {
    errmsg = "invalid SOS head (wrong record type)";
    goto ERROR;
  }
  if( RSF_Rl_eor(fp->sos0.tail, RT_SOS) == 0 ) {
    errmsg = "invalid SOS tail (wrong record type)";
    goto ERROR;
  }
  if( RSF_Rl_sos(fp->sos0) == 0 ) {
    errmsg = "invalid SOS (head and tail sizes don't match)";
    goto ERROR;
  }
  if( check_application_code(fp, &fp->sos0) < 0 ) {
    errmsg = "wrong application code";
    goto ERROR;
  }

  fp->slot = RSF_Set_file_slot(fp);        // insert into file table
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: %s mode, slot %d, reading directory\n",
          __func__, open_mode_to_str(fp->mode), fp->slot);
  if( RSF_Read_directory(fp) < 0 ) {         // read directory from all segments
    errmsg = " error while reading directory";
    RSF_Purge_file_slot(fp);               // remove from file table in case of error
    goto ERROR;
  }

  fp->last_op = OP_NONE;
  fp->seg_max = 0;
  if (segsizep) *segsizep = segsize;

  if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_EXTRA) print_start_of_segment(&fp->sos0);
  // fprintf(stderr,"RSF_Open_file: 4, fp = %p\n", fp);
  // fprintf(stderr,"after open %ld, next write = %ld\n", lseek(fp->fd, 0L , SEEK_CUR), fp->next_write);
  handle.p = fp;
  return handle;

ERROR:
  Lib_Log(APP_LIBFST, APP_ERROR, "%s: '%s' %s\n", __func__, fname, errmsg);
  if(fp != NULL) {
    if(fp->name != NULL) free(fp->name);  // free allocated string for file name
    free(fp);                             // free allocated structure
  }
  handle.p = NULL;
  return handle;                          // return a NULL handle
}

//! Close a RSF compact file segment, without closing the file. *The file must be valid*
//!  - write directory record
//!  - write EOS record for compact segment
//!  - rewrite original SOS (segment 0 SOS if FUSING segments)
//! \return 1 on success, something else on error
static int32_t RSF_Close_compact_segment(
    RSF_File* fp //!< Pointer to a valid open RSF file
) {
    uint64_t vdir_size;
    off_t offset_vdir, offset_eof;
    start_of_segment sos = SOS;
    end_of_segment eos = EOS;

    if ((fp->mode & RSF_RO) == RSF_RO) return 1;    // file open in read only mode, nothing to do
    if (fp->next_write == 0) return 1;              // No open segment, nothing to do
    if (fp->seg_max != 0) return 0;                 // not a compact segment

    if ((fp->mode & RSF_FUSE) == RSF_FUSE) {
        if (fp->seg_base > 0) {                     // no need to fuse if segment 0
            lseek(fp->fd, fp->seg_base , SEEK_SET);             // seek to start of this segment
            write(fp->fd, &sos, sizeof(start_of_segment));      // nullify original start_of_segment
            fp->seg_base = 0;                                   // segment 0 will become the only segment in file
            fp->dir_read = 0;                                   // all directory entries will be written
            Lib_Log(APP_LIBFST, APP_DEBUG, "%s: fusing all segments into one\n", __func__);
        }
        return 1;   // NO ERROR
    }

    // write directory, but move dir_read to avoid writng same record directory entries again
    offset_vdir = fp->next_write - fp->seg_base;                    // vdir offset into segment
    vdir_size = RSF_Write_vdir(fp);                                 // write vdir
    if (vdir_size == 0) offset_vdir = 0;                             // no directory
    fp->dir_read = fp->vdir_used;                                   // keep current directory info (no writing again)
    lseek(fp->fd, fp->next_write , SEEK_SET);                       // set position of write pointer

    // write active compact segment EOS
    offset_eof = fp->next_write - fp->seg_base  + sizeof(end_of_segment);
    //   eos.l.head.rlm = fp->meta_dim;
    eos.l.head.rlm = fp->rec_meta;
    eos.l.head.rlmd = fp->dir_meta;
    RSF_64_to_32(eos.l.head.rl, sizeof(end_of_segment));
    //   eos.h.meta_dim = fp->meta_dim;
    RSF_64_to_32(eos.h.vdir,  offset_vdir);                         // vdir record position in file
    RSF_64_to_32(eos.h.vdirs, vdir_size);                           // vdir record size
    RSF_64_to_32(eos.h.sseg, offset_eof);                           // segment size including EOS
    RSF_64_to_32(eos.h.seg,  offset_eof - sizeof(end_of_segment));  // segment size excluding EOS
    //   eos.h.tail.rlm = fp->meta_dim;
    eos.h.tail.rlm = fp->rec_meta;
    RSF_64_to_32(eos.h.tail.rl, sizeof(end_of_segment));
    write(fp->fd, &eos, sizeof(end_of_segment));               // write end of compact segment

    // rewrite active segment SOS (segment 0 SOS if FUSING)
    lseek(fp->fd, fp->seg_base , SEEK_SET);
    read(fp->fd, &sos, sizeof(start_of_segment));   // read start of segment (to keep appl signature)
    RSF_64_to_32(sos.vdir,  offset_vdir);           // vdir record position in file
    RSF_64_to_32(sos.vdirs, vdir_size);             // directory record size
    RSF_64_to_32(sos.sseg,  offset_eof);            // segment size including EOS
    RSF_64_to_32(sos.seg,  offset_eof - sizeof(end_of_segment));             // segment size excluding EOS
    sos.head.rlm = 0;    // fix SOR of appropriate SOS record if file was open in write mode
    sos.head.rlmd = fp->dir_meta;
    lseek(fp->fd, fp->seg_base , SEEK_SET);
    write(fp->fd, &sos, sizeof(start_of_segment));      // rewrite start of active segment

    fp->next_write = 0;

    return 1;   // NO ERROR
}

//! Close a RSF sparse file segment
//!  - Write directory record (no longer needed, as the new segment will get the directory)
//!  - Rewrite SOS of sparse segment (becomes compact)
//!  - Write EOS of compact segment
//!  - Write SOS for the sparse segment that gets the remaining space
//!  - Write a split EOS record for the sparse segment
//! This is an internal function, it skips a few checks
//! \return Always 1?
static int32_t RSF_Close_sparse_segment(
    RSF_File* fp //!< Pointer to a *valid and open* RSF file
) {
    off_t offset_eof;
    uint64_t rl_eos;
    start_of_segment sos = SOS;
    end_of_segment eos = EOS;

    if( (fp->mode & RSF_RO) == RSF_RO) return 1;    // file open in read only mode, nothing to do
    if (!is_segment_sparse(fp)) return 1;

    // write directory, but update dir_read to avoid writing same record information again
    off_t vdir_offset = fp->next_write - fp->seg_base;                    // vdir offset into segment
    const uint64_t vdir_size = RSF_Write_vdir(fp);                                 // write vdir
    if(vdir_size == 0) vdir_offset = 0;                             // no directory
    fp->dir_read = fp->vdir_used;                                   // keep current directory info
    lseek(fp->fd, fp->next_write , SEEK_SET);                       // set position of write pointer

    // write compact segment EOS
    offset_eof = fp->next_write - fp->seg_base  + sizeof(end_of_segment);
    eos.l.head.rlm = fp->rec_meta;
    eos.l.head.rlmd = fp->dir_meta;
    RSF_64_to_32(eos.l.head.rl, sizeof(end_of_segment));
    RSF_64_to_32(eos.h.vdir,  vdir_offset);                         // vdir record position in file
    RSF_64_to_32(eos.h.vdirs, vdir_size);                           // vdir record size
    RSF_64_to_32(eos.h.sseg, offset_eof);                           // segment size including EOS
    RSF_64_to_32(eos.h.seg,  offset_eof - sizeof(end_of_segment));  // segment size excluding EOS
    eos.h.tail.rlm = fp->rec_meta;
    RSF_64_to_32(eos.h.tail.rl, sizeof(end_of_segment));
    write(fp->fd, &eos, sizeof(end_of_segment));               // write end of compact segment

    // write sparse segment SOS
    const uint64_t sparse_start = lseek(fp->fd, 0L, SEEK_CUR);  // start of sparse segment after compact EOS
    const uint64_t sparse_top = fp->seg_base + fp->seg_max;     // end of sparse segment
    const uint64_t sparse_size = sparse_top - sparse_start;     // new sparse segment size
    RSF_64_to_32(sos.seg, 0L);
    RSF_64_to_32(sos.sseg, sparse_size);
    write(fp->fd, &sos, sizeof(start_of_segment));

    // fix sparse segment EOS
    memcpy(&eos, &fp->eos1, sizeof(eos));                           // get original EOS
    rl_eos = sparse_size - sizeof(start_of_segment);                // length covered by EOS
    RSF_64_to_32(eos.l.head.rl, rl_eos);
    write(fp->fd, &eos.l, sizeof(end_of_segment_lo));          // write low part of EOS
    RSF_64_to_32(eos.h.tail.rl, rl_eos);
    RSF_64_to_32(eos.h.sseg, sparse_size);                          // size of sparse segment
    off_t cur = lseek(fp->fd, rl_eos - sizeof(end_of_segment_hi) - sizeof(end_of_segment_lo), SEEK_CUR);
    Lib_Log(APP_LIBFST, APP_EXTRA, "%s: sparse eos at %12.12x\n", __func__, cur);
    write(fp->fd, &eos.h, sizeof(end_of_segment_hi));          // write high part of EOS

    // make original segment (sparse) into a compact segment (cannot be segment 0)
    lseek(fp->fd, fp->seg_base , SEEK_SET);
    read(fp->fd, &sos, sizeof(start_of_segment));              // read start of segment (to keep appl signature)
    RSF_64_to_32(sos.vdir,  vdir_offset);                           // vdir record position in file
    RSF_64_to_32(sos.vdirs, vdir_size);                             // vdir record size
    RSF_64_to_32(sos.sseg,  offset_eof);                            // segment size including EOS
    RSF_64_to_32(sos.seg,  offset_eof - sizeof(end_of_segment));    // segment size excluding EOS
    sos.head.rlm = 0;                                               // fix SOR of SOS record
    sos.head.rlmd = fp->dir_meta;
    cur = lseek(fp->fd, fp->seg_base , SEEK_SET) ;
    Lib_Log(APP_LIBFST, APP_EXTRA, "%s: original sos rewrite at %12.12x\n", __func__, cur);
    write(fp->fd, &sos, sizeof(start_of_segment));             // rewrite start of active segment

    return 1;
}

//! Close sparse segment (if appropriate), then open a new one
//! \return 1 on success, 0 on failure
int32_t RSF_Switch_sparse_segment(RSF_handle h, int64_t min_size){
    RSF_File *fp = (RSF_File *) h.p;
    if (!RSF_Is_file_valid(fp)) return 0;   // something not O.K. with fp
    if ((fp->mode & RSF_RO) == RSF_RO) return 1;    // file open in read only mode, nothing to do

    if (RSF_Close_sparse_segment(fp) != 1) return 0;
    fp->next_write = 0;
    RSF_Ensure_new_segment(fp, min_size);

    return 1;
}

//! Reset write flag of the given file, by setting it to 0 if the file was open in exclusive write, or
//! by decrementing it by one if it was open in parallel (shared) write.
//! *This function does not perform any check on the segment to change.*
//! \return 1 on success, 0 or negative on failure
int32_t RSF_Reset_write_flag(
    const int32_t file_descriptor,     //!< Handle to an open and valid RSF file
    const int full_reset    //!< Whether to completely reset the flag (if 1), or simply decrement writer count (0)
) {
    int32_t status = 0;

    // ---------- Lock file ---------- //
    RSF_File_lock(file_descriptor, 1);

    // Rewrite first start of segment to indicate that we will no longer write to this file
    lseek(file_descriptor, 0, SEEK_SET);
    start_of_segment sos0;
    ssize_t num_read = read(file_descriptor, &sos0, sizeof(start_of_segment));
    if (num_read < sizeof(start_of_segment)) return 0;
    // print_start_of_segment(&fp->sos0);

    const uint32_t old_flag = sos0.head.rlm;

    // Set to zero if exclusive (compact mode), decrement if sparse
    if (sos0.head.rlm == RSF_EXCLUSIVE_WRITE || full_reset == 1) {
        sos0.head.rlm = 0;
    }
    else if (sos0.head.rlm > 0) {
        sos0.head.rlm = sos0.head.rlm - 1;
    }

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: rewriting segment 0 header, rlm = %x (was %x)\n",
            __func__, sos0.head.rlm, old_flag);
    if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_EXTRA) print_start_of_segment(&sos0);

    lseek(file_descriptor, 0, SEEK_SET);
    const ssize_t num_written = write(file_descriptor, &sos0, sizeof(start_of_segment));  // rewrite start of segment 0
    if (num_written != sizeof(start_of_segment)) {
        Lib_Log(APP_LIBFST, APP_ERROR, "%s: Could not rewrite first segment! (%ld)\n", __func__, num_written);
        goto UNLOCK;
    }

    status = 1;

UNLOCK:
    RSF_File_lock(file_descriptor, 0);
    // --------- Unlock file --------- //

    return status;
}

//! Close the given RSF file
//!
//! We discriminate between compact segment (exclusive write) and sparse segment (parallel write)
//! In sparse mode
//!     2 SOS records (first SOS in file and SOS of sparse segment) must be rewritten
//!     1 EOS (split EOS) record must be rewritten
//! In compact mode
//!     2 SOS records (first SOS in file and active segment SOS) must be rewritten
//!     1 EOS record (at end of file) must be rewritten
//! In fuse mode: All segments will be fused into one
//!     the original SOS for the active segment is reinitialized to 0
//!     the first SOS in file becomes the only good one and points to the final EOS
//!     the EOS at end of file becomes the only good one
//!     all other EOS/SOS records in file (if any) will be ignored
//!     all DIR records except the last one will be ignored
//! \return 1 on success, 0 if fails.
int32_t RSF_Close_file(RSF_handle h){
    RSF_File *fp = (RSF_File *) h.p;

    if(!RSF_Is_file_valid(fp)) return 0;   // something not O.K. with fp

    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: Closing file %s, open in mode %s\n",
            __func__, fp->name, open_mode_to_str(fp->mode));

    if( (fp->mode & RSF_RO) == RSF_RO) goto CLOSE;  // file open in read only mode, nothing to rewrite

    if ((fp->mode & RSF_RW) == RSF_RW && fp->next_write < 0) {
        if (!fp->is_new && !fp->has_rewritten)
        Lib_Log(APP_LIBFST, APP_WARNING,
                "%s: Closing a file that was opened in write mode, but nothing was written! (%s)\n",
                __func__, fp->name);
    }
    else if (is_segment_sparse(fp)) {
        RSF_Close_sparse_segment(fp);
    }
    else {
        RSF_Close_compact_segment(fp);
    }

    RSF_Reset_write_flag(fp->fd, 0);

    Lib_Log(APP_LIBFST, APP_EXTRA, "%s: EOF at %lx\n", __func__, lseek(fp->fd, 0L, SEEK_END));

CLOSE :
    close(fp->fd);                                  // close file
    RSF_Purge_file_slot(fp);                        // remove from file table

    // free memory associated with file
    free(fp->name);                                 // free file name buffer

    // free dirblocks and vdir
    directory_block *purge;
    while(fp->dirblocks != NULL){
        purge = fp->dirblocks->next;    // remember next block address
        free(fp->dirblocks);            // free current block
        fp->dirblocks = purge;          // next block
    }

    if (fp->vdir != NULL) free(fp->vdir);
    free(fp);          // free file control structure

    return 1;
}

// dump the contents of a file's directory in condensed format
void RSF_Dump_vdir(RSF_handle h) {
  RSF_File *fp = (RSF_File *) h.p;
  int32_t i, j, meta_dim;
  uint64_t wa, rl;
  uint32_t *meta;
  vdir_entry *ventry;

  if(!RSF_Is_file_valid(fp)) return;   // something not O.K. with fp

  for(i = 0; i < fp->vdir_used; i++){
    ventry = fp->vdir[i];
    wa = RSF_32_to_64(ventry->wa);
    rl = RSF_32_to_64(ventry->rl);
//     meta_dim = (ventry->ml) >> 16;
    meta_dim = DIR_ML(ventry->ml);
    meta = &(ventry->meta[0]);
    fprintf(stderr,"%12.12lx (%8.8lx)", wa, rl);
    for(j = 0; j < meta_dim; j++) fprintf(stderr," %8.8x", meta[j]);
    fprintf(stderr,"\n");
  }
}

const char* readable_size(char* buffer, const int64_t val, const int num_char) {
  const char suffix[] = {' ', 'k', 'M', 'G', 'T', 'P', 'E'};
  int num_div = 0;
  int64_t printed_val = val;
  int64_t threshold = 1;
  for (int i = 0; i < num_char; i++) threshold *= 10;
  if (printed_val > threshold) threshold /= 10; // We have one less character available
  while (printed_val > threshold) {
    printed_val /= 1000;
    num_div++;
  }
  if (num_div > 0) {
    snprintf(buffer, num_char + 2, "%*ld%c", num_char - 1, printed_val, suffix[num_div]);
    // fprintf(stderr, "initial num = %ld, new num = %ld, num div = %d, threshold = %d\n", val, printed_val, num_div, threshold);
  }
  else {
    const char format[] = {'%', '0' + num_char, 'l', 'd', '\0'};
    snprintf(buffer, num_char + 1, format, printed_val);
  }
  return buffer;
}

const char* truncated_hex(char* buffer, const uint64_t num, const int num_char) {
  uint64_t left = num;
  int num_digits = 1;
  uint64_t mask = 0xf;
  while (left > 0xf) {
    left /= 16;
    num_digits++;
    if (num_digits <= num_char) {
      mask <<= 4;
      mask |= 0xf;
    }
  };
  const int actual_num_char = num_digits > num_char ? num_char - 1 : num_char;
  const char* dot = num_digits > num_char ? "*" : "";
  if (num_digits > num_char) mask >>= 4;

  // fprintf(stderr, "num = %lx, num_char = %d, num_digits = %d, mask = %lx, actual_num_char = %d, masked num = %lx\n",
  //        num, num_char, num_digits, mask, actual_num_char, num & mask);

  snprintf(buffer, num_char + 1, "%s%*.*lx", dot, actual_num_char, actual_num_char, num & mask);
  return buffer;
}

// dump the contents of a file in condensed format
void RSF_Dump(char *name, int verbose){
  int fd = open(name, O_RDONLY);
  start_of_record sor;
  end_of_record   eor;
  start_of_segment sos;
  end_of_segment   eos;
  off_t reclen, datalen, tlen, eoslen, read_len;
  ssize_t nc;
  uint64_t segsize, ssize;
  disk_vdir *vd = NULL;
  vdir_entry *ventry;
  uint32_t *meta;
  uint32_t *data;
  int ndata;
  off_t dir_offset, dir_addr, vdir_addr, rec_offset, offset, seg_offset, dir_seg_offset, l_offset;
  int64_t wa, rl;
  int tabplus = 0;
  uint64_t seg_bot, seg_top, seg_vdir;
  int segment = 0;
  uint64_t eof;
  char buffer[4096];
  char *temp, *temp0;
  uint32_t *tempm;
  uint32_t nmeta;
  uint64_t temps;

  const int max_num_meta = 7;

  if(fd < 0) return;
  eof = lseek(fd, 0L, SEEK_END);
  lseek(fd, 0L, SEEK_SET);
  dir_addr = 0;
  vdir_addr = 0;
  offset = 0;
  rec_offset = offset;  // current position
  seg_offset = 0;
  dir_seg_offset = 0;
  seg_bot = 0;
  seg_top = 0;
  seg_vdir = 0;

  fprintf(stderr,"RSF file dump utility, file =%s\n",name);
  fprintf(stderr,"=============================================================================================\n"
                 "V = RSF version\n"
                 "RL = raw record length (bytes)\n"
                 "PL = payload length (DL+ML*4) (bytes)\n"
                 "DL = data length (bytes)\n"
                 "ML = record metadata length (32 bit units)\n"
                 "rlmd = directory metadata length (32 bit units)\n"
                 "rlm = same as ML (sor|eor)\n"
                 "RT = record type\n"
                 "RC = record class\n"
                 "=============================================================================================\n");
  fprintf(stderr,"    Type Rec-#  Offset         RL(   PL)   V RC       DL  ML  ");
  for (int i = 0; i < max_num_meta; i++) {
    fprintf(stderr, "         ");
  }
  fprintf(stderr, " rlmd rlm|Sanity check\n");


  nc = read(fd, &sor, sizeof(sor)); // Read first record
  int rec_index = 0;
  int in_segment = 0;
  start_of_segment in_sos = SOS;

  // Print records 1 by 1
  while (nc > 0) {

    // ---------- Generic record info ----------
    reclen = RSF_32_to_64(sor.rl);
    datalen = reclen - sizeof(sor) - sizeof(eor);
    // tabplus = ((rec_offset < seg_dir) && (rec_offset < seg_vdir)) ? 8 : 0;  // only effective for sos, eos, dir records
    tabplus = (rec_offset < seg_vdir) ? 8 : 0;  // only effective for sos, eos, dir records
    char buf1[32];
    char buf2[32];
    char buf3[32];
    char buf4[32];
    snprintf(buffer, sizeof(buffer), "[%-4.4s] %5d [%s], %s(%s),",
             rt_to_str(sor.rt) + 3, rec_index, truncated_hex(buf3, rec_offset, 9), readable_size(buf1, reclen, 5),
             readable_size(buf2, datalen, 5));

    // ---------- Specific record info ----------
    int num_meta = -1;
    uint8_t version;
    rsf_rec_type rec_type;
    rsf_rec_class rec_class;
    switch(sor.rt) {
      case RT_VDIR :
      {
        dir_offset = lseek(fd, -sizeof(sor), SEEK_CUR);
        vd = (disk_vdir *) malloc(datalen + sizeof(sor));
        nc = read(fd, vd, datalen + sizeof(sor) );  // read directory
        fprintf(stderr,"%2s%s           , records  = %8d, dir address %s %s addr %s, rlm = %d",
                in_segment == 1 ? "  " : "<>", buffer,
                vd->entries, truncated_hex(buf1, dir_offset, 8), (dir_offset == vdir_addr) ? "==": "!=",
                truncated_hex(buf2, vdir_addr, 8), vd->sor.rlm);
        break;
      }
      case RT_DEL:
      case 0 :
      case 5 :
      case RT_DATA :
      case RT_XDAT :
      {
        read_len = sor.rlm * sizeof(uint32_t);
        data = (uint32_t *) malloc(read_len);
        nc = read(fd, data, read_len);               // read metadata part
        lseek(fd, datalen - nc, SEEK_CUR);           // skip rest of record
        ndata = datalen/sizeof(int32_t) - sor.rlm;
        extract_meta0(data[0], &version, &rec_class, &rec_type);
        fprintf(stderr,"  %s %2d %2x %6d*%d, %2d [", buffer, version, rec_class, ndata, sor.dul, sor.rlm);
        num_meta = sor.rlm <= max_num_meta ? sor.rlm : max_num_meta - 1;
        const int first = version == 0 ? RSF_META_RESERVED_V0 : RSF_META_RESERVED;
        for (int i=0; i < num_meta; i++) {
          fprintf(stderr," %8.8x", data[i + first]);
        }
        if (num_meta < sor.rlm) {
          fprintf(stderr,"   ...   ");
        }
        else if (sor.rlm < max_num_meta) {
          for (int i = sor.rlm; i < max_num_meta; i++) {
            fprintf(stderr, "         ");
          }
        }
        fprintf(stderr,"], %d, %d", sor.rlmd, sor.rlm);
        if(data) free(data);
        data = NULL;
        break;
      }
      case RT_FILE :
      {
        read_len = sor.rlm * sizeof(uint32_t);
        data = (uint32_t *) malloc(read_len);
        nc = read(fd, data, read_len);               // read metadata part
        temp0 = (char *) data;                       // start of metadata
        temp = temp0 + nc -1;                        // end of metadata
        while((temp[ 0] == 0) && (temp > temp0)) temp--;    // skip trailing nulls
        while((temp[-1] != 0) && (temp > temp0)) temp--;    // back until null is found
        temp0 = temp -1;
        tempm = (uint32_t *) temp0;
        nmeta = tempm - data;
        temps = RSF_32_to_64((data+nmeta-2));
        lseek(fd, datalen - nc, SEEK_CUR);           // skip rest of record
        ndata = datalen/sizeof(int32_t) - sor.rlm;
        fprintf(stderr,"  %s %6d B, %2d [ %8.8x ", buffer, ndata, sor.rlm, data[0]);
        for(int j=1; j<nmeta-2; j++) fprintf(stderr,"%8.8x ", data[j]);
        fprintf(stderr,"] '%s'[%ld]", temp, temps);
        // for(i=0; i<sor.rlm; i++) {
        //   fprintf(stderr," %8.8x", data[i]);
        // }
        fprintf(stderr,", %d, %d", sor.rlmd, sor.rlm);
        if(data) free(data);
        data = NULL;
        break;
      }
      case RT_SOS :
      {
        lseek(fd, -sizeof(sor), SEEK_CUR);
        seg_offset = rec_offset;
        nc = read(fd, &sos, sizeof(sos) - sizeof(eor));

        if (in_segment == 0) {
          in_sos = sos;
          vdir_addr = RSF_32_to_64(sos.vdir);
          // fprintf(stderr, "In sos, seg size = %lu\n", RSF_32_to_64(in_sos.sseg));
        }

        in_segment++;
        // meta_dim = sos.meta_dim;
        // dir_addr = RSF_32_to_64(sos.dir);
        segsize   = RSF_32_to_64(sos.sseg);
        ssize     = RSF_32_to_64(sos.seg);
        if(seg_offset >= seg_top) {
          seg_bot = seg_offset;
          seg_top = seg_bot + segsize - 1;
          seg_vdir = seg_bot + vdir_addr;
          dir_seg_offset = seg_offset;
        }
        fprintf(stderr,"%s",((ssize == 0 && segsize != 0) || in_segment != 1) ? "<>" : "  " );
        fprintf(stderr,"%s '", buffer);
        for(int i=0; i<8; i++) {
          fprintf(stderr,"%c", sos.sig1[i]);
        }
        fprintf(stderr,"', seg size = %s, dir_offset = %s %s, rlm = %d",
                readable_size(buf1, segsize, 5), truncated_hex(buf2, dir_addr, 8), truncated_hex(buf3, vdir_addr, 8),
                sos.head.rlm);
        break;
      }
      case RT_EOS :
      {
        lseek(fd, -sizeof(sor), SEEK_CUR);
        nc = read(fd, &eos, sizeof(eos) - sizeof(eor));          // read full EOS without end of record part
        eoslen = RSF_32_to_64(eos.l.head.rl);
        if(eoslen > sizeof(eos)) {                                // sparse segment EOS
          lseek(fd, eoslen - sizeof(eos) - sizeof(end_of_segment_hi) + sizeof(eor), SEEK_CUR);
          nc = read(fd, &eos.h, sizeof(end_of_segment_hi) - sizeof(eor));   // get high part of EOS
        }

        const uint64_t eos_seg_size = RSF_32_to_64(eos.h.sseg);
        const uint64_t sos_seg_size = RSF_32_to_64(sos.sseg);
        const uint64_t in_sos_seg_size = RSF_32_to_64(in_sos.sseg);
        // fprintf(stderr, "eos seg size = %lu, sos seg size = %lu\n", eos_seg_size, in_sos_seg_size);
        if (eos_seg_size == sos_seg_size || eos_seg_size == in_sos_seg_size) in_segment--;
        // in_segment--;
        // fprintf(stderr, "in_segment = %d\n", in_segment);

        segsize  = RSF_32_to_64(eos.h.sseg);
        ssize    = RSF_32_to_64(sos.seg);

        if (in_segment == 0) vdir_addr = RSF_32_to_64(eos.h.vdir);
        fprintf(stderr,"%s",(reclen > sizeof(end_of_segment) || in_segment > 0) ? "<>" : "  " );
        fprintf(stderr,"%s             seg size = %s, dir_offset = %8.8lx %8.8lx, rlm = %d", buffer,
                readable_size(buf1, segsize, 5), dir_addr, vdir_addr, eos.l.head.rlm);
        break;
      }
      default :
        lseek(fd, datalen, SEEK_CUR);   // skip data
        break;
    } // end switch record type

    nc = read(fd, &eor, sizeof(eor));
    tlen = RSF_32_to_64(eor.rl);
    if(tlen != reclen){
      fprintf(stderr,"|%d rl = %s|%s S(%2.2d) %s\n",
              eor.rlm, readable_size(buf1, reclen, 4), readable_size(buf2, tlen, 4), segment, (tlen==reclen) ? ", O.K." : ", RL ERROR");
    }else{
      fprintf(stderr,"|%d S(%2.2d) %s\n",eor.rlm, segment, ", O.K.");
    }
    if(tabplus == 0 && sor.rt == RT_EOS) segment++;
    if(tlen != reclen) goto END;; // Inconsistency in record length between SOR and EOR, so we stop there

    // Print content of virtual directory (if desired)
    if(sor.rt == RT_VDIR  && vd != NULL  && vd->entries > 0){
      if( (verbose > 0 && tabplus == 0) || (verbose > 10) ){
        fprintf(stderr," Directory  WA(SEG)    WA(FILE) WA(ENTRY) B   RLM  RLMD     RL  V RC RT META \n");
        char* e = (char *) &(vd->entry[0]);
        l_offset = (tabplus == 0) ? dir_seg_offset : seg_offset;
        for(int i=0; i < vd->entries; i++){
          ventry = (vdir_entry *) e;
          const int32_t ml = (ventry->ml >> 16);
          const int32_t mlr = ventry->ml & 0xFFFF;
          wa = RSF_32_to_64(ventry->wa);
          rl = RSF_32_to_64(ventry->rl);
          const uint32_t entry_offset = RSF_32_to_64(ventry->entry_offset);
          meta = &(ventry->meta[0]);
          extract_meta0(meta[0], &version, &rec_class, &rec_type);
          fprintf(stderr," [%6d] %s [%s] %s %1d %5d %5d %s",
                  i, truncated_hex(buf1, wa, 9), truncated_hex(buf2, wa+l_offset, 9),
                  truncated_hex(buf3, entry_offset, 9), ventry->dul, mlr, ml,readable_size(buf4, rl, 6));
          fprintf(stderr," %2d %2.2x %2.2x", version, rec_class, rec_type);
          if (rec_class != RC_FILE) {
            const int num_meta = ml <= max_num_meta ? ml : max_num_meta - 1;
            const int first = version == 0 ? RSF_META_RESERVED_V0 : RSF_META_RESERVED;
            for(int j=first; j<num_meta; j++) {
              fprintf(stderr," %8.8x", meta[j]);
            }
            if (num_meta < ml) fprintf(stderr, "   ...   ");
            fprintf(stderr,"\n");
          }else{
            temp0 = (char *) meta;
            temp = temp0 + (ml * sizeof(uint32_t)) -1;
            while((temp[ 0] == 0) && (temp > temp0)) temp--;    // skip trailing nulls
            while((temp[-1] != 0) && (temp > temp0)) temp--;    // back until null is found
            temp0 = temp -1;
            tempm = (uint32_t *) temp0;
            nmeta = tempm - meta;
            temps = RSF_32_to_64((meta+nmeta-2));
            // fprintf(stderr," %p %p",tempm, meta);
            for(int j=1; j<nmeta-2; j++)
              fprintf(stderr," %8.8x", meta[j]);
            fprintf(stderr," '%s' [%ld]\n", temp, temps);
          }
          e = e + sizeof(vdir_entry) + ml * sizeof(uint32_t);
        }
        fprintf(stderr,"-------------------------------------------------\n");
      }
    }
    if(vd) {
      free(vd);
      vd = NULL;
    }
    rec_index++;
    rec_offset = lseek(fd, offset = 0, SEEK_CUR);  // current position
    if(rec_offset >= eof) break;
    nc = read(fd, &sor, sizeof(sor));
    if((nc > 0) && (nc < sizeof(sor))){
      fprintf(stderr," invalid sor, len = %ld, expected = %ld\n", nc, sizeof(sor));
      break;
    }
  } // end while (nc > 0)
END :
  // fprintf(stderr,"DEBUG: DUMP: EOF at %lx\n", eof);
  close(fd);
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: file '%s' closed, fd = %d, EOF = %lx\n", __func__, name, fd, eof);
}

//! \return File slot of given file handle
int32_t RSF_Get_file_slot(RSF_handle h) {
  RSF_File *fp = (RSF_File *) h.p;
  return fp->slot;
}

void print_start_of_record(start_of_record* sor) {
  Lib_Log(APP_LIBFST, APP_ALWAYS,
          "type %s, rec meta length %d, zr %x, length %d, dir meta length %d, unused bits %d, elem length %d\n",
          rt_to_str(sor->rt), sor->rlm, sor->zr, RSF_32_to_64(sor->rl), sor->rlmd, sor->ubc, sor->dul);
}

void print_start_of_segment(start_of_segment* sos) {
  Lib_Log(APP_LIBFST, APP_ALWAYS, "head: "); print_start_of_record(&sos->head);
  char marker[9];
  strncpy(marker, (const char*)sos->sig1, sizeof(marker) - 1);
  marker[8] = '\0';
  Lib_Log(APP_LIBFST, APP_ALWAYS,
         "sig1 %s, sign %x, size (exc) %lu, size (inc) %lu, dir record offset %lu, dir record size %lu\n",
         marker, sos->sign, RSF_32_to_64(sos->seg), RSF_32_to_64(sos->sseg), RSF_32_to_64(sos->vdir),
         RSF_32_to_64(sos->vdirs));
}

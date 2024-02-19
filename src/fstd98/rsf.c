/*
 * Copyright (C) 2021  Environnement et Changement climatique Canada
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * Author:
 *     M. Valin,   Recherche en Prevision Numerique, 2021
 */
#include "fstd98/rsf_internal.h"

static int32_t verbose = RSF_DIAG_WARN ;
static char *diag_text[RSF_DIAG_DEBUG2+1] ;
static int diag_init = 1 ;

char *RSF_diag_level_text(int32_t level){
  int i ;
  if(diag_init){
    for(i = 0 ; i <= RSF_DIAG_DEBUG2 ; i++) diag_text[i] = "INVALID" ;
    diag_text[RSF_DIAG_NONE]  = "NONE" ;
    diag_text[RSF_DIAG_ERROR] = "ERROR" ;
    diag_text[RSF_DIAG_WARN]  = "WARN" ;
    diag_text[RSF_DIAG_INFO]  = "INFO" ;
    diag_text[RSF_DIAG_NOTE]  = "NOTE" ;
    diag_text[RSF_DIAG_DEBUG0] = "LOW DEBUG" ;
    diag_text[RSF_DIAG_DEBUG1] = "MID DEBUG" ;
    diag_text[RSF_DIAG_DEBUG2] = "MAX DEBUG" ;
    diag_init = 0 ;
  }
  if(level >= 0 && level <= RSF_DIAG_DEBUG2) {
    return diag_text[level] ;
  }else{
    return "INVALID" ;
  }
}

int32_t RSF_set_diag_level(int32_t level){
  int32_t old_level = verbose ;
  if(level == RSF_DIAG_NONE || level == RSF_DIAG_ERROR || level == RSF_DIAG_WARN || 
     level == RSF_DIAG_INFO || level == RSF_DIAG_NOTE  || level == RSF_DIAG_DEBUG0 ||
     level == RSF_DIAG_DEBUG1 || level == RSF_DIAG_DEBUG2 ) {
    verbose = level ;
  }
  return old_level ;
}

// =================================  table of pointers to rsf files (slots) =================================
static pointer *rsf_files = NULL ;         // global table of pointers to rsf files (slot table)
static int rsf_files_open = 0 ;            // number of rsf files currently open
static size_t max_rsf_files_open = 1024 ;  // by default no more than 1024 files can be open simultaneously

// allocate global table of pointers (slot table) to rsf files if not already done
// return table address if successful, NULL if table cannot be allocated
static void *RSF_Slot_table_allocate()
{
  struct rlimit rlim ;

  if(rsf_files != NULL) return rsf_files ;                    // slot table already allocated
  getrlimit(RLIMIT_NOFILE, &rlim) ;                           // get open files number limit for process
  if(rlim.rlim_cur > max_rsf_files_open) max_rsf_files_open = rlim.rlim_cur ;
  max_rsf_files_open = (max_rsf_files_open <= 131072) ? max_rsf_files_open : 131072 ; // never more than 128K files
  return  calloc(sizeof(pointer), max_rsf_files_open) ;       // allocate zro filled table for max number of allowed files
}

// find slot matching p in global slot table
// p      pointer to RSF_file structure
// return slot number if successful, -1 in case of error
static int32_t RSF_Find_file_slot(void *p)
{
  int i ;
  if(rsf_files == NULL) rsf_files = RSF_Slot_table_allocate() ;  // first time around, allocate table
  if(rsf_files == NULL) return -1 ;

  if(p == NULL) return -1 ;
  for(i = 0 ; i < max_rsf_files_open ; i++) {
    if(rsf_files[i] == p) return i ;  // slot number
  }
  return -1 ;   // not found
}

//! Find a free slot in global slot table and set it to p
//! \return Slot number if successful, -1 if table is full
static int32_t RSF_Set_file_slot(
    void *p //!< [in] Pointer to an RSF_file structure
) {
  if(rsf_files == NULL) rsf_files = RSF_Slot_table_allocate() ;  // first time around, allocate table
  if(rsf_files == NULL) return -1 ;

  for(int i = 0 ; i < max_rsf_files_open ; i++) {
    if(rsf_files[i] == NULL) {
      rsf_files[i] = p ;
      rsf_files_open ++ ;     // one more open file
      Lib_Log(APP_LIBFST, APP_EXTRA, "%s: RSF file table slot %d assigned, p = %p\n", __func__, i, p);
      return i ;              // slot number
    }
  }
  return -1 ;     // pointer not found or table full
}

// remove existing pointer p from global slot table
// p      pointer to RSF_file structure
// return former slot number if successful , -1 if error
static int32_t RSF_Purge_file_slot(void *p)
{
  int i ;

  if(rsf_files == NULL) return -1 ;  // no file table

  for(i = 0 ; i < max_rsf_files_open ; i++) {
    if(rsf_files[i] == p) {
      rsf_files[i] = (void *) NULL ;
      rsf_files_open-- ;     // one less open file
      Lib_Log(APP_LIBFST, APP_EXTRA,"%s: RSF file table slot %d freed, p = %p\n", __func__, i, p);
      return i ;             // slot number
    }
  }
  return -1 ;   // not found
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
  return key64 >> 32 ;
}

//! Extract record index from a 64-bit record key (it's in the lower 32 bits).
static inline uint32_t key64_to_index(int64_t key64) {
  return (key64 & 0xFFFFFFFFl) - 1 ;
}

static inline int64_t make_key(
  const int32_t slot,      //!< starts at 0
  const int32_t record_id  //!< starts at 0
) {
  return (((int64_t)(slot + 1) << 32) + record_id + 1);
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

  const uint32_t index = key64_to_index(key64) ;
  if(index > 0xFFFFF)                 // ERROR, index larger than 20 bits
    return (1 << 31);                 // return huge negative value

  const uint32_t slot = key64_to_file_slot(key64) ;
  if(slot > 0x3FF)                    // ERROR, slot number larger than 10 bits
    return (1 << 31) ;                // return huge negative value

  return ((index + 1) << 10) | slot | (1 << 30) ;  // index, slot, and bit 30 set to 1 to indicate RSF handle
}

//! Check whether the given 32-bit key is a valid RSF key and whether it _could_ be a valid XDF key
//! \return 1 for a valid 32 bit RSF key
//! \return 0 for a possibly valid 32 bit XDF key
//! \return -1 if invalid 32 bit key
int32_t RSF_Key32_type(int32_t key32){
  if(key32 < 0) return BAD_KEY32 ;
  return (key32 >> 30) ? RSF_KEY32 : XDF_KEY32 ;
}

//! \copydoc key64_to_file_slot
uint32_t RSF_Key64_to_file_slot(int64_t key64) {
  return key64_to_file_slot(key64) ;
}

// generate a 64 bit record key  (slot:32, index:32)
// from a 32 bit RSF key (sign:1, one:1 , index : 20, slot:10)
// code may have to be added to deal properly with negative key32 values
// code may be added to check that slot and index make sense
// an error should be returned if bit 30 is 0, i.e. ((key32 >> 30) & 1) == 0
int64_t RSF_Key64(int32_t key32){
  uint64_t key64 = (key32 & 0x3FF) ;          // slot number (10 bits)
//   if(key32 < 0) return ERR_BAD_HNDL ;         // invalid key (negative)
//   if(key32 >> 30 == 0) return ERR_BAD_HNDL ;  // not a valid rsf key (bit 30 off)
  key64 <<= 32 ;                              // shift to proper position
  key64 |= ((key32 >> 10) & 0xFFFFF) ;        // add 20 bit record index
  return key64 ;
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
  const uint32_t slot  = key64_to_file_slot(key64);
  h.p = rsf_files[slot - 1];
  return h;
}

//! Check if given file points to a valid RSF_File structure
//! fp     pointer to RSF_File structure
//! return slot number + 1 if valid structure, otherwise return 0
static uint32_t RSF_Valid_file(RSF_File *fp){
  if(fp == NULL) {
    Lib_Log(APP_LIBFST, APP_ERROR, "%s: file handle is NULL\n", __func__);
    return 0 ;                   // fp is a NULL pointer
  }
  if(fp->fd < 0) {
    Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid fd < 0 (%d)\n", __func__, fp->fd);
    return 0 ;                   // file is not open, ERROR
  }
  // get file slot from file handle table if not initialized
  if(fp->slot < 0) fp->slot = RSF_Find_file_slot(fp) ;
  // check validity of fp->slot
  if((fp->slot < 0) || (fp->slot >= max_rsf_files_open)) {
      Lib_Log(APP_LIBFST, APP_ERROR, "%s: slot number found in file handle is invalid\n", __func__);
    return 0 ;                   // not in file handle table
  }
  if(fp != rsf_files[fp->slot] ) {
    Lib_Log(APP_LIBFST, APP_ERROR, "%s: inconsistent slot data %p %p, slot = %d\n",
            fp, __func__, rsf_files[fp->slot], fp->slot);
    return 0 ;                   // inconsistent slot
  }
  return fp->slot + 1;
}

//! Verify that the application code given when opening the file matches the one from the
//! file on disk (given SOS)
//! \return 0 if they match, -1 if they don't
static inline int check_application_code(RSF_File *fp, start_of_segment* sos) {
  for (int i = 0; i < 4; i++) {
    if (fp->appl_code[i] != sos->sig1[4+i]) return -1;
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
static directory_block *RSF_Add_vdir_block(RSF_File *fp, uint32_t min_block)
{
  directory_block *dd ;
  size_t sz ;
  void *p ;

  sz = sizeof(directory_block) + ( (min_block > DIR_BLOCK_SIZE) ? min_block : DIR_BLOCK_SIZE ) ;
  p = calloc(sz, sizeof(char)) ;         // allocate a block filled with nulls
  if(p == NULL) return NULL ;            // malloc failed
  dd = (directory_block *) p ;
  dd->next = fp->dirblocks ;             // next block pointer -> start of current list
  dd->cur = dd->entries ;                // beginning of entries storage area (insertion point)
  dd->top = (uint8_t *)p + sz ;                     // last usable addess in block is dd->top -1
  fp->dirblocks = dd ;                   // new start of blocks list
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: added block of size %ld, next = %p, fp->dir = %p\n",
          __func__, sz, dd->next, fp->dirblocks);
  return dd ;
}

// manage metadata directory structures associated with file fp
// make sure that there is room for at least one more entry
// fp    pointer to RSF file control structure
static void RSF_Vdir_setup(RSF_File *fp){
  int i ;
  if(fp->dirblocks == NULL) {                                       // first time around
    RSF_Add_vdir_block(fp, 0) ;                                     // create first entries block (default size)
    fp->vdir_size = DISK_VDIR_BASE_SIZE ;                           // record size of an empty directory
  }
  if(fp->vdir_used >= fp->vdir_slots) {                             // table is full, reallocate with a larger size
    fp->vdir_slots += DIR_SLOTS_INCREMENT ;                         // increase number of slots
    // reallocate vdir table with a larger size
    fp->vdir = (vdir_entry **) realloc(fp->vdir, fp->vdir_slots * sizeof(void *)) ;
    for(i = fp->vdir_used ; i < fp->vdir_slots ; i++)
      fp->vdir[i] = NULL ;                                          // nullify new slots
  }
}

// add a new VARIABLE length metadata entry into memory directory (for a new record)
// fp     pointer to RSF file control structure
// meta   pointer to metadata array (32 bit elements)
// mlr    lower 16 bits record metadata length
//        upper 16 bits directory metadata length (if zero, same as record metadata length)
// wa     byte address of record in file
// rl     record length in bytes (should always be a multiple of 4)
// return index key of record, -1 in case of error (upper 32 bits, file slot number, lower 32 bits record number)
//        both slot number and record number in origin 1, so 0 would be an invalid record index key
static int64_t RSF_Add_vdir_entry(RSF_File *fp, uint32_t *meta, uint32_t mlr, uint64_t wa, uint64_t rl, uint32_t element_size)
{
  directory_block *dd ;
  int needed ;                         // needed size for entry to be added
  vdir_entry *entry ;
  int i ;
  int64_t index, slot ;
  uint32_t mld ;

  // fprintf(stderr, "RSF_Add_vdir_entry: meta data length = 0x%x, record length %d, elem size %d\n", mlr, rl, element_size);

  if(fp == NULL) goto ERROR ;          // invalid file struct pointer
  if( ! (slot = RSF_Valid_file(fp)) ) goto ERROR ;     // something not O.K. with fp if RSF_Valid_file returned 0
  slot <<= 32 ;                        // file slot number (origin 1)
  RSF_Vdir_setup(fp) ;
  dd = fp->dirblocks ;                 // current directory entries block

  mld = DIR_ML(mlr) ;                  // length of directory metadata
  mlr = REC_ML(mlr) ;                  // length of record metadata

  needed = sizeof(vdir_entry) +        // base size for entry to be added
           mld * sizeof(uint32_t) ;    // directory metadata size
  if(dd->top - dd->cur < needed) {     // add a new block if no room for entry in current block
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: need %d, have %ld, allocating a new block\n",
            __func__, needed, dd->top - dd->cur);
    dd = RSF_Add_vdir_block(fp, needed) ;
  }
  if(dd == NULL) goto ERROR ;          // new block creation failed
  entry = (vdir_entry *) dd->cur ;     // pointer to insertion point in block
  RSF_64_to_32(entry->wa, wa) ;        // record address in file
  RSF_64_to_32(entry->rl, rl) ;        // record length
  entry->ml = DRML_32(mld, mlr) ;      // composite entry with directory and record metadata length
  entry->dul = element_size ;          // data element size
  for(i = 0 ; i < mld ; i++)
    entry->meta[i] = meta[i] ;         // copy directory metadata
  dd->cur += needed ;                  // update insertion point

  fp->vdir[fp->vdir_used] = entry ;    // enter pointer to entry into vdir array
  fp->vdir_used++ ;                    // update number of directory entries in use
  fp->vdir_size += needed ;            // update worst case directory size (used to compute directory record size)
  index = fp->vdir_used ;              // update number of entries in use
  index |= slot ;                      // record key (file slot , record index) (both ORIGIN 1)
  return index ;

ERROR:
  return -1 ;
}

// retrieve the contents of a record memory directory entry using key (file slot , record index) (both ORIGIN 1)
// fp     pointer to RSF file control structure
// key    record key from RSF_Add_vdir_entry, RSF_Lookup, RSF_Scan_vdir, etc ...
// wa     record address in file (bytes)
// rl     record length (bytes)
// meta   pointer to memory directory metadata for record
// return length of directory metadata in 32 bit units, -1 if error
//        in case of error, 
static int32_t RSF_Get_vdir_entry(RSF_File *fp, int64_t key, uint64_t *wa, uint64_t *rl, uint32_t **meta){
  int32_t slot, indx ;
  vdir_entry *ventry ;
  char *error ;

  *wa = 0 ;           // precondition for failure
  *rl = 0 ;
  *meta = NULL ;
  if( ! (slot = RSF_Valid_file(fp)) ) {
    error = "invalid file struct pointer" ;
    goto ERROR ;
  }
  if( slot != (key >> 32) ){
    error = "inconsistent slot in key" ;
    goto ERROR ;                    // wrong slot for fp
  }
  if( (indx = (key & 0x7FFFFFFF) - 1) >= fp->vdir_used){
    error = "invalid record number" ;
    goto ERROR ;  // invalid record number, too large
  }

  ventry = fp->vdir[indx] ;
  *wa = RSF_32_to_64(ventry->wa) ;   // file address
  *rl = RSF_32_to_64(ventry->rl) ;   // record length
  *meta = &(ventry->meta[0]) ;       // pointer to metadata
  return DIR_ML(ventry->ml) ;        // return directory metadata length

ERROR :
  Lib_Log(APP_LIBFST, APP_ERROR, "%s: %s\n", __func__, error);
  return -1 ;
}

//! scan vdir (VARIABLE length metadata) of file fp to find a record where (metadata & mask)  matches (criteria & mask)
//! criteria, mask are arrays of 32 bit items (like metadata)
//! if a matching function has been associated with the file, it is used insteaad of the default match function
//!
//! \return  key for record file slot(index) in upper 32 bits, record index in lower 32 bits (both in origin 1)
//!          -1 in case of error (or no match?)
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
  int64_t key ;
  int index, dir_meta ;
  vdir_entry *ventry ;
  uint32_t *meta ;
  uint32_t rt0, class0, class_meta ;
  RSF_Match_fn *scan_match = NULL ;
  // uint32_t mask0 = 0xFFFFFFFF ;   // default mask0 is all bits active
  uint32_t mask0 = (~0) ;   // default mask0 is all bits active
  char *error ;
  const int64_t badkey = -1 ;
  int reject_a_priori ;

  const int64_t slot = RSF_Valid_file(fp);
  if (!slot) {
    // something wrong with fp
    Lib_Log(APP_LIBFST, APP_ERROR, "%s: key = %16.16lx, invalid file reference\n", __func__, key0);
    return ERR_NO_FILE ;
  }

  if( (key0 != 0) && (key64_to_file_slot(key0) != slot) ) {
    Lib_Log(APP_LIBFST, APP_ERROR, "%s: key = %16.16lx, inconsistent file slot\n", __func__, key0);
    // slot in key different from file table slot
    return ERR_BAD_UNIT ;
  }

  // slot is origin 1 (zero is invalid)
  key = (slot << 32) ;                      // move to upper 32 bits
  if( key0 < -1 ) key0 = 0 ;                // first record position for this file
  index = key0 & 0x7FFFFFFF ;               // starting ordinal for search (one more than what key0 points to)
  if(index >= fp->vdir_used) {
    Lib_Log(APP_LIBFST, APP_TRIVIAL, "%s: key = %16.16lx, beyond last record\n", __func__, key0);
    return badkey;
  }

  if(criteria == NULL){                     // no criteria specified, anything matches
    lcrit = 0 ;
  }else{                                    // criteria were specified
    // mask[0] == 0 will deactivate any record type / class based selection
    mask0 = mask ? mask[0] : mask0 ;                  // set mask0 to default if mask is NULL

    rt0   = (criteria[0] & mask0) & 0xFF ;            // low level record type match target
    // check for valid type for a data record (RT_DATA, RT_XDAT, RT_FILE, RT_CUSTOM->RT_DEL-1 are valid record types)
    // if record type is not a valid selection criterium , selection will ignore it
    // rt0 == 0 means no selection based upon record type
    if(rt0 >= RT_DEL || rt0 == RT_NULL || rt0 == RT_SOS || rt0 == RT_EOS || rt0 == RT_VDIR) rt0 = 0 ;

    class0 = criteria[0] >> 8 ;                       // get class from criteria[0] (upper 24 bits)
    if(class0 == 0) class0 = fp->rec_class ;          // if 0, use default class for file
    class0 &= (mask0 >> 8) ;                          // apply mask to get final low level record class match target
    // class0 == 0 at this point means record class will not be used for selection
  }

  // get metadata match function associated to this file, if none associated, use default function
  scan_match = (fp->matchfn != NULL) ? fp->matchfn : &RSF_Default_match ;
//   scan_match = fp->matchfn ;                // get metadata match function associated to this file
//   if(scan_match == NULL)
//     scan_match = &RSF_Default_match ;       // no function associated, use default function

  if (App_LogLevel(NULL) >= APP_EXTRA) {
    int i ;
    fprintf(stderr,"DEBUG2: RSF_Scan_vdir\n") ;
    fprintf(stderr,"criteria ") ;
    for(i = 0 ; i < lcrit ; i++)
      fprintf(stderr," %8.8x", criteria[i]) ;
    fprintf(stderr,"\n") ;
    if(mask) {
      fprintf(stderr,"mask     ") ;
      for(i = 0 ; i < lcrit ; i++)
        fprintf(stderr," %8.8x", mask[i]) ;
      fprintf(stderr,"\n") ;
    }
  }

  for( ; index < fp->vdir_used ; index++ ){ // loop over records in directory starting from requested position
    ventry = fp->vdir[index] ;              // get entry
    if(lcrit == 0) goto MATCH ;             // no criteria specified, everything matches
    meta = ventry->meta ;                   // entry metadata from directory
    // fprintf(stderr,"meta[%2d] ",index) ;
    // for(int i = 0 ; i < lcrit ; i++)
    //   fprintf(stderr," %8.8x", meta[i]) ;
    // fprintf(stderr,"\n") ;
    // the first element of meta, mask, criteria is pre-processed here, and sent to matching function
    reject_a_priori = (rt0 != 0) && ( rt0 != (meta[0] & 0xFF) ) ;    // record type mismatch ?
    //     if( reject_a_priori )      continue ;    // record type mismatch

    class_meta = meta[0] >> 8 ;                                        // get class of record from meta[0]
    reject_a_priori = reject_a_priori | ( (class0 != 0) && ( (class0 & class_meta) == 0 ) ) ;   // class mismatch ?
    //     if( reject_a_priori ) continue ;   // class mismatch, no bit in common

    dir_meta = DIR_ML(ventry->ml) ;                    // length of directory metadata
    // more criteria than metadata or less criteria than metadata will be dealt with by the matching function
    // the first element of criteria, meta, mask (if applicable) will be ignored
    // dir_meta is the number of metadata elements
    // NOTE: we may want to pass reject_a_priori and all criteria, mask and directory metadata
    // to the matching function in case it makes use of it
    // therefore NOT BUMPING pointers and NOT DECREASING sizes
    if((*scan_match)(criteria, meta, mask , lcrit, dir_meta, reject_a_priori) == 1 ){   // do we have a match ?
      goto MATCH ;
    }
    // if((*scan_match)(criteria+1, meta+1, mask ? mask+1 : NULL, lcrit-1, dir_meta-1, reject_a_priori) == 1 ){   // do we have a match ?
    //   goto MATCH ;
    // }
  }

  // falling through, return an invalid key
  Lib_Log(APP_LIBFST, APP_INFO, "%s: key = %16.16lx, no match found\n", __func__, key0);
  return badkey;

MATCH:
  // upper 32 bits of key contain the file "slot" number (origin 1)
  key = key + index + 1 ;               // add record number (origin 1) to key
  *wa = RSF_32_to_64(ventry->wa) ;      // address of record in file
  *rl = RSF_32_to_64(ventry->rl) ;      // record length
  Lib_Log(APP_LIBFST, APP_EXTRA, "%s: SUCCESS, key = %16.16lx\n\n", __func__, key);
  return key ;                          // return key value containing file "slot" and record index
}

//! read file directory (all segments) into memory directory
//! this function reads both FIXED and VARIABLE metadata directories
//! \return number of records found in segment directories (-1 upon error)
static int32_t RSF_Read_directory(
    RSF_File *fp //!< pointer to file control struct
){
  int32_t entries = 0 ;
  int32_t l_entries, num_directories ;
  int32_t num_segments = 0 ;
  uint64_t sos_seg ;
  uint64_t wa, rl ;
  start_of_segment sos ;
  off_t segment_offset ;
  ssize_t nc ;
  disk_vdir *vdir = NULL ;
  vdir_entry *ventry ;
  char *e ;
  uint32_t *meta ;
  char *errmsg = "" ;

  if(fp->dir_read > 0){  // redundant call, directory already read
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: directory ALREADY READ %d entries\n", __func__, fp->dir_read);
    return fp->dir_read ;
  }

  segment_offset = 0 ;                                             // first segment at beginning of file
  num_directories = 0 ;
  while(1){                                                 // loop over segments
    lseek(fp->fd, segment_offset , SEEK_SET) ;              // go to start of segment

    nc = read(fp->fd, &sos, sizeof(start_of_segment)) ;     // try to read start of segment record
    if(nc < sizeof(start_of_segment)) break ;               // end of file reached, done
    num_segments++ ;                                        // bump segment count

    if (RSF_Rl_sor(sos.head, RT_SOS) == 0) {
      Lib_Log(APP_LIBFST, APP_ERROR, "%s: invalid start of segment, %d entries, %d segments\n", __func__, errmsg, entries, num_segments);
      return -1;
    }

    const uint64_t segment_size = RSF_32_to_64(sos.sseg) ;
    sos_seg  = RSF_32_to_64(sos.seg) ;                      // would be 0 for a sparse segment
    if(segment_size == 0) break ;                               // open, compact segment, this is the last segment
    // remember free space in non open sparse segments
    if(sos_seg == 0 && sos.head.rlm == 0) {                 // ONLY process sparse segments NOT currently open for write
      if(fp->sparse_segs == NULL){                          // sparse segments table not allocated yet
        Lib_Log(APP_LIBFST, APP_DEBUG, "%s: allocating sparse_segs table\n", __func__) ;
        fp->sparse_segs = malloc(1024 * sizeof(sparse_entry)) ;
        fp->sparse_table_size = 1024 ;
        fp->sparse_table_used = 0 ;
      }
      fp->sparse_segs[fp->sparse_table_used].base = segment_offset ;
      fp->sparse_segs[fp->sparse_table_used].size = segment_size - sizeof(start_of_segment) ;
      Lib_Log(APP_LIBFST, APP_DEBUG, "%s: segment %d sparse at %12.12lx, space available = %ld, rlm = %d\n", 
              __func__, num_segments-1, fp->sparse_segs[fp->sparse_table_used].base,
              fp->sparse_segs[fp->sparse_table_used].size, sos.head.rlm) ;
      if(fp->sparse_table_used < fp->sparse_table_size-1)
        fp->sparse_table_used++ ;  // do not overflow table
    }
    const uint64_t vdir_offset  = RSF_32_to_64(sos.vdir) ;  // offset of vdir within segment
    const uint64_t vdir_size    = RSF_32_to_64(sos.vdirs) ; // vdir size from start of record

    Lib_Log(APP_LIBFST, APP_EXTRA, "%s: segment %d (size %ld) dir offset: %ld (size %ld)\n",
            __func__, num_segments - 1, segment_size, vdir_offset, vdir_size);

    if(vdir_size > 0 && vdir_offset > 0) {                     // non empty VARIABLE metadata segment
      l_entries = 0 ;
      num_directories++ ;
      vdir = (disk_vdir *) malloc(vdir_size) ;              // allocate space to read segment directory
      lseek(fp->fd, segment_offset + vdir_offset, SEEK_SET) ;
      nc = read(fp->fd, vdir, vdir_size) ;                  // read segment directory
      e = (char *) &(vdir->entry[0]) ;
      for(int i = 0 ; i < vdir->entries ; i++){
        l_entries++ ;
        entries++ ;
        ventry = (vdir_entry *) e ;
        wa = RSF_32_to_64(ventry->wa) + segment_offset ;
        rl = RSF_32_to_64(ventry->rl) ;
        meta = &(ventry->meta[0]) ;
        RSF_Add_vdir_entry(fp, meta, ventry->ml, wa, rl, ventry->dul) ;    // add entry into in memory directory
        const int meta_length = DIR_ML(ventry->ml) ;
        e = e + sizeof(vdir_entry) + meta_length * sizeof(uint32_t) ;
      }
      if(vdir) free(vdir) ;                                 // free memory used to read segment directory from file
      vdir = NULL ;                                         // to avoid a potential double free
      Lib_Log(APP_LIBFST, APP_DEBUG, "%s: found %d entries in segment %d\n", __func__, l_entries, num_segments-1) ;
    }
    segment_offset += segment_size ;                        // offset of the start of the next segment
  }  // while(1) loop over segments
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: directory entries = %d, num_segments = %d, num_directories = %d \n",
          __func__, entries, num_segments, num_directories) ;
  fp->dir_read = entries ;
  return entries ;                                          // return number of records found in segment directories
}

//! Compute the size of the (combined) in-memory directory record. This is the sum of what is
//! on disk and what was written since (but not yet committed to disk).
//! \return Directory record size associated with file associated with fp
static size_t RSF_Vdir_record_size(RSF_File *fp){
  int32_t i, ml ;
  size_t dir_rec_size ;
  dir_rec_size = sizeof(disk_vdir) + sizeof(end_of_record) ;                      // fixed part + end_of_record
  for(i = fp->dir_read ; i < fp->vdir_used ; i++){                                // add entries (wa/rl/ml/metadata)
    ml = DIR_ML(fp->vdir[i]->ml) ;
    dir_rec_size += ( sizeof(vdir_entry) + ml * sizeof(uint32_t) );  // fixed part + metadata
  }
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: dir rec size = %ld %ld\n", __func__, dir_rec_size, fp->vdir_size);
  return dir_rec_size ;
}

//! Write directory to file from memory directory (variable length metadata).
//! It is NOT assumed that the file is correctly positioned
static int64_t RSF_Write_vdir(RSF_File *fp){
  int32_t slot ;
  vdir_entry *entry ;
  disk_vdir *vdir ;
  end_of_record *eorp ;
  size_t dir_entry_size ;
  size_t dir_rec_size ;
  ssize_t n_written ;
  uint8_t *p = NULL ;
  uint8_t *e ;
  int i ;
  uint64_t wa64 ;
  int32_t ml ;

  if( ! (slot = RSF_Valid_file(fp)) ) return 0 ;             // something wrong with fp
  if(fp->dir_read >= fp->vdir_used) return 0 ;               // nothing to write

  if(fp->vdir == NULL) return 0 ;
  dir_rec_size = RSF_Vdir_record_size(fp) ;          // size of directory record to be written
  if( ( p = malloc(dir_rec_size) ) == NULL ) return 0 ;      // allocation failed

  vdir = (disk_vdir *) p ;
  eorp = (end_of_record *) (p + dir_rec_size - sizeof(end_of_record)) ;  // point to eor at end of record

  vdir->sor.rt = RT_VDIR ;                           // adjust start of record
  vdir->sor.rlm = 1 ;                                // indicates variable length metadata
  RSF_64_to_32(vdir->sor.rl, dir_rec_size) ;         // record length
  vdir->sor.zr = ZR_SOR ;                            // SOR marker
  vdir->entries = fp->vdir_used - fp->dir_read ;     // number of directory entries in record

  eorp->rt = RT_VDIR ;                               // adjust end or record
  eorp->rlm = 1 ;                                    // indicates variable length metadata
  RSF_64_to_32(eorp->rl, dir_rec_size) ;             // record length
  eorp->zr = ZR_EOR ;                                // EOR marker

  e = (uint8_t *) &(vdir->entry[0]) ;                // start of directory metadata portion

  // do not start at entry # 0, but entry # fp->dir_read (only write entries from "active" segment)
  // when "fusing" segments, fp->dir_read will be reset to 0
  Lib_Log(APP_LIBFST, APP_DEBUG,
          "%s: skipping %d records, segment base = %lx, dir_rec_size = %ld, dir_read = %d, vdir_used = %d\n",
          __func__, fp->dir_read, fp->seg_base, dir_rec_size, fp->dir_read, fp->vdir_used);

  for(i = fp->dir_read ; i < fp->vdir_used ; i++){             // fill from in memory directory
    entry = (vdir_entry *) e ;
    ml = DIR_ML(fp->vdir[i]->ml) ;
    dir_entry_size = sizeof(vdir_entry) + ml * sizeof(uint32_t) ;
    memcpy(entry, fp->vdir[i], dir_entry_size) ;   // copy entry from memory directory
    wa64 = RSF_32_to_64(entry->wa) ;               // adjust wa64 (file address -> offset in segment)
    wa64 -= fp->seg_base ;
    RSF_64_to_32(entry->wa, wa64) ;
    e += dir_entry_size ;
  }

  if(fp->next_write != fp->current_pos)             // set position after last write (SOS or DATA record) if not there already
    fp->current_pos = lseek(fp->fd, fp->next_write , SEEK_SET) ;
  n_written = write(fp->fd, vdir, dir_rec_size) ;   // write directory record
  fp->next_write += n_written ;                     // update last write position
  fp->current_pos = fp->next_write ;                // update file current position
  fp->last_op = OP_WRITE ;                          // last operation was a write
  Lib_Log(APP_LIBFST, APP_EXTRA, "%s: fp->next_write = %lx, vdir record size = %ld \n",
          __func__, fp->next_write, dir_rec_size) ;
  if(n_written != dir_rec_size) {                   // everything written ?
    dir_rec_size = 0 ;                              // error, return 0 as directory size
  }

  if(p) free(p) ;
  return dir_rec_size ;
}


// =================================  user callable rsf file functions =================================

// default directory match function (user overridable)
//
// match the first ncrit words of criteria and meta where mask has bits set to 1
// where mask has bits set to 0, a match is assumed
// if mask == NULL, a direct binary criteria vs meta match is used, as if mask was all 1s
// criteria and mask MUST have the same dimension : ncrit
// meta dimension nmeta may be larger than ncrit (only the first ncrit words will be considered)
// ncrit > nmeta has an undefined behavior and considered as a NO MATCH for now
// return : 1 in case of match 0,  in case of no match
int32_t RSF_Default_match(uint32_t *criteria, uint32_t *meta, uint32_t *mask, int ncrit, int nmeta, int reject_a_priori)
{
  int i ;

  // NOTE : first item in criteria / meta / mask no longer skipped  by caller
  //        it is ignored here, and reject_a_priori is used as is reflects 
  //        the condition derived from the first element
  //        the USER SUPPLIED replacement for this function may act otherwise
  if(reject_a_priori) return 0 ; // the user supplied function might act differently and re-analize criteria[0] meta[0] mask[0]
  ncrit-- ;
  nmeta-- ;
  criteria++ ;
  meta++ ;
  if(mask != NULL) mask++ ;

  if(ncrit > nmeta) return 0;  // too many criteria, no match
  if(ncrit <= 0) return 1 ;    // no criteria, we assume a match

  if(mask != NULL) {           // caller supplied a mask
    if (App_LogLevel(NULL) >= APP_EXTRA) {
      int j ;
      fprintf(stderr,"criteria, meta = ["); for(j=0 ; j<ncrit ; j++) fprintf(stderr,", %8.8x %8.8x", criteria[j], meta[j]); fprintf(stderr,"]\n");
    }
    for(i = 0 ; i < ncrit ; i++){
      if( (criteria[i] & mask[i]) != (meta[i] & mask[i]) ) {
        Lib_Log(APP_LIBFST, APP_EXTRA, "%s: MISMATCH at %d, criteria = %8.8x, meta = %8.8x, mask = %8.8x, ncrit = %d, nmeta = %d\n",
                __func__, i, criteria[i], meta[i], mask[i], ncrit, nmeta) ;
      return 0 ;  // mismatch, no need to go any further
      }
    }
  }else{   // caller did not supply a mask
    for(i = 0 ; i < ncrit ; i++){
      if( criteria[i] != meta[i] ) return 0 ;  // mismatch, no need to go any further
    }
  }
  Lib_Log(APP_LIBFST, APP_EXTRA, "%s: ncrit = %d, MATCH O.K.\n", __func__, ncrit);
  return 1 ;   // if we get here, there is a match
}

// same as RSF_Default_match but ignores mask
int32_t RSF_Base_match(uint32_t *criteria, uint32_t *meta, uint32_t *mask, int ncrit, int nmeta, int reject_a_priori)
{
  int i ;

  return RSF_Default_match(criteria, meta, NULL, ncrit, nmeta, reject_a_priori) ;

//   if(ncrit > nmeta) return 0;  // too many criteria, no match
//   if(verbose >= RSF_DIAG_DEBUG2) {
//     fprintf(stderr,"DEBUG2: calling rsf_base_match, ncrit = %d\n", ncrit);
//   }
//   for(i = 0 ; i < ncrit ; i++){
//     if( criteria[i] != meta[i] ) return 0 ;  // mismatch, no need to go any further
//   }
//   return 1 ;   // if we get here, we have a match
}

// is this a vlid data record structure ?
int32_t RSF_Valid_record(RSF_record *r){
  void *p = (void *) r ;
  int64_t rsz ;
  if( r->sor != (void *) r->d )                                             return 1 ; // sor is wrong
  if( (r->sor + sizeof(start_of_record)) != (void *) r->meta)               return 2 ; // meta - sor is wrong
  if( (r->data - (void *) r->meta) != sizeof(int32_t) * r->rec_meta )       return 3 ; // meta_size is inconsistent
  rsz = (r->rsz >= 0) ? r->rsz : -(r->rsz)  ;                                          // use absolute value of rsz
  if( (void *)&r->d[r->max_data] > ( p + rsz - sizeof(end_of_record) ) )    return 4 ; // data would go beyond EOR
  return 0 ;
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
    RSF_handle h,
    //!> Max size (in 32-bit units) of record metadata. Use rec_meta from file if too small.
    int32_t rec_meta,
    //!> Max size (in 32-bit units) of record metadata. Will be bounded by [rec_meta].
    //!> Use dir_meta from file if too small.
    int32_t dir_meta,
    //!> Max size (in bytes) of data that the record can hold.
    size_t max_data,
    //!> [optional] Array that will hold the record.
    void *t,
    //!> [optional] Size of the [t] array. If 0, [t] must be a previously created record.
    int64_t szt
) {
  RSF_File *fp = (RSF_File *) h.p ;
  RSF_record *r ;
  void *p ;
  start_of_record *sor ;
  end_of_record   *eor ;

  if( ! RSF_Valid_file(fp) ) return NULL ;
  // calculate space needed for the requested "record"
  if(rec_meta < fp->rec_meta) rec_meta = fp->rec_meta ;
  if(dir_meta < fp->dir_meta) dir_meta = fp->dir_meta ;
  if(dir_meta > rec_meta)     dir_meta = rec_meta ;

  const size_t record_size =
      sizeof(start_of_record) +                         // start of record marker
      sizeof(uint32_t) * rec_meta +                     // metadata size in bytes
      max_data +                                        // maximum data payload size
      sizeof(end_of_record) ;                           // end of record marker

  if(t != NULL){                                        // caller supplied space

    p = t ;                                             // use caller supplied space
    r = (RSF_record *) p ;                              // pointer to record structure
    if(szt == 0) {                                      // passing a previously allocated record
      if( RSF_Valid_record(r) != 0 ) return NULL ;      // inconsistent info in structure
      szt = (r->rsz >= 0) ? r->rsz : -(r->rsz) ;        // get space size from existing record
    }else{
      if(szt < 0) return NULL ;                         // szt MUST be positive for caller supplied space
      r->rsz = -szt ;                                   // caller allocated space, szt stored as a negative value
    }
    if(szt < record_size) return NULL ;                 // not enough space to build record

  }else{                                                // create a new record

    p = malloc( record_size + sizeof(RSF_record) ) ;    // allocated record + overhead
    if(p == NULL) return NULL ;                         // malloc failed 
    r = (RSF_record *) p ;                              // pointer to record structure
    r->rsz = record_size + sizeof(RSF_record) ;         // allocated memory size (positive, as it is allocated here)

  }  // if(t != NULL)

  p += sizeof(RSF_record) ;   // skip overhead. p now points to data record part (SOR)

  sor = (start_of_record *) p ;
  r->sor  = sor ;
  sor->zr = ZR_SOR ; sor->rt = RT_DATA ; sor->rlm = rec_meta ; sor->rlmd = dir_meta ; sor->dul = 0 ;
  RSF_64_to_32(sor->rl, record_size) ; // provisional sor, assuming full record

  eor = (end_of_record *) (p + record_size - sizeof(end_of_record)) ;
  r->eor  = eor ;
  eor->zr = ZR_EOR ; eor->rt = RT_DATA ; eor->rlm = rec_meta ;
  RSF_64_to_32(eor->rl, record_size) ; // provisional eor, assuming full record

  r->rec_meta = rec_meta ;       // metadata sizes in 32 bit units (metadata filling is not tracked)
  r->dir_meta = dir_meta ;
  r->elem_size = 0 ;                                 // unspecified data element length
  r->meta = (uint32_t *) (p + sizeof(start_of_record)) ;                                                // points to metadata
  bzero(r->meta, sizeof(uint32_t) * rec_meta) ;      // set metadata to 0
  r->max_data  = max_data ;                          // max data payload for this record
  r->data_size = 0 ;                                 // no data in record yet
  r->data = (void *)  (p + sizeof(start_of_record) + sizeof(uint32_t) *  rec_meta) ;                    // points to data payload
  // r-> rsz already set, > 0 if allocated by RSF_New_record, < 0 otherwise

  return r ; // return address of record
}

// store metadata into record allocated by RSF_New_record
int32_t RSF_Record_add_meta(
    RSF_record *r,      //!< [in,out] Handle to record being updated
    uint32_t *meta,     //!< [in]     The new metadata
    int32_t rec_meta,   //!< [in]     Size of the metadata (32-bit elements)
    int32_t dir_meta,   //!< [in]     Size of directory metadata (32-bit elements, must hold within record meta)
    uint32_t elem_size  //!< [in]     Size in bytes of data elements stored in the record. Must be in [1, 2, 4, 8]
){
  int i ;

  if(rec_meta != r->rec_meta) return 0 ;               // inconsistemt record metadata size
  if(dir_meta > rec_meta) dir_meta = rec_meta ;        // dir_meta is bounded by rec_meta
  r->rec_meta = rec_meta ;
  r->dir_meta = dir_meta ;
  r->elem_size = elem_size ;
  for(i=0 ; i<rec_meta ; i++) r->meta[i] = meta[i] ;  // copy metadata
  return rec_meta ;
}

// add data to payload in record allocated by RSF_New_record
// returns how much free space remains available (-1 if data_bytes was too large)
// r points to a "record" allocated/initialized by RSF_New_record
// data points to data to be added to current record payload
// data_bytes is size in bytes of added data
int64_t RSF_Record_add_bytes(RSF_record *r, void *data, size_t data_bytes){
  start_of_record *sor = (start_of_record *) r->sor;

  if(sor->dul != DT_08 && sor->dul != 0)          return -1 ; // element size should be bytes
  if(data_bytes <= 0)                             return -1 ; // invalid size
  if( (r->data_size + data_bytes) > r->max_data ) return -1 ; // data to insert too large
  sor->dul = DT_08 ;
  memcpy(r->data + r->data_size , data, data_bytes ) ;        // add data to current payload
  r->data_size = r->data_size + data_bytes ;                  // update payload size
  return (r->max_data -r->data_size) ;                        // free space remaining
}

//! similar to RSF_Record_add_bytes
//! \return Free space remaining in the record
int64_t RSF_Record_add_elements(
    RSF_record *r,              //!< [in, out] Handle to record to update
    void *data,                 //!< [in]      The data to add
    size_t num_data_elements,   //!< [in]      How many elements we are putting there
    int data_element_size       //!< [in]      Size of each data element in bytes (can do 1, 2, 4 or 8)
) {
  start_of_record *sor = (start_of_record *) r->sor;
  size_t data_bytes ;

  if(num_data_elements <=0 || data_element_size <= 0) return -1 ; // invalid data element size or number of elements
  if(data_element_size >2 && 
     data_element_size != 4 && 
     data_element_size != 8) return -1 ;                      // invalid data element size
  if(sor->dul == 0) sor->dul = data_element_size ;            // dul not initialized, set to data_element_size
  if(sor->dul != data_element_size)               return -1 ; // data element size chage
  data_bytes = num_data_elements * data_element_size ;        // size in bytes of data to add
  if( (r->data_size + data_bytes) > r->max_data ) return -1 ; // data to insert too large
  memcpy(r->data + r->data_size , data, data_bytes ) ;        // add data to current payload
  r->data_size = r->data_size + data_bytes ;                  // update payload size
  return (r->max_data -r->data_size) ;                        // free space remaining
}

//! Set internal data of the given record to indicate that it contains a certain number of elements
//! _BE CAREFUL. It should match the data that actually is in the record._
int64_t RSF_Record_set_num_elements(
    RSF_record *r,        //!< [in, out] Handle to record to update
    size_t num_elements,  //!< [in]      How many elements there are (should be)
    int element_size      //!< [in]      Size of the elements (in bytes)
) {
  // Validate element size input
  if (element_size >2 && element_size != 4 && element_size != 8) return -1;

  // Check or set element size in SOR (must not change)
  start_of_record *sor = (start_of_record *) r->sor;
  if (sor->dul == 0) sor->dul = element_size;
  if (sor->dul != element_size) return -1;

  // Check and set number of element (can't be larger than space in record)
  const size_t num_bytes = num_elements * element_size;
  if (num_bytes > r->max_data) return -1;
  r->data_size = num_bytes;
  return num_bytes;
}

// free dynamic record allocated by RSF_New_record
void RSF_Free_record(RSF_record *r){
  if(r->rsz > 0) {
//     fprintf(stderr, "RSF_Free_record DEBUG: freeing memory at %16.16p\n", r);
    free(r) ;    // only if allocated by RSF_New_record
  }
  return ;
}

// space available for more data in record allocated by RSF_New_record
int64_t RSF_Record_free_space(RSF_record *r){  // asssuming record payload is "managed"
  return (r->max_data -r->data_size) ;
}

//! Size of record (in bytes) allocated by RSF_New_record.
//! Assuming record payload is "managed".
int64_t RSF_Record_allocated(RSF_record *r){
  return (r->rsz >= 0) ? r->rsz : -(r->rsz) ;
}

//! Maximum data payload size in record (in bytes) allocated by RSF_New_record.
//! Asssuming record payload is "managed".
int64_t RSF_Record_max_space(RSF_record *r){
  return r->max_data ;
}

//! Pointer to data payload in record allocated by RSF_New_record.
//! Asssuming record payload is "managed".
void *RSF_Record_data(RSF_record *r){
  return r->data ;
}

//! Current size of data payload in record (in bytes) allocated by RSF_New_record.
//! Asssuming record payload is "managed".
uint64_t RSF_Record_data_size(RSF_record *r){
  return r->data_size ;
}

// pointer to metadata in record allocated by RSF_New_record
void *RSF_Record_meta(RSF_record *r){
  return r->meta ;
}

//! Size of metadata (32-bit elements) in record allocated by RSF_New_record.
uint32_t RSF_Record_meta_size(RSF_record *r){
  return r->rec_meta ;
}

// adjust record created with RSF_New_record (make it ready to write)
// the sor and eor components will be adjusted to properly reflect data payload size
// return size of adjusted record (amount of data to write)
size_t RSF_Adjust_data_record(RSF_handle h, RSF_record *r){
  RSF_File *fp = (RSF_File *) h.p ;
  start_of_record *sor ;
  end_of_record   *eor ;
  size_t new_size ;
  size_t data_bytes ;

  if( ! RSF_Valid_file(fp) ) return 0L ;
  data_bytes = r->data_size ;                           // get data size from record
  if(data_bytes == 0) return 0L ;                       // no data in record

  sor = r->sor ;
  if(sor->dul == 0) return 0L ;                         // uninitialized data element size
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: data_bytes = %ld, element size = %d bytes\n", __func__, data_bytes,sor->dul ) ;
  new_size = RSF_Record_size(r->rec_meta, data_bytes) ; // properly rounded up size
  eor = r->sor + new_size - sizeof(end_of_record) ;

  // adjust eor and sor to reflect actual record contents
  sor->zr = ZR_SOR ; sor->rt = RT_DATA ; sor->rlm = r->rec_meta ; sor->rlmd = r->dir_meta ;
  RSF_64_to_32(sor->rl, new_size) ;
  eor->zr = ZR_EOR ; eor->rt = RT_DATA ; eor->rlm = r->rec_meta ;
  RSF_64_to_32(eor->rl, new_size) ;

  return new_size ;   // adjusted size of record
}

// return used space in active segment
// if the handle is invalid, -1 is returned
// mostly used for debug purposes
int64_t RSF_Used_space(RSF_handle h){
  RSF_File *fp = (RSF_File *) h.p ;
  uint64_t used ;

  if( ! RSF_Valid_file(fp) ) return -1 ;

  used = fp->next_write - fp->seg_base ;

  return used ;
}

// return available space in active segment (accounting for directory space and other overhead)
// if segment is "compact", an insanely large number is returned
// if the handle is invalid, -1 is returned
// mostly used for debug purposes
int64_t RSF_Available_space(RSF_handle h){
  RSF_File *fp = (RSF_File *) h.p ;
  uint64_t used ;

  if( ! RSF_Valid_file(fp) ) return -1 ;
  if(fp->seg_max == 0) return 0x7FFFFFFFFFFFFFFFl ;           // no practical limit if not a sparse segment

  used = fp->next_write - fp->seg_base +                      // space already used in segment
         fp->vdir_size +                                      // directory current record size (worst case)
         sizeof(end_of_segment) +                             // end of compact segment
         sizeof(start_of_segment) + sizeof(end_of_segment) +  // sparse segment at end (sos + eos)
         ARBITRARY_OVERHEAD ;                                 // arbitrary overhead
  return (fp->seg_max - used - sizeof(start_of_record) - sizeof(end_of_record)) ;
}

//! Write a null record, no metadata, sparse data, no entry in directory
//! record_size = data size (not including record overhead)
//! mostly used for debug purposes, error if not enough room in segment
//! \return record size if successful, 0 otherwise
uint64_t RSF_Put_null_record(
    RSF_handle h,       //!< Handle to file where we want to add the null record
    size_t record_size  //!< Size of the "data" the record will contain (it's data section will take that much space)
) {
  RSF_File *fp = (RSF_File *) h.p ;
  start_of_record sor = SOR ;      // start of data record
  end_of_record   eor = EOR ;      // end of data record
  uint64_t needed ;
  int64_t free_space ;
  ssize_t nc ;

  if( ! RSF_Valid_file(fp) ) return 0 ;

  if (RSF_Ensure_new_segment(fp) < 0) return 0; // Can't put a null record if we don't have a writeable segment

  // Verify there is enough space in the segment
  needed = record_size ;
  if (fp->seg_max > 0) {
    free_space = RSF_Available_space(h) ;
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: free = %ld, needed = %ld\n", __func__, free_space, needed);
    if( free_space < needed ) {
      Lib_Log(APP_LIBFST, APP_DEBUG, "%s: segment overflow\n", __func__) ;
      return 0 ;
    }
  }

  // Create + commit the record
  needed = needed + sizeof(start_of_record) + sizeof(end_of_record) ;
  sor.rt = RT_NULL ;
  RSF_64_to_32(sor.rl, needed) ;
  eor.rt = RT_NULL ;
  RSF_64_to_32(eor.rl, needed) ;
  lseek(fp->fd, fp->next_write, SEEK_SET) ;
  nc = write(fp->fd, &sor, sizeof(start_of_record)) ;
  lseek(fp->fd, record_size, SEEK_CUR) ;
  nc = write(fp->fd, &eor, sizeof(end_of_record)) ;
  fp->next_write += needed ;
  return(needed) ;
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
  RSF_File *fp = (RSF_File *) h.p ;
  uint64_t record_size, total_size, extra ;
  int64_t slot, available, desired ;
  start_of_record sor = SOR ;      // start of data record
  end_of_record   eor = EOR ;      // end of data record
  ssize_t nc ;
  uint32_t meta0, rt0, class0 ;
  off_t gap ;
  size_t data_bytes = 0 ;
  int i ;

  if( ! RSF_Valid_file(fp) ) goto ERROR ;          // something not O.K. with fp
  if( RSF_Ensure_new_segment(fp) < 0 ) goto ERROR ;// We don't have write access to the file 
  if( fp->next_write <= 0) goto ERROR ;            // next_write address is not set

  if( record != NULL ){                            // using a pre allocated record ?
    if( RSF_Valid_record(record) != 0 ) goto ERROR ;    // make sure record is valid
    data_bytes = record->data_size ;               // take data_bytes from record
    // get dir_meta and rec_meta from record to compute record size
    dir_meta = record->dir_meta ;                  // should never be 0
    if(dir_meta == 0) goto ERROR ;
    rec_meta = record->rec_meta ;                  // should never be 0
    if(rec_meta == 0) goto ERROR ;
    // adjust to actual size of data in record structure (metadata assumed already set)
    record_size = RSF_Adjust_data_record(h, record) ;
    if(record_size != RSF_Record_size(rec_meta, data_bytes)) goto ERROR ;
    if(((start_of_record *) record->sor)->dul == 0) goto ERROR ;    // uninitialized data element size

  }else{                                           // NOT a preallocated record, use chunk sizes
    for(i = 0 ; i < nchunks ; i++) {
      data_bytes += chunk_size[i] ;                // sum of chunk sizes
    }
    // metadata stored in directory may be shorter than metadata in file record
    if(rec_meta == 0) rec_meta = fp->rec_meta ;    // get default metadata length from file structure
    if(rec_meta < fp->rec_meta) goto ERROR ;       // metadata too small, must be at least fp->rec_meta
    if(dir_meta == 0) dir_meta = rec_meta ;        // default size of directory metadata (record metadata size)
    if(dir_meta > rec_meta) dir_meta = rec_meta ;  // directory metadata size cannot be larger than record metadata size
    record_size = RSF_Record_size(rec_meta, data_bytes) ;
  }

  Lib_Log(APP_LIBFST, APP_EXTRA, "%s: data_bytes = %ld, record_size = %ld %lx\n", __func__, data_bytes, record_size, record_size) ;
  // write record if enough room left in segment (always O.K. if compact segment)
  if(fp->seg_max > 0){                                                 // write into a sparse segment
    available = RSF_Available_space(h) ;           // available space in sparse segment according to current conditions
    desired   = record_size +                      // rounded up size of this record
                sizeof(vdir_entry) +               // directory space needed for this record
                rec_meta * sizeof(uint32_t) ;
    if(desired > available) {
      Lib_Log(APP_LIBFST, APP_INFO, "%s: sparse segment OVERFLOW, switching to new segment\n", __func__);
      extra = desired +                          // extra space needed for this record and associated dir entry
              NEW_SEGMENT_OVERHEAD ;             // extra space needed for closing sparse segment
      RSF_Switch_sparse_segment(h, extra) ;      // switch to a new segment (minimum size = extra)
    }
  }
  lseek(fp->fd , fp->next_write , SEEK_SET) ; // position file at fp->next_write
  Lib_Log(APP_LIBFST, APP_EXTRA, "%s: write at %lx\n", __func__, fp->next_write) ;

  // write record into file
  if( record != NULL){                                              // using a pre allocated, pre filled record structure

    meta = record->meta ;                                           // set meta to address of metadata from record
    meta0 = meta[0] ;                                               // save meta[0]
    rt0 = meta0 & 0xFF ;                                            // lower 8 bits
    class0 = meta0 >> 8 ;                                           // upper 24 bits
    if(rt0 != RT_XDAT)
      if(rt0 < RT_CUSTOM || rt0 >= RT_DEL) rt0 = RT_DATA ;          // RT_DATA (normal data record) by default
    if(class0 == 0) class0 = (fp->rec_class & 0xFFFFFF) ;           // fp->rec_class if unspecified
    meta[0] = (class0 << 8) | ( rt0 & 0xFF) ;                       // RT + record class
    ((start_of_record *) record->sor)->rt = rt0 ;                   // update record type in start_of_record
    ((end_of_record *)   record->eor)->rt = rt0 ;                   // update record type in end_of_record
    // data element size will be taken from record, element_size is ignored
    nc = write(fp->fd, record->sor, record_size) ;                   // write record into file from record structure

  }else{                                                            // using meta and chunk data

    meta0 = meta[0] ;                                               // save meta[0]
    rt0 = meta0 & 0xFF ;                                            // record type in lower 8 bits
    class0 = meta0 >> 8 ;                                           // record class in upper 24 bits
    if(rt0 != RT_XDAT)
      if(rt0 < RT_CUSTOM || rt0 >= RT_DEL) rt0 = RT_DATA ;          // RT_DATA (normal data record) by default
    if(class0 == 0) class0 = (fp->rec_class & 0xFFFFFF) ;           // fp->rec_class if unspecified
    meta[0] = (class0 << 8) | ( rt0 & 0xFF) ;                       // RT + record class
    sor.rlm = DIR_ML(rec_meta) ;
    sor.rt = rt0 ;                                                  // update record type in start_of_record
    sor.rlmd = dir_meta ;
    sor.dul = element_size ;
    sor.ubc = 0 ;
    RSF_64_to_32(sor.rl, record_size) ;
    nc = write(fp->fd, &sor, sizeof(start_of_record)) ;             // write start of record
    nc = write(fp->fd, meta, rec_meta * sizeof(uint32_t)) ;         // write metadata

    for(i = 0 ; i < nchunks ; i++) {                                // write data chunks (or create appropriate holes)
      gap = chunk_size[i] ;                                         // round up the size of the data to write
      if(chunks[i] != NULL) {                                       // data or hole ?
        nc = write(fp->fd, chunks[i], gap) ;                        // write gap data bytes
      } else {
        lseek(fp->fd, gap, SEEK_CUR) ;                              // create a hole instead of writing data
      }
    }
    gap = RSF_Round_size(data_bytes) - data_bytes ;
    if( gap > 0) lseek(fp->fd, gap, SEEK_CUR) ;                     // round up write size

    eor.rlm = DIR_ML(rec_meta) ;
    eor.rt = rt0 ;                                                  // update record type in end_of_record
    RSF_64_to_32(eor.rl, record_size) ;
    nc = write(fp->fd, &eor, sizeof(end_of_record)) ;               // write end_of_record
  }

  // update directory in memory
  slot = RSF_Add_vdir_entry(fp, meta, DRML_32(dir_meta, rec_meta), fp->next_write, record_size,element_size ) ;
  meta[0] = meta0 ;                                                 // restore meta[0] to original value

  fp->next_write += record_size ;         // update fp->next_write and fp->current_pos
  fp->current_pos = fp->next_write ;
  fp->last_op = OP_WRITE ;                // last operation was write
  fp->nwritten += 1 ;                     // update unmber of writes
  // return slot/index for record (0 in case of error)
  return slot ;
ERROR :
  return 0 ;
}

int64_t RSF_Put_bytes_new(RSF_handle h, RSF_record *record, 
                      uint32_t *meta, uint32_t rec_meta, uint32_t dir_meta, 
                      void *data, size_t data_bytes, int element_size){
  void *chunks = data ;
  size_t chunk_size = data_bytes ;
  // call RSF_Put_chunks with one chunk of size data_bytes
  return RSF_Put_chunks(h, record, meta, rec_meta, dir_meta, &chunks, &chunk_size, 1, element_size) ;
}

//! Write data/meta or record into a file. data size is specified in bytes
//! record is a pointer to a RSF_record struct. if record is not NULL, data and meta are ignored
//! meta is a pointer to record metadata
//! meta[0] is not really part of the record metadata, it is used to pass optional extra record type information
//! meta_size is the size (in 32 bit elements) of the metadata (must be >= file metadata dimension)
//! data is a pointer to record data (if NULL, a gap will be inserted in the file)
//! data_bytes is the size (in bytes) of the data portion
//! element_size is the length in bytes of the data elements (endianness management)
//! if meta is NULL, data is a pointer to a pre allocated record ( RSF_record ), rec_meta and dir_meta are ignored
//! RSF_Adjust_data_record may have to be called
//! lower 16 bits of meta_size : length of metadata written to disk
//! upper 16 bits of meta_size : length of metadata written into vdir (0 means same as disk)
//! written into vdir <= written to disk
//! \return Record handle (index) if successful, 0 otherwise
int64_t RSF_Put_bytes(
    RSF_handle h,       //!< [in,out] Handle to the file where to write
    RSF_record *record, //!< [in]     [Optional] Record to write. If non-NULL, data and meta are ignored.
    uint32_t *meta,     //!< [in]     [Optional] Pointer to record metadata to write. Ignored if [record] is non-NULL.
    uint32_t rec_meta,  //!< [in]     [Optional] Size of record metadata. Ignored if [record] is non-NULL.
    uint32_t dir_meta,  //!< [in]     [Optional] Size of directory metadata. Ignored if [record] is non-NULL.
    void *data,         //!< [in]     [Optional] Data to write to the file. Ignored if [record] is non-NULL.
    size_t data_bytes,  //!< [in]     [Optional] Size (in bytes) of the data to write. Ignored if [record] is non-NULL.
    int element_size    //!< [in]     [Optional] Size of data elements (in bytes). Ignored if [record] is non-NULL.
) {
  RSF_File *fp = (RSF_File *) h.p ;
  uint64_t record_size, total_size, extra ;
  int64_t slot, available, desired ;
  start_of_record sor = SOR ;      // start of data record
  end_of_record   eor = EOR ;      // end of data record
  ssize_t nc ;
  uint32_t meta0, rt0, class0 ;
  off_t gap ;

  if( ! RSF_Valid_file(fp) ) goto ERROR ;          // something not O.K. with fp
  if( RSF_Ensure_new_segment(fp) < 0 ) goto ERROR; // Don't have write permission
  if( fp->next_write <= 0) goto ERROR ;            // next_write address is not set

  // metadata stored in directory may be shorter than record metadata
  if( record != NULL ){                            // using a pre allocated record ?
    if( RSF_Valid_record(record) != 0 ) goto ERROR ;    // make sure record is valid
    // get dir_meta and rec_meta from record to compute record size
    dir_meta = record->dir_meta ;                  // should never be 0
    rec_meta = record->rec_meta ;                  // should never be 0
  }

  if(rec_meta == 0) rec_meta = fp->rec_meta ;      // get default metadata length from file structure
  if(rec_meta < fp->rec_meta) goto ERROR ;         // metadata too small, must be at least fp->rec_meta
  if(dir_meta == 0) dir_meta = rec_meta ;          // default size of directory metadata (record metadata size)
  if(dir_meta > rec_meta) dir_meta = rec_meta ;    // directory metadata size vannot be larger than record metadata size

  record_size = RSF_Record_size(rec_meta, data_bytes) ;
  // fprintf(stderr,"RSF_Put_bytes DEBUG : data_bytes = %ld, record_size = %ld %lx\n", data_bytes, record_size, record_size) ;
  // write record if enough room left in segment (always O.K. if compact segment)
  if(fp->seg_max > 0){                                                 // write into a sparse segment
    available = RSF_Available_space(h) ;           // available space in sparse segment according to current conditions
    desired   = record_size +                      // rounded up size of this record
                sizeof(vdir_entry) +               // directory space needed for this record
                rec_meta * sizeof(uint32_t) ;
    if(desired > available) {
      Lib_Log(APP_LIBFST, APP_DEBUG, "%s: sparse segment OVERFLOW, switching to new segment\n", __func__);
      extra = desired +                          // extra space needed for this record and associated dir entry
              NEW_SEGMENT_OVERHEAD ;             // extra space needed for closing sparse segment
      RSF_Switch_sparse_segment(h, extra) ;      // switch to a new segment (minimum size = extra)
    }
  }
  lseek(fp->fd , fp->next_write , SEEK_SET) ; // position file at fp->next_write
  // fprintf(stderr,"RSF_Put_bytes DEBUG : write at %lx\n", fp->next_write) ;
  // write record 
  if( record != NULL){                                              // using a pre allocated, pre filled record structure

    total_size = RSF_Adjust_data_record(h, record) ;                // adjust to actual data size (rec_meta assumed already set)
    if(total_size != record_size) goto ERROR ;
    if(((start_of_record *) record->sor)->dul == 0) goto ERROR ;    // uninitialized data element size
    dir_meta = record->dir_meta ;                                   // get metadata sizes from record
    rec_meta = record->rec_meta ;
    if(dir_meta == 0) dir_meta = rec_meta ;
    if(dir_meta > rec_meta) dir_meta = rec_meta ;
    meta = record->meta ;                                           // set meta to address of metadata from record
    meta0 = meta[0] ;                                               // save meta[0]
    rt0 = meta0 & 0xFF ;                                            // lower 8 bits
    class0 = meta0 >> 8 ;                                           // upper 24 bits
    if(rt0 != RT_XDAT)
      if(rt0 < RT_CUSTOM || rt0 >= RT_DEL) rt0 = RT_DATA ;          // RT_DATA (normal data record) by default
    if(class0 == 0) class0 = (fp->rec_class & 0xFFFFFF) ;           // fp->rec_class if unspecified
    meta[0] = (class0 << 8) | ( rt0 & 0xFF) ;                       // RT + record class
    ((start_of_record *) record->sor)->rt = rt0 ;                   // alter record type in start_of_record
    ((end_of_record *)   record->eor)->rt = rt0 ;                   // alter record type in end_of_record
    // data element size taken from record, element_size ignored
    nc = write(fp->fd, record->sor, total_size) ;                   // write record structure to disk

  }else{                                                            // using meta and data

    meta0 = meta[0] ;                                               // save meta[0]
    rt0 = meta0 & 0xFF ;                                            // record type in lower 8 bits
    class0 = meta0 >> 8 ;                                           // record class in upper 24 bits
    if(rt0 != RT_XDAT)
      if(rt0 < RT_CUSTOM || rt0 >= RT_DEL) rt0 = RT_DATA ;          // RT_DATA (normal data record) by default
    if(class0 == 0) class0 = (fp->rec_class & 0xFFFFFF) ;           // fp->rec_class if unspecified
    meta[0] = (class0 << 8) | ( rt0 & 0xFF) ;                       // RT + record class
    sor.rlm = DIR_ML(rec_meta) ;
    sor.rt = rt0 ;                                                  // alter record type in start_of_record
    sor.rlmd = dir_meta ;
    sor.dul = element_size ;
    sor.ubc = 0 ;
    RSF_64_to_32(sor.rl, record_size) ;
    nc = write(fp->fd, &sor, sizeof(start_of_record)) ;             // write start of record
    nc = write(fp->fd, meta, rec_meta * sizeof(uint32_t)) ;         // write metadata
    gap = RSF_Round_size(data_bytes) ;                              // round up the size of the data to write
    if(data != NULL) {
      nc = write(fp->fd, data, gap) ;                               // write data
    } else {
      lseek(fp->fd, gap, SEEK_CUR) ;                                // create a gap where data would have been written
    }
    eor.rlm = DIR_ML(rec_meta) ;
    eor.rt = rt0 ;                                                  // alter record type in end_of_record
    RSF_64_to_32(eor.rl, record_size) ;
    nc = write(fp->fd, &eor, sizeof(end_of_record)) ;               // write end_of_record
  }

  // update directory in memory
  slot = RSF_Add_vdir_entry(fp, meta, DRML_32(dir_meta, rec_meta), fp->next_write, record_size,element_size ) ;
  meta[0] = meta0 ;                                                 // restore meta[0] to original value

  fp->next_write += record_size ;         // update fp->next_write and fp->current_pos
  fp->current_pos = fp->next_write ;
  fp->last_op = OP_WRITE ;                // last operation was write
  fp->nwritten += 1 ;                     // update unmber of writes
  // return slot/index for record (0 in case of error)
  return slot ;
ERROR :
  return 0 ;
}

// similar to RSF_Put_bytes, write data elements of a specified size into the file
// data_elements     : number of data items to add
// data_element_size : size in bytes of each data element
int64_t RSF_Put_data(RSF_handle h, void *data_record,
                       uint32_t *meta, uint32_t rec_meta, uint32_t dir_meta,
                       void *data, size_t data_elements, int data_element_size){
  if(data_record == NULL){     // data and metadata
    return RSF_Put_bytes(h, NULL, meta, rec_meta, dir_meta, data, data_elements * data_element_size, data_element_size) ;
  } else {                     // a pre allocated data record struct
    return RSF_Put_bytes(h, data_record, NULL, 0, 0, NULL, data_elements * data_element_size, data_element_size) ;
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
  return RSF_Put_bytes(h, record, NULL, 0, 0, NULL, data_bytes, 0) ;
}

// retrieve file contained in RSF file ans restore it under the name alias
// also get associated metadata pointer and metadata length
// index : returned by RSF_Lookup when finding a record
// if alias is NULL, the file's own name will be used
// in case of success, index is returned,
// in case of error, -1 is returned, meta and meta_size are set to NULL and 0 respectively
int64_t RSF_Get_file(RSF_handle h, int64_t key, char *alias, uint32_t **meta, uint32_t *meta_size){
  RSF_File *fp = (RSF_File *) h.p ;
  start_of_record sor ;
  uint8_t copy_buf[1024*16] ;
  uint64_t wa, rl ;
  uint32_t *vmeta, *tempm ;
  int64_t slot ;
  int32_t ml ;
  char *temp0, *temp, *filename ;
  uint64_t file_size ;
  uint32_t nmeta ;
  int fd ;
  off_t offset ;
  ssize_t nread, nwritten, nc, toread, towrite ;

  *meta = NULL ;
  *meta_size = 0 ;
  if( ! (slot = RSF_Valid_file(fp)) ) goto ERROR ; // something wrong with fp

  ml = RSF_Get_vdir_entry(fp, key, &wa, &rl, &vmeta) ;
  if(ml == -1) goto ERROR ;                        // key not found

  temp0 = (char *) vmeta ;                               // start of metadata
  temp  = temp0 + ml * sizeof(uint32_t) ;                // end of metadata
  while((temp[ 0] == '\0') && (temp > temp0)) temp -- ;
  while((temp[-1] != '\0') && (temp > temp0)) temp -- ;
  temp0 = temp -1 ;
  tempm = (uint32_t *) temp0 ;
  nmeta = tempm - vmeta ;
  file_size = RSF_32_to_64(vmeta + nmeta - 2) ;
  filename = (alias != NULL) ? alias : temp ;
//   fprintf(stderr,"RSF_Get_file DEBUG : filename = '%s' [%ld] retrieved as '%s'\n", temp, file_size, filename) ;

  fp->last_op = OP_READ ;

  offset = wa ;
  lseek(fp->fd, offset, SEEK_SET) ;                               // position at start of record
  nc = read(fp->fd, &sor, sizeof(start_of_record)) ;              // read start of record
//   fprintf(stderr,"RSF_Get_file DEBUG : rlm = %d\n", sor.rlm);
  lseek(fp->fd, sor.rlm * sizeof(uint32_t), SEEK_CUR) ;           // skip metadata
  nread = 0 ; nwritten = 0 ;
  fd = open(filename, O_WRONLY | O_CREAT | O_EXCL, 0777) ;
  if(fd == -1) {
    Lib_Log(APP_LIBFST, APP_ERROR, "%s: failed to create file '%s'\n", __func__, filename) ;
    goto ERROR ;
  }else{
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: successfully created file '%s'\n", __func__, filename) ;
  }
  towrite = file_size ;
  while(towrite > 0) {
    toread = (towrite < sizeof(copy_buf)) ? towrite : sizeof(copy_buf) ;
    towrite -= toread ;
    nread += read(fp->fd, copy_buf, toread) ;
    nwritten += write(fd, copy_buf, toread) ;
//     fprintf(stderr,".") ;
  }
//   fprintf(stderr,"\n");
//   fprintf(stderr,"RSF_Get_file DEBUG : read %ld, written %ld \n", nread, nwritten) ;
  close(fd) ;
  if(nread != nwritten) goto ERROR ;
  Lib_Log(APP_LIBFST, APP_INFO, "%s: successfully copied image of '%s' into '%s'\n", __func__, temp, filename) ;

  *meta = vmeta ;       // address of directory metadata
  *meta_size = ml ;     // directory metadata length
  return key ;
ERROR :
  return -1 ;
}

// store an external file into a RSF file
// the external file will be opened as Read-Only
// meta and meta_size have the same use as for RSF_Put_file
// return key to record
// TODO : use macros to manage metadata sizes
int64_t RSF_Put_file(RSF_handle h, char *filename, uint32_t *meta, uint32_t meta_size){
  RSF_File *fp = (RSF_File *) h.p ;
  start_of_record sor = SOR ;      // start of data record
  end_of_record   eor = EOR ;      // end of data record
  int fd ;
  int64_t slot = -1 ;              // precondition for error
  int64_t index = -1 ;             // precondition for error
  uint64_t record_size ;           // sor + metadata + file + eor
  int32_t vdir_meta ;
  off_t file_size0, file_size2 ;
  int32_t unused ;
  size_t name_len = 1024 ;         // maximum allowed name length
  ssize_t nc, nread, nwritten ;
  int i ;
  uint32_t meta0 ;
  int extra_meta ;
  struct mmeta{
  uint32_t rl[2] ;
  char name[] ;
  } *fmeta ;
  uint32_t *dir_meta, *file_meta ;
  uint64_t needed, extra ;
  uint8_t copy_buf[1024*1024] ;

  dir_meta = NULL ; file_meta = NULL ; fmeta = NULL ;

  if( ! (slot = RSF_Valid_file(fp)) ) goto ERROR ; // something wrong with fp
  slot <<= 32 ;
  if( RSF_Ensure_new_segment(fp) < 0 ) goto ERROR; // Don't have write permission
  if( fp->next_write <= 0) goto ERROR ;            // next_write address is not set

  //   vdir_meta = (meta_size >> 16) ;                  // keep the upper 16 bits for future use
  vdir_meta = DIR_ML(meta_size) ;                  // directory metadata length
  //   meta_size &= 0xFFFF ;                            // only keep the lower 16 bits
  meta_size = REC_ML(meta_size) ;                  // record metadata length
  fd = open(filename, O_RDONLY) ;
  // fprintf(stderr,"RSF_Put_file DEBUG : file = '%s', fd = %d\n", filename, fd) ;
  if(fd < 0) goto ERROR ;
  file_size0 = lseek(fd, 0L, SEEK_END) ;           // get file size
  // file_size2 = ((file_size0 + 3) & (~0x3)) ;
  file_size2 = RSF_Round_size(file_size0) ;        // file size rounded up to a multiple of 4
  unused = file_size2 - file_size0 ;               // number of unused bytes
  name_len = strnlen(filename, name_len) ;         // length of file name

  extra_meta = 2 +                                 // file size (2 x 32 bit integers)
               ((name_len + 5) >> 2) ;             // name length + 2 rounded up to a multiple of 4 characters
  // fprintf(stderr,"RSF_Put_file DEBUG: name_len = %ld, extra_meta = %d, meta_size = %d\n", name_len, extra_meta, meta_size) ;
  fmeta = (struct mmeta *) calloc(extra_meta, sizeof(uint32_t)) ;  // extra metadata
  RSF_64_to_32(fmeta->rl, file_size0) ;                     // record size
  fmeta->name[0] = '\0' ;
  for(i=0 ; i<name_len ; i++) fmeta->name[i+1] = filename[i] ;  // copy filename into fmeta->name

  // directory metadata cannot be longer than record metadata
  if(vdir_meta > meta_size) vdir_meta = meta_size ;
  dir_meta = (uint32_t *) calloc(vdir_meta + extra_meta, sizeof(uint32_t)) ; // metadata for in memory directory
  memcpy(dir_meta, meta, vdir_meta * sizeof(uint32_t)) ;        // copy meta[0 -> vdir_meta-1] into dir_meta
  memcpy(dir_meta + vdir_meta, (uint32_t *) fmeta, extra_meta * sizeof(uint32_t)) ;    // copy extra metadata into dir_meta

  file_meta = (uint32_t *) calloc(meta_size + extra_meta, sizeof(uint32_t)) ; // metadata for record in file
  memcpy(file_meta, meta, meta_size * sizeof(uint32_t)) ;        // copy meta[0 -> meta_size-1] into file_meta
  memcpy(file_meta + meta_size, (uint32_t *) fmeta, extra_meta * sizeof(uint32_t)) ; // copy extra metadata into file_meta
  // fprintf(stderr,"RSF_Put_file DEBUG : file_meta [") ;
  // for(i=0 ; i<vdir_meta + extra_meta ; i++) fprintf(stderr," %8.8x", file_meta[i]) ; fprintf(stderr,"\n") ;
  record_size = sizeof(start_of_record) + 
                meta_size * sizeof(uint32_t) +   // file record metadata size
                extra_meta * sizeof(uint32_t) +  // extra metadata (unrounded file size and file name)
                file_size2 +                     // file size rounded up to a multiple of 4
                sizeof(end_of_record) ;

  if(fp->seg_max > 0){                             // write into a sparse segment not allowed for now
    extra  = record_size +                         // this record
             sizeof(end_of_segment) +              // end of fixed segment
             sizeof(start_of_segment) +            // new sparse segment (SOS + EOS)
             sizeof(end_of_segment) +
             ARBITRARY_OVERHEAD ;                  // arbitrary overhead
    needed = fp->next_write +
             fp->vdir_size +                       // current directory size (worst case)
             sizeof(vdir_entry) +                  // space for this entry in directory
             (meta_size + extra_meta) * sizeof(uint32_t) +
             extra ;                               // for this record and segment overhead
    if(needed > fp->seg_max + fp->seg_base) {
      Lib_Log(APP_LIBFST, APP_ERROR, "%s: sparse segment OVERFLOW\n", __func__) ;
      // switch to a new segment
      RSF_Switch_sparse_segment(h, extra) ;
      // goto ERROR ;
    }
  }

  sor.rt = RT_FILE ;
  sor.rlm = meta_size + extra_meta ;          // size of record metadata
  sor.rlmd = sor.rlm ;                        // size of directory metadata
  sor.ubc = unused << 3 ;                     // unused bits in data part of record
  sor.dul = 1 ;                               // record contents is bytes (endianness management)
  RSF_64_to_32(sor.rl, record_size) ;
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: name = '%s', size = %ld(%ld), vdir_meta = %d, extra_meta = %d, meta_size = %d, record_size = %ld\n", 
          __func__, filename, file_size0, file_size2, vdir_meta, extra_meta, meta_size, record_size);
  nc = write(fp->fd, &sor, sizeof(start_of_record)) ;

  meta0 = RT_FILE + (RT_FILE_CLASS << 8) ;          // record type and class
  file_meta[0] = meta0 ;
  meta[0] = meta0 ;                                 // update caller's meta[0]
  nc = write(fp->fd, file_meta, (meta_size + extra_meta) * sizeof(uint32_t)) ;

  // nc = RSF_Copy_file(fp->fd, fd, file_size) ;
  // lseek(fp->fd, file_size2, SEEK_CUR) ;   // will be replaced by RSF_Copy_file
  nread = nwritten = 0 ;
  lseek(fd, 0l, SEEK_SET) ;                  // rewind file before copying it
  nc = read(fd, copy_buf, sizeof(copy_buf) );
  while(nc > 0){
    nread += nc ;
    if(nread > file_size2) break ;           // file being copied is changing size
    nwritten += write(fp->fd, copy_buf, nc) ;
    nc = read(fd, copy_buf, sizeof(copy_buf) );
  }
  if(nwritten < file_size2){
    nc = write(fp->fd, copy_buf, file_size2 - nwritten) ;  // pad
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: read %ld bytes, wrote %ld bytes, padded with %ld bytes\n",
            __func__, nread, nwritten, file_size2 - nwritten) ;
  }

  eor.rt = RT_FILE ;
  RSF_64_to_32(eor.rl, record_size) ;
  eor.rlm = meta_size + extra_meta ;
  nc = write(fp->fd, &eor, sizeof(end_of_record)) ;

  close(fd) ;

  dir_meta[0] = meta0 ;
  // directory and record metadata lengths will be identical
  index = RSF_Add_vdir_entry(fp, dir_meta, DRML_32(vdir_meta + extra_meta, vdir_meta + extra_meta), fp->next_write, record_size, DT_08) ; // add to directory

  fp->next_write  = lseek(fp->fd, 0l, SEEK_CUR) ;
  fp->current_pos = fp->next_write ;
  fp->last_op = OP_WRITE ;                // last operation was write
  fp->nwritten += 1 ;                     // update unmber of writes

ERROR:
  if(fmeta) free(fmeta) ;
  if(file_meta) free(file_meta) ;
  if(dir_meta) free(dir_meta) ;
  return index ;
}

//! Get key to record from file fp, matching criteria & mask
//! \return Key to the first record found that matches the criteria/mask
//! \sa RSF_Scan_vdir
int64_t RSF_Lookup(
    RSF_handle h,       //!< Handle to file to search
    int64_t key0,       //!< Slot/index where to start searching (beginning, if key0 <= 0)
    uint32_t *criteria, //!< What to look for
    uint32_t *mask,     //!< Mask on the criteria (ignore attribute if bit is 0)
    uint32_t lcrit      //!< How many criteria there are (same as number of masks)
) {
  RSF_File *fp = (RSF_File *) h.p ;

  // fprintf(stderr,"in RSF_Lookup key = %16.16lx, crit = %8.8x @%p, mask = %8.8x @%p\n", key0, criteria[0], criteria, mask[0], mask) ;
  // fprintf(stderr, "          ");
  // for(int i=0 ; i<lcrit ; i++)
  //   fprintf(stderr,"%8.8x ", criteria[i]) ;
  // fprintf(stderr,"\n          ");
  // for(int i=0 ; i<lcrit ; i++)
  //   fprintf(stderr,"%8.8x ", mask[i]) ;
  // fprintf(stderr,"\n") ;
  uint64_t wa, rl ;  // wa and rl not sent back to caller
  const int64_t record_key =  RSF_Scan_vdir(fp, key0, criteria, mask, lcrit, &wa, &rl) ;
  return record_key;
}

static RSF_record_info info0 = { 0, 0, 0, 0, 0, 0, 0, 0 } ;

//! Get information about a record at a specific index within the given file
//! \return a RSF_record_info structure
RSF_record_info RSF_Get_record_info_by_index(
    RSF_handle h,         //!< Pointer to a RSF file
    const uint32_t index  //!< Record index
) {
  RSF_File *fp = (RSF_File *) h.p ;

  const int32_t fslot = RSF_Valid_file(fp);
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

  RSF_record_info info ;
  info.wa = RSF_32_to_64( fp->vdir[index]->wa ) ;     // record address in file
  info.rl = RSF_32_to_64( fp->vdir[index]->rl ) ;     // record size
  info.elem_size = fp->vdir[index]->dul ;
  info.wa_meta = info.wa + sizeof(start_of_record) ;  // metadata address in file
  info.rec_meta = REC_ML(fp->vdir[index]->ml) ;       // record metadata length
  info.dir_meta = DIR_ML(fp->vdir[index]->ml) ;       // directory metadata length
  info.dir_meta0 = info.dir_meta ;
  info.meta = fp->vdir[index]->meta ;
  info.wa_data = info.wa_meta +                       // data address in file
                 info.rec_meta * sizeof(int32_t) ;
  info.data_size = info.rl -                          // length of data payload
                   sizeof(start_of_record) -
                   sizeof(end_of_record) -
                   info.rec_meta * sizeof(int32_t) ;
  info.fname = NULL ;                                 // if not a file container
  info.file_size = 0 ;                                // if not a file container
  if((info.meta[0] & 0xFF) == RT_FILE){                      // file container detected
    char *temp0 = (char *) info.meta ;
    char *temp = (char *) &info.meta[info.dir_meta] ;             // end of metadata
    while((temp[ 0] == '\0') && (temp > temp0)) temp-- ;    // skip trailing nulls
    while((temp[-1] != '\0') && (temp > temp0)) temp-- ;    // back until null is found
    info.fname = temp ;                                     // file name from directory metadata
    info.dir_meta0 = (uint32_t *) temp - info.meta - 2 ;    // metadata length excluding file name and file length
    info.file_size = info.meta[info.dir_meta0] ;
    info.file_size <<= 32 ;
    info.file_size += info.meta[info.dir_meta0+1] ;
//     fprintf(stderr,"RSF_Get_record_info DEBUG: file container %s detected\n", temp);
  }
  return info ;
}

//! Determine whether the given record key is found in the given file
int32_t RSF_Is_record_in_file(RSF_handle h, const int64_t key) {
  const uint32_t record_slot = key64_to_file_slot(key);
  const uint32_t file_slot = RSF_Valid_file(h.p);
  return (file_slot == record_slot);
}

//! Get information about a record
//! \return a RSF_record_info structure
RSF_record_info RSF_Get_record_info(
    RSF_handle h, //!< Pointer to a RSF file
    const int64_t key   //!< Record identifier
) {
  RSF_File *fp = (RSF_File *) h.p ;

  const int32_t fslot = RSF_Valid_file(fp);
  const int32_t slot  = key >> 32 ;
  if (slot != fslot) {
    Lib_Log(APP_LIBFST, APP_ERROR, "%s: inconsistent file slot\n", __func__);
    return info0;
  }

  // Get information from vdir using index from key (Lower 32 bits of key, starting from 0)
  const uint32_t index = (key & 0xFFFFFFFFul) - 1;
  return RSF_Get_record_info_by_index(h, index);
}

// USER CALLABLE FUNCTION
// read nelem items of size itemsize into buf from  RSF file at "address"
// return number of items read
// this function assumes that the user got address / nelem*itemsize from the proper RSF functions
ssize_t RSF_Read(RSF_handle h, void *buf, int64_t address, int64_t nelem, int itemsize) {
  RSF_File *fp = (RSF_File *) h.p ;
  ssize_t nbytes ;
  int32_t fslot = RSF_Valid_file(fp) ;
  size_t count = nelem * itemsize ;
  off_t offset ;

  if(fslot == 0) return ERR_NO_FILE ;

  offset = lseek(fp->fd, offset = address, SEEK_SET) ;   // start of data to read in file
  nbytes = read(fp->fd, buf, count) ;
  fp->current_pos = offset + nbytes ;                    // current position set after data read
  fp->last_op = OP_READ ;                                // last operation was a read operation

  return nbytes / itemsize ;
}

// write nelem items of size itemsize from buf into  RSF file at "address"
// return number of items written
// THIS CODE WOULD BE TOTALLY UNSAFE and is there only as an axample
// ssize_t RSF_Write(RSF_handle h, void *buf, int64_t address, int64_t nelem, int64_t itemsize) {
//   RSF_File *fp = (RSF_File *) h.p ;
//   ssize_t nbytes ;
//   int32_t fslot = RSF_Valid_file(fp) ;
//   size_t count = nelem * itemsize ;
//   off_t offset ;
// 
//   if(fslot == 0) return ERR_NO_FILE ;
// 
//   offset = lseek(fp->fd, offset = address, SEEK_SET) ;   // start of record in file
//   nbytes = write(fp->fd, buf, count) ;
//   fp->current_pos = offset + nbytes ;                     // current position set after data
//   fp->last_op = OP_WRITE ;                                // last operation was a write operation
// 
//   return nbytes / itemsize ;
// }

//! Read the content of the specified record from the file. Option to only read metadata
//! The caller is responsible for freeing the allocated space
//! \return a RSF_record_handle if everyting went fine, NULL in case or error
RSF_record *RSF_Get_record(
    RSF_handle h,               //!< Handle to open file in which record is located
    const int64_t key,          //!< from RSF_Lookup, RSF_Scan_vdir
    const int32_t metadata_only //!< [in] 1 if we only want to read the metadata, 0 otherwise
){
  RSF_File *fp = (RSF_File *) h.p ;
  RSF_record *record = NULL;
  void *p ;
  uint32_t rlm, rlmd, rlmd0, rlm0 ;
  int i ;
  ssize_t nc ;
  uint64_t recsize, wa, payload ;
  off_t offset ;
  char *errmsg ;
  int32_t fslot ;
  RSF_record_info info = RSF_Get_record_info(h, key) ;

  errmsg = NULL ;                     // error message already printed
  if(info.wa == 0) goto ERROR ;       // error detected by RSF_Get_record_info

  errmsg = "invalid fp" ;
  if( (fslot = RSF_Valid_file(fp)) == 0 ) goto ERROR ;             // something wrong with fp

  const int32_t slot = key64_to_file_slot(key);
  // fprintf(stderr,"RSF_Get_record DEBUG: keyslot = %d, file slot = %d, fslot = %d\n", slot, fp->slot, fslot) ;
  errmsg = "inconsistent file slot" ;
  if(slot != fslot) goto ERROR ;
  // get wa and rl from vdir, using key
  const uint32_t record_id = key64_to_index(key);
  wa = RSF_32_to_64( fp->vdir[record_id]->wa ) ;            // record position in file
  rlm0 = REC_ML(fp->vdir[record_id]->ml) ;                  // record metadata length from directory
  rlmd0 = DIR_ML(fp->vdir[record_id]->ml) ;                 // directory metadata length from directory
  
  // Determine size to read
  if (metadata_only == 1) {
    recsize = sizeof(start_of_record) + rlm0 *sizeof(uint32_t);
  }
  else {
    recsize = RSF_32_to_64( fp->vdir[record_id]->rl ) ;
  }

  recsize = info.rl ;
  wa = info.wa ;
  rlm0 = info.rec_meta ;
  rlmd0 = info.dir_meta ;

  if( (p = malloc(recsize + sizeof(RSF_record))) ) {
    record = (RSF_record *) p ;
    offset = lseek(fp->fd, offset = wa, SEEK_SET) ;   // start of record in file
    nc = read(fp->fd, record->d, recsize) ;           // read record from file
    fp->current_pos = offset + nc ;                   // current position set after data read
    fp->last_op = OP_READ ;                           // last operation was a read operation
    errmsg = "error while reading record" ;
    if(nc != recsize) goto ERROR ;
    // TODO : adjust the record struct using data from record (especially meta_size)
    record->rsz = recsize ;
    record->sor = p + sizeof(RSF_record) ;
    rlm = ((start_of_record *)record->sor)->rlm ;     // record metadata length from record
    rlmd = ((start_of_record *)record->sor)->rlmd ;   // directory metadata length from record
    errmsg = "inconsistent metadata length between directory and record" ;
    // fprintf(stderr,"RSF_Get_record DEBUG: key = %ld, wa = %8.8lx, rl = %8.8lx, rlm = %d %d, rlmd = %d %d\n", 
    //         key, wa, recsize, rlm, rlm0, rlmd, rlmd0);
    if(rlm != rlm0 || rlmd != rlmd0) goto ERROR ;     // inconsistent metadata lengths
    //     sor = (start_of_record *) record->sor ;
    record->rec_meta = rlm ;
    record->dir_meta = rlmd ;
    record->meta = record->sor + sizeof(start_of_record) ;

    if (metadata_only == 1) {
      record->eor = NULL;
      record->data = NULL;
      record->data_size = 0;
      record->max_data = 0;
    }
    else {
      record->eor = p + sizeof(RSF_record) + recsize - sizeof(end_of_record) ;
      record->data = record->sor + sizeof(start_of_record) + sizeof(uint32_t) * rlm ;
      record->data_size = (char *)(record->eor) - (char *)(record->data) ;
      record->max_data = record->data_size ;
    }
  }else{
    errmsg = "error allocating memory for record" ;
  }

  return record ;

ERROR:
  if(errmsg) Lib_Log(APP_LIBFST, APP_ERROR, "%s: %s\n", __func__, errmsg);
  if(record) free(record) ;
  return NULL ;
}

uint32_t RSF_Get_num_records(RSF_handle h) {
  RSF_File *fp = (RSF_File *) h.p ;
  if (! RSF_Valid_file(fp) ) return UINT_MAX;
  return fp->vdir_used ;
}

uint32_t RSF_Get_num_records_at_open(RSF_handle h) {
  RSF_File *fp = (RSF_File *) h.p ;
  if (! RSF_Valid_file(fp) ) return UINT_MAX;
  return fp->dir_read ;
}

int32_t RSF_Get_mode(RSF_handle h) {
  RSF_File *fp = (RSF_File *) h.p ;
  return (int32_t)fp->mode;
}

//! Check whether the given handle points to a valid RSF file
//! \return 1 if the file is valid, 0 if not
//! \sa RSF_Valid_file
int32_t RSF_Valid_handle(
    RSF_handle h //!< The handle to check
) {
  RSF_File *fp = (RSF_File *) h.p ;
  if( RSF_Valid_file(fp) ) return 1 ;
  return 0 ;
}

// lock/unlock file associated with file descriptor fp->fd
// lock == 1 : lock, lock == 0 : unlock
static int32_t RSF_File_lock(RSF_File *fp, int lock){
  struct flock file_lock ;
  int status ;

  file_lock.l_whence = SEEK_SET ;                // locked area is by position
  file_lock.l_start = 0 ;                        // base of segment to be locked
  file_lock.l_len = 0 ;                          // lock entire file

  if(lock){                                      // LOCK file
    file_lock.l_type = F_WRLCK ;                 // exclusive lock
    status = fcntl(fp->fd, F_SETLK, &file_lock) ;    // do not wait
    while ( status != 0) {
      status = fcntl(fp->fd, F_GETLK, &file_lock) ;  // find which process holds the lock
      Lib_Log(APP_LIBFST, APP_DEBUG, "%s: %d owned by pid = %d\n", __func__, getpid(), file_lock.l_pid) ;
      usleep(5000) ;                             // wait 1 millisecond
      status = fcntl(fp->fd, F_SETLK, &file_lock) ;  // try again
    }
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: locked by pid %d\n", __func__, getpid());

  }else{                                         // UNLOCK file
    file_lock.l_type = F_UNLCK ;                 // release lock
    status = fcntl(fp->fd, F_SETLK, &file_lock) ;
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: released by pid %d\n", __func__, getpid());
  }
  return status ;                                // 0 if successful, -1 in case of error
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
  RSF_File_lock(fp, 1) ;
  usleep(1000) ;

  // Go to beginning of file and try reading the first start_of_segment
  start_of_segment first_sos = SOS;
  lseek(fp->fd, 0, SEEK_SET);
  const ssize_t num_bytes_read = read(fp->fd, &first_sos, sizeof(start_of_segment));

  if (num_bytes_read == 0) { // File does not exist yet
    // Create an empty first segment
    Lib_Log(APP_LIBFST, APP_TRIVIAL, "%s: Creating a new, empty segment\n", __func__);

    for (int i = 0; i < 4; i++) { // Set application code
      first_sos.sig1[4+i] = fp->appl_code[i];
    }
    first_sos.head.rlm  = fp->seg_max_hint > 0 ? 1 : RSF_EXCLUSIVE_WRITE;
    first_sos.head.rlmd = DIR_ML(fp->dir_meta);
    first_sos.tail.rlm  = 0;
    RSF_64_to_32(first_sos.seg,  sizeof(start_of_segment)) ;
    RSF_64_to_32(first_sos.sseg, sizeof(start_of_segment) + sizeof(end_of_segment)) ;
    
    end_of_segment first_eos = EOS;
    RSF_64_to_32(first_eos.h.seg,  sizeof(start_of_segment)) ;
    RSF_64_to_32(first_eos.h.sseg, sizeof(start_of_segment) + sizeof(end_of_segment)) ;

    // Commit segment header/footer
    write(fp->fd, &first_sos, sizeof(start_of_segment));
    write(fp->fd, &first_eos, sizeof(end_of_segment));
  }
  else {
    // Check if anyone else is writing in this file
    if (first_sos.head.rlm == RSF_EXCLUSIVE_WRITE) {
      Lib_Log(APP_LIBFST, APP_WARNING, "%s: file %s is already open for exclusive write\n", __func__, fp->name);
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
    }
    else {
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
  RSF_File_lock(fp, 0) ;
  // --------- Unlock file ---------- //

  return return_value;
}

//! Create an empty RSF segment : start of segment, empty directory, end of segment
static int32_t RSF_Ensure_new_segment(
    RSF_File *fp    //!< [in,out] Pointer to file where the segment is being added
) {
  ssize_t nc ;
  int i ;
  off_t start = 0 ;
  off_t top ;
  off_t align ;
  start_of_segment new_sos = SOS ;
  start_of_segment sos2 = SOS ;
  end_of_segment eos = EOS ;
  end_of_segment eos2 = EOS ;
  end_of_segment last_eos ;

  int status = -1 ;

  // If mode is RSF_RW, we know that this process has already marked the file as being open and reserved
  // for writing, so that flag is the only check needed to go on with creating the new segment
  if ((fp->mode & RSF_RW) != RSF_RW) {
    Lib_Log(APP_LIBFST, APP_ERROR, "%s: File is not open in write mode\n", __func__);
    return -1;
  }

  if (fp->next_write > 0) return 0; // Segment is already created

  // --------- Lock file --------- //
  RSF_File_lock(fp, 1) ;
  usleep(1000) ;

  // Update first SOS from file, if open in sparse mode (other processes may have modified it)
  if (fp->sos0.head.rlm != RSF_EXCLUSIVE_WRITE) {
    lseek(fp->fd, 0, SEEK_SET);
    nc = read(fp->fd, &fp->sos0, sizeof(start_of_segment)) ;
  }

  // Read last end-of-segment and get start location of new segment
  off_t new_segment_start = lseek(fp->fd, -sizeof(end_of_segment), SEEK_END) ;
  nc = read(fp->fd, &last_eos, sizeof(end_of_segment)) ;
  new_segment_start += sizeof(end_of_segment) ;

  // Check that last EOS (high part) looks valid
  if(RSF_Rl_eosh(last_eos.h) == 0) {
    Lib_Log(APP_LIBFST, APP_ERROR, "%s: bad end of segment in file\n", __func__);
    goto RETURN ;
  }

  // Initialize new SOS as a copy of the first SOS
  memcpy(&new_sos, &fp->sos0, sizeof(start_of_segment)) ;
  new_sos.head.rlm  = fp->rec_meta;
  new_sos.head.rlmd = fp->dir_meta;

  if (fp->seg_max_hint > 0) {
    // We have a sparse segment

    // Determine sparse segment size from hint, subject to alignment on block boundary
    top = new_segment_start + fp->seg_max_hint - 1 ;                    // Last address of segment
    align = 1 ; align <<= SPARSE_BLOCK_ALIGN ; align  = align -1 ;      // Compute alignment pattern
    top |= align ;                                                      // Align top address on block boundary
    top = top + 1 ;
    const uint64_t sparse_size = top - new_segment_start ;              // Adjusted size of sparse segment
    const uint64_t sparse_rl = sparse_size - sizeof(start_of_segment) ; // Sparse segment record length

    // Fill eos and new_sos with proper values (sparse segment size, record length)
    RSF_64_to_32(eos.h.tail.rl, sparse_rl) ;
    RSF_64_to_32(eos.h.sseg, sparse_size) ;
    RSF_64_to_32(eos.l.head.rl, sparse_rl) ;

    // Write end of segment (high part) at the correct position (this will create the "hole" in the sparse file
    lseek(fp->fd, new_segment_start + sparse_size - sizeof(end_of_segment_hi), SEEK_SET) ;
    nc = write(fp->fd, &eos.h, sizeof(end_of_segment_hi)) ;
    RSF_64_to_32(new_sos.sseg, sparse_size) ;                       // sseg MUST be non zero in a new sparse segment
    fp->seg_max = sparse_size ;

  } else {
    // We have a compact segment

    eos.l.head.rlm = fp->sos0.head.rlmd ;
    eos.h.tail.rlm = fp->sos0.head.rlmd ;
    RSF_64_to_32(new_sos.sseg, 0L) ; // Unlimited size
    fp->seg_max = 0 ;
    fp->seg_max_hint = 0 ;

  } // if sparse

  RSF_64_to_32(new_sos.vdir, 0L) ;                                   // no directory, set size and offset to 0 ;
  RSF_64_to_32(new_sos.vdirs, 0L) ;
  RSF_64_to_32(new_sos.seg, 0L) ;                                   // seg = 0 when a segment is open

  // Copy new SOS into RSF_File and write it to disk at end of file
  // memcpy(&(fp->sos1), &new_sos, sizeof(start_of_segment)) ;
  lseek(fp->fd, new_segment_start, SEEK_SET) ;
  nc = write(fp->fd, &new_sos, sizeof(start_of_segment)) ;

  // Copy new EOS into RSF_File and write it to disk after its SOS (only commit the lower part for sparse files)
  memcpy(&(fp->eos1), &eos, sizeof(end_of_segment)) ;
  nc = write(fp->fd, &eos.l, sizeof(end_of_segment_lo)) ;
  if (fp->seg_max_hint == 0) nc = write(fp->fd, &eos.h, sizeof(end_of_segment_hi)) ;

  fp->seg_base   = new_segment_start ;                                     // base offset of active segment
  fp->next_write = new_segment_start + sizeof(start_of_segment) ;
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: %s EOF at %lx\n",
          __func__, (fp->seg_max_hint > 0) ? "sparse" : "compact", lseek(fp->fd, 0L, SEEK_END)) ;
  fp->current_pos = lseek(fp->fd, fp->next_write, SEEK_SET) ;
  fp->last_op = OP_WRITE ;

  status = 0 ;
  // fprintf(stderr,"after segment create %ld\n", lseek(fp->fd, 0L , SEEK_CUR));
  // system("ls -l demo0.rsf") ;
RETURN :
  RSF_File_lock(fp, 0) ;
  // -------- Unlock file -------- //

  return status ;
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

    const uint64_t segment_size = RSF_32_to_64(sos.sseg) ;
    segment_offset += segment_size;
    num_segments++;
  }

  result = 1;

END:
  close(fd) ;
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
    const int32_t mode,
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

  RSF_File *fp = (RSF_File *) malloc(sizeof(RSF_File)) ;   // allocate a new RSF_File structure
  RSF_handle handle ;
  char *errmsg = "" ;

  if(fp == NULL) {
    errmsg = " allocation failed" ;
    goto ERROR ;
  }

  const int64_t segsize = segsizep ? *segsizep : 0;     // segsize will be 0 if segsizep is NULL

  if(segsize < 0) goto ERROR ;           // invalid segment size

  RSF_File_init(fp) ;                    // set safe initial values

  fp->name = realpath(fname, NULL) ;     // get canonical path

  for (int i = 0; i < 4; i++) fp->appl_code[i] = appl[i];
  fp->dir_meta = DIR_ML(dir_meta_length);
  fp->rec_meta = REC_ML(dir_meta_length);
  fp->seg_max_hint = segsize ;

  fp->mode = mode;
  if(fp->mode == 0) fp->mode = RSF_RW ;  // automatic mode, try to open for read+write with RO fallback

  // Open file and determine actual mode, based on whether we were able to open the file in the requested mode.
  switch(fp->mode & (RSF_RO | RSF_RW | RSF_AP)){

    case RSF_RO:                         // open for read only
      errmsg = " file not found" ;
      if( (fp->fd = open(fname, O_RDONLY)) == -1 ) goto ERROR ;  // file does not exist or is not readable
      break ;

    case RSF_RW:                         // open for read+write, create if it does not exist
      fp->fd = open(fname, O_RDWR | O_CREAT, 0777) ;
      if(fp->fd == -1){                  // fallback, try to open in read only mode
        fp->mode = RSF_RO ;
        errmsg = " file does not exist or is not readable" ;
        if( (fp->fd = open(fname, O_RDONLY)) == -1 ) goto ERROR ;  // file does not exist or is not readable
      }
      // Try to lock the file for writing. If that doesn't work, set it to read-only.
      // This creates the first segment if it didn't already exist
      if (RSF_Lock_for_write(fp) < 0) {
        fp->mode = RSF_RO ;
      }
      break ;

    case RSF_AP:                         // to be added later
      fp->mode = RSF_RW ;                    // open existing file for read+write, no fallback
      errmsg = " cannot open in write mode" ;
      if( (fp->fd = open(fname, O_RDWR, 0777)) == -1) goto ERROR ;  // cannot open in write mode
      break ;

    default:
      errmsg = " opening mode is not valid" ;
      goto ERROR ;
      break ;
  }
  errmsg = "" ;

  RSF_Vdir_setup(fp) ;

  // Verify that there is something to read in the file
  lseek(fp->fd, fp->seg_base, SEEK_SET) ;               // first segment of the file
  if (read(fp->fd, &fp->sos0, sizeof(start_of_segment)) < sizeof(start_of_segment)) {
    errmsg = " file is empty" ;
    close(fp->fd) ;
    goto ERROR ;  // invalid SOS (too short)
  }
  // Lib_Log(APP_LIBFST, APP_DEBUG, "%s: SOS:\n", __func__);
  // print_start_of_segment(&fp->sos0);

  fp->dir_meta = fp->sos0.head.rlmd ;
  fp->rec_meta = fp->dir_meta ;

  if( RSF_Rl_sor(fp->sos0.head, RT_SOS) == 0 ) goto ERROR ;      // invalid SOS (wrong record type)
  if( RSF_Rl_eor(fp->sos0.tail, RT_SOS) == 0 ) goto ERROR ;      // invalid SOS (wrong record type)
  if( RSF_Rl_sos(fp->sos0) == 0 ) goto ERROR ;
  if( check_application_code(fp, &fp->sos0) < 0 ) goto ERROR ;

  fp->slot = RSF_Set_file_slot(fp) ;        // insert into file table
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: %s mode, slot %d, reading directory\n",
          __func__, open_mode_to_str(fp->mode), fp->slot);
  if( RSF_Read_directory(fp) < 0 ){         // read directory from all segments
    RSF_Purge_file_slot(fp) ;               // remove from file table in case of error
    goto ERROR ;
  }

  fp->last_op = OP_NONE ;
  fp->seg_max = 0 ;
  if (segsizep) *segsizep = segsize ;

  if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_DEBUG) print_start_of_segment(&fp->sos0);
  // fprintf(stderr,"RSF_Open_file: 4, fp = %p\n", fp);
  // fprintf(stderr,"after open %ld, next write = %ld\n", lseek(fp->fd, 0L , SEEK_CUR), fp->next_write);
  handle.p = fp ;
  return handle ;

ERROR:
  Lib_Log(APP_LIBFST, APP_ERROR, "%s: '%s' %s\n", __func__, fname, errmsg);
  if(fp != NULL) {
    if(fp->name != NULL) free(fp->name) ;  // free allocated string for file name
    free(fp) ;                             // free allocated structure
  }
  handle.p = NULL ;
  return handle ;                          // return a NULL handle
}

// close a RSF compact file segment, without closing the file
// write directory record
// write EOS record for compact segment
// rewrite original SOS, (segment 0 SOS if FUSING segments)
int32_t RSF_Close_compact_segment(RSF_handle h){
  RSF_File *fp = (RSF_File *) h.p ;
  int slot ;
  ssize_t nc ;
  uint64_t vdir_size ;
  off_t offset_vdir, offset_eof, cur ;
  start_of_segment sos = SOS ;
  end_of_segment eos = EOS ;

  if( ! (slot = RSF_Valid_file(fp)) ) return 0 ;   // something not O.K. with fp
  if( (fp->mode & RSF_RO) == RSF_RO) return 1 ;    // file open in read only mode, nothing to do

  if(fp->seg_max != 0) return 1 ;                  // not a compact segment

  if( (fp->mode & RSF_FUSE) == RSF_FUSE ) {
    if(fp->seg_base > 0) {                                      // no need to fuse if segment 0
      lseek(fp->fd, fp->seg_base , SEEK_SET) ;                  // seek to start of this segment
      nc = write(fp->fd, &sos, sizeof(start_of_segment)) ;      // nullify original start_of_segment
      fp->seg_base = 0 ;                                        // segment 0 will become the only segment in file
      fp->dir_read = 0 ;                                        // all directory entries will be written
      Lib_Log(APP_LIBFST, APP_DEBUG, "%s: fusing all segments into one\n", __func__) ;
    }
    return 1 ;   // NO ERROR
  }

  // write directory, but move dir_read to avoid writng same record directory entries again
  offset_vdir = fp->next_write - fp->seg_base ;                    // vdir offset into segment
  vdir_size = RSF_Write_vdir(fp) ;                                 // write vdir
  if(vdir_size == 0) offset_vdir = 0 ;                             // no directory
  fp->dir_read = fp->vdir_used ;                                   // keep current directory info (no writing again)
  fp->current_pos = lseek(fp->fd, fp->next_write , SEEK_SET) ;     // set position of write pointer

  // write active compact segment EOS
  offset_eof = fp->next_write - fp->seg_base  + sizeof(end_of_segment);
//   eos.l.head.rlm = fp->meta_dim ;
  eos.l.head.rlm = fp->rec_meta ;
  eos.l.head.rlmd = fp->dir_meta ;
  RSF_64_to_32(eos.l.head.rl, sizeof(end_of_segment)) ;
//   eos.h.meta_dim = fp->meta_dim ;
  RSF_64_to_32(eos.h.vdir,  offset_vdir) ;                         // vdir record position in file
  RSF_64_to_32(eos.h.vdirs, vdir_size) ;                           // vdir record size
  RSF_64_to_32(eos.h.sseg, offset_eof) ;                           // segment size including EOS
  RSF_64_to_32(eos.h.seg,  offset_eof - sizeof(end_of_segment)) ;  // segment size excluding EOS
//   eos.h.tail.rlm = fp->meta_dim ;
  eos.h.tail.rlm = fp->rec_meta ;
  RSF_64_to_32(eos.h.tail.rl, sizeof(end_of_segment)) ;
  nc = write(fp->fd, &eos, sizeof(end_of_segment)) ;               // write end of compact segment

  // rewrite active segment SOS (segment 0 SOS if FUSING)
  lseek(fp->fd, fp->seg_base , SEEK_SET) ;
  nc = read(fp->fd, &sos, sizeof(start_of_segment)) ;   // read start of segment (to keep appl signature)
  RSF_64_to_32(sos.vdir,  offset_vdir) ;           // vdir record position in file
  RSF_64_to_32(sos.vdirs, vdir_size) ;             // directory record size
  RSF_64_to_32(sos.sseg,  offset_eof) ;            // segment size including EOS
  RSF_64_to_32(sos.seg,  offset_eof - sizeof(end_of_segment)) ;             // segment size excluding EOS
  sos.head.rlm = 0 ;    // fix SOR of appropriate SOS record if file was open in write mode
  sos.head.rlmd = fp->dir_meta ;
  cur = lseek(fp->fd, fp->seg_base , SEEK_SET) ;
  nc = write(fp->fd, &sos, sizeof(start_of_segment)) ;      // rewrite start of active segment

  return 1 ;   // NO ERROR
}

// close a RSF sparse file segment, allocate a new one of the same size, switch file to new segment
// write directory record (no longer needed, as the new segment will get the directory)
// SOS of sparse segment must be rewritten (segment becomes compact)
// EOS of compact segment must be written
// SOS for the sparse segment that gets the remaining space must be written
// a split EOS record for the sparse segment must be written
// lock file
// allocate a new sparse segment
// write SOS and split EOS for new segment
// unlock file
// switch fp control fields to new segment
int32_t RSF_Switch_sparse_segment(RSF_handle h, int64_t min_size){
  RSF_File *fp = (RSF_File *) h.p ;
  int i, slot ;
  off_t offset_vdir, offset_eof, cur ;
  ssize_t nc ;
  uint64_t vdir_size, sparse_start, sparse_top, sparse_size, rl_eos, rl_sparse, align ;
  start_of_segment sos = SOS ;
  start_of_segment sos2 = SOS ;
  end_of_segment eos = EOS ;
  end_of_segment eos2 = EOS ;
// sleep(1) ;
  if( ! (slot = RSF_Valid_file(fp)) ) return 0 ;   // something not O.K. with fp
  if( (fp->mode & RSF_RO) == RSF_RO) return 1 ;    // file open in read only mode, nothing to do

  if(fp->seg_max == 0) return 1 ; // not a sparse segment

  // write directory, but update dir_read to avoid writing same record information again
  offset_vdir = fp->next_write - fp->seg_base ;                    // vdir offset into segment
  vdir_size = RSF_Write_vdir(fp) ;                                 // write vdir
  if(vdir_size == 0) offset_vdir = 0 ;                             // no directory
  fp->dir_read = fp->vdir_used ;                                   // keep current directory info
  fp->current_pos = lseek(fp->fd, fp->next_write , SEEK_SET) ;     // set position of write pointer

  // write compact segment EOS
  offset_eof = fp->next_write - fp->seg_base  + sizeof(end_of_segment);
//   eos.l.head.rlm = fp->meta_dim ;
  eos.l.head.rlm = fp->rec_meta ;
  eos.l.head.rlmd = fp->dir_meta ;
  RSF_64_to_32(eos.l.head.rl, sizeof(end_of_segment)) ;
//   eos.h.meta_dim = fp->meta_dim ;
  RSF_64_to_32(eos.h.vdir,  offset_vdir) ;                         // vdir record position in file
  RSF_64_to_32(eos.h.vdirs, vdir_size) ;                           // vdir record size
  RSF_64_to_32(eos.h.sseg, offset_eof) ;                           // segment size including EOS
  RSF_64_to_32(eos.h.seg,  offset_eof - sizeof(end_of_segment)) ;  // segment size excluding EOS
//   eos.h.tail.rlm = fp->meta_dim ;
  eos.h.tail.rlm = fp->rec_meta ;
  RSF_64_to_32(eos.h.tail.rl, sizeof(end_of_segment)) ;
// cur = lseek(fp->fd, 0L, SEEK_CUR) ;
// fprintf(stderr,"RSF_Switch DEBUG : eos at %12.12x\n", cur) ;
  nc = write(fp->fd, &eos, sizeof(end_of_segment)) ;               // write end of compact segment

  // write sparse segment SOS
  sparse_start = lseek(fp->fd, 0L, SEEK_CUR) ;                     // start of sparse segment after compact EOS
  sparse_top = fp->seg_base + fp->seg_max ;                        // end of sparse segment
  sparse_size = sparse_top - sparse_start ;                        // new sparse segment size
  RSF_64_to_32(sos.seg, 0L) ;
  RSF_64_to_32(sos.sseg, sparse_size) ;
// cur = lseek(fp->fd, 0L, SEEK_CUR) ;
// fprintf(stderr,"RSF_Switch DEBUG : sparse sos at %12.12x\n", cur) ;
  nc = write(fp->fd, &sos, sizeof(start_of_segment)) ;

  // fix sparse segment EOS
  memcpy(&eos, &fp->eos1, sizeof(eos)) ;                           // get original EOS
  rl_eos = sparse_size - sizeof(start_of_segment) ;                // length covered by EOS
  RSF_64_to_32(eos.l.head.rl, rl_eos) ;
  nc = write(fp->fd, &eos.l, sizeof(end_of_segment_lo)) ;          // write low part of EOS
  RSF_64_to_32(eos.h.tail.rl, rl_eos) ;
  RSF_64_to_32(eos.h.sseg, sparse_size) ;                          // size of sparse segment
  cur = lseek(fp->fd, rl_eos - sizeof(end_of_segment_hi) - sizeof(end_of_segment_lo), SEEK_CUR);
// fprintf(stderr,"RSF_Switch DEBUG : sparse eos at %12.12x\n", cur) ;
  nc = write(fp->fd, &eos.h, sizeof(end_of_segment_hi)) ;          // write high part of EOS

  // make original segment (sparse) into a compact segment (cannot be segment 0)
  cur = lseek(fp->fd, fp->seg_base , SEEK_SET) ;
  nc = read(fp->fd, &sos, sizeof(start_of_segment)) ;              // read start of segment (to keep appl signature)
  RSF_64_to_32(sos.vdir,  offset_vdir) ;                           // vdir record position in file
  RSF_64_to_32(sos.vdirs, vdir_size) ;                             // vdir record size
  RSF_64_to_32(sos.sseg,  offset_eof) ;                            // segment size including EOS
  RSF_64_to_32(sos.seg,  offset_eof - sizeof(end_of_segment)) ;    // segment size excluding EOS
  sos.head.rlm = 0 ;                                               // fix SOR of SOS record
  sos.head.rlmd = fp->dir_meta ;
  cur = lseek(fp->fd, fp->seg_base , SEEK_SET) ;
  // fprintf(stderr,"RSF_Switch DEBUG : original sos rewrite at %12.12x\n", cur) ;
  nc = write(fp->fd, &sos, sizeof(start_of_segment)) ;             // rewrite start of active segment

  // create the new segment 
  // TODO : reuse an existing inactive sparse segment if possible

  RSF_File_lock(fp, 1) ;    // lock file
  fp->seg_base = lseek(fp->fd, 0L, SEEK_END) ;
  sparse_start = fp->seg_base ;
  fp->seg_max = (fp->seg_max > min_size) ? fp->seg_max : min_size ;  // minimum segment size
  sparse_top = fp->seg_base + fp->seg_max ;                          // top of segment
  align = 1 ; align <<= SPARSE_BLOCK_ALIGN ; align = align - 1 ;     // alignment mask
  sparse_top = sparse_top - 1 ; 
  sparse_top |= align ;                                              // align top of segment on block boundary
  sparse_top = sparse_top + 1 ;
  sparse_size = sparse_top - sparse_start ;
  fp->seg_max = sparse_size ;                                        // actual segment size
  rl_sparse = sparse_size - sizeof(start_of_segment) ;               // end of sparse segment record length
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: sparse segment size = %ld\n", __func__, sparse_size) ;

  // new sparse segment SOS    ( from RSF_New_empty_segment)
  // sos2.meta_dim = fp->meta_dim ;
  for(i=0 ; i<8 ; i++) sos2.sig1[i] = sos.sig1[i] ;            // copy signature
  RSF_64_to_32(sos2.seg, 0L) ;
  RSF_64_to_32(sos2.sseg, sparse_size) ;
  RSF_64_to_32(sos2.vdir, 0L) ;
  RSF_64_to_32(sos2.vdirs, 0L) ;
  sos2.head.rlm = 0xFFFF ; // Marker for exclusive write?
  sos2.head.rlmd = fp->dir_meta ;
  sos2.tail.rlm = 0 ;
  nc = write(fp->fd, &sos2, sizeof(start_of_segment)) ;
  fp->next_write = fp->seg_base + sizeof(start_of_segment) ;
  // new sparse segment EOS
  // write end of segment (low part)
  // write end of segment (high part) at the correct position (this will create the "hole" in the sparse file
  // fill eos2 with proper values (sparse segment size, record length)
  RSF_64_to_32(eos2.l.head.rl, rl_sparse) ;
  memcpy(&(fp->eos1), &eos2, sizeof(end_of_segment)) ;            // copy eos into RSF_File structure
  nc = write(fp->fd, &eos2.l, sizeof(end_of_segment_lo)) ;

  // eos2.h.meta_dim = fp->meta_dim ;
  RSF_64_to_32(eos2.h.seg, 0L) ;
  RSF_64_to_32(eos2.h.sseg, sparse_size) ;                      // segment length = sparse_size
  RSF_64_to_32(eos2.h.vdir, 0L) ;
  RSF_64_to_32(eos2.h.vdirs, 0L) ;
  RSF_64_to_32(eos2.h.tail.rl, rl_sparse) ;
  lseek(fp->fd, sparse_start + sparse_size - sizeof(end_of_segment_hi), SEEK_SET) ;
  nc = write(fp->fd, &eos2.h, sizeof(end_of_segment_hi)) ;
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: sparse EOF at %8.8lx\n", __func__, sparse_start + sparse_size) ;
  fp->current_pos = sparse_top ;

  RSF_File_lock(fp, 0) ;    // unlock file

  return 1 ;
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
  RSF_File *fp = (RSF_File *) h.p ;
  int32_t slot ;
  start_of_segment sos = SOS ;
  end_of_segment eos = EOS ;
  off_t offset, offset_vdir, offset_eof, cur ;
  uint64_t vdir_size, sparse_size, rl_eos ;
  ssize_t nc ;
  uint64_t sparse_start, sparse_top ;
  directory_block *purge ;

  if( ! (slot = RSF_Valid_file(fp)) ) return 0 ;   // something not O.K. with fp

  if( (fp->mode & RSF_RO) == RSF_RO) goto CLOSE ;  // file open in read only mode, nothing to rewrite
  // fprintf(stderr,"before close position %ld, next_write = %ld \n", lseek(fp->fd, 0L , SEEK_CUR), fp->next_write);
  // fprintf(stderr,"RSF_Close_file DEBUG : beginning of close ");

  if ((fp->mode & RSF_RW) == RSF_RW && fp->next_write < 0) {
    Lib_Log(APP_LIBFST, APP_WARNING, "%s: Closing a file that was opened in write mode, but nothing was written!\n", __func__);
    goto RESET_WRITE_FLAG;
  }

  // Prepare for fusing, if necessary
  if( (fp->mode & RSF_FUSE) == RSF_FUSE ) {
    lseek(fp->fd, fp->seg_base , SEEK_SET) ;                  // seek to start of this segment
    nc = write(fp->fd, &sos, sizeof(start_of_segment)) ;      // nullify original start_of_segment
    // fp->current_pos = lseek(fp->fd, fp->next_write , SEEK_SET) ;  // reset position of write pointer
    fp->seg_base = 0 ;                                        // segment 0 will become the only segment in file
    fp->dir_read = 0 ;                                        // all directory entries will be written
    Lib_Log(APP_LIBFST, APP_DEBUG, "%s: fusing segments into one\n", __func__) ;
  }

  offset_vdir = fp->next_write - fp->seg_base ;                    // vdir offset into segment
  vdir_size = RSF_Write_vdir(fp) ;                                 // write vdir
  if(vdir_size == 0) offset_vdir = 0 ;

  // fprintf(stderr,"RSF_Close_file DEBUG :, offset_dir = %lx, offset_eof = %lx\n", offset_dir, offset_eof) ;
  // fprintf(stderr,"offset_dir = %16lo, offset_eof = %16lo\n",offset_dir, offset_eof);
  fp->current_pos = lseek(fp->fd, fp->next_write , SEEK_SET) ;  // reset position of write pointer
  // fprintf(stderr,"before write directory %ld\n", lseek(fp->fd, 0L , SEEK_CUR));
  offset_eof = fp->next_write - fp->seg_base  + sizeof(end_of_segment);
  // fprintf(stderr,"after write directory %ld\n", lseek(fp->fd, 0L , SEEK_CUR));

  // fprintf(stderr,"RSF_Close_file DEBUG :, after write directory, pos = %lx\n", lseek(fp->fd, 0L, SEEK_CUR));
  // write compact end of segment
  //   eos.l.head.rlm = fp->meta_dim ;
  eos.l.head.rlm = fp->rec_meta ;
  eos.l.head.rlmd = fp->dir_meta ;
  RSF_64_to_32(eos.l.head.rl, sizeof(end_of_segment)) ;
  //   eos.h.meta_dim = fp->meta_dim ;
  RSF_64_to_32(eos.h.vdir,  offset_vdir) ;         // vdir record position in file
  RSF_64_to_32(eos.h.vdirs, vdir_size) ;           // vdir record size
  RSF_64_to_32(eos.h.sseg, offset_eof) ;           // segment size including EOS
  RSF_64_to_32(eos.h.seg,  offset_eof - sizeof(end_of_segment)) ;           // segment size excluding EOS
  //   eos.h.tail.rlm = fp->meta_dim ;
  eos.h.tail.rlm = fp->rec_meta ;
  RSF_64_to_32(eos.h.tail.rl, sizeof(end_of_segment)) ;
  nc = write(fp->fd, &eos, sizeof(end_of_segment)) ;    // write end of compact segment
  sparse_start = lseek(fp->fd, 0L, SEEK_CUR) ;

  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: '%s' EOS, rt = %d, zr = %d, rl = %8.8x %8.8x, rlm = %d\n", 
          __func__, fp->name, eos.h.tail.rt, eos.h.tail.zr, eos.h.tail.rl[0], eos.h.tail.rl[1], eos.h.tail.rlm);
  // fprintf(stderr,"RSF_Close_file DEBUG : middle of close, segmax = %ld, sparse_start = %ld, sparse_top = %ld\n",fp->seg_max, sparse_start, sparse_top);
  // system("ls -l demo0.rsf");

  if(fp->seg_max > 0){                                // this was a sparse segment
    sparse_start = lseek(fp->fd, 0L, SEEK_CUR) ;
    sparse_top = fp->seg_base + fp->seg_max ;
    sparse_size = sparse_top - sparse_start ;         // new sparse segment size, original end of sparse segment - after compact eos
    //     fprintf(stderr,"DEBUG: CLOSE: sparse_size = %lx %ld, offset_eof = %lx %ld\n", sparse_size, sparse_size, offset_eof, offset_eof) ;

    RSF_64_to_32(sos.seg, 0L) ;
    RSF_64_to_32(sos.sseg, sparse_size) ;
    //     cur = lseek(fp->fd, 0L, SEEK_CUR) ;
    //     fprintf(stderr,"DEBUG: CLOSE: sos at %lx\n",cur) ;
    nc = write(fp->fd, &sos, sizeof(start_of_segment)) ;

    memcpy(&eos, &fp->eos1, sizeof(eos)) ;         // get or1ginal EOS
    rl_eos = sparse_size - sizeof(start_of_segment) ;
    RSF_64_to_32(eos.l.head.rl, rl_eos) ;
    //     cur = lseek(fp->fd, 0L, SEEK_CUR) ;
    //     fprintf(stderr,"DEBUG: CLOSE: eos.l at %lx\n",cur) ;
    nc = write(fp->fd, &eos.l, sizeof(end_of_segment_lo)) ;

    RSF_64_to_32(eos.h.tail.rl, rl_eos) ;
    RSF_64_to_32(eos.h.sseg, sparse_size) ;
    cur = lseek(fp->fd, rl_eos - sizeof(end_of_segment_hi) - sizeof(end_of_segment_lo), SEEK_CUR);
    nc = write(fp->fd, &eos.h, sizeof(end_of_segment_hi)) ;
    //     fprintf(stderr,"DEBUG: CLOSE: eos.h %lx to %lx\n", cur, cur + nc -1);
  }

  // Fix start of active segment: set segment size, directory address/size, write-open marker, dir meta size
  lseek(fp->fd, fp->seg_base , SEEK_SET) ;
  nc = read(fp->fd, &sos, sizeof(start_of_segment)) ;           // Read start of segment (to keep rest of params)
  RSF_64_to_32(sos.vdir,  offset_vdir) ;                        // Set position of directory record in file
  RSF_64_to_32(sos.vdirs, vdir_size) ;                          // Set size of directory record
  RSF_64_to_32(sos.sseg,  offset_eof) ;                         // Set segment size, including EOS
  RSF_64_to_32(sos.seg,  offset_eof - sizeof(end_of_segment)) ; // Set segment size, excluding EOS
  sos.head.rlm = 0 ;    // fix SOR of appropriate SOS record if file was open in write mode
  sos.head.rlmd = fp->dir_meta ;
  cur = lseek(fp->fd, fp->seg_base , SEEK_SET) ;
  nc = write(fp->fd, &sos, sizeof(start_of_segment)) ;          // Rewrite SOS
  // fprintf(stderr,"RSF_Close_file DEBUG : start_of_segment at %ld\n",fp->seg_base);
  // print_start_of_segment(&sos);


RESET_WRITE_FLAG:
  // ---------- Lock file ---------- //
  RSF_File_lock(fp, 1) ;
  // Rewrite first start of segment to indicate that we will no longer write to this file
  lseek(fp->fd, offset = 0 , SEEK_SET) ;
  nc = read(fp->fd, &fp->sos0, sizeof(start_of_segment)) ;
  // print_start_of_segment(&fp->sos0);

  // Set to zero if exclusive (compact mode), decrement if sparse
  if (fp->sos0.head.rlm == RSF_EXCLUSIVE_WRITE) {
    fp->sos0.head.rlm = 0;
  }
  else if (fp->sos0.head.rlm > 0) {
    fp->sos0.head.rlm = fp->sos0.head.rlm -1 ;
  }

  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: rewriting segment 0 header, rlm = %d\n", __func__, fp->sos0.head.rlm);
  lseek(fp->fd, offset = 0 , SEEK_SET) ;
  nc = write(fp->fd, &fp->sos0, sizeof(start_of_segment)) ;  // rewrite start of segment 0
  if (Lib_LogLevel(APP_LIBFST, NULL) >= APP_DEBUG) print_start_of_segment(&fp->sos0);
  usleep(10000) ;
  RSF_File_lock(fp, 0) ;
  // --------- Unlock file --------- //

  //   fprintf(stderr,"DEBUG: CLOSE: EOF at %lx\n", lseek(fp->fd, 0L, SEEK_END)) ;

CLOSE :
  close(fp->fd) ;                                  // close file
  RSF_Purge_file_slot(fp) ;                        // remove from file table
  // free memory associated with file
  free(fp->name) ;                                 // free file name buffer
  //  free dirblocks and vdir
  while(fp->dirblocks != NULL){
    purge = fp->dirblocks->next ;    // remember next block address
    free(fp->dirblocks) ;            // free current block
    fp->dirblocks = purge ;          // next block
  }
  if(fp->vdir != NULL) free(fp->vdir) ;
  free(fp) ;          // free file control structure

  return 1 ;
}

// dump the contents of a file's directory in condensed format
void RSF_Dump_vdir(RSF_handle h) {
  RSF_File *fp = (RSF_File *) h.p ;
  int32_t i, j, slot, meta_dim ;
  uint64_t wa, rl  ;
  uint32_t *meta ;
  vdir_entry *ventry ;

  if( ! (slot = RSF_Valid_file(fp)) ) return;   // something not O.K. with fp

  for(i = 0 ; i < fp->vdir_used ; i++){
    ventry = fp->vdir[i] ;
    wa = RSF_32_to_64(ventry->wa) ;
    rl = RSF_32_to_64(ventry->rl) ;
//     meta_dim = (ventry->ml) >> 16 ;
    meta_dim = DIR_ML(ventry->ml) ;
    meta = &(ventry->meta[0]) ;
    fprintf(stderr,"%12.12lx (%8.8lx)", wa, rl) ;
    for(j = 0 ; j < meta_dim ; j++) fprintf(stderr," %8.8x", meta[j]) ;
    fprintf(stderr,"\n") ;
  }
}

// dump the contents of a file in condensed format
void RSF_Dump(char *name, int verbose){
  int fd = open(name, O_RDONLY) ;
  start_of_record sor ;
  end_of_record   eor ;
  start_of_segment sos ;
  end_of_segment   eos ;
  int rec_index = 0 ;
  off_t reclen, datalen, tlen, eoslen, read_len ;
  ssize_t nc ;
  char *tab[] = { " NULL ", "[DATA]", "[XDAT]", "[SOS] ", "[EOS] ", " NULL ", "[VLD] ", "[FILE]",
                  " NULL ", "[DATA]", " XDAT ", " _sos_", " _eos_", " NULL ", " _vld_", " FILE "} ;
  uint64_t segsize, ssize ;
  disk_vdir *vd = NULL ;
  vdir_entry *ventry ;
  char *e ;
  uint32_t *meta ;
  uint32_t *data ;
  int ndata ;
  off_t dir_offset, dir_addr, vdir_addr, rec_offset, offset, seg_offset, dir_seg_offset, l_offset ;
  int64_t wa, rl ;
  int tabplus = 0 ;
  uint64_t seg_bot, seg_top, seg_dir, seg_vdir ;
  int segment = 0 ;
  uint64_t eof ;
  char buffer[4096] ;
  char *temp, *temp0 ;
  uint32_t *tempm ;
  uint32_t nmeta ;
  uint64_t temps ;

  const int max_num_meta = 12;

  if(fd < 0) return ;
  eof = lseek(fd, 0L, SEEK_END) ;
  lseek(fd, 0L, SEEK_SET) ;
  dir_addr = 0 ;
  vdir_addr = 0 ;
  offset = 0 ;
  rec_offset = offset ;  // current position
  seg_offset = 0 ;
  dir_seg_offset = 0 ;
  nc = read(fd, &sor, sizeof(sor)) ;
  seg_bot = 0 ;
  seg_top = 0 ;
  seg_dir = 0 ;
  seg_vdir = 0 ;

  fprintf(stderr,"RSF file dump utility, file =%s\n",name);
  fprintf(stderr,"=============================================================================================\n");
  fprintf(stderr,"RL = raw record length (bytes)\n"
                 "PL = payload length (DL+ML*4) (bytes)\n") ;
  fprintf(stderr,"DL = data length (bytes)\n"
                 "ML = record metadata length (32 bit units)\n") ;
  fprintf(stderr,"rlmd = directory metadata length (32 bit units)\n"
                 "rlm = same as ML (sor|eor)\n") ;
  fprintf(stderr,"=============================================================================================\n");
  fprintf(stderr,"    RT   Rec-#  Offset             RL(    PL)        DL  ML  ");
  for (int i = 0; i < max_num_meta; i++) {
    fprintf(stderr, "         ");
  }
  fprintf(stderr, " rlmd rlm|Sanity check\n");

  // Print records 1 by 1
  while (nc > 0) {
    
    // ---------- Generic record info ----------
    reclen = RSF_32_to_64(sor.rl) ;
    datalen = reclen - sizeof(sor) - sizeof(eor) ;
    // tabplus = ((rec_offset < seg_dir) && (rec_offset < seg_vdir)) ? 8 : 0 ;  // only effective for sos, eos, dir records
    tabplus = (rec_offset < seg_vdir) ? 8 : 0 ;  // only effective for sos, eos, dir records
    if (sor.rt > 7) {
      snprintf(buffer,sizeof(buffer),"%2.2x %5d [%12.12lx], %6ld(%6ld),",
               sor.rt, rec_index, rec_offset, reclen, datalen) ;
      sor.rt = 0 ;
    } else {
      snprintf(buffer, sizeof(buffer), "%s %5d [%12.12lx], %6ld(%6ld),",
               tab[sor.rt+tabplus], rec_index, rec_offset, reclen, datalen) ;
    }

    // ---------- Specific record info ----------
    int num_meta = -1;
    switch(sor.rt){
      case RT_VDIR :
        dir_offset = lseek(fd, -sizeof(sor), SEEK_CUR) ; 
        vd = (disk_vdir *) malloc(datalen + sizeof(sor)) ;
        nc = read(fd, vd, datalen + sizeof(sor) ) ;  // read directory
        fprintf(stderr,"  %s           , records  = %8d, dir address %8.8lx %s addr %8.8lx, rlm = %d", buffer,
                vd->entries, dir_offset, (dir_offset == vdir_addr) ? "==": "!=", vdir_addr, vd->sor.rlm) ;
        break ;
      case 0 :
      case 5 :
      case RT_DATA :
      case RT_XDAT :
        read_len = sor.rlm * sizeof(uint32_t) ;
        data = (uint32_t *) malloc(read_len) ;
        nc = read(fd, data, read_len) ;               // read metadata part
        lseek(fd, datalen - nc, SEEK_CUR) ;           // skip rest of record
        ndata = datalen/sizeof(int32_t) - sor.rlm;
        fprintf(stderr,"  %s %6d*%d, %2d [", buffer, ndata, sor.dul, sor.rlm) ;
        num_meta = sor.rlm <= max_num_meta ? sor.rlm : max_num_meta - 1;
        for(int i=0 ; i<num_meta ; i++) {
          fprintf(stderr," %8.8x", data[i]) ; 
        }
        if (num_meta < sor.rlm) {
          fprintf(stderr,"   ...   ") ; 
        }
        else if (sor.rlm < max_num_meta) {
          for (int i = sor.rlm; i < max_num_meta; i++) {
            fprintf(stderr, "         ");
          }
        }
        fprintf(stderr,"], %d, %d", sor.rlmd, sor.rlm) ;
        if(data) free(data) ;
        data = NULL ;
        break ;
      case RT_FILE :
        read_len = sor.rlm * sizeof(uint32_t) ;
        data = (uint32_t *) malloc(read_len) ;
        nc = read(fd, data, read_len) ;               // read metadata part
        temp0 = (char *) data ;                       // start of metadata
        temp = temp0 + nc -1 ;                        // end of metadata
        while((temp[ 0] == 0) && (temp > temp0)) temp-- ;    // skip trailing nulls
        while((temp[-1] != 0) && (temp > temp0)) temp-- ;    // back until null is found
        temp0 = temp -1 ;
        tempm = (uint32_t *) temp0 ;
        nmeta = tempm - data ;
        temps = RSF_32_to_64((data+nmeta-2)) ;
        lseek(fd, datalen - nc, SEEK_CUR) ;           // skip rest of record
        ndata = datalen/sizeof(int32_t) - sor.rlm;
        fprintf(stderr,"  %s %6d B, %2d [ %8.8x ", buffer, ndata, sor.rlm, data[0]) ;
        for(int j=1 ; j<nmeta-2 ; j++) fprintf(stderr,"%8.8x ", data[j]) ;
        fprintf(stderr,"] '%s'[%ld]", temp, temps) ;
        // for(i=0 ; i<sor.rlm ; i++) {
        //   fprintf(stderr," %8.8x", data[i]) ; 
        // }
        fprintf(stderr,", %d, %d", sor.rlmd, sor.rlm) ;
        if(data) free(data) ;
        data = NULL ;
        break ;
      case RT_SOS :
        lseek(fd, -sizeof(sor), SEEK_CUR) ; 
        seg_offset = rec_offset ;
        nc = read(fd, &sos, sizeof(sos) - sizeof(eor)) ;
        // meta_dim = sos.meta_dim ;
        // dir_addr = RSF_32_to_64(sos.dir) ;
        vdir_addr = RSF_32_to_64(sos.vdir) ;
        segsize  = RSF_32_to_64(sos.sseg) ;
        ssize    = RSF_32_to_64(sos.seg) ;
        if(seg_offset >= seg_top) {
          seg_bot = seg_offset ;
          seg_top = seg_bot + segsize - 1 ;
          seg_dir = seg_bot + dir_addr ;
          seg_vdir = seg_bot + vdir_addr ;
          dir_seg_offset = seg_offset ;
        }
        fprintf(stderr,"%s",(ssize == 0 && segsize != 0) ? "<>" : "  " ) ;
        fprintf(stderr,"%s '", buffer) ;
        for(int i=0 ; i<8 ; i++) {
          fprintf(stderr,"%c", sos.sig1[i]) ;
        }
        fprintf(stderr,"', seg size = %8ld, dir_offset = %8.8lx %8.8lx, rlm = %d", 
                segsize, dir_addr, vdir_addr, sos.head.rlm) ;
        break ;
      case RT_EOS :
        lseek(fd, -sizeof(sor), SEEK_CUR) ;
        nc = read(fd, &eos, sizeof(eos) - sizeof(eor)) ;          // read full EOS without end of record part
        eoslen = RSF_32_to_64(eos.l.head.rl) ;
        if(eoslen > sizeof(eos)) {                                // sparse segment EOS
          lseek(fd, eoslen - sizeof(eos) - sizeof(end_of_segment_hi) + sizeof(eor), SEEK_CUR) ;
          nc = read(fd, &eos.h, sizeof(end_of_segment_hi) - sizeof(eor)) ;   // get high part of EOS
        }
        // meta_dim = eos.h.meta_dim ;
        segsize  = RSF_32_to_64(eos.h.sseg) ;
        ssize    = RSF_32_to_64(sos.seg) ;
        // dir_addr = RSF_32_to_64(eos.h.dir) ;
        vdir_addr = RSF_32_to_64(eos.h.vdir) ;
        fprintf(stderr,"%s",(reclen > sizeof(end_of_segment)) ? "<>" : "  " ) ;
        fprintf(stderr,"%s             seg size = %8ld, dir_offset = %8.8lx %8.8lx, rlm = %d", buffer,
                segsize, dir_addr, vdir_addr, eos.l.head.rlm) ;
        break ;
      default :
        lseek(fd, datalen, SEEK_CUR) ;   // skip data
        break ;
    }
    nc = read(fd, &eor, sizeof(eor)) ;
    tlen = RSF_32_to_64(eor.rl) ;
    if(tlen != reclen){
      fprintf(stderr,"|%d rl = %ld|%ld S(%2.2d) %s\n",eor.rlm, reclen, tlen, segment, (tlen==reclen) ? ", O.K." : ", RL ERROR") ;
    }else{
      fprintf(stderr,"|%d S(%2.2d) %s\n",eor.rlm, segment, ", O.K.") ;
    }
    if(tabplus == 0 && sor.rt == RT_EOS) segment++ ;
    if(tlen != reclen) goto END ; ; // Inconsistency in record length between SOR and EOR, so we stop there

    // Print content of virtual directory (if desired)
    if(sor.rt == RT_VDIR  && vd != NULL  && vd->entries > 0){
      if( (verbose > 0 && tabplus == 0) || (verbose > 10) ){
        fprintf(stderr," Directory  WA(SEG)       WA(FILE)    B   RLM  RLMD       RL     RT CLASS  META \n") ;
        e = (char *) &(vd->entry[0]) ;
        l_offset = (tabplus == 0) ? dir_seg_offset : seg_offset ;
        for(int i=0 ; i < vd->entries ; i++){
          ventry = (vdir_entry *) e ;
          const int32_t ml = (ventry->ml >> 16) ;
          const int32_t mlr = ventry->ml & 0xFFFF ;
          wa = RSF_32_to_64(ventry->wa) ;
          rl = RSF_32_to_64(ventry->rl) ;
          meta = &(ventry->meta[0]) ;
          fprintf(stderr," [%6d] %12.12lx [%12.12lx] %1d %5d %5d %12.12lx", i, wa, wa+l_offset, ventry->dul, mlr, ml, rl) ;
          fprintf(stderr," %2.2x %6.6x", meta[0] & 0xFF, meta[0] >> 8) ;
          if( (meta[0] & 0xFF) != RT_FILE){
            const int num_meta = ml <= max_num_meta ? ml : max_num_meta - 1;
            for(int j=1 ; j<num_meta ; j++) {
              fprintf(stderr," %8.8x", meta[j]) ;
            }
            if (num_meta < ml) fprintf(stderr, "   ...   ");
            fprintf(stderr,"\n") ;
          }else{
            temp0 = (char *) meta ;
            temp = temp0 + (ml * sizeof(uint32_t)) -1 ;
            while((temp[ 0] == 0) && (temp > temp0)) temp-- ;    // skip trailing nulls
            while((temp[-1] != 0) && (temp > temp0)) temp-- ;    // back until null is found
            temp0 = temp -1 ;
            tempm = (uint32_t *) temp0 ;
            nmeta = tempm - meta ;
            temps = RSF_32_to_64((meta+nmeta-2)) ;
            // fprintf(stderr," %p %p",tempm, meta) ;
            for(int j=1 ; j<nmeta-2 ; j++)
              fprintf(stderr," %8.8x", meta[j]) ;
            fprintf(stderr," '%s' [%ld]\n", temp, temps) ;
          }
          e = e + sizeof(vdir_entry) + ml * sizeof(uint32_t) ;
        }
        fprintf(stderr,"-------------------------------------------------\n") ;
      }
    }
    if(vd) {
      free(vd) ;
      vd = NULL ;
    }
    rec_index++ ;
    rec_offset = lseek(fd, offset = 0, SEEK_CUR) ;  // current position
    if(rec_offset >= eof) break ;
    nc = read(fd, &sor, sizeof(sor)) ;
    if((nc > 0) && (nc < sizeof(sor))){
      fprintf(stderr," invalid sor, len = %ld, expected = %ld\n", nc, sizeof(sor)) ;
      break ;
    }
  }
END :
  // fprintf(stderr,"DEBUG: DUMP: EOF at %lx\n", eof) ;
  close(fd) ;
  Lib_Log(APP_LIBFST, APP_DEBUG, "%s: file '%s' closed, fd = %d, EOF = %lx\n", __func__, name, fd, eof) ;
}

//! \return File slot of given file handle
int32_t RSF_File_slot(RSF_handle h) {
  RSF_File *fp = (RSF_File *) h.p ;
  return fp->slot ;
}

void print_start_of_record(start_of_record* sor) {
  Lib_Log(APP_LIBFST, APP_ALWAYS,
          "type %s, rec meta length %d, zr %x, length %d, dir meta length %d, unused bits %d, elem length %d\n",
          rt_to_str(sor->rt), sor->rlm, sor->zr, RSF_32_to_64(sor->rl), sor->rlmd, sor->ubc, sor->dul);
}

void print_start_of_segment(start_of_segment* sos) {
  Lib_Log(APP_LIBFST, APP_ALWAYS, "head: "); print_start_of_record(&sos->head);
  unsigned char marker[9];
  strncpy(marker, sos->sig1, 8);
  marker[8] = '\0';
  Lib_Log(APP_LIBFST, APP_ALWAYS,
         "sig1 %s, sign %x, size (exc) %lu, size (inc) %lu, dir record offset %lu, dir record size %lu\n",
         marker, sos->sign, RSF_32_to_64(sos->seg), RSF_32_to_64(sos->sseg), RSF_32_to_64(sos->vdir),
         RSF_32_to_64(sos->vdirs));
}

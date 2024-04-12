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

/*
 Random Segmented Files PUBLIC interface
 RSF_RO implies that the file MUST exist
 RSF_RW implies create file if it does not exist
 RSF_AP implies that the file MUST exist and will be written into (implies RSF_RW)
 RSF_NSEG means that the file will be mostly "write" (only new sparse segment will be accessible)
 RSF_PSEG means parallel segment mode (mostly write, read from local segment only)
 RSF_FUSE means consolidate segments into ONE (ignored if RSF_RW not set or new file)
 otherwise the last segment gets extended and the other segments remain untouched

RSF_NSEG and RSF_PSEG : deferred implementation
#define RSF_NSEG    16
#define RSF_PSEG    32

RT_DATA_CLASS  : class for "vanilla" data records
RT_FILE_CLASS  : class for file container records

RSF_META_RESERVED : number of metadata items reserved for internal use
meta[0] : used for record class mask

DT_08 ... DT_64  : length of data elements in record (for endianness management) (1 2 4 8 bytes)
*/

#ifndef RMN_RSF_H__
#define RMN_RSF_H__

#include <limits.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/resource.h>


//! \{
//! \name Opening modes
#define RSF_RO       2
#define RSF_RW       4
#define RSF_AP       8
#define RSF_FUSE  1024
//! \}

/* meta[0] used for Record Type (RT) and Record Class */
/* meta[1] possibly used for data map length */
/* #define RSF_META_RESERVED 1 */
// #define RSF_META_RESERVED 2

//! Number of 32-bit elements at the beginning of the search metadata of
//! every record.
//! The first one is used to hold RSF version, record type and record class
#define RSF_META_RESERVED 1

//! \{
//! \name Record types
static const uint8_t RT_NULL   = 0;     //!< No type (can match any record)
static const uint8_t RT_DATA   = 1;     //!< Data record
static const uint8_t RT_XDAT   = 2;     //!<
static const uint8_t RT_SOS    = 3;     //!< Start-of-segment record
static const uint8_t RT_EOS    = 4;     //!< End-of-segment record
static const uint8_t RT_VDIR   = 6;     //!< Directory record
static const uint8_t RT_FILE   = 7;     //!< File record
static const uint8_t RT_CUSTOM = 8;
static const uint8_t RT_DEL    = 0x80;  //!< Deleted record
//! \}

//! \{
//! \name Record classes
static const uint8_t RC_NULL = 0;
static const uint8_t RC_DATA = 1;    //!< Indicate a record that contains data
static const uint8_t RC_FILE = 0x80; //!< Indicate a record that contains a file
//! \}

#define DT_ANY 0
#define DT_08  1
#define DT_16  2
#define DT_32  4
#define DT_64  8

#define RSF_DIAG_NONE    0
#define RSF_DIAG_ERROR   1
#define RSF_DIAG_WARN    2
#define RSF_DIAG_INFO    3
#define RSF_DIAG_NOTE    4
#define RSF_DIAG_DEBUG0  6
#define RSF_DIAG_DEBUG1  7
#define RSF_DIAG_DEBUG2  8

#define RSF_KEY32  1
#define XDF_KEY32  0
#define BAD_KEY32 -1

typedef struct{   // this struct only contains a pointer to the actual full control structure
  void *p ;
} RSF_handle ;

//! Description of a RSF record + its content
typedef struct{
  void     *sor ;      //!< start of record address ( pointer to RSF_record.d )
  uint32_t *meta ;     //!< pointer to metadata array ( sor + sizeof(sor) )
  void     *data ;     //!< pointer to start of data payload array (data map or data)
  void     *eor ;      //!< end of record address ( (void *) RSF_record.d + max_data )
  uint64_t data_size ; //!< actual data size in bytes (may remain 0 in unmanaged records)
  uint64_t max_data ;  //!< maximum data payload size in bytes
  int64_t rsz ;        //!< allocated size of RSF_record in bytes (including SOR, metadata, data and EOR)
  uint16_t dir_meta ;  //!< directory metadata size in uint32_t units
  uint16_t rec_meta ;  //!< record metadata size in uint32_t units
  uint16_t elem_size ; //!< length of data elements in d[] (1/2/4/8 bytes) (endianness management)
  uint8_t  rec_type ;  //!< Type of record (data, directory, etc)
  uint8_t  rec_class ; //!< Class of record (data vs file?) TODO clarify
  uint8_t  d[] ;       //!< dynamic data array (bytes)
} RSF_record ;

//! Record information. This struct MUST BE TREATED AS READ-ONLY.
typedef struct{
  uint64_t wa ;        //! address of record in file
  uint64_t rl ;        //! record length in bytes (including SOR, metadata, data and EOR)
  uint64_t wa_data ;   //! address of data in file
  uint64_t data_size ; //! actual data size in bytes (may remain 0 in unmanaged records)
  uint64_t wa_meta ;   //! address of metadata in file
  const uint32_t *meta ;  //! pointer to directory metadata (DO NOT STORE INTO)
  const char     *fname ; //! pointer to filename if file container (DO NOT STORE INTO)
  uint64_t file_size ; //! true file size (from metadata) if file container
  uint16_t dir_meta ;  //! directory metadata size in uint32_t units
  uint16_t dir_meta0 ; //! size excluding file name and file length in file containers
  uint16_t rec_meta ;  //! record metadata size in uint32_t units
  uint16_t elem_size ; //! length of data elements (1/2/4/8 bytes) (endianness management)
// NOTE: add something for data map length ?
} RSF_record_info ;

// metadata matching function (normally supplied by application)
typedef int32_t RSF_Match_fn(uint32_t *criteria, uint32_t *meta, uint32_t *mask, int ncrit, int nmeta, int reject_a_priori) ;

int32_t RSF_set_diag_level(int32_t level) ;
char *RSF_diag_level_text(int32_t level) ;

int32_t RSF_Default_match(uint32_t *criteria, uint32_t *meta, uint32_t *mask, int ncrit, int nmeta, int reject_a_priori) ;
int32_t RSF_Base_match(uint32_t *criteria, uint32_t *meta, uint32_t *mask, int ncrit, int nmeta, int reject_a_priori) ;  // ignores mask


RSF_handle RSF_Open_file(const char *fname, const int32_t mode, const int32_t dir_meta_length, const char *appl, int64_t *segsize);

int64_t RSF_Lookup(RSF_handle h, int64_t key0, uint32_t *criteria, uint32_t *mask, uint32_t lcrit) ;

int32_t RSF_Get_mode(RSF_handle h) ;

RSF_record_info RSF_Get_record_info_by_index(RSF_handle h, uint32_t key) ;
RSF_record_info RSF_Get_record_info(RSF_handle h, int64_t key) ;
RSF_record *RSF_Get_record(RSF_handle h, const int64_t key, const int32_t metadata_only, void* prealloc_space) ;
uint32_t RSF_Get_num_records(RSF_handle) ;
uint32_t RSF_Get_num_records_at_open(RSF_handle h) ;
void* RSF_Get_next(RSF_handle h) ;
int32_t RSF_Is_record_in_file(RSF_handle h, const int64_t key) ;

int64_t RSF_Put_bytes(RSF_handle h, RSF_record *record, uint32_t *meta, uint32_t rec_meta, uint32_t dir_meta, void *data, size_t data_size, int data_element) ;
int64_t RSF_Put_data(RSF_handle h, void *data_record, uint32_t *meta, uint32_t rec_meta, uint32_t dir_meta, void *data, size_t data_elements, int element_size) ;
int64_t RSF_Put_record(RSF_handle h, RSF_record *record, size_t data_size) ;

int64_t RSF_Put_file(RSF_handle h, char *filename, uint32_t *meta, uint32_t meta_size) ;
int64_t RSF_Get_file(RSF_handle h, int64_t key, char *alias, uint32_t **meta, uint32_t *meta_size) ;

void *RSF_Get_record_meta(RSF_handle h, int64_t key, int32_t *metasize, uint64_t *datasize) ;

int32_t RSF_Close_file(RSF_handle h) ;


void RSF_Dump(char *name, int verbose) ;


void RSF_Dump_dir(RSF_handle h) ;

void RSF_Dump_vdir(RSF_handle h) ;

int32_t RSF_Valid_handle(RSF_handle h) ;

//! create pointer to a new allocated record (C)
RSF_record *RSF_New_record(RSF_handle h, int32_t rec_meta, int32_t dir_meta, const uint8_t rec_type,
                           const uint8_t rec_class, const size_t max_data, void * const t, int64_t szt) ;
int32_t RSF_Record_add_meta(RSF_record *r, uint32_t *meta, int32_t rec_meta, int32_t dir_meta, uint32_t data_elem) ;  // add metadata
int64_t RSF_Record_add_bytes(RSF_record *r, void *data, size_t data_size) ;  // add data bytes
int64_t RSF_Record_add_elements(RSF_record *r, void *data, size_t num_data_elements, int data_element_size) ; //!< \copydoc RSF_Record_add_elements
int64_t RSF_Record_set_num_elements(RSF_record *r, size_t num_elements, int element_size) ; //!< \copydoc RSF_Record_set_num_elements
void RSF_Free_record(RSF_record * r) ;                       // free the space allocated to that record
int32_t RSF_Valid_record(RSF_record *r) ;                    // does this look like a valid record ?
int64_t RSF_Record_free_space(RSF_record *r) ;               // space available for more data in record allocated by RSF_New_record
int64_t RSF_Record_allocated(RSF_record *r) ;                // allocated size of record allocated by RSF_New_record
int64_t RSF_Record_max_space(RSF_record *r) ;                // maximum data payload size in record allocated by RSF_New_record
void *RSF_Record_data(RSF_record *r) ;                       // pointer to data payload in record allocated by RSF_New_record
uint64_t RSF_Record_data_size(RSF_record *r) ;               // size of data payload in record allocated by RSF_New_record
void *RSF_Record_meta(RSF_record *r) ;                       // pointer to metadata in record allocated by RSF_New_record
uint32_t RSF_Record_meta_size(RSF_record *r) ;               // size of metadata in record allocated by RSF_New_record

int64_t RSF_Used_space(RSF_handle h) ;
int64_t RSF_Available_space(RSF_handle h) ;
uint64_t RSF_Put_null_record(RSF_handle h, size_t record_size) ;
int32_t RSF_Key32(int64_t key64) ;
int64_t RSF_Key64(int32_t key32) ;
uint32_t RSF_Key64_to_file_slot(int64_t key64) ;
int32_t RSF_Key32_type(int32_t key32) ;
int32_t RSF_File_slot(RSF_handle h) ;
int64_t RSF_Make_key(const int32_t slot, const int32_t record_id) ;
RSF_handle RSF_Key32_to_handle(int32_t key32) ;
RSF_handle RSF_Key64_to_handle(int64_t key64) ;
int32_t RSF_Basic_check(const char* filename) ;
int64_t RSF_Checkpoint(RSF_handle h) ;

#endif // RMN_RSF_H__

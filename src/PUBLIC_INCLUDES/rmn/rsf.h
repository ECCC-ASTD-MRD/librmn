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
#if defined(__GFORTRAN__) && ! defined(IN_FORTRAN_CODE)
#define IN_FORTRAN_CODE
#endif

/* the following defines are used in both Fortran and C */

#if ! defined(RSF_VERSION)

#define RSF_VERSION_STRING "1.0.0"
#define RSF_VERSION 10000
#define RSF_RO       2
#define RSF_RW       4
#define RSF_AP       8
#define RSF_FUSE  1024

/* meta[0] used for Record Type (RT) and Record Class */
/* meta[1] possibly used for data map length */
/* #define RSF_META_RESERVED 1 */
#define RSF_META_RESERVED 2

#define RT_NULL    0
#define RT_DATA    1
#define RT_XDAT    2
#define RT_SOS     3
#define RT_EOS     4
#define RT_VDIR    6
#define RT_FILE    7
#define RT_CUSTOM  8
#define RT_DEL  0x80

#define RT_DATA_CLASS 1
#define RT_FILE_CLASS 0x80000

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

#if defined(IN_FORTRAN_CODE)
! Fortran definitions

  type, BIND(C) :: RSF_handle
    private
    type(C_PTR) :: p
  end type

  type, BIND(C) :: RSF_record         ! not a totally honest description (C dynamic array is omitted)
    private                           ! MUST REFLECT EXACTLY C struct RSF_record (see below)
    type(C_PTR) :: sor                ! pointer to start of record descriptor (not used by Fortran)
    type(C_PTR) :: meta               ! pointer to integer metadata array
    type(C_PTR) :: data               ! pointer to start of integer data array (data map or data)
!    type(C_PTR) :: chunks             ! pointer to start of integer data chunks array
    type(C_PTR) :: eor                ! pointer to end of record descriptor (not used by Fortran)
    integer(C_INT64_T) :: data_size   ! data payload size in bytes (may remain 0 in unmanaged records)
    integer(C_INT64_T) :: max_data    ! maximum data payload size in bytes
    integer(C_INT64_T) :: rsz         ! allocated size of RSF_record
    integer(C_INT16_T) :: dir_meta    ! directory metadata size in 32 bit units (max 0xFFFF)
    integer(C_INT16_T) :: rec_meta    ! record metadata size in 32 bit units (max 0xFFFF)
    integer(C_INT16_T) :: elem_size   ! length of data elements (1/2/4/8 bytes) (endianness management)
    integer(C_INT16_T) :: reserved    ! alignment (maybe use for data map length ?)
    ! dynamic data array follows, see C struct
  end type

  type, BIND(C) :: RSF_record_info_c  ! MUST REFLECT EXACTLY C struct RSF_record_info (see below)
    private
    integer(C_INT64_T) wa             ! address of record in file
    integer(C_INT64_T) rl             ! record length
    integer(C_INT64_T) wa_data        ! address of data in file
    integer(C_INT64_T) data_size      ! actual data size in bytes (may remain 0 in unmanaged records)
    integer(C_INT64_T) wa_meta        ! address of metadata in file
    type(C_PTR)        meta           ! pointer to directory metadata
    type(C_PTR)        fname          ! pointer to filename if file container (NULL pointer otherwise)
    integer(C_INT64_T) file_size      ! true file size (from metadata) if file container (0 otherwise)
    integer(C_INT16_T) dir_meta       ! directory metadata size in uint32_t units
    integer(C_INT16_T) dir_meta0      ! size excluding file name and file length in file containers
    integer(C_INT16_T) rec_meta       ! record metadata size in uint32_t units
    integer(C_INT16_T) elem_size      ! length of data elements (1/2/4/8 bytes) (endianness management)
!   NOTE: add something for data map length ?
  end type

  type, BIND(C) :: RSF_record_handle
    private
    type(C_PTR) :: record      ! pointer to RSF_record (see above)
    !  type(RSF_record_handle)   :: h
    !  type(RSF_record), pointer :: r
    !  call C_F_POINTER(h%record, r)
    !  r%meta, r%data, r%meta_size, r%data_size, etc ... now accessible in module procedures
  end type
#else // IN_FORTRAN_CODE
// C includes and definitions

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
  uint16_t reserved ;  //!< alignment (maybe use for data map length ?)
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

// typedef struct{   // this struct only contains a pointer to the actual composite record
//   void *p ;
// } RSF_record_handle ;

// metadata matching function (normally supplied by application)
typedef int32_t RSF_Match_fn(uint32_t *criteria, uint32_t *meta, uint32_t *mask, int ncrit, int nmeta, int reject_a_priori) ;

#endif // IN_FORTRAN_CODE

#if defined(IN_FORTRAN_CODE)
interface
#endif

#if defined(IN_FORTRAN_CODE)
  function RSF_set_diag_level(level) result(oldlevel) BIND(C,name='RSF_set_diag_level')
    import :: C_INT32_T
    implicit none
    integer(C_INT32_T), intent(IN), value :: level
    integer(C_INT32_T) :: oldlevel
  end function RSF_set_diag_level
#else
  int32_t RSF_set_diag_level(int32_t level) ;
  char *RSF_diag_level_text(int32_t level) ;
#endif

#if defined(IN_FORTRAN_CODE)
  function RSF_Default_match(criteria, meta, mask, ncrit, nmeta, reject_a_priori) result(status) bind(C,name='RSF_Default_match')
    import :: C_INT32_T
    implicit none
    integer(C_INT32_T), intent(IN), dimension(*) :: criteria, meta, mask
    integer(C_INT32_T), intent(IN), value :: ncrit, nmeta, reject_a_priori
    integer(C_INT32_T) :: status
  end function RSF_Default_match

  function RSF_Base_match(criteria, meta, mask, ncrit, nmeta, reject_a_priori) result(status) bind(C,name='RSF_Base_match')
    import :: C_INT32_T
    implicit none
    integer(C_INT32_T), intent(IN), dimension(*) :: criteria, meta, mask
    integer(C_INT32_T), intent(IN), value :: ncrit, nmeta, reject_a_priori
    integer(C_INT32_T) :: status
  end function RSF_Base_match
#else
  int32_t RSF_Default_match(uint32_t *criteria, uint32_t *meta, uint32_t *mask, int ncrit, int nmeta, int reject_a_priori) ;
  int32_t RSF_Base_match(uint32_t *criteria, uint32_t *meta, uint32_t *mask, int ncrit, int nmeta, int reject_a_priori) ;  // ignores mask
#endif

#if defined(IN_FORTRAN_CODE)
  function RSF_Open_file(fname, mode, dir_meta_length, appl, segsize) result(handle) bind(C,name='RSF_Open_file')
    import :: RSF_handle, C_CHAR, C_INT32_T, C_INT64_T
    implicit none
    character(C_CHAR), intent(IN), dimension(*) :: fname
    integer(C_INT32_T), intent(IN), value :: mode
    integer(C_INT32_T), intent(IN), value :: dir_meta_length
    character(C_CHAR), intent(IN), dimension(4) :: appl
    integer(C_INT64_T), intent(IN) :: segsize
    type(RSF_handle) :: handle
  end function RSF_Open_file
#else
  RSF_handle RSF_Open_file(const char *fname, const int32_t mode, const int32_t dir_meta_length, const char *appl, int64_t *segsize);
#endif

#if defined(IN_FORTRAN_CODE)
  function RSF_Lookup(handle, key0, criteria, mask, lcrit) result(key) bind(C,name='RSF_Lookup')
    import :: RSF_handle, C_INT32_T, C_INT64_T
    implicit none
    type(RSF_handle), intent(IN), value :: handle
    integer(C_INT64_T), intent(IN), value :: key0
    integer(C_INT32_T), intent(IN), dimension(*) :: criteria, mask
    integer(C_INT32_T), intent(IN), value :: lcrit
    integer(C_INT64_T) :: key
  end function RSF_Lookup
#else
  int64_t RSF_Lookup(RSF_handle h, int64_t key0, uint32_t *criteria, uint32_t *mask, uint32_t lcrit) ;
#endif

#if defined(IN_FORTRAN_CODE)
  function RSF_Get_mode(handle) result(mode) bind(C, name = 'RSF_Get_mode')
    import :: RSF_handle, C_INT32_T
    implicit none
    type(RSF_handle), intent(IN), value :: handle
    integer(C_INT32_T) :: mode
  end function RSF_Get_mode
#else
  int32_t RSF_Get_mode(RSF_handle h) ;
#endif

#if defined(IN_FORTRAN_CODE)
  function RSF_Get_record(handle, key) result(rh) bind(C,name='RSF_Get_record')
    import :: RSF_handle, C_INT32_T, C_INT64_T, RSF_record_handle
    implicit none
    type(RSF_handle), intent(IN), value :: handle
    integer(C_INT64_T), intent(IN), value :: key
    type(RSF_record_handle) :: rh
  end function RSF_Get_record
  function RSF_Get_record_info_by_index(fh, index) result(info) BIND(C, name='RSF_Get_record_info')
    import :: RSF_handle, C_INT32_T, RSF_record_info_c
    implicit none
    type(RSF_handle), intent(IN), value :: fh
    integer(C_INT32_T), intent(IN), value :: index
    type(RSF_record_info_c) :: info
  end function RSF_Get_record_info_by_index
  function RSF_Get_record_info(fh, key) result(info) BIND(C, name='RSF_Get_record_info')
    import :: RSF_handle, C_INT64_T, RSF_record_info_c
    implicit none
    type(RSF_handle), intent(IN), value :: fh
    integer(C_INT64_T), intent(IN), value :: key
    type(RSF_record_info_c) :: info
  end function RSF_Get_record_info
  function RSF_Get_num_records(handle) result(num_records) bind(C, name = 'RSF_Get_num_records')
    import :: RSF_handle, C_INT32_T
    type(RSF_handle), intent(IN), value :: handle
    integer(C_INT32_T) :: num_records
  end function RSF_Get_num_records
  function RSF_Get_num_records_at_open(handle) result(num_records) bind(C, name = 'RSF_Get_num_records_at_open')
    import :: RSF_handle, C_INT32_T
    type(RSF_handle), intent(IN), value :: handle
    integer(C_INT32_T) :: num_records
  end function RSF_Get_num_records_at_open
#else
  RSF_record_info RSF_Get_record_info_by_index(RSF_handle h, uint32_t key) ;
  RSF_record_info RSF_Get_record_info(RSF_handle h, int64_t key) ;
  RSF_record *RSF_Get_record(RSF_handle h, int64_t key) ;
  uint32_t RSF_Get_num_records(RSF_handle) ;
  uint32_t RSF_Get_num_records_at_open(RSF_handle h) ;
#endif

#if defined(IN_FORTRAN_CODE)
  end interface

  interface RSF_Put   ! generic put interface
  function RSF_Put_bytes(handle, record, meta, meta_size, data, data_bytes) result(key) bind(C,name='RSF_Put_bytes')
    import :: RSF_handle, RSF_record, C_INT32_T, C_PTR, C_SIZE_T, C_INT64_T
    implicit none
    type(RSF_handle), intent(IN), value :: handle
    type(RSF_record), intent(IN) :: record                 ! C expects a pointer to RAF record
    integer(C_INT32_T), intent(IN), dimension(*) :: meta
    type(C_PTR), value :: data
    integer(C_INT32_T), intent(IN), value :: meta_size
    integer(C_SIZE_T), intent(IN), value :: data_bytes
    integer(C_INT64_T) :: key
  end function RSF_Put_bytes

  function RSF_Put_data(handle, record, meta, meta_size, data, data_size, element_size) result(key) bind(C,name='RSF_Put_data')
    import :: RSF_handle, RSF_record, C_INT32_T, C_PTR, C_SIZE_T, C_INT64_T
    implicit none
    type(RSF_handle), intent(IN), value :: handle
    type(RSF_record), intent(IN) :: record                 ! C expects a pointer to RAF record
    integer(C_INT32_T), intent(IN), dimension(*) :: meta
    type(C_PTR), value :: data
    integer(C_INT32_T), intent(IN), value :: meta_size
    integer(C_SIZE_T), intent(IN), value :: data_size
    integer, intent(IN), value :: element_size
    integer(C_INT64_T) :: key
  end function RSF_Put_data

  function RSF_Put_record(handle, r, data_size) result(key) bind(C,name='RSF_Put_record')
    import :: RSF_handle, C_INT64_T, C_SIZE_T, RSF_record
    implicit none
    type(RSF_handle), intent(IN), value :: handle
    type(RSF_record), intent(IN) :: r
    integer(C_SIZE_T), intent(IN), value :: data_size
    integer(C_INT64_T) :: key
  end function RSF_Put_record
  end interface

  interface
#else
  int64_t RSF_Put_bytes(RSF_handle h, RSF_record *record, uint32_t *meta, uint32_t rec_meta, uint32_t dir_meta, void *data, size_t data_size, int data_element) ;
  int64_t RSF_Put_data(RSF_handle h, void *data_record, uint32_t *meta, uint32_t rec_meta, uint32_t dir_meta, void *data, size_t data_elements, int element_size) ;
  int64_t RSF_Put_record(RSF_handle h, RSF_record *record, size_t data_size) ;
#endif

#if defined(IN_FORTRAN_CODE)
#else
  int64_t RSF_Put_file(RSF_handle h, char *filename, uint32_t *meta, uint32_t meta_size) ;
  int64_t RSF_Get_file(RSF_handle h, int64_t key, char *alias, uint32_t **meta, uint32_t *meta_size) ;
#endif
#if defined(IN_FORTRAN_CODE)
  function RSF_Get_record_meta(handle, key, metasize, datasize) result(p) bind(C,name='RSF_Get_record_meta')
    import :: RSF_handle, C_INT32_T, C_INT64_T, C_PTR
    implicit none
    type(RSF_handle), intent(IN), value :: handle
    integer(C_INT64_T), intent(IN), value :: key
    integer(C_INT32_T), intent(OUT) :: metasize
    integer(C_INT64_T), intent(OUT) :: datasize
    type(C_PTR) :: p
  end function RSF_Get_record_meta
#else
  void *RSF_Get_record_meta(RSF_handle h, int64_t key, int32_t *metasize, uint64_t *datasize) ;
#endif

#if defined(IN_FORTRAN_CODE)
  function RSF_Close_file(handle) result (status) bind(C,name='RSF_Close_file')
    import :: RSF_handle, C_INT32_T
    implicit none
    type(RSF_handle), intent(IN), value :: handle
    integer(C_INT32_T) :: status
  end function RSF_Close_file
#else
  int32_t RSF_Close_file(RSF_handle h) ;
#endif

#if defined(IN_FORTRAN_CODE)
  subroutine RSF_Dump(name, verbose) bind(C,name='RSF_Dump')
    import :: C_CHAR
    implicit none
    character(C_CHAR), dimension(*), intent(IN) :: name
    integer, intent(IN), value :: verbose
  end subroutine RSF_Dump
#else
  void RSF_Dump(char *name, int verbose) ;
#endif

#if defined(IN_FORTRAN_CODE)
  subroutine RSF_Dump_dir(handle) bind(C,name='RSF_Dump_dir')
    import :: RSF_handle, C_INT32_T
    implicit none
    type(RSF_handle), intent(IN), value :: handle
  end subroutine RSF_Dump_dir
#else
  void RSF_Dump_dir(RSF_handle h) ;
#endif

#if defined(IN_FORTRAN_CODE)
  subroutine RSF_Dump_vdir(handle) bind(C,name='RSF_Dump_vdir')
    import :: RSF_handle, C_INT32_T
    implicit none
    type(RSF_handle), intent(IN), value :: handle
  end subroutine RSF_Dump_vdir
#else
  void RSF_Dump_vdir(RSF_handle h) ;
#endif

#if defined(IN_FORTRAN_CODE)
  function RSF_Valid_handle(handle) result (status) bind(C,name='RSF_Valid_handle')
    import :: RSF_handle, C_INT32_T
    implicit none
    type(RSF_handle), intent(IN), value :: handle
    integer(C_INT32_T) :: status
  end function RSF_Valid_handle
#else
  int32_t RSF_Valid_handle(RSF_handle h) ;
#endif

#if defined(IN_FORTRAN_CODE)
!> allocate a new record handle (Fortran)
!> \copydoc RSF_New_record
  function RSF_New_record_handle(handle, rec_meta, dir_meta, max_data, t, szt) result(rh) bind(C,name='RSF_New_record')
    import :: RSF_handle, C_INT32_t, C_INT64_T, RSF_record_handle, C_PTR
    implicit none
    type(RSF_handle), intent(IN), value :: handle
    type(C_PTR), value :: t
    integer(C_INT32_t), intent(IN), value :: rec_meta, dir_meta
    integer(C_INT64_T), intent(IN), value :: max_data, szt
    type(RSF_record_handle) :: rh
  end function RSF_New_record_handle

! free the space allocated to that record
  subroutine RSF_Free_record_handle(rh) bind(C,name='RSF_Free_record')
    import :: C_INT32_T, RSF_record_handle
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
  end subroutine RSF_Free_record_handle

! get free space in record
  function RSF_Record_free_space(rh) result(s) bind(C,name='RSF_Record_free_space')
    import :: RSF_record_handle, C_INT64_T
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
    integer(C_INT64_T) :: s
  end function RSF_Record_free_space

! allocated size of record
  function RSF_Record_allocated(rh) result(s) bind(C,name='RSF_Record_allocated')
    import :: RSF_record_handle, C_INT64_T
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
    integer(C_INT64_T) :: s
  end function RSF_Record_allocated

! get max payload space in record
  function RSF_Record_max_space(rh) result(s) bind(C,name='RSF_Record_max_space')
    import :: RSF_record_handle, C_INT64_T
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
    integer(C_INT64_T) :: s
  end function RSF_Record_max_space

! get pointer to payload in record
  function RSF_Record_data(rh) result(p) bind(C,name='RSF_Record_data')
    import :: RSF_record_handle, C_PTR
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
    type(C_PTR) :: p
  end function RSF_Record_data

! get used payload space in record
  function RSF_Record_data_size(rh) result(s) bind(C,name='RSF_Record_data_size')
    import :: RSF_record_handle, C_INT64_T
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
    integer(C_INT64_T) :: s
  end function RSF_Record_data_size

! get pointer to metadata in record
  function RSF_Record_meta(rh) result(p) bind(C,name='RSF_Record_meta')
    import :: RSF_record_handle, C_PTR
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
    type(C_PTR) :: p
  end function RSF_Record_meta

! get metadata size in record
  function RSF_Record_meta_size(rh) result(s) bind(C,name='RSF_Record_meta_size')
    import :: RSF_record_handle, C_INT32_T
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
    integer(C_INT32_T) :: s
  end function RSF_Record_meta_size

! does this look like a valid record ?
  function RSF_Valid_record_handle(rh) result(s) bind(C,name='RSF_Valid_record')
    import :: RSF_record_handle, C_INT32_T
    implicit none
    type(RSF_record_handle), intent(IN), value :: rh
    integer(C_INT32_T) :: s
  end function RSF_Valid_record_handle
#else
  // create pointer to a new allocated record (C)
  RSF_record *RSF_New_record(RSF_handle h, int32_t rec_meta, int32_t dir_meta, size_t max_data, void *t, int64_t szt) ;
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
#endif

#if defined(IN_FORTRAN_CODE)
#else
int64_t RSF_Used_space(RSF_handle h) ;
int64_t RSF_Available_space(RSF_handle h) ;
uint64_t RSF_Put_null_record(RSF_handle h, size_t record_size) ;
int32_t RSF_Key32(int64_t key64) ;
int64_t RSF_Key64(int32_t key32) ;
uint32_t RSF_Key64_to_file_slot(int64_t key64) ;
int32_t RSF_Key32_type(int32_t key32) ;
int32_t RSF_File_slot(RSF_handle h) ;
RSF_handle RSF_Key32_to_handle(int32_t key32) ;
#endif

#if defined(IN_FORTRAN_CODE)
end interface
#endif

#endif

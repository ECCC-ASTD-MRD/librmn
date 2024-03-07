# Table of Contents
1. [Introduction](#introduction)
2. [Examples](#examples)
    1. [Opening and closing](#ex-open-close)
    2. [Searching and reading](#ex-search-read)
    3. [Writing](#ex-write)
3. [API](#api)
   1. [C](#c)
      1. [Structs](#c-structs)
      2. [File Functions](#c-file-functions)
      3. [Record Functions](#c-record-functions)
   2. [Fortran](#fortran)
      1. [Structs](#fortran-structs)
      2. [File Functions](#fortran-file-functions)
      3. [Record Functions](#fortran-record-functions)
   3. [Named datatypes](#named-datatypes)
   
# Introduction
* A new backend called RSF (Random Segment Files) has been implemented for standard files (FSTD). It can be used through the [old interface](#ols-interface), while removing some of the limitations related to the previous backend (XDF). 
* [A new interface](#new-interface) called FST24 (in contrast to the old FST98) has also been implemented. It provides a more modern way to access and manipulate standard files, whether they are in the RSF or XDF format.
* New [extended metadata](../Meta/README.md) is now available, but note that you will have to switch to the new API to use it

## New features of RSF
* RSF are implemented on the concept of sparse files
* The size limits on files and records have been removed (more exactly pushed to the exabyte realm)
* RSF files can be concatenated and still be a valid RSF file (ie: `cat file1.rsf >> file2.rsf`)
* [Parallel write](#parallel-write) by multiple processes into the same file
* RSF files may be used as containers for other files

## Upcoming features of RSF
* New compression schemes
* Sub tile reading of larger records
* Multiple no data values

## New interface - FST24

* In the `fst24` interface, Standard Files are manipulated trough two derived types (or structs, in C)
   * `fst_file` is an opaque handle to a file, and allows for file operations like opening, searching, reading and writing.
   * `fst_record` encapsulates all attributes of a record, as well as its data. It represents an item that is written to or read from a file.
* Datatypes are specified through 2 parameters: type (real, integer, etc.) and size (in number of bits). This avoids the need to specify size separately before a read/write operation.

### Searching
Most attributes of `fst_record` are considered _directory metadata_ and are searchable (for both `RSF` and `XDF`). Search criteria are specified differently
depending on whether you are using the Fortran or the C interface.
Criteria are set on a file before performing the search. [Examples are available below](#ex-search-read).
* In Fortran: Search parameters are specified directly (and individually) as function parameters. Any unspecified parameter will be ignored during the search.
* In C: Search parameters are specified as a set, represented by an `fst_record` instance. Any attribute of `fst_record` that is left at its default value will be ignored during the search.

<a id="parallel-write"></a>

## Parallel write

Several processes can open the same RSF file and write to it simultaneously. This can be done with either the `fst24` or the `fst98` interface,
 by adding `PARALLEL` to the options when opening the RSF file. There are a few things to be aware of:

* Parallel write is only available for RSF-type files, so if the file being opened is new, it must have `RSF` in addition to `PARALLEL` (or the `FST_OPTIONS` environment variable must contain `BACKEND=RSF`)
    | Fortran | C |
    |----------|---|
    | `my_file % open('file_name.fst', 'RSF+PARALLEL')` | `my_file = fst24_open("file_name.fst", "RSF+PARALLEL");` |
    | `fstouv(iun, 'RSF+PARALLEL')`                     | `c_fstouv(iun, "RSF+PARALLEL");` |
* If a file is already open in read-only mode (`R/O` or no option) or in exclusive-write mode (`R/W` without `PARALLEL`), it has to be closed first before it can be open for parallel write.
* Each process that opens a file for parallel write reserves a _segment_ of a given size, *which will take that much space on disk regardless of how much data that process writes to the file*. The size of the segments should be chosen so as to minimize the amount of unused space.
    * Desired segment size is controlled by `SEGMENT_SIZE_MB` within `FST_OPTIONS`. It takes an integer, and the units are megabytes (MB). For example:
        ```bash
        export FST_OPTIONS="BACKEND=RSF;SEGMENT_SIZE_MB=4"
        ```
    * When a process writes a record, it goes into the segment
    * If a segment is full or too small to hold a record, the segment is closed and committed to disk, and a new one is opened
    * New segments have the largest of either `SEGMENT_SIZE_MB` or the size of the record being written
    * When a segment is committed to the file, any unfilled space in it will also be written to disk. This means the file will take more space on disk than just its data content.

<a id="old-interface"></a>

## Old Interface - FST98

* The old `fst98` API is still supported and can manage the new RSF backend. In Fortran, a new module has been created and we recommend its use:
```Fortran
use rmn_fst98
```
* To create a file in the RSF format, you can specify it during the opening call: 
```Fortran
fstouv(unit_num, 'RND+RSF')
```
* You can also export the BACKEND option through the FST_OPTIONS environment variable and set it to RSF so that new standard files will automatically use the RSF format
```Bash
export FST_OPTIONS="BACKEND=RSF"
```

# Examples

<a id="ex-open-close"></a>
## Opening and closing a file

<table><tr><td style="width:50%">

 **Fortran** </td><td style="width:50%">
 **C**</td></tr>
<tr>
<td>

```fortran
program fst24_interface
    use rmn_fst24
    implicit none

    type(fst_file) :: my_file
    logical :: success
  
    success = my_file % open('my_file.fst')
  
    if (.not. success) then
        ! Deal with error
    end if
  
    success = my_file % close()

end program
```
</td><td>

```c
#include <rmn.h>

int main(void)
{



    fst_file* my_file = fst24_open("my_file.fst", NULL);
  
    if (my_file == NULL) {
        // Deal with error
    }
  
    fst24_close(my_file);
    free(my_file);
}
```
</td>
</tr>
<tr><td>

```fortran
! Open specifically as RSF
success = my_file % open('my_file.fst', 'RSF')

! Open for parallel write
success = my_file % open('my_file.fst', 'RSF+PARALLEL')
```
</td><td>

```c
// Open specifically as RSF
my_file = fst24_open("my_file.fst", "RSF");

// Open for parallel write
my_file = fst24_open("my_file.fst", "RSF+PARALLEL");
```
</td></tr>
</table>

<a id="ex-search-read"></a>
## Finding and reading a record

<table><tr><td style="width:50%">

**Fortran**

</td><td style="width:50%">

**C**

</td></tr>
<tr><td>

```fortran
use rmn_fst24

type(fst_file)   :: my_file
type(fst_record) :: my_record
type(fst_record), dimension(100) :: many_records
logical :: success
integer :: num_records, i

success = my_file % open('my_file.fst')

!-----------------------------
! Looking for a single record and reading its data
if (my_file % find_next(my_record)) then ! First record
    success = my_record % read()   ! Read data from disk

    if (my_record % ip1 > 10) then
        print *, record % data
    end if
end if

!-----------------------------
! Searching in a loop


success = my_file %                                    &
    set_search_criteria(nomvar = 'ABC', ip3 = 25)
do while (my_file % find_next(my_record))
    ! Do stuff with this record
end do

!-----------------------------
! Reading in a loop


success = my_file %                                    &
    set_search_criteria(ip1 = 200, ig2 = 2)
do while (my_file % read_next(my_record))
    ! Do stuff with the data
end do

!-----------------------------
! Find several records at once
success = my_file % set_search_criteria(typvar = 'P')

! Find up to [size(many_records)] records
num_records = my_file % find_all(many_records)

do i = 1, num_records
    success = many_records(i) % read()
    ! Do stuff with the content
end do


success = my_file % close()
```
</td><td>

```c
#include <rmn.h>

fst_record result;
fst_record criteria;




fst_file * my_file = fst24_open("my_file.fst", NULL);

//----------------------------
// Looking for a single record and reading its data
if (fst24_find_next(my_file, &result)) { // First record
    fst24_read(&result);    // Read data from disk
    float* data = (float*)result.data;
    for (int i = 0; i < result.ni; i++) {
        printf("data %d = %f\n", i, data[i]);
    }
}

//----------------------------
// Searching in a loop
criteria = default_fst_record; // Wildcard for all param
strcpy(criteria.nomvar, "ABC");
criteria.ip3 = 25;
fst24_set_search_criteria(my_file, &criteria);
while (fst24_find_next(my_file, &result)) {
    // Do stuff with the record
}

//----------------------------
// Reading in a loop
criteria = default_fst_record; // Wildcard for all param
criteria.ip1 = 200;
criteria.ig2 = 2;
fst24_set_search_criteria(my_file, &criteria);
while (fst24_read_next(my_file, &result)) {
    // Do stuff with the data
}

//----------------------------
// Find several records at once
strcpy(criteria.typvar, "P"); // Add 1 criteria
fst_record many_records[100];
// Find up to 100 records
const int num_records = fst24_find_all(
        my_file, many_records, 100);
for (int i = 0; i < num_records; i++) {
    fst24_read(many_records[i]);
    // Do stuff with the content
}

fst24_close(my_file);
free(my_file);
```
</td>
</table>

<a id="ex-write"></a>

## Creating and writing a record

<table><tr><td style="width:50%">

**Fortran**

</td><td style="width:50%">

**C**

</td></tr>
<tr><td>

```fortran
use fst24

type(fst_file)   :: my_file
type(fst_record) :: my_record
logical :: success

real, dimension(100, 200), target :: my_data

! Initialize data
[...]

success = my_file % open('my_file.fst', 'RSF')

my_record % data => c_loc(my_data)
my_record % ni = 100
my_record % nj = 200
my_record % nk = 1
my_record % datyp = FST_TYPE_REAL
my_record % dasiz = 32
my_record % npak  = -32
[...]

success = my_file % write(my_record)

success = my_file % close()
```
</td><td>

```c
#include <rmn.h>

fst_record my_record;



float my_data[100][200];

// Initialize data
[...]

fst_file* my_file = fst24_open("my_file.fst", "RSF");

my_record.data = my_data;
my_record.ni = 100;
my_record.nj = 200;
my_record.nk = 1;
my_record.datyp = FST_TYPE_REAL;
my_record.dasiz = 32;
my_record.npak  = -32;
[...]

fst24_write(my_file, &my_record, 0);
fst24_close(my_file);
free(my_file);
```
</td></tr></table>

# API

## C
<a id="c-struct"></a>
### Structs
```C
typedef struct {
    int32_t       iun;          //!< File unit, used by fnom
    int32_t       file_index;   //!< File index in list of open files (the list is different for RSF vs XDF)
    fst_file_type type;         //!< Type of file (RSF, XDF, etc.)
    struct fst24_file_* next;   //!< Next file in linked list of files (if any)
} fst_file;

typedef struct {
    int64_t version;

    // 64-bit elements first
    fst_file* file;   //!< FST file where the record is stored
    void*   data;     //!< Record data
    void*   metadata; //!< Record metadata
    int64_t flags;    //!< Record status flags
    int64_t dateo;    //!< Origin Date timestamp
    int64_t datev;    //!< Valid Date timestamp
    int64_t handle;   //!< Handle to specific record (if stored in a file)

    // 32-bit elements
    int32_t datyp; //!< Data type of elements
    int32_t dasiz; //!< Number of bits per elements
    int32_t npak;  //!< Compression factor (none if 0 or 1). Number of bit if negative
    int32_t ni;    //!< First dimension of the data field (number of elements)
    int32_t nj;    //!< Second dimension of the data field (number of elements)
    int32_t nk;    //!< Thierd dimension of the data field (number of elements)
    int64_t alloc; //!< Size of allocated memory for data

    int32_t deet; //!< Length of the time steps in seconds (deet)
    int32_t npas; //!< Time step number

    int32_t ip1;  //!< Vertical level
    int32_t ip2;  //!< Forecast hour
    int32_t ip3;  //!< User defined identifier

    int32_t ig1;  //!< First grid descriptor
    int32_t ig2;  //!< Second grid descriptor
    int32_t ig3;  //!< Third grid descriptor
    int32_t ig4;  //!< Fourth grid descriptor

    int32_t dummy; // To make explicit the fact that the strings start at a 64-bit boundary

    char typvar[ALIGN_TO_4(FST_TYPVAR_LEN + 1)]; //!< Type of field (forecast, analysis, climatology)
    char grtyp [ALIGN_TO_4(FST_GTYP_LEN + 1)];   //!< Type of geographical projection
    char nomvar[ALIGN_TO_4(FST_NOMVAR_LEN + 1)]; //!< Variable name
    char etiket[ALIGN_TO_4(FST_ETIKET_LEN + 1)]; //!< Label
} fst_record;

typedef struct {
    int32_t dateo, datev, datestamps;
    int32_t level;
    int32_t datyp, npak, ni, nj, nk;
    int32_t deet, npas;
    int32_t ip1, ip2, ip3, decoded_ip;
    int32_t grid_info, ig1234;
    int32_t typvar, nomvar, etiket;
} fst_record_fields;
```

<a id="c-file-functions"></a>
### File Functions
```C
//! Check whether the given file path is a readable FST file
//! \return TRUE (1) if the file makes sense, FALSE (0) if an error is detected
int32_t   fst24_is_valid(
    const char* file_name //!< [in] Path of the file to open
); 

//! Verify that the file pointer is valid and the file is open
//! \return 1 if the pointer is valid and the file is open, 0 otherwise
int32_t   fst24_is_open(
    const fst_file* file  //!< [in] File pointer
);

//! Open a standard file (FST). Will create it if it does not already exist
//! \return A handle to the opened file. NULL if there was an error
fst_file* fst24_open(
    const char* file_name,  //!< [in] Path of the file to open
    const char* options     //!< [in] A list of options, as a string, with each pair of options separated by a comma or a '+'
);

//! Close the given standard file
//! \return 0 if no error, a negative number otherwise
int32_t   fst24_close(
    fst_file* file   //!< [in] Path of the file to open
);

//! Get the number of records in a standard file
//! \return Number of records
int64_t fst24_get_num_records(
    const fst_file* file   //!< [in] file pointer
);
    
//! Read data from file, for a given record
//! \return TRUE (1) if no error, FALSE (0) if an error is detected
int32_t fst24_read(
    fst_record* record //!< [in,out] Record for which we want to read data. Must have a valid handle!
);

//! Read only meta for the given record
void* fst24_read_metadata(
    fst_record* record //!< [in,out] Record for which we want to read metadata. Must have a valid handle!
);

//! Read the next record (data and all) that corresponds to the previously-set search criteria
//! Search through linked files, if any
//! \return TRUE (1) if able to read a record, FALSE (0) or a negative number otherwise (not found or error)
int32_t fst24_read_next(
    fst_file* file,     //!< Handle to open file we want to search
    fst_record* record  //!< [out] Record content and info, if found
);

//! Write the given record into the given standard file
//! \return 0 if everything was a success, a negative error code otherwise
int32_t fst24_write(
    fst_file* file,            //!< [in] file pointer
    const fst_record* record,  //!< [in] Record to be written
    int rewrit                 //!< [in] Rewrite flag
);

//! Indicate a set of criteria that will be used whenever we will use "find next record" 
//! for the given file, within the FST 24 implementation.
//! If for some reason the user also makes calls to the old interface (FST 98) for the
//! same file (they should NOT), these criteria will be used if the file is RSF, but not with the
//! XDF backend.
//! \return TRUE (1) if the inputs are valid (open file, OK criteria struct), FALSE (0) or a negative number otherwise
int32_t fst24_set_search_criteria(
    fst_file* file,              //!< [in] file pointer
    const fst_record* criteria   //!< [in] record with metadata values to search for
);

//! Find the next record in a file that matches the given criteria
//! \return TRUE if a record was found, FALSE otherwise (not found, file not open, etc.)
int32_t fst24_find(
    fst_file* file,             //!< [in] File in which we are looking. Must be open
    const fst_record* criteria, //!< [in] Search criteria
    fst_record* result          //!< [out] First record in the file that matches the criteria
);

//! Find the next record in the given file that matches the previously set
//! criteria (either with a call to fst24_set_search_criteria or a search with explicit
//! criteria)
//! \return TRUE if a record was found, FALSE otherwise (not found, file not open, etc.)
int32_t fst24_find_next(
    fst_file* file,     //!< [in\ File we are searching. Must be open
    fst_record* result  //!< [out] Record information if found (no data or advanced metadata, unless included in search)
);

//! Find all record that match the criteria specified with fst24_set_search_criteria
//! \return Number of records found, 0 if none or if error.
int32_t fst24_find_all(
    fst_file* file,               //!< [in] File to search
    fst_record* results,          //!< [in,out] List of records found. Must be already allocated
    const int32_t max_num_results //!< [in] Size of the given list of records. We will stop looking if we find that many
);

//! Link the given list of files together, so that they are treated as one for the purpose
//! of searching and reading. Once linked, the user can use the first file in the list
//! as a replacement for all the given files.
//! \return TRUE (1) if files were linked, FALSE (0) or a negative number otherwise
int32_t fst24_link(
    fst_file** file,         //!< [in] List of file pointer
    const int32_t num_files  //!< [in] number of file pointer in the list
);   

//! Unlink the given file(s). The files are assumed to have been linked by
//! a previous call to fst24_link, so only the first one should be given as input
//! \return TRUE (1) if unlinking was successful, FALSE (0) or a negative number otherwise
int32_t fst24_unlink(
    fst_file* file     //!< [in] File to unlink
); 

//! Print a summary of the records found in the given file (including any linked files)
//! \return a negative number if there was an error, TRUE (1) if all was OK
int32_t fst24_print_summary(
    fst_file* file,                     //!< Handle to an open file
    const fst_record_fields* fields     //!< [optional] What fields we want to see printed
);
```

<a id="c-record-functions"></a>
### Record Functions
```C
//! Creates a new record and assign the data pointer or allocate data memory
//! \return new record
fst_record fst24_record_new(
    void   *data,   //!< Data pointer to assign, or allocate internal array if NULL
    int32_t type,   //!< Data type
    int32_t nbits,  //!< Number of bits per data element
    int32_t ni,     //!< I horizontal size
    int32_t nj,     //!< J horizontal size
    int32_t nk      //!< K vertical size
);

//! Free a record
//! \return TRUE (1) if no error, FALSE (0) if an error is detected
int32_t fst24_record_free(
    fst_record* record      //!< [in] record pointer
);
```

## Fortran
<a id="fortran-structs"></a>
### Structs

```fortran
type :: fst_file
contains
    procedure, nopass :: is_valid
    procedure, pass   :: is_open
    procedure, pass   :: open
    procedure, pass   :: close
    procedure, pass   :: get_num_records
    procedure, pass   :: get_unit

    procedure, pass :: set_search_criteria
    procedure, pass :: find_next
    procedure, pass :: find_all 
    procedure, pass :: read_next

    procedure, pass :: write

    procedure, pass :: checkpoint
    procedure, pass :: print_summary
    procedure, pass :: unlink

    ! Sequential files
    procedure, pass :: eof
    procedure, pass :: weo
    procedure, pass :: rewind
end type fst_file


!> Representation of an FST record. It allows to get and set basic information about the record and its data,
!> and to easily read, write, search a file
!>
!> It contains a (private) copy of itself that is compatible with the C interface so that the C functions can
!> be called directly on it. Whenever a call to a C function occurs, the attributes of this type are synchronized with the
!> underlying C version of the record.
type, public :: fst_record
    type(C_PTR) :: data     = C_NULL_PTR    !< Pointer to the data
    type(C_PTR) :: metadata = C_NULL_PTR    !< Pointer to the metadata

    integer(C_INT64_T) :: dateo    = -1 !< Origin Date timestamp
    integer(C_INT64_T) :: datev    = -1 !< Valid Date timestamp

    integer(C_INT32_T) :: datyp = -1    !< Data type of elements
    integer(C_INT32_T) :: dasiz = -1    !< Number of bits per elements
    integer(C_INT32_T) :: npak  = -1    !< Compression factor (none if 0 or 1). Number of bit if negative
    integer(C_INT32_T) :: ni    = -1    !< First dimension of the data field (number of elements)
    integer(C_INT32_T) :: nj    = -1    !< Second dimension of the data field (number of elements)
    integer(C_INT32_T) :: nk    = -1    !< Thierd dimension of the data field (number of elements)

    integer(C_INT32_T) :: deet  = -1    !< Length of the time steps in seconds (deet)
    integer(C_INT32_T) :: npas  = -1    !< Time step number

    integer(C_INT32_T) :: ip1   = -1    !< Vertical level
    integer(C_INT32_T) :: ip2   = -1    !< Forecast hour
    integer(C_INT32_T) :: ip3   = -1    !< User defined identifier

    integer(C_INT32_T) :: ig1   = -1    !< First grid descriptor
    integer(C_INT32_T) :: ig2   = -1    !< Second grid descriptor
    integer(C_INT32_T) :: ig3   = -1    !< Third grid descriptor
    integer(C_INT32_T) :: ig4   = -1    !< Fourth grid descriptor

    character(len=2)  :: typvar = ''    !< Type of field (forecast, analysis, climatology)
    character(len=1)  :: grtyp  = ''    !< Type of geographical projection
    character(len=4)  :: nomvar = ''    !< Variable name
    character(len=12) :: etiket = ''    !< Label
contains
    procedure, pass :: has_same_info
    procedure, pass :: read
    procedure, pass :: read_metadata

    procedure, pass :: print
    procedure, pass :: print_short
end type fst_record
```

<a id="fortran-file-functions"></a>
### File Functions

```fortran
!> Check whether the file at the given path is a valid standard file
!> \return .true. if the given path is a valid standard file, .false. otherwise
function is_valid(filename) result(is_valid)
    implicit none
    character(len=*), intent(in) :: filename
    logical :: is_valid
end function is_valid

!> Check whether this file is open
function is_open(this) result(is_open)
    implicit none
    class(fst_file), intent(in) :: this !< fst24_file instance
    logical :: is_open !< Whether this file is open
end function is_open

!> Open a standard file (FST). Will create it if it does not already exist
function open(this, filename, options) result(could_open)
    class(fst_file),intent(inout)        :: this     !< fst_file instance. Must not be an already-open file
    character(len=*), intent(in)           :: filename !< Name of the file we want to open
    character(len=*), intent(in), optional :: options  !< Additional options to pass

    logical :: could_open  !< Whether we were able to open the file
end function open

!> Close the given standard file
function close(this) result(could_close)
    implicit none
    class(fst_file), intent(inout) :: this  !< fst_file instance we want to close
    logical :: could_close                  !< Whether we were actually able to close it
end function close

!> Get number of record in file (including linked files). 0 if file is invalid or not open.
function get_num_records(this) result(num_records)
    implicit none
    class(fst_file), intent(in) :: this
    integer(C_INT64_T) :: num_records
end function get_num_records

!> Get unit of the file if open, 0 otherwise
function get_unit(this) result(status)
    implicit none
    class(fst_file), intent(inout) :: this
    integer(C_INT32_T) :: status
end function get_unit

!> Indicate a set of criteria that will be used whenever we use "find next record" 
!> for the given file, within the FST 24 implementation.
!> If for some reason the user also makes calls to the old interface (FST 98) for the
!> same file (they should NOT), these criteria will be used if the file is RSF, but not with the
!> XDF backend.
!> Return .true. if we were able to set the criteria, .false. if file was not open (or other error)
function set_search_criteria(this,                                                                   &
        dateo, datev, datyp, dasiz, npak, ni, nj, nk,                                                &
        deet, npas, ip1, ip2, ip3, ig1, ig2, ig3, ig4, typvar, grtyp, nomvar, etiket) result(success)
    implicit none
    class(fst_file), intent(inout) :: this
    integer(C_INT64_T), intent(in), optional :: dateo, datev
    integer(C_INT32_T), intent(in), optional :: datyp, dasiz, npak, ni, nj, nk
    integer(C_INT32_T), intent(in), optional :: deet, npas, ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=2),  intent(in), optional :: typvar
    character(len=1),  intent(in), optional :: grtyp
    character(len=4),  intent(in), optional :: nomvar
    character(len=12), intent(in), optional :: etiket
    logical :: success
end function set_search_criteria

!> Find the next record in the given file that matches the previously set
!> criteria (either with a call to fst24_set_search_criteria or a search with explicit
!> criteria)
!> Search through linked files, if any.
!> Return .true. if we found a record, .false. if not or if error
function find_next(this, record) result(found)
    implicit none
    class(fst_file), intent(in) :: this                 !< File we are searching
    type(fst_record), intent(inout), optional :: record !< Information of the record found. Left unchanged if nothing found
    type(C_PTR) :: c_record
    logical :: found
end function find_next

!> Find all record that match the criteria specified with fst24_set_search_criteria.
!> Search through linked files, if any.
!> Return Number of records found, up to size(records)
function find_all(this, records) result(num_found)
    implicit none
    class(fst_file), intent(in) :: this     !< File we are searching. Must be open
    !> [in,out] Array where the records found will be put.
    !> We stop searching after we found enough records to fill it.
    type(fst_record), dimension(:), intent(inout) :: records
    integer(C_INT32_T) :: num_found
end function find_all

!> Read the next record (data and all) that corresponds to the previously-set search criteria
!> Search through linked files, if any
!> Return .true. if we read a record, .false. if none found or if error
function read_next(this, record) result(found)
    implicit none
    class(fst_file), intent(in) :: this         !< File to search
    type(fst_record), intent(inout) :: record   !< Record that was read (left unchanged if nothing was found)
    logical :: found
end function read_next

!> Write the given record into the given standard file
!> Return Whether the write was successful
function write(this, record, rewrite) result(success)
    implicit none
    class(fst_file),  intent(inout) :: this     !< File where we want to write
    type(fst_record), intent(inout) :: record   !< Record we want to write
    logical, intent(in), optional     :: rewrite!< Whether we want to rewrite an existing record (default .false.)
    logical :: success
end function write

!> Perform a checkpoint on the given file:
!> commit (meta)data to disk if the file has changed in memory
!> Return Whether the underlying call was successful
function checkpoint(this) result(success)
    implicit none
    class(fst_file), intent(inout) :: this
    logical :: success
end function checkpoint

!> Print a summary of the records found in the given file (including any linked files)
!> All optional parameters are booleans determining whether we print the corresponding field.
subroutine print_summary(this,                                                                       &
        dateo, datev, datestamps, level, datyp, ni, nj, nk,                                          &
        deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234, typvar, nomvar, etiket)
    implicit none
    class(fst_file), intent(in) :: this
    logical, intent(in), optional :: dateo, datev, datestamps, level, datyp, ni, nj, nk
    logical, intent(in), optional :: deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234
    logical, intent(in), optional :: typvar, nomvar, etiket
end subroutine print_summary

!> Link the given files so that they can be searched and read as one.
!> Return Whether the linking was successful
function fst24_link(files) result(success)
    implicit none
    type(fst_file), dimension(:), intent(inout) :: files
    logical :: success
end function fst24_link

!> Unlink files that are linked into the given file
!> Return Whether the unlinking was successful
function unlink(this) result(success)
    implicit none
    class(fst_file), intent(inout) :: this !< File to unlink. Must be the first in the list when linking occurred
    logical :: success
end function unlink

!> Get the level of end of file for the sequential file
!> Only works with sequential files
function eof(this) result(status)
    implicit none
    class(fst_file), intent(inout) :: this
    integer(C_INT32_T) :: status
end function eof

!> Write a logical end of file on a sequential file
!> Only works with sequential files
function weo(this,level) result(status)
    implicit none
    class(fst_file), intent(inout) :: this
    integer, intent(in) :: level 
    integer(C_INT32_T) :: status
end function weo

!> Rewinds a RPN standard sequential file
!> Only works with sequential files
function rwd(this) result(status)
    implicit none
    class(fst_file), intent(inout) :: this
    integer(C_INT32_T) :: status
end function rwd
```

<a id="fortran-record-functions"></a>

### Record Functions

```fortran
!> Check whether two records have identical information (except data). This will sync the underlying C struct
!> \return .true. if the two records have the same information (not data/metadata), .false. otherwise
function fst24_record_has_same_info(this, other) result(has_same_info)
    implicit none
    class(fst_record), intent(inout) :: this
    type(fst_record),  intent(inout) :: other
    logical :: has_same_info
end function fst24_record_has_same_info

!> Read the data and metadata of a given record from its corresponding file
!> Return Whether we were able to do the reading
function read(this) result(success)
    implicit none
    class(fst_record), intent(inout) :: this  !< fst_record instance. If must be a valid record already found in a file
    logical :: success
end function read

!> Read only metadata for the given record
!> Return .true. If we were able to read the metadata, .false. otherwise
function read_metadata(this) result(success)
    implicit none
    class(fst_record), intent(inout) :: this !< fst_record instance. If must be a valid record already found in a file
    logical :: success
end function read_metadata

!> Print all members of the given record struct
!> Causes an update of the underlying C struct
subroutine print(this)
    implicit none
    class(fst_record), intent(inout) :: this !< fst_record instance we want to print
end subroutine print

!> Print record information on one line (with an optional header)
!> Causes an update of the underlying C struct
!> Refer to fst_record_fields for the meaning of undocumented parameters
subroutine print_short(                                                                        &
        this, prefix, print_header, dateo, datev, datestamps, level, datyp, ni, nj, nk,        &
        deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234, typvar, nomvar, etiket)
    implicit none
    class(fst_record), intent(inout) :: this !< fst_record instance whose information we want to print
    character(len=*), intent(in), optional :: prefix !< [optional] Text we want to add at the start of the line
    logical, intent(in), optional :: print_header !< [optional] Whether we want to print a header above the line to name the fields
    logical, intent(in), optional :: dateo, datev, datestamps, level, datyp, ni, nj, nk
    logical, intent(in), optional :: deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234
    logical, intent(in), optional :: typvar, nomvar, etiket
end subroutine short
```

## Named datatypes

These are also available in Fortran
```C
static const int32_t FST_TYPE_BINARY    = 0;
static const int32_t FST_TYPE_FREAL     = 1;
static const int32_t FST_TYPE_UNSIGNED  = 2; //!< Unsigned integer
static const int32_t FST_TYPE_FCHAR     = 3; //!< Characters (not compressed)
static const int32_t FST_TYPE_SIGNED    = 4; //!< Signed integer
static const int32_t FST_TYPE_REAL      = 5; //!< Real number (32 or 64 bits)
static const int32_t FST_TYPE_IEEE_16   = 6;
static const int32_t FST_TYPE_STRING    = 7; //!< Characters (compressed)
static const int32_t FST_TYPE_COMPLEX   = 8; //!< Complex number (32 or 64 bits)

static const int32_t FST_TYPE_TURBOPACK = 128;
```

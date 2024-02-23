# Table of Contents
1. [Introduction](#introduction)
2. [API](#api)
   1. [C](#c)
      1. [Struct](#c-structs)
      2. [File Functions](#c-file-functions)
      3. [Record Functions](#c-record-functions)
   2. [Fortran](#fortran)
      1. [Struct](#fortran-structs)
      2. [File Functions](#fortran-file-functions)
      3. [Record Functions](#fortran-record-functions)
   
# Introduction
* A new backend called RSF (Random Segment Files) has been implemented for standard files (FSTD). It can use the old interface, while removing some of the limitations related to the previous backend (XDF), but we also now provide a [new modern interface](#new-interface). 
* New [extended metadata](../Meta/README.md) is now available, but note that you will have to switch to the new API to use it

## New features of RSF
* RSF are implemented on the concept of sparse files
* The size limit of file and records has been removed (more exactly pushed to the exabyte realm)
* RSF can file can be concatenated and still be a valid RSF file (ie: cat file1.rsf >> file2.rsf)
* Parallel write by multiple processes into the same file
* RSF files may be used as containers for other files

## Upcomming features of RSF
* New compression schemes
* Sub tile reading of larger records
* Multiple no data values

## Old Interface 
* The old API is still supported and can manage the new RSF backend. A new module has been created and we recommend its use:
```Fortran
use rmn_fstd98
```
* To create a file in the RSF format, you can specify it during the opening call: 
```Fortran
fstouv(unit_num, 'RND+RSF')
```
* You can also export the BACKEND option through the FST_OPTIONS environment variable and set it to RSF so that new standard files will automatically use the RSF format
```Bash
export FST_OPTIONS="BACKEND=RSF"
```

## New interface
*

# API

## C
<a id="c-struct"></a>
### Struct
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
    int32_t datyp;//!< Data type of elements
    int32_t dasiz;//!< Number of bits per elements
    int32_t npak; //!< Compression factor (none if 0 or 1). Number of bit if negative
    int32_t ni;   //!< First dimension of the data field (number of elements)
    int32_t nj;   //!< Second dimension of the data field (number of elements)
    int32_t nk;   //!< Thierd dimension of the data field (number of elements)

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

//! Read data from file, for the next record fitting search parameters
//! \return TRUE (1) if no error, FALSE (0) if an error is detected
int32_t fst24_read_next(
    fst_file* file,    //!< [in] file pointer
    fst_record* record //!< [in,out] Record for which we want to read data
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
//! \return TRUE if the inputs are valid (open file, OK criteria struct), FALSE otherwise
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

//! Link multiple files together that will be seen as a singel file by other fst24 functions
//! Passing either file pointer from the link list to search function wil parse the following file in the list
//! \return TRUE (1) if no error, FALSE (0) if an error is detected
int32_t fst24_link(
    fst_file** file,         //!< [in] List of file pointer
    const int32_t num_files  //!< [in] number of file pointer in the list
);   

//! Unlink previously linked files
//! Passing either file pointer from the link will unlink the following file in the list
//! \return TRUE (1) if no error, FALSE (0) if an error is detected
int32_t fst24_unlink(
    fst_file* file     //!< [in] File to unlink
); 

//! Print information on the file's records (as the voir utility does)
//! which metadata information to print can be specified, default is NOMVAR,TYPVAR,ETIKET,NI,NJ,NK,IP1,IP2,IP3,DATEO,STAMP,DATYP,
//! \return TRUE (1) if no error, FALSE (0) if an error is detected
int32_t fst24_print_summary(
    fst_file* file,                   //!< [in] file pointer
    const fst_record_fields* fields   //!< [in] descriptions of metadata fields to display
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
### Struct
<a id="fortran-file-functions"></a>
### File Functions
<a id="fortran-record-functions"></a>
### Record Functions

# Table of Contents
1. [Introduction](#introduction)
    1. [RSF Features](#new-features-of-rsf)
    1. [New Interface: `fst24`](#new-interface-fst24)
    1. [Parallel Write](#parallel-write)
    1. [Data Types](#data-types)
    1. [Old Interface: `fst98`](#old-interface-fst98)
2. [Examples](#examples)
    1. [Opening and Closing](#opening-and-closing-a-file)
    2. [Searching and Reading](#finding-and-reading-a-record)
    3. [Writing](#creating-and-writing-a-record)
3. [API](#api)
   1. [C](#c)
      1. [Structs](#structs)
      2. [File Functions](#file-functions)
      2. [Query Functions](#query-functions)
      3. [Record Functions](#record-functions)
   2. [Fortran](#fortran)
      1. [Structs](#structs-1)
      2. [File Functions](#file-functions-1)
      2. [Query Functions](#query-functions-1)
      3. [Record Functions](#record-functions-1)
   
# Introduction
* A new backend called RSF (Random Segment Files) has been implemented for standard files (FSTD, or FST). It can be used through the [old interface](#old-interface-fst98), while removing some of the limitations related to the previous backend (XDF). 
* [A new interface](#new-interface-fst24) called `fst24` (in contrast to the old `fst98`) has also been implemented. It provides a more modern way to access and manipulate standard files, whether they are in the RSF or XDF format.
* New [extended metadata](../Meta/README.md) is now available, but note that you will have to switch to the new API to use it

## New features of RSF
* RSF are implemented on the concept of sparse files
* The size limits on files and records have been removed (more exactly pushed to the exabyte realm)
* RSF files can be concatenated and still be a valid RSF file (ie: `cat file1.rsf >> file2.rsf`)
* [Parallel write](#parallel-write) by multiple processes into the same file
* RSF files may be used as containers for other files
* (With the `fst24` interface) Reentrant API: the same file object may be read and written by several threads of a process

### Upcoming features of RSF
* New compression schemes
* Sub tile reading of larger records
* Multiple no data values

## New interface: fst24

* In the `fst24` interface, Standard Files are manipulated trough three derived types (or structs, in C)
   * `fst_file` is an opaque handle to a file, and allows for file operations like opening, searching, reading and writing.
   * `fst_record` encapsulates all attributes of a record, as well as its data. It represents an item that is written to or read from a file.
   * `fst_query` represents a search through a file. It contains the search criteria as well as the current position of the search within the file.
* Datatypes are specified through 2 parameters: type (real, integer, etc.) and size (in number of bits). This avoids the need to specify size separately before a read/write operation.

### Searching and Reading
Searches are made through a query (`fst_query`). Several queries can be made concurrently. Each of them retains its own index within the searched file,
so that they can progress in their search, even if another query has been run between different searches.

Most attributes of `fst_record` are considered _directory metadata_ and are searchable (for both `RSF` and `XDF`). Search criteria are specified differently
depending on whether you are using the Fortran or the C interface:

* In **Fortran**: Search parameters are specified directly (and individually) as function parameters. Any unspecified parameter will be ignored during the search.
* In **C**: Search parameters are specified as a set, represented by an `fst_record` instance. Any attribute of `fst_record` that is left at its default value will be ignored during the search.

[Examples are available below](#finding-and-reading-a-record).

### Correspondance with old interface functions

There is not a 1-on-1 correspondance between the functions of the two interfaces but the following table shows a rough equivalence:

| fst98 function         | fst24 (Fortran)             | fst24 (C)                        |
| -----------------      | -------------------         | ---------------------------      |
| `fnom` + `fstouv`      | `file % open`               | `fst24_open`                     |
| `fstecr`               | `file % write(record)`      | `fst24_write(file, record)`      |
| `fstinf` <br> `fstsui` | `query % find_next(record)` | `fst24_find_next(query, record)` |
| `fstinl`               | `query % find_all(records)` | `fst24_find_all(query, records)` |
| `fstlir` <br> `fstlic` <br> `fstlis` | `query % read_next(record)` | `fst24_read_next(query, record)` |
| `fstluk`               | `record % read`             | `fst24_read_record(record)`      |
| `fstvoi`               | `file % print_summary`      | `fst24_print_summary(file)`      |
| `fstfrm`               | `file % close`              | `fst24_close(file)`              |
| `fstnbr`               | `file % get_num_records`    | `fst24_get_num_records(file)`    |
| `fstnbrv`              | [N/A]                       | [N/A]                            |
| `fstcheck`             | `fst_file % is_valid(path)` | `fst24_is_valid(path)`           |
| `fstckp`               | `file % flush`              | `fst24_flush(file)`              |
| `fsteff`               | [Not yet implemented]       | [Not yet implemented]            |
| `fstprm`               | Params are available in derived type | Params are available in struct |
| `fstlnk`               | `fst24_link`                | `fst24_link`                     |
| `fstmsq`               | [N/A]                       | [N/A]                            |
| `fstweo`               | `file % weo`                | [N/A]                            |
| `fsteof`               | `file % eof`                | `fst24_eof(file)`                |
| `fstrwd`               | `file % rwd`                | [N/A]                            |
| `fstskp`               | [N/A]                       | [N/A]                            |
| `fstapp`               | [N/A]                       | [N/A]                            |
| `fstcvt`               | [N/A]                       | [N/A]                            |

## Parallel write

Several processes can open the same RSF file and write to it simultaneously. This can be done with either the `fst24` or the `fst98` interface,
 by adding `PARALLEL` to the options when opening the RSF file. There are a few things to be aware of:

* Parallel write is only available for RSF-type files, so if the file being opened is new, it must have `RSF` in addition to `PARALLEL` (or the `FST_OPTIONS` environment variable must contain `BACKEND=RSF`)

  |       | Fortran | C |
  | ----- | ------- | - |
  | fst24 | `my_file % open('file_name.fst', 'RSF+PARALLEL')` | `my_file = fst24_open("file_name.fst", "RSF+PARALLEL");` |
  | fst98 | `fstouv(iun, 'RSF+PARALLEL')`                     | `c_fstouv(iun, "RSF+PARALLEL");` |

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

## Data Types

We are introducing names for the existing datatypes. These names are available from C and Fortran, for both the `fst98` and `fst24` interfaces.
In addition to the 9 base types, there are 2 flags that can be combined (added) to these types to indicate additional compression (`FST_TYPE_TURBOPACK`)
or the presence of "missing values" (`FSTD_MISSING_FLAG`).

Some combinations of data type, size and compression are not possible. When such a combination is requested, one or more of the data parameters will
be converted to obtain a valid combination. You can see the final parameters used for writing the data to file when the log level is set to `INFO` or
higher (with either `APP_VERBOSE` or `APP_VERBOSE_FST` environment variables; see the documentation for [App](https://gitlab.science.gc.ca/RPN-SI/App) for more information.)

### Real Datatypes

There are 3 different types for storing real-valued data. We recommend the use of `FST_TYPE_REAL` (`F`) everywhere, unless you know and understand exactly
what you want or if your data covers a *wide* range of values (several orders of magnitude).
When storing 32-bit values into 16 bits or less, it uses the 16-bit IEEE format, which usually preserves more information than simply truncating a 32-bit IEEE number.
When storing into 17-24 bits, it reverts to the `FST_TYPE_REAL_OLD_QUANT` (`R`) scheme, *which is not reversible*, meaning that repeatedly compacting and uncompacting the same data may lead to (small) differences.
When storing into 25-32 bits, it reverts to the `FST_TYPE_REAL_IEEE` (`E`) scheme, and will be stored as untruncated 32-bit IEEE floats.

If you really want truncated IEEE floats, you may select it directly with `FST_TYPE_REAL_IEEE` and a desired number of stored bits. The same holds for
truncating `FST_TYPE_REAL_OLD_QUANT` numbers.

For real numbers, turbocompression is only available for 2D data stored in 16 bits or less. 3D data may be disguised as 2D by combining 2 of the dimensions,
but this will affect compression performance (size gains) at the border between levels.

### Type Names

```c
//!> Raw binary data. Its elements can have any size, and it is not subject to interpretation by the FST layer.
//!> Identified with X.
static const int32_t FST_TYPE_BINARY = 0;

//!> Real-valued data using the old quantification scheme.
//!> Identified with R.
//!> This quantification is lossy and not reversible (non-cyclic)
//!> If trying to store with [31-32] bits, automatically converted to FST_TYPE_REAL_IEEE
static const int32_t FST_TYPE_REAL_OLD_QUANT = 1;

//!> Unsigned integer data
static const int32_t FST_TYPE_UNSIGNED = 2;

//!> Characters (not compressed)
static const int32_t FST_TYPE_CHAR = 3;

//!> Signed integer data
static const int32_t FST_TYPE_SIGNED = 4;

//!> Real-valued data using IEEE format (no quantification), in 32 or 64 bits. Identified with E.
//!> When trying to store data with number of bits in the range [33-63], the original data size is
//!> preserved (either 32 or 64 bits).
//!> When trying to store 64-bit (double) data with 32 bits or less, it is first converted to 32-bit IEEE (float)
//!> When trying to store 32-bit (float) data with less than 32 bits, the extra bits are simply truncated from the
//!> mantissa (so don't go too low).
static const int32_t FST_TYPE_REAL_IEEE = 5;

//!> Real-valued data using a new quantification scheme.
//!> *This is the recommended REAL type to use.*
//!> This quantification scheme is lossy, but reversible (cyclic)
//!> Depending on number of bits requested for storage, a conversion may be performed at write-time.
//!>   if > 24 -> use FST_TYPE_REAL_IEEE with 32 bits
//!>   if [17-23] -> use FST_TYPE_REAL_OLD_QUANT with that number of bits
//!>   if < 16 -> quantify to 16, then truncate any extra bit from the new mantissa
static const int32_t FST_TYPE_REAL = 6;

//!> Characters (compressed)
static const int32_t FST_TYPE_STRING = 7;

//!> Complex number (32 or 64 bits)
static const int32_t FST_TYPE_COMPLEX = 8;


/////////////////////
// Additional flags

//!> When added or |'d to a base type, indicate that we want to apply additional lossless compression to the data
static const int32_t FST_TYPE_TURBOPACK = 128;

#define FSTD_MISSING_FLAG 64 //!< When this flag is ON in a datatype, it indicates that some data points are missing
```

## Old Interface: fst98

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

## Opening and Closing a File

The new default when opening a file is *read-only* rather than *read-write*. Therefore, to open a file that does not exist yet, the `R/W` option must be present.

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
  
    success = my_file % open('my_file.fst', options = 'R/W')
  
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

## Finding and Reading a Record

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
type(fst_query)  :: my_query
type(fst_record), dimension(100) :: many_records
logical :: success
integer :: num_records, i

success = my_file % open('my_file.fst')

!-----------------------------
! Looking for a single record and reading its data
my_query = my_file % new_query() ! Match everything

if (my_query % find_next(my_record)) then
    success = my_record % read() ! Read data from disk

    ! Do something with the data
    if (my_record % ip1 > 10) then
        print *, record % data
    end if

end if

! Free the query once you no longer need it
call my_query % free()

!-----------------------------
! Searching in a loop



my_query = my_file % new_query(nomvar = 'AB', ip3 = 25)
do while (my_query % find_next(my_record))
    ! Do stuff with this record
end do
call my_query % free()

!-----------------------------
! Reading in a loop



my_query = my_file % new_query(ip1 = 200, ig2 = 2)
do while (my_query % read_next(my_record))
    ! Do stuff with the data
end do
call my_query % free()

!-----------------------------
! Find several records at once
my_query = my_file % new_query(typvar = 'P')


! Find up to [size(many_records)] records
num_records = my_query % find_all(many_records)

do i = 1, num_records
    success = many_records(i) % read()
    ! Do stuff with the content
end do
call my_query % free()


success = my_file % close()
```
</td><td>

```c
#include <rmn.h>

fst_record result = default_fst_record;
fst_record criteria = default_fst_record;





fst_file * my_file = fst24_open("my_file.fst", NULL);

//----------------------------
// Looking for a single record and reading its data
fst_query * my_query = fst24_new_query(
        my_file, NULL, NULL); // Match everything
if (fst24_find_next(my_query, &result)) {
    fst24_read(&result); // Read data from disk

    // Do something with the data
    float* data = (float*)result.data;
    for (int i = 0; i < result.ni; i++) {
        printf("data %d = %f\n", i, data[i]);
    }
}

// Free the query once you no longer need it
fst24_query_free(my_query);

//----------------------------
// Searching in a loop
criteria = default_fst_record; // Wildcard everywhere
strcpy(criteria.nomvar, "ABC");
criteria.ip3 = 25;
my_query = fst24_new_query(my_file, &criteria, NULL);
while (fst24_find_next(my_file, &result)) {
    // Do stuff with the record
}
fst24_query_free(my_query);

//----------------------------
// Reading in a loop
criteria = default_fst_record; // Wildcard everywhere
criteria.ip1 = 200;
criteria.ig2 = 2;
my_query = fst24_new_query(my_file, &criteria, NULL);
while (fst24_read_next(my_file, &result)) {
    // Do stuff with the data
}
fst24_query_free(my_query);

//----------------------------
// Find several records at once
strcpy(criteria.typvar, "P"); // Add 1 criteria
my_query = fst24_new_query(my_file, &criteria, NULL);
fst_record many_records[100];
// Find up to 100 records
const int num_records =
    fst24_find_all(my_query, many_records, 100);
for (int i = 0; i < num_records; i++) {
    fst24_read(many_records[i]);
    // Do stuff with the content
}
fst24_query_free(my_query);

fst24_close(my_file);
```
</td>
</tr>
</table>

### Working with Several Queries

<table><tr><td style="width:50%">

**Fortran**

</td><td style="width:50%">

**C**

</td></tr>
<tr><td>

```fortran
type(fst_query)  :: q_label_a, q_label_b
type(fst_record) :: rec_a, rec_b

[...]







q_label_a = my_file % new_query(etiket = 'LABEL A')
q_label_b = my_file % new_query(etiket = 'LABEL B')

! For each record found with query A, process 
! 3 records from query B
do while (q_label_a % find_next(rec_a))
    do i = 1, 3
        if (q_label_b % read_next(rec_b))
            ! Process record
        end if
    end do
end do

! Want to do stuff with records with label A again?
call q_label_a % rewind()
do while (q_label_a % read_next(rec_a))
    ! Do stuff
end do

! Wanna find all records with label B again?
! "find_all" rewinds automatically
num_recs = q_label_b % find_all(many_records)

call q_label_a % free()
call q_label_b % free()
```

</td><td>

```c
fst_record rec_a = default_fst_record;
fst_record rec_b = default_fst_record;

[...]

fst_record crit_a = default_fst_record;
fst_record crit_b = default_fst_record;

strcpy(crit_a.etiket, "LABEL A");
strcpy(crit_b.etiket, "LABEL B");

fst_query* q_a = fst24_new_query(my_file, &crit_a, NULL);
fst_query* q_b = fst24_new_query(my_file, &crit_b, NULL);

// For each record found with query A, process 
// 3 records from query B
while (fst24_find_next(q_a, &rec_a)) {
    for (int i = 0; i < 3; i++) {
        if (fst24_read_next(q_b, &rec_b)) {
            // Process record
        }
    }
}

// Want to do stuff with records with label A again?
fst24_rewind(q_a);
while (fst24_read_next(q_a, &rec_a)) {
    // Do stuff
}

// Wanna find all records with label B again?
// "find_all" rewinds automatically
num_recs = fst24_find_all(q_b, many_records, 10000);

fst24_query_free(q_a);
fst24_query_free(q_b);
```

</td></tr>
<tr><td>

```fortran
! "Nested" queries









q_a = my_file % new_query(etiket = 'LA', grtyp = 'X')
do while (q_a % find_next(rec_a))
    ! Look for every record with nomvar "VARB" that
    ! has the same ip1 as record A
    q_b = my_file %                                  &
        new_query(nomvar = 'VARB', ip1 = rec_a % ip1)
    num_rec = q_b % find_all(many_records)
  
    [...]

    call q_b % free()
end do

call q_a % free()
```

</td><td>

```c
// "Nested" queries

crit_a = default_fst_record;
crit_b = default_fst_record;

strcpy(crit_a.etiket, "LA");
strcpy(crit_a.grtyp, "X");

strcpy(crit_b.nomvar, "VARB");

q_a = fst24_new_query(my_file, &crit_a, NULL);
while (fst24_find_next(q_a, &rec_a)) {
    // Look for every record with nomvar "VARB" tha
    // has the same ip1 as record A
    crit_b.ip1 = rec_a.ip1;
    q_b = fst24_new_query(my_file, &crit_b, NULL);
    n_rec = fst24_find_all(q_b, many_records, 10000);

    [...]

    fst24_query_free(q_b);
}

fst24_query_free(q_a);
```

</td></tr>
</table>

### Reading 2D slices into a pre-allocated 3D array

<table><tr><td style="width:50%">

**Fortran**

</td><td style="width:50%">

**C**

</td></tr>
<tr><td>

```fortran
type(fst_file) :: test_file
type(fst_record) :: record
logical :: success

real, dimension(NUM_X, NUM_Y, NUM_LEVEL), target :: data

do k = 1, NUM_LEVEL
    record % data = c_loc(data(1, 1, k))

    success = test_file % read(record, ig1 = k)
end do
```

</td><td>

```c

fst_record rec = default_fst_record;
fst_record criteria = default_fst_record;

float data[NUM_LEVEL][NUM_X][NUM_Y];

for (int i_level = 0; i_level < NUM_LEVEL; i_level++) {
    rec.data = &data[i_level];
    criteria.ig1 = i_level + 1;
    fst24_read(test_file, &criteria, NULL, &rec);
}
```

</td></tr>
<tr><td>

```fortran
! In Fortran, the reading can also be done with a single line
do k = 1, NUM_LEVEL
    success = file % read(record, data = c_loc(data(1, 1, k)), ig1 = k)
end do
```

</td></tr>
</table>

### Obtaining a Fortran pointer to the data

In Fortran, it is possible to obtain a pointer to the record data for easier manipulation
```fortran
type(fst_record) :: my_record
logical :: success
integer,             dimension(:, :),    pointer :: data_i4
real(kind = real64), dimension(:, :, :), pointer :: data_r8

[...]
success = my_record % read()   ! Read data from disk
call my_record % get_data_array(data_i4)    ! Only works if the record contains 2D integer data
print *, data_i4(1:4, :)

[...]
call my_record % get_data_array(data_r8)    ! Only works if the record contains 3D real (double) data
print *, data_r8(3, 2:5, 1)
```


<a id="ex-write"></a>

## Creating and writing a record

The new default when opening a file is *read-only* rather than *read-write*. Therefore, to create a file, the `R/W` option must be present.

<table><tr><td style="width:50%">

**Fortran**

</td><td style="width:50%">

**C**

</td></tr>
<tr><td>

```fortran
program create_and_write
  use rmn_fst24
  implicit none

  
  type(fst_file)   :: my_file
  type(fst_record) :: my_record
  logical :: success
  
  real, dimension(100, 200), target :: my_data
  
  ! Initialize data
  my_data=100.


  success = my_file % open('my_file.fst', 'R/W+RSF')
  ! if (.not. success) error stop 1
  
  my_record % data = c_loc(my_data)
  my_record % ni = 100
  my_record % nj = 200
  my_record % nk = 1
  my_record % data_type = FST_TYPE_REAL
  my_record % data_bits = 32
  my_record % pack_bits = 32

  my_record % deet = 0
  my_record % npas = 0
  my_record % ip1 = 0
  my_record % ip2 = 0
  my_record % ip3 = 0
  my_record % ig1 = 0
  my_record % ig2 = 0
  my_record % ig3 = 0
  my_record % ig4 = 0
  
  success = my_file % write(my_record)
  ! if (.not. success) error stop 1

  success = my_file % close()
  ! if (.not. success) error stop 1
  



end program create_and_write
```
</td><td>

```c
#include <rmn.h>

int main(void) {
  
  float my_data[100][200];
  
  // Initialize data
  for (int i = 0; i < 100; i++)
    for (int j = 0; j < 200; j++)
      my_data[i][j] = 100.0;
  
  fst_file* my_file = 
        fst24_open("my_file.fst", "R/W+RSF");
  if (my_file == NULL)
    return -1;
  
  fst_record my_record = default_fst_record;

  my_record.data = my_data;
  my_record.ni = 100;
  my_record.nj = 200;
  my_record.nk = 1;
  my_record.data_type = FST_TYPE_REAL;
  my_record.data_bits = 32;
  my_record.pack_bits = 32;

  my_record.deet = 0;
  my_record.npas = 0;
  my_record.ip1 = 0;
  my_record.ip2 = 0;
  my_record.ip3 = 0;
  my_record.ig1 = 0;
  my_record.ig2 = 0;
  my_record.ig3 = 0;
  my_record.ig4 = 0;
  
  if (fst24_write(my_file, &my_record, FST_NO) <= 0)
    return -1;
  
  if (fst24_close(my_file) <= 0)
    return -1;
  
  return 0;
}
```
</td></tr>
<tr><td>

```fortran
! With turbocompression

my_record % data_type = FST_TYPE_REAL + FST_TYPE_TURBOPACK
my_record % data_bits = 32
my_record % pack_bits = 16

success = my_file % write(my_record)
```

</td><td>

```c
// With turbocompression

my_record.data_type = FST_TYPE_REAL | FST_TYPE_TURBOPACK;
my_record.data_bits = 32;
my_record.pack_bits = 16;

fst24_write(my_file, &my_record, FST_NO);
```

</td></tr>
</table>

# API

## C

### Structs

```c

typedef struct fst24_file_ fst_file; // Forward declare
typedef struct fst_query_ fst_query; // Forward declare

typedef struct {

    // 64-bit elements first
    const fst_file* file;   //!< FST file where the record is stored
    void*   data;     //!< Record data
    void*   metadata; //!< Record metadata

    int32_t dateo;    //!< Origin Date timestamp
    int32_t datev;    //!< Valid Date timestamp

    // 32-bit elements
    int32_t data_type; //!< Data type of elements. See FST_TYPE_* constants.
    int32_t data_bits; //!< Number of bits per input element
    int32_t pack_bits; //!< Number of stored (compressed) bits per element
    int32_t ni;     //!< First dimension of the data field (number of elements)
    int32_t nj;     //!< Second dimension of the data field (number of elements)
    int32_t nk;     //!< Third dimension of the data field (number of elements)
    int32_t num_meta_bytes; //!< Size of the metadata in bytes

    int32_t deet; //!< Length of the time steps in seconds (deet)
    int32_t npas; //!< Time step number

    int32_t ip1;  //!< Vertical level
    int32_t ip2;  //!< Forecast hour
    int32_t ip3;  //!< User defined identifier

    int32_t ig1;  //!< First grid descriptor
    int32_t ig2;  //!< Second grid descriptor
    int32_t ig3;  //!< Third grid descriptor
    int32_t ig4;  //!< Fourth grid descriptor

    char typvar[ALIGN_TO_4(FST_TYPVAR_LEN + 1)]; //!< Type of field (forecast, analysis, climatology)
    char grtyp [ALIGN_TO_4(FST_GTYP_LEN + 1)];   //!< Type of geographical projection
    char nomvar[ALIGN_TO_4(FST_NOMVAR_LEN + 1)]; //!< Variable name
    char etiket[ALIGN_TO_4(FST_ETIKET_LEN + 1)]; //!< Label

} fst_record;

typedef struct {
    //!> Several encodings can represent the same floating point value stored in an IP. When setting ip1_all
    //!> (and ip2_all, and ip3_all), we indicate that we want to match with any encoding that result in the same
    //!> encoded value in the given criterion. If not set, we will only match with the specific encoding given.
    int32_t ip1_all;
    int32_t ip2_all; //!< When trying to match a certain IP2, match all encodings that result in the same encoded value
    int32_t ip3_all; //!< When trying to match a certain IP3, match all encodings that result in the same encoded value
    int32_t stamp_norun; //!< Date contains a run in the first 3 bits that must not be checked (used in older files)
} fst_query_options;

typedef struct {
    int32_t dateo, datev, datestamps;
    int32_t level;
    int32_t data_type, nijk;
    int32_t deet, npas;
    int32_t ip1, ip2, ip3, decoded_ip;
    int32_t grid_info, ig1234;
    int32_t typvar, nomvar, etiket;
    int32_t metadata;
} fst_record_fields;
```

### File Functions

```c
//! Verify that the file pointer is valid and the file is open
//! \return 1 if the pointer is valid and the file is open, 0 otherwise
int32_t fst24_is_open(const fst_file* const file);

//! \return Pointer to the name of the file, if open. NULL otherwise
const char* fst24_file_name(const fst_file* const file);

//! Test if the given path is a readable FST file
//! \return TRUE (1) if the file makes sense, FALSE (0) if an error is detected
int32_t fst24_is_valid(const char* const filePath);

//! Open a standard file (FST)
//!
//! File will be created if it does not already exist
//! \return A handle to the opened file. NULL if there was an error
fst_file* fst24_open(
    const char* const filePath,  //!< Path of the file to open
    const char* const options     //!< A list of options, as a string, with each pair of options separated by a comma or a '+'
);

//! Close the given standard file and free the memory associated with the struct
//! \todo What happens if closing a linked file?
//! \return TRUE (1) if no error, FALSE (0) or a negative number otherwise
int32_t fst24_close(fst_file* const file);

//! Commit data and metadata to disk if the file has changed in memory
//! \return A negative number if there was an error, 0 or positive otherwise
int32_t fst24_flush(
    const fst_file* const file //!< Handle to the open file we want to checkpoint
);

//! Get the number of records in a file including linked files
//! \return Number of records in the file and in any linked files
int64_t fst24_get_num_records(
    const fst_file* const file    //!< [in] Handle to an open file
);

//! Print a summary of the records found in the given file (including any linked files)
//! \return a negative number if there was an error, TRUE (1) if all was OK
int32_t fst24_print_summary(
    fst_file* const file, //!< [in] Handle to an open file
    const fst_record_fields* const fields //!< [optional] What fields we want to see printed
);

//! Write the given record into the given standard file
//! \return TRUE (1) if everything was a success, a negative error code otherwise
int32_t fst24_write(
    fst_file* file,     //!< The file where we want to write
    fst_record* record, //!< The record we want to write
    const int rewrite   //!< Whether we want to overwrite FST_YES, skip FST_SKIP or write again FST_NO an existing record
);

//! Search a file with given criteria and read the first record that matches these criteria.
//! Search through linked files, if any.
//! \return TRUE (1) if able to find and read a record, FALSE (0) or a negative number otherwise (not found or error)
int32_t fst24_read(
    const fst_file* const file,         //!< File we want to search
    const fst_record* criteria,         //!< [Optional] Criteria to be used for the search
    const fst_query_options* options,   //!< [Optional] Options to modify how the search will be performed
    fst_record* const record            //!< [out] Record content and info, if found
);

//! Create a search query that will apply the given criteria during a search in a file.
//! \return A pointer to a search query if the inputs are valid (open file, OK criteria struct), NULL otherwise
fst_query* fst24_new_query(
    const fst_file* const file, //!< File that will be searched with the query
    const fst_record* criteria, //!< [Optional] Criteria to be used for the search. If NULL, will look for any record
    const fst_query_options* options //!< [Optional] Options to modify how the search will be performed
);

//! Link the given list of files together, so that they are treated as one for the purpose
//! of searching and reading. Once linked, the user can use the first file in the list
//! as a replacement for all the given files.
//! \return TRUE (1) if files were linked, FALSE (0) or a negative number otherwise
int32_t fst24_link(
    fst_file** files,           //!< List of handles to open files
    const int32_t num_files     //!< How many files are in the list
);

//! Unlink the given file(s). The files are assumed to have been linked by
//! a previous call to fst24_link, so only the first one should be given as input
//! \return TRUE (1) if unlinking was successful, FALSE (0) or a negative number otherwise
int32_t fst24_unlink(fst_file* const file);

//! Move to the end of the given sequential file
//! \return The result of \ref c_fsteof if the file was open, FALSE (0) otherwise
int32_t fst24_eof(const fst_file* const file);
```

### Query Functions

```c

//! Find the next record in the given file that matches the given query criteria.
//!
//! Searches through linked files, if any.
//! \return TRUE (1) if a record was found, FALSE (0) or a negative number otherwise (not found, file not open, etc.)
int32_t fst24_find_next(
    fst_query* const query, //!< [in] Query used for the search. Must be for an open file.
    //!> [in,out] Will contain record information if found and, optionally, metadata (if included in search).
    //!> If NULL, we will only check for the existence of a match to the query, without extracting any data from that
    //!> match. If not NULL, must be a valid, initialized record.
    fst_record* record
);

//! Read the next record (data and all) that corresponds to the search criteria
//! Search through linked files, if any
//! \return TRUE (1) if able to read a record, FALSE (0) or a negative number otherwise (not found or error)
int32_t fst24_read_next(
    fst_query* const query,   //!< Query used for the search
    fst_record* const record  //!< [out] Record content and info, if found
);

//! Find all record that match the given query.
//! Search through linked files, if any.
//! \return Number of records found, 0 if none or if error.
int32_t fst24_find_all(
    fst_query* query,             //!< Query used for the search
    fst_record* results,          //!< [in,out] List of records found. Must be already allocated
    const int32_t max_num_results //!< [in] Size of the given list of records. We will stop looking if we find that many
);

//! Reset start index of search without changing the criteria
//! \return TRUE (1) if file is valid and open, FALSE (0) otherwise
int32_t fst24_rewind_search(fst_query* const query);

//! \return Whether the given query pointer is a valid query. A query's file must be
//! open for the query to be valid.
int32_t fst24_query_is_valid(const fst_query* const q);

//! Free memory used by the given query
void fst24_query_free(fst_query* const query);
```

### Record Functions

```c
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

//! Read only metadata for the given record
//! \return A pointer to the metadata, NULL if error (or no metadata)
void* fst24_read_metadata(
    fst_record* record //!< [in,out] Record for which we want to read metadata. Must have a valid handle!
);

//! Read the data and metadata of a given record from its corresponding file
//! \return TRUE (1) if reading was successful FALSE (0) or a negative number otherwise
int32_t fst24_read_record(
    fst_record* const record //!< [in,out] Record for which we want to read data. Must have a valid handle!
);

//! Retreive record's data minimum and maximum value
void fst24_bounds(
    const fst_record *record, //!< [in] Record with its data already available in memory
    double *Min,  //!< [out] Mimimum value (NaN if not retreivable)
    double *Max   //!< [out] Maximum value (NaN if not retreivable)
);

//! Free a record
//! \return TRUE (1) if no error, FALSE (0) if an error is detected
int32_t fst24_record_free(
    fst_record* record      //!< [in] record pointer
);

//! Delete a record from its file on disk
//! \return TRUE if we were able to delete the record, FALSE otherwise
int32_t fst24_delete(
    fst_record* const record //!< The record we want to delete
);

//! Copy the legacy metadata and extended metadata
//!   FST_META_ALL  : All meta data
//!   FST_META_TIME : Time related metadata (dateo,datev,deet,npas)
//!   FST_META_GRID : Grid related metadata (grtyp,ig1-4)
//!   FST_META_INFO : Variable metadata (nomvar,typvar,etiket,ip1-3)
//!   FST_META_SIZE : Data type and size related metadata (data_type,data_bits,pack_bits,ni,nj,nk)
//!   FST_META_EXT  : Extended metadata
//! \return 1 if the given two records have the same parameters (*except their pointers and handles*),
//!         0 otherwise
int32_t fst24_record_copy_metadata(
     fst_record* a,            //!< Destination record
     const fst_record* b,      //!< Source record
     int what                  //!< select which part of the metadata to copy (default: FST_META_ALL) thay can be combined with + (ie: FST_META_TIME+FST_META_INFO)
);

//! \return 1 if the given two records have the same metadata
//!         0 otherwise
int32_t fst24_record_has_same_info(const fst_record* a, const fst_record* b);

//! Check if two fst_record structs point to the exact same record, in the same file
//! \return TRUE (1) if they are the same, FALSE (0) if not or if they do not point to any specific record in a file
static inline int32_t fst24_record_is_same(const fst_record* const a, const fst_record* const b);

//! Print every difference between the attributes of the given 2 fst_struct
void fst24_record_diff(const fst_record* a, const fst_record* b);
```

## Fortran

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
    procedure, pass   :: get_name

    procedure, pass :: new_query
    procedure, pass :: read
    procedure, pass :: write

    procedure, pass :: flush
    procedure, pass :: print_summary
    procedure, pass :: unlink

    ! Sequential files
    procedure, pass :: eof
    procedure, pass :: weo
    procedure, pass :: rewind
end type fst_file

type :: fst_query
contains
    procedure, pass :: is_valid
    procedure, pass :: find_next
    procedure, pass :: find_all
    procedure, pass :: read_next
    procedure, pass :: rewind
    procedure, pass :: free
end type fst_query


!> Representation of an FST record. It allows to get and set basic information about the record and its data,
!> and to easily read, write, search a file
!>
!> It contains a (private) copy of itself that is compatible with the C interface so that the C functions can
!> be called directly on it. Whenever a call to a C function occurs, the attributes of this type are synchronized with the
!> underlying C version of the record.
type, public :: fst_record
    type(C_PTR) :: data     = C_NULL_PTR    !< Pointer to the data
    type(C_PTR) :: metadata = C_NULL_PTR    !< Pointer to the metadata

    integer(C_INT32_T) :: dateo    = -1 !< Origin Date timestamp
    integer(C_INT32_T) :: datev    = -1 !< Valid Date timestamp

    integer(C_INT32_T) :: data_type = -1    !< Data type of elements
    integer(C_INT32_T) :: data_bits = -1    !< Number of bits per elements
    integer(C_INT32_T) :: pack_bits = -1    !< Compression factor (none if 0 or 1). Number of bit if negative
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
    procedure, pass :: allocate 
    procedure, pass :: free
    procedure, pass :: has_same_info
    procedure, pass :: is_same
    procedure, pass :: read
    procedure, pass :: read_metadata
    procedure, pass :: delete
    procedure, pass :: copy_metadata
    procedure, pass :: get_data_array

    procedure, pass :: print
    procedure, pass :: print_short

end type fst_record
```

### File Functions

```fortran
!> Check whether the file at the given path is a valid standard file
!> Return .true. if the given path is a valid standard file, .false. otherwise
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

!> Return Name of the file if open, an empty string otherwise
function fst24_file_get_name(this) result(name)
    implicit none
    class(fst_file), intent(in) :: this
    character(len=:), pointer :: name
end function get_name

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

!> Create a search query that will apply the given criteria during a search in a file.
!> Return A valid fst_query if the inputs are valid (open file, OK criteria struct), an invalid query otherwise
function new_query(this,                                                                             & 
        dateo, datev, data_type, data_bits, pack_bits, ni, nj, nk,                                              &
        deet, npas, ip1, ip2, ip3, ig1, ig2, ig3, ig4, typvar, grtyp, nomvar, etiket, metadata,                 &
        ip1_all, ip2_all, ip3_all,stamp_norun) result(query)
    implicit none
    class(fst_file), intent(inout) :: this
    integer(C_INT32_T), intent(in), optional :: dateo, datev
    integer(C_INT32_T), intent(in), optional :: data_type, data_bits, pack_bits, ni, nj, nk
    integer(C_INT32_T), intent(in), optional :: deet, npas, ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=2),  intent(in), optional :: typvar
    character(len=1),  intent(in), optional :: grtyp
    character(len=4),  intent(in), optional :: nomvar
    character(len=12), intent(in), optional :: etiket
    logical, intent(in), optional :: ip1_all, ip2_all, ip3_all !< Whether we want to match any IP encoding
    logical, intent(in), optional :: stamp_norun !< Whether validitydate contians run number in last 3 bit
    type(meta), intent(in), optional :: metadata
    type(fst_query) :: query
end function new_query

!> Search a file with given criteria and read the first record that matches these criteria.
!> Search through linked files, if any.
!> Return .true. if we found a record, .false. if not or if error
function read(this, record, data,                                                                               &
        dateo, datev, data_type, data_bits, pack_bits, ni, nj, nk,                                              &
        deet, npas, ip1, ip2, ip3, ig1, ig2, ig3, ig4, typvar, grtyp, nomvar, etiket, metadata,                 &
        ip1_all, ip2_all, ip3_all,stamp_norun) result(found)
    implicit none
    class(fst_file), intent(inout) :: this
    type(fst_record), intent(inout) :: record !< Information of the record found. Left unchanged if nothing found

    !> Where to put the data being read. Can also be specified by setting the
    !> `data` attribute of the record being read.
    type(C_PTR), intent(in), optional :: data

    integer(C_INT32_T), intent(in), optional :: dateo, datev
    integer(C_INT32_T), intent(in), optional :: data_type, data_bits, pack_bits, ni, nj, nk
    integer(C_INT32_T), intent(in), optional :: deet, npas, ip1, ip2, ip3, ig1, ig2, ig3, ig4
    character(len=2),  intent(in), optional :: typvar
    character(len=1),  intent(in), optional :: grtyp
    character(len=4),  intent(in), optional :: nomvar
    character(len=12), intent(in), optional :: etiket
    logical, intent(in), optional :: ip1_all, ip2_all, ip3_all !< Whether we want to match any IP encoding
    logical, intent(in), optional :: stamp_norun !< Whether validitydate contians run number in last 3 bit
    type(meta), intent(in), optional :: metadata
    logical :: found
end function read

!> Write the given record into the given standard file
!> Return Whether the write was successful
function write(this, record, rewrite) result(success)
    implicit none
    class(fst_file),  intent(inout) :: this     !< File where we want to write
    type(fst_record), intent(inout) :: record   !< Record we want to write
    integer, intent(in), optional   :: rewrite  !< Whether we want to overwrite FST_YES, skip FST_SKIP or write again FST_NO an existing record (default NO)
    logical :: success
end function write

!> Perform a checkpoint on the given file:
!> commit (meta)data to disk if the file has changed in memory
!> Return Whether the underlying call was successful
function flush(this) result(success)
    implicit none
    class(fst_file), intent(inout) :: this
    logical :: success
end function flush

!> Print a summary of the records found in the given file (including any linked files)
!> All optional parameters are booleans determining whether we print the corresponding field.
subroutine print_summary(this,                                                                       &
        dateo, datev, datestamps, level, data_type, ni, nj, nk,                                      &
        deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234, typvar, nomvar, etiket)
    implicit none
    class(fst_file), intent(in) :: this
    logical, intent(in), optional :: dateo, datev, datestamps, level, data_type, ni, nj, nk
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

### Query Functions

```fortran

!> Find the next record in the given file that matches the given query criteria.
!> Search through linked files, if any.
!> Return .true. if we found a record, .false. if not or if error
function find_next(this, record) result(found)
    implicit none
    class(fst_query), intent(in) :: this                !< Query we are using for the search
    type(fst_record), intent(inout), optional :: record !< Information of the record found. Left unchanged if nothing found
    type(C_PTR) :: c_record
    logical :: found
end function find_next

!> Find all record that match the given query.
!> Search through linked files, if any.
!> Return Number of records found, up to size(records)
function find_all(this, records) result(num_found)
    implicit none
    class(fst_query), intent(in) :: this     !< Query used for the search
    !> [in,out] Array where the records found will be put.
    !> We stop searching after we found enough records to fill it.
    type(fst_record), dimension(:), intent(inout) :: records
    integer(C_INT32_T) :: num_found
end function find_all

!> Read the next record (data and all) that corresponds to the given query criteria
!> Search through linked files, if any
!> Return .true. if we read a record, .false. if none found or if error
function read_next(this, record) result(found)
    implicit none
    class(fst_query), intent(in) :: this        !< Query used for the search
    type(fst_record), intent(inout) :: record   !< Record that was read (left unchanged if nothing was found)
    logical :: found
end function read_next

!> Return Whether the given query pointer is a valid query. A query's file must be
!> open for the query to be valid.
pure function is_valid(this) result(is_valid)
    implicit none
    class(fst_query), intent(in) :: this
    logical :: is_valid
    integer(C_INT32_T) :: is_valid_c
    is_valid = .false.
    if (c_associated(this % query_ptr)) then
        is_valid_c = is_query_valid(this % query_ptr)
        if (is_valid_c == 1) is_valid = .true.
    end if
end function is_valid

!> Reset start index of search without changing the criteria
subroutine rewind(this)
    implicit none
    class(fst_query), intent(inout) :: this
    integer(C_INT32_T) :: c_status 
    if (this % is_valid()) c_status = fst24_rewind_search(this % query_ptr)
end subroutine rewind

!> Free memory used by the given query
subroutine free(this)
    implicit none
    class(fst_query), intent(inout) :: this
    if (this % is_valid()) then
        call fst24_query_free(this % query_ptr)
        this % query_ptr = c_null_ptr
    end if
end subroutine free
```

<a id="fortran-record-functions"></a>

### Record Functions

```fortran
!> Allocates internal data
function allocate(this,data,type,size,ni,nj,nk) result(success)
    implicit none
    class(fst_record), intent(inout), target :: this
    integer(C_INT32_T), intent(in) :: type, size, ni, nj, nk
    type(C_PTR), intent(in) :: data
    type(C_PTR) :: c_record
    logical :: success
end function

!> Free a record's internal memory. Its data will only be freed if it is *not* managed by the user.
subroutine free(this)
    implicit none
    class(fst_record), intent(inout), target :: this
    integer(C_INT32_T) :: c_status
    logical :: success
end subroutine

!> Obtain a fortran array pointer to the data. If the pointer does not match the type, size and rank of the
!> data stored in the record, it will be nullified
subroutine get_data_array(this, array_pointer)
    implicit none
    class(fst_record), intent(in) :: this
    type(*), dimension(*), pointer, intent(out) :: array_pointer
end subroutine get_data_array

!> Check whether two records have identical information (except data). This will sync the underlying C struct
!> \return .true. if the two records have the same information (not data/metadata), .false. otherwise
function has_same_info(this, other) result(has_same_info)
    implicit none
    class(fst_record), intent(inout) :: this
    type(fst_record),  intent(inout) :: other
    logical :: has_same_info
end function has_same_info

!> Determine whether another record points to the same as this one (same record in same file)
!> \return Whether the two instances point to the exact same record. .false. if one (or both) of them
!> does not point to a particular record
pure function fst24_record_is_same(this, other) result(is_same)
    implicit none
    class(fst_record), intent(in) :: this   !< fst_record instance
    type(fst_record), intent(in) :: other   !< fst_record to which we are comparing this
    logical :: is_same
end function is_same

!> Read the data and metadata of a given record from its corresponding file
!> Return Whether we were able to do the reading
function read(this,data) result(success)
    implicit none
    class(fst_record), intent(inout) :: this  !< fst_record instance. If must be a valid record already found in a file

    !> Where to put the data being read (optional). Can also be specified by setting the
    !> `data` attribute of the record being read.
    type(C_PTR), intent(in), optional :: data

    logical :: success
end function read

!> Read only metadata for the given record
!> Return .true. If we were able to read the metadata, .false. otherwise
function read_metadata(this) result(success)
    implicit none
    class(fst_record), intent(inout) :: this !< fst_record instance. If must be a valid record already found in a file
    logical :: success
end function read_metadata

!> Copy the legacy metadata and extended metadata. The what parameter allows to select which part of the metadata to copy (default: FST_META_ALL) thay can be combined with + (ie: FST_META_TIME+FST_META_INFO)
!>   FST_META_ALL  : All meta data
!>   FST_META_TIME : Time related metadata (dateo,datev,deet,npas)
!>   FST_META_GRID : Grid related metadata (grtyp,ig1-4)
!>   FST_META_INFO : Variable metadata (nomvar,typvar,etiket,ip1-3)
!>   FST_META_SIZE : Data type and size related metadata (data_type,data_bits,pack_bits,ni,nj,nk)
!>   FST_META_EXT  : Extended metadata
!> Return .true. if we were able to copy the metadata, .false. otherwise
function copy_metadata(this,record,what) result(success)
    implicit none
    class(fst_record), intent(inout) :: this !< fst_record instance. If must be a valid record already found in a file
    integer(C_INT32_T), intent(in), optional :: what
    type(fst_record), target :: record
    logical :: success
end function fst24_record_copy_metadata

!> Delete a record from its file on disk
!> Return .true. if we were able to delete the record, .false. otherwise
function delete(this) result(success)
    implicit none
    class(fst_record), intent(inout) :: this !< fst_record instance, must be a valid record previously found in a file
    logical :: success
end function delete

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
        this, prefix, print_header, dateo, datev, datestamps, level, data_type, ni, nj, nk,    &
        deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234, typvar, nomvar, etiket)
    implicit none
    class(fst_record), intent(inout) :: this !< fst_record instance whose information we want to print
    character(len=*), intent(in), optional :: prefix !< [optional] Text we want to add at the start of the line
    logical, intent(in), optional :: print_header !< [optional] Whether we want to print a header above the line to name the fields
    logical, intent(in), optional :: dateo, datev, datestamps, level, data_type, ni, nj, nk
    logical, intent(in), optional :: deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234
    logical, intent(in), optional :: typvar, nomvar, etiket
end subroutine short
```

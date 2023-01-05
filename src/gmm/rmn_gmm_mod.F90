module rmn_gmm
    use, intrinsic :: iso_c_binding
    use iso_fortran_env, only : int32, int64, real32, real64

    implicit none

public
    !  GMM_USER_FLAGS

    !> In restart file
    integer, parameter :: GMM_FLAG_RSTR      =     1
    !> Init to zero
    integer, parameter :: GMM_FLAG_IZER      =     2
    !> Init to NaNs
    integer, parameter :: GMM_FLAG_INAN      =     4
    !> Field is invalid and cannot be used
    integer, parameter :: GMM_FLAG_IINV      =     8
    !> Field has been read from restart
    integer, parameter :: GMM_FLAG_READ      =    16
    !> Field has been "created" (used to detect multiple create calls when field read from restart)
    integer, parameter :: GMM_FLAG_CRTD      =    32
    !> Field is staggered along X
    integer, parameter :: GMM_FLAG_STAG_X    =    64
    !> Field is staggered along Y
    integer, parameter :: GMM_FLAG_STAG_Y    =   128
    !> Field is staggered along Z
    integer, parameter :: GMM_FLAG_STAG_Z    =   256

    !> Read checkpoint
    logical, parameter :: GMM_READ_CKPT = .true.
    !> Write checkpoint
    logical, parameter :: GMM_WRIT_CKPT = .false.

    !  GMM VARIABLE SIZES

    !> Max. length for variable name
    integer, parameter :: GMM_MAXNAMELENGTH = 32
    integer, parameter :: GMM_META_SIZE     = 28

    !  GMM_ERROR_CODES

    !> Returned status code when everything is OK
    integer, parameter :: GMM_OK                       =  0
    !> Generic error code when a problem is detected
    integer, parameter :: GMM_ERROR                    = -1
    integer, parameter :: GMM_KEY_NOT_FOUND            = -2
    integer, parameter :: GMM_VAR_NOT_FOUND            = -3
    integer, parameter :: GMM_INCONSISTENT_DIMS        = -4
    integer, parameter :: GMM_ARRAY_ALREADY_EXISTS     = -5
    integer, parameter :: GMM_VARIABLE_ALREADY_CREATED = -6
    integer, parameter :: GMM_POINTER_TABLE_OVERFLOW   = -7

    !  GMM_MESSAGE_LEVELS

    integer, parameter :: GMM_MSG_DEBUG  =  6
    integer, parameter :: GMM_MSG_INFO   =  5
    integer, parameter :: GMM_MSG_WARN   =  4
    integer, parameter :: GMM_MSG_ERROR  =  3
    integer, parameter :: GMM_MSG_SEVERE =  2
    integer, parameter :: GMM_MSG_FATAL  =  1

    !> Dimensioning elements
    type :: gmm_layout
        SEQUENCE
        !> Dimension is low:high, useful contents 1..n
        integer :: low
        integer :: high
        integer :: halo
        integer :: halomax
        integer :: n
    end type

    type :: gmm_attributes
        SEQUENCE
        !> Unique key, used mainly for vmmget interface
        integer(kind = int64) :: key
        !> Extra keys used for lookup in external tables or additional flags
        integer(kind = int64) :: uuid1, uuid2
        !> How the field must be initialized (default=-1, no init)
        integer :: initmode
        !> Flags (valid, restart, .... )
        integer :: flags
    end type

    type :: gmm_metadata
        SEQUENCE
        type(gmm_layout), dimension(4) :: l
        type(gmm_attributes) :: a
    end type

    integer, parameter :: GMM_NULL_FLAGS = 0
    type(gmm_layout), parameter :: GMM_NULL_LAYOUT = gmm_layout(0, 0, 0, 0, 0)
    type(gmm_layout), parameter, dimension(4) :: GMM_NULL_LAYOUTS = (/GMM_NULL_LAYOUT, GMM_NULL_LAYOUT, GMM_NULL_LAYOUT, GMM_NULL_LAYOUT/)
    type(gmm_attributes), parameter :: GMM_NULL_ATTRIB = gmm_attributes(GMM_KEY_NOT_FOUND, 0, 0, 0, 0)
    type(gmm_metadata), parameter :: GMM_NULL_METADATA = gmm_metadata(GMM_NULL_LAYOUTS, GMM_NULL_ATTRIB)


    ! Private stuff

    !> Max number of directory pages
    integer, private, parameter :: MAX_PAGES = 16
    !> Max number of entries in directory page
    integer, private, parameter :: PAGE_SIZE = 128
    !> Shift for entry number in page
    integer, private, parameter :: NTRY_NB_SHFT = 0
    !> Mask for entry number in page
    integer, private, parameter :: NTRY_NB_MASK = 127
    !> Shift for page number in directory
    integer, private, parameter :: PAGE_NB_SHFT = 7
    !> Mask for page number in directory
    integer, private, parameter :: PAGE_NB_MASK = 15
    !> Shift for extension code
    integer, private, parameter :: EXTN_NB_SHFT = 11
    !> Mask for extension code
    integer, private, parameter :: EXTN_NB_MASK = 511
    !> Shift for magic number
    integer, private, parameter :: MAGC_NB_SHFT = 32
    !> Mask for magic number (all ones)
    integer, private, parameter :: MAGC_NB_MASK = -1

    !> Flags kept from user specified flags upon creation
    integer, private, parameter :: FLAGS_KEPT_ON_CREATE = GMM_FLAG_IZER + GMM_FLAG_INAN + GMM_FLAG_RSTR
    !> Flags propagated to restart file
    integer, private, parameter :: FLAGS_KEPT_IN_RESTART = GMM_FLAG_IZER + GMM_FLAG_INAN + GMM_FLAG_RSTR + GMM_FLAG_IINV

    type, private :: p_gmm_metadata
        ! Try to remove the SEQUENCE statement to see if it matters
        ! SEQUENCE
        type(gmm_layout), dimension(4) :: l
        type(gmm_attributes) :: a
        integer :: data_type
        integer :: pointer_table_index
        type(c_ptr) :: array_addr
        ! Name of the field
        character(len = GMM_MAXNAMELENGTH) :: name
    end type

    type, private :: directory_page
        type(p_gmm_metadata), dimension(:), pointer :: entry
    end type

    type(directory_page), private, dimension(MAX_PAGES) :: directory

    !> Total number of entries in directory
    integer, private :: used = 0
    !> Number of pages in directory
    integer, private :: table_size = 0
    !> Temporary, set by add_directory_entry
    integer, private :: cur_page = 0
    !> Temporary, set by add_directory_entry
    integer, private :: cur_entry = 0
    !> Last entry in last page
    integer, private :: last_entry = PAGE_SIZE
    integer, private :: file_unit = 0
    logical, private :: restart_mode = .false.
    !> Total number of array creations
    integer, private :: ordinal = 0
    integer, private :: gmm_verbose_level = 0


    type, private :: gmm_p_141
        integer(kind = int32), pointer :: p(:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_141), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs141
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_141 = 0

    type, private :: gmm_p_142
        integer(kind = int32), pointer :: p(:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_142), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs142
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_142 = 0

    type, private :: gmm_p_143
        integer(kind = int32), pointer :: p(:,:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_143), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs143
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_143 = 0

    type, private :: gmm_p_144
        integer(kind = int32), pointer :: p(:,:,:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_144), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs144
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_144 = 0

    type, private :: gmm_p_181
        integer(kind = int64), pointer :: p(:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_181), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs181
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_181 = 0

    type, private :: gmm_p_182
        integer(kind = int64), pointer :: p(:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_182), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs182
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_182 = 0

    type, private :: gmm_p_183
        integer(kind = int64), pointer :: p(:,:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_183), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs183
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_183 = 0

    type, private :: gmm_p_184
        integer(kind = int64), pointer :: p(:,:,:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_184), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs184
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_184 = 0

    type, private :: gmm_p_241
        real(kind = real32), pointer :: p(:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_241), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs241
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_241 = 0

    type, private :: gmm_p_242
        real(kind = real32), pointer :: p(:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_242), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs242
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_242 = 0

    type, private :: gmm_p_243
        real(kind = real32), pointer :: p(:,:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_243), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs243
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_243 = 0

    type, private :: gmm_p_244
        real(kind = real32), pointer :: p(:,:,:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_244), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs244
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_244 = 0

    type, private :: gmm_p_281
        real(kind = real64), pointer :: p(:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_281), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs281
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_281 = 0

    type, private :: gmm_p_282
        real(kind = real64), pointer :: p(:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_282), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs282
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_282 = 0

    type, private :: gmm_p_283
        real(kind = real64), pointer :: p(:,:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_283), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs283
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_283 = 0

    type, private :: gmm_p_284
        real(kind = real64), pointer :: p(:,:,:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_284), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs284
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_284 = 0

    type, private :: gmm_p_381
        complex(kind = 4), pointer :: p(:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_381), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs381
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_381 = 0

    type, private :: gmm_p_382
        complex(kind = 4), pointer :: p(:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_382), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs382
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_382 = 0

    type, private :: gmm_p_383
        complex(kind = 4), pointer :: p(:,:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_383), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs383
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_383 = 0

    type, private :: gmm_p_384
        complex(kind = 4), pointer :: p(:,:,:,:)
        integer(kind = int64) :: key
    end type
    type(gmm_p_384), private, dimension(MAX_PAGES * PAGE_SIZE) :: gmm_ptrs384
    !> Number of pages in directory
    integer, private :: gmm_p_table_size_384 = 0


    ! Generic interfaces

    !> Create a new entry/array
    interface gmm_create
        module procedure gmm_create184
        module procedure gmm_create144
        module procedure gmm_create284
        module procedure gmm_create244
        module procedure gmm_create384
        module procedure gmm_create183
        module procedure gmm_create143
        module procedure gmm_create283
        module procedure gmm_create243
        module procedure gmm_create383
        module procedure gmm_create182
        module procedure gmm_create142
        module procedure gmm_create282
        module procedure gmm_create242
        module procedure gmm_create382
        module procedure gmm_create181
        module procedure gmm_create141
        module procedure gmm_create281
        module procedure gmm_create241
        module procedure gmm_create381
    end interface


    !>  Obtain the pointer and/or stored metadata
    interface gmm_get
        module procedure gmm_get184
        module procedure gmm_get144
        module procedure gmm_get284
        module procedure gmm_get244
        module procedure gmm_get384
        module procedure gmm_get183
        module procedure gmm_get143
        module procedure gmm_get283
        module procedure gmm_get243
        module procedure gmm_get383
        module procedure gmm_get182
        module procedure gmm_get142
        module procedure gmm_get282
        module procedure gmm_get242
        module procedure gmm_get382
        module procedure gmm_get181
        module procedure gmm_get141
        module procedure gmm_get281
        module procedure gmm_get241
        module procedure gmm_get381
    end interface


    interface gmm_add_table_entry
        module procedure gmm_add_table_entry141
        module procedure gmm_add_table_entry142
        module procedure gmm_add_table_entry143
        module procedure gmm_add_table_entry144
        module procedure gmm_add_table_entry181
        module procedure gmm_add_table_entry182
        module procedure gmm_add_table_entry183
        module procedure gmm_add_table_entry184
        module procedure gmm_add_table_entry241
        module procedure gmm_add_table_entry242
        module procedure gmm_add_table_entry243
        module procedure gmm_add_table_entry244
        module procedure gmm_add_table_entry281
        module procedure gmm_add_table_entry282
        module procedure gmm_add_table_entry283
        module procedure gmm_add_table_entry284
        module procedure gmm_add_table_entry381
        module procedure gmm_add_table_entry382
        module procedure gmm_add_table_entry383
        module procedure gmm_add_table_entry384
    end interface


contains


#include <rmn/template.hf>
#define FNCNAME(fun) CAT(fun,EXTENSION)


! Integer functions:
#define DATATYPE integer
#define DATALENGTH 4

! 141
#define DIM 1
#define EXTENSION 141
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 142
#define DIM 2
#define EXTENSION 142
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 143
#define DIM 3
#define EXTENSION 143
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 144
#define DIM 4
#define EXTENSION 144
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

#undef DATALENGTH
#define DATALENGTH 8

! 181
#define DIM 1
#define EXTENSION 181
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 182
#define DIM 2
#define EXTENSION 182
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 183
#define DIM 3
#define EXTENSION 183
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 184
#define DIM 4
#define EXTENSION 184
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

#undef DATALENGTH
#undef DATATYPE


! Real Functions:
#define DATATYPE real
#define DATALENGTH 4

! 241
#define DIM 1
#define EXTENSION 241
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 242
#define DIM 2
#define EXTENSION 242
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 243
#define DIM 3
#define EXTENSION 243
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 244
#define DIM 4
#define EXTENSION 244
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

#undef DATALENGTH
#define DATALENGTH 8

! 281
#define DIM 1
#define EXTENSION 281
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 282
#define DIM 2
#define EXTENSION 282
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 283
#define DIM 3
#define EXTENSION 283
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 284
#define DIM 4
#define EXTENSION 284
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

#undef DATALENGTH
#undef DATATYPE

! Complex functions:

#define DATATYPE complex
#define DATALENGTH 8

! 381
#define DIM 1
#define EXTENSION 381
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 382
#define DIM 2
#define EXTENSION 382
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 383
#define DIM 3
#define EXTENSION 383
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

! 384
#define DIM 4
#define EXTENSION 384
#include "gmm_pointer_table.tmpl90"
#include "gmm_checkpoint.tmpl90"
#include "gmm_dealloc_ptr.tmpl90"
#include "gmm_get.tmpl90"
#include "gmm_create.tmpl90"
#include "gmm_update_tpi_key.tmpl90"
#include "undefiner.tmpl90"

#undef DATALENGTH
#undef DATATYPE


#undef FNCNAME


    !> Checkpoint read or write for all known types
    integer function gmm_checkpoint_all(read_or_write)
        use app
        implicit none

        logical :: read_or_write
        integer :: code, istat, ier

#include <rmn/fnom.hf>

        ! Read checkpoint file one record at a time
        if (read_or_write) then
            if (restart_mode) then
                call lib_log(APP_LIBGMM,APP_WARNING,'gmm_checkpoint_all: Restart file already read')
                gmm_checkpoint_all = GMM_OK
                return
            endif
            if (file_unit == 0) then
                ! Open checkpoint file
                istat = fnom(file_unit, 'gmm_restart', 'SEQ+UNF+FTN+OLD', 0)
                write(app_msg,*) 'gmm_checkpoint_all: Open restart file, status=', istat
                call lib_log(APP_LIBGMM,APP_DEBUG,app_msg)
                if (istat < 0) then
                    file_unit = 0
                    gmm_checkpoint_all = GMM_ERROR
                    return
                endif
            endif
            do while(.true.)
                read(file_unit, end = 999) code
                ! We are in restart mode if a single record is read from restart file
                restart_mode = .true.
                if (-1 == code) then
                    call lib_log(APP_LIBGMM,APP_ERROR,'gmm_checkpoint_all: This cannot happen')
                else if (code == 184) then
                    call gmm_checkpoint_184(.true.)
                else if (code == 144) then
                    call gmm_checkpoint_144(.true.)
                else if (code == 284) then
                    call gmm_checkpoint_284(.true.)
                else if (code == 244) then
                    call gmm_checkpoint_244(.true.)
                else if (code == 384) then
                    call gmm_checkpoint_384(.true.)
                else if (code == 183) then
                    call gmm_checkpoint_183(.true.)
                else if (code == 143) then
                    call gmm_checkpoint_143(.true.)
                else if (code == 283) then
                    call gmm_checkpoint_283(.true.)
                else if (code == 243) then
                    call gmm_checkpoint_243(.true.)
                else if (code == 383) then
                    call gmm_checkpoint_383(.true.)
                else if (code == 182) then
                    call gmm_checkpoint_182(.true.)
                else if (code == 142) then
                    call gmm_checkpoint_142(.true.)
                else if (code == 282) then
                    call gmm_checkpoint_282(.true.)
                else if (code == 242) then
                    call gmm_checkpoint_242(.true.)
                else if (code == 382) then
                    call gmm_checkpoint_382(.true.)
                else if (code == 181) then
                    call gmm_checkpoint_181(.true.)
                else if (code == 141) then
                    call gmm_checkpoint_141(.true.)
                else if (code == 281) then
                    call gmm_checkpoint_281(.true.)
                else if (code == 241) then
                    call gmm_checkpoint_241(.true.)
                else if (code == 381) then
                    call gmm_checkpoint_381(.true.)
                else
                    write(app_msg,*) 'gmm_checkpoint_all: unrecognized type=', code, ' in restart file'
                    call lib_log(APP_LIBGMM,APP_ERROR,app_msg)
                    call qqexit(1)
                endif
            end do
        else
            ! Write all tables to checkpoint file
            if (file_unit == 0) then
                ! Open checkpoint file
                istat = fnom(file_unit, 'gmm_restart', 'SEQ+UNF+FTN', 0)
                write(app_msg,*) 'gmm_checkpoint_all: Open restart file, status=', istat
                call lib_log(APP_LIBGMM,APP_DEBUG,app_msg)

                if (istat < 0) then
                    file_unit = 0
                    gmm_checkpoint_all = GMM_ERROR
                    return
                endif
            endif
            call gmm_checkpoint_184(.false.)
            call gmm_checkpoint_144(.false.)
            call gmm_checkpoint_284(.false.)
            call gmm_checkpoint_244(.false.)
            call gmm_checkpoint_384(.false.)
            call gmm_checkpoint_183(.false.)
            call gmm_checkpoint_143(.false.)
            call gmm_checkpoint_283(.false.)
            call gmm_checkpoint_243(.false.)
            call gmm_checkpoint_383(.false.)
            call gmm_checkpoint_182(.false.)
            call gmm_checkpoint_142(.false.)
            call gmm_checkpoint_282(.false.)
            call gmm_checkpoint_242(.false.)
            call gmm_checkpoint_382(.false.)
            call gmm_checkpoint_181(.false.)
            call gmm_checkpoint_141(.false.)
            call gmm_checkpoint_281(.false.)
            call gmm_checkpoint_241(.false.)
            call gmm_checkpoint_381(.false.)
        endif
    999 ier = fclos(file_unit)
        file_unit = 0
        gmm_checkpoint_all = GMM_OK
    end function gmm_checkpoint_all


    integer function gmm_delete(iname)
        use iso_fortran_env, only : int64

        implicit none

        character(len = *), intent(in) :: iname

        integer(kind = int64) :: key
        integer :: datatype

        key = 0
        call check_directory_entry(iname, key)
        if (cur_page == 0 .or. cur_entry == 0) then
            ! Quick check using key was not successful
            call find_directory_entry(iname, key)
        endif
        if (cur_page == 0 .or. cur_entry == 0) then
            ! Variable not found
            key= GMM_KEY_NOT_FOUND
            gmm_delete = GMM_VAR_NOT_FOUND
        else
            datatype = directory(cur_page)%entry(cur_entry)%data_type
            directory(cur_page)%entry(cur_entry)%name = 'Variable deleted upon request'
            dtype: select case (datatype)
                case (184)
                    call gmm_dealloc_ptr184()
                case (144)
                    call gmm_dealloc_ptr144()
                case (284)
                    call gmm_dealloc_ptr284()
                case (244)
                    call gmm_dealloc_ptr244()
                case (384)
                    call gmm_dealloc_ptr384()
                case (183)
                    call gmm_dealloc_ptr183()
                case (143)
                    call gmm_dealloc_ptr143()
                case (283)
                    call gmm_dealloc_ptr283()
                case (243)
                    call gmm_dealloc_ptr243()
                case (383)
                    call gmm_dealloc_ptr383()
                case (182)
                    call gmm_dealloc_ptr182()
                case (142)
                    call gmm_dealloc_ptr142()
                case (282)
                    call gmm_dealloc_ptr282()
                case (242)
                    call gmm_dealloc_ptr242()
                case (382)
                    call gmm_dealloc_ptr382()
                case (181)
                    call gmm_dealloc_ptr181()
                case (141)
                    call gmm_dealloc_ptr141()
                case (281)
                    call gmm_dealloc_ptr281()
                case (241)
                    call gmm_dealloc_ptr241()
                case (381)
                    call gmm_dealloc_ptr381()
            end select dtype
            directory(cur_page)%entry(cur_entry)%l = GMM_NULL_LAYOUT
            directory(cur_page)%entry(cur_entry)%a = GMM_NULL_ATTRIB
            gmm_delete = GMM_OK
        endif
    end function gmm_delete


    subroutine check_directory_entry(name, key)
        use iso_fortran_env, only : int64

        implicit none

        character(len = *) :: name
        integer(kind = int64), intent(in) :: key

        character(len = GMM_MAXNAMELENGTH) :: l_name
        integer :: temp
        logical :: found

        found = .false.
        if (cur_page == 0 .and. cur_entry == 0) then
            return
        endif

        l_name = trim(name)
        temp = ishft(key, -PAGE_NB_SHFT)
        cur_page = iand(PAGE_NB_MASK, temp)
        ! Keep page number below number of pages in directory
        cur_page = min(cur_page + 1, table_size)
        temp = ishft(key, -NTRY_NB_SHFT)
        cur_entry = iand(NTRY_NB_MASK, temp)
        ! Keep entry number <= directory page size
        cur_entry = min(cur_entry + 1, PAGE_SIZE)
        found = key == directory(cur_page)%entry(cur_entry)%a%key
        found = found .and. ( directory(cur_page)%entry(cur_entry)%name == l_name )
        if (.not. found) then
            ! NOT FOUND, return zeroes
            cur_page = 0
            cur_entry = 0
        endif
    end subroutine check_directory_entry


    !> Find entry called name in directory starting from beginning of directory (the hard way)
    !> upon exit cur_page and cur_entry are nonzero if desired entry found
    subroutine find_directory_entry(name, key)
        implicit none
        character(len = *) :: name
        integer(kind = int64), optional :: key
        integer :: i

        character(len = GMM_MAXNAMELENGTH) :: l_name

        l_name = trim(name)
#ifdef DEBUG
        print *, 'looking for name=', l_name, '='
#endif
        cur_page = 1
        cur_entry = 1
        do i = 1, used
            if (directory(cur_page)%entry(cur_entry)%name == l_name) then
                if (present(key)) then
                    key = directory(cur_page)%entry(cur_entry)%a%key
                endif
                return
            endif
            cur_entry = cur_entry + 1
            if (cur_entry > PAGE_SIZE) then
                cur_page = cur_page + 1
                cur_entry = 1
            endif
        enddo
        ! NOT FOUND, return zeroes
        cur_page = 0
        cur_entry = 0

        key = GMM_KEY_NOT_FOUND

    end subroutine find_directory_entry


    !> Locate/create a new properly initialized entry in directory
    subroutine add_directory_entry
        implicit none
        integer :: i

        ! first time around, nullify all pointers
        if ( table_size == 0 ) then
            do i = 1, MAX_PAGES
                nullify(directory(i)%entry)
            enddo
        endif

        used = used + 1
        last_entry = last_entry + 1
        ! Need new directory page ?
        if ( last_entry > PAGE_SIZE ) then
            ! YES add page and initialize entries
            table_size = table_size + 1
            last_entry = 1
            ! directory overflow ?
            if (table_size .le. MAX_PAGES) then
                allocate(directory(table_size)%entry(PAGE_SIZE))
            else
                ! OOPS, yes
                call qqexit(1)
            endif
            ! initialize directory entries
            do i = 1, PAGE_SIZE
                ! Invalid layout
                directory(table_size)%entry(i)%l = GMM_NULL_LAYOUT
                ! Null attributes
                directory(table_size)%entry(i)%a = GMM_NULL_ATTRIB
            enddo
            cur_entry = 1
        else
            cur_entry = last_entry
        endif
        cur_page = table_size
    end subroutine add_directory_entry


    subroutine gmm_dumpinfo(fldstat)
        use app
        use iso_fortran_env, only : int64

        implicit none

        logical, intent(in), optional :: fldstat
        integer :: i, l_page, l_entry, nelm, crc
        type(gmm_layout), dimension(4) :: dims

        interface
            integer function f_calc_crc(obj, nbelem, seed, stride) bind(c, name = "calc_crc")
                use, intrinsic :: iso_c_binding, only: c_ptr
                type(c_ptr), value, intent(in) :: obj
                integer, intent(in) :: nbelem
                integer, intent(in) :: seed
                integer, value, intent(in) :: stride
            end function
        end interface

        integer(kind = int64) :: ptraddr

        l_page = 1
        l_entry = 1
        write(app_msg,*) 'gmm_dumpinfo: Number of variables in use is', used
        call lib_log(APP_LIBGMM,APP_ALWAYS,app_msg)
        do i = 1, used
            dims = directory(l_page)%entry(l_entry)%l
            nelm = ( (dims(1)%high - dims(1)%low +1) * &
                    (dims(2)%high - dims(2)%low +1) * &
                    (dims(3)%high - dims(3)%low +1) * &
                    (dims(4)%high - dims(4)%low +1) )
            ptraddr = transfer(directory(l_page)%entry(l_entry)%array_addr, ptraddr)
            if (present(fldstat)) then
                print *, 'Appel a statfld a ecrire, fldstat=', fldstat
                print '(a,a,a,i10)', &
                    'Name=', directory(l_page)%entry(l_entry)%name, &
                    ' addr=', ptraddr
            else
                crc = f_calc_crc(directory(l_page)%entry(l_entry)%array_addr, nelm, 0, 1)
                print '(a,a,a,i10,a,i10)', &
                    'Name=', directory(l_page)%entry(l_entry)%name, &
                    ' addr=', ptraddr, &
                    ' checksum=', crc
            endif
            l_entry = l_entry + 1
            if (l_entry > PAGE_SIZE) then
                l_page = l_page + 1
                l_entry = 1
            endif
        enddo
    end subroutine gmm_dumpinfo


    !> Encode/pack type(gmm_metadata) in a basic Fortran type
    integer function gmm_encodemeta(meta, output)
        implicit none

        type(gmm_metadata), intent(in) :: meta
        integer, dimension(:), intent(out) :: output

        if (size(output) < GMM_META_SIZE) then
            gmm_encodemeta = GMM_ERROR
            return
        endif

        output(1:GMM_META_SIZE) = transfer(meta, output)

        gmm_encodemeta = GMM_OK
    end function gmm_encodemeta


    !> Decode/unpack meta into type(gmm_metadata)
    integer function gmm_decodemeta(meta, input)
        implicit none

        type(gmm_metadata), intent(out):: meta
        integer, dimension(:), intent(in) :: input

        if (size(input) < GMM_META_SIZE) then
        gmm_decodemeta = GMM_ERROR
        return
        endif

        meta = transfer(input(1:GMM_META_SIZE), meta)

        gmm_decodemeta = GMM_OK
    end function gmm_decodemeta


    !> Get the stored metadata
    integer function gmm_getmeta(varname, meta)
        use app
        use iso_fortran_env, only : int64

        implicit none

        character(len = *), intent(in) :: varname
        type(gmm_metadata), intent(out) :: meta

        integer(kind = int64) :: key

        key = 0
        call find_directory_entry(varname,key)

        if (key == GMM_KEY_NOT_FOUND) then
            write(app_msg,*) 'gmm_getmeta: Variable ', varname, ' not found'
            call lib_log(APP_LIBGMM,APP_WARNING,app_msg)
            gmm_getmeta = GMM_ERROR
            return
        endif

        meta%a = directory(cur_page)%entry(cur_entry)%a
        meta%l = directory(cur_page)%entry(cur_entry)%l
        gmm_getmeta = 0
    end function gmm_getmeta


    integer function gmm_getmeta2(iname, meta)
        implicit none

        ! name (partially redundant with attributes)
        character(len = *), intent(in) :: iname
        ! attributes (name in attributes is not used)
        type(gmm_metadata), intent(out) :: meta

        gmm_getmeta2 = gmm_getmeta(iname, meta)
    end function gmm_getmeta2


    integer function gmm_nkeys()
        implicit none

        gmm_nkeys = used
    end function gmm_nkeys


    !> Get the list of keys/labels
    integer function gmm_keys(taglist, pattern)
        implicit none

        character(len=*), intent(out) :: taglist(:)
        character(len=*), intent(in), optional :: pattern
        integer :: i, strlen_pattern, nkeys, maxkeys

        nkeys = 0
        maxkeys = nkeys
        gmm_keys = -1

        maxkeys = size(taglist)
        if (used > maxkeys) then
            return
        endif
        cur_page = 1
        cur_entry = 1
        if (present(pattern)) then
            strlen_pattern = len_trim(pattern)
            gmm_keys = 0
            do i = 1, used
                if (directory(cur_page)%entry(cur_entry)%name(1:strlen_pattern) == pattern(1:strlen_pattern)) then
                    taglist(nkeys+1) = directory(cur_page)%entry(cur_entry)%name
                    nkeys = nkeys + 1
                endif
                cur_entry = cur_entry + 1
                if (cur_entry > PAGE_SIZE) then
                    cur_page = cur_page + 1
                    cur_entry = 1
                endif
            enddo
            gmm_keys = nkeys
        else
            do i = 1, used
                taglist(i) = directory(cur_page)%entry(cur_entry)%name
                cur_entry = cur_entry + 1
                if (cur_entry > PAGE_SIZE) then
                    cur_page = cur_page + 1
                    cur_entry = 1
                endif
            enddo
            gmm_keys = used
        endif
    end function gmm_keys


    !> Rename a GMM entry (update the label)
    integer function gmm_rename(old_varname, new_varname)
        use app
        use iso_fortran_env, only : int64

        implicit none

        character(len=*), intent(in) :: old_varname, new_varname

        integer(kind = int64) :: key

        key = GMM_KEY_NOT_FOUND
        call find_directory_entry(new_varname, key)
        if (key >= 0) then
            write(app_msg,*) 'gmm_rename: Variable ', trim(new_varname), ' is already defined'
            call lib_log(APP_LIBGMM,APP_WARNING,app_msg)
            gmm_rename = GMM_ERROR
            return
        endif

        key = GMM_KEY_NOT_FOUND
        call find_directory_entry(old_varname, key)
        if (key == GMM_KEY_NOT_FOUND) then
            write(app_msg,*) 'gmm_rename: Variable ', trim(old_varname), ' not defined'
            call lib_log(APP_LIBGMM,APP_WARNING,app_msg)
            gmm_rename = GMM_ERROR
            return
        endif

        directory(cur_page)%entry(cur_entry)%name = new_varname
        write(app_msg,*) 'gmm_rename: Variable ', trim(old_varname), ' renamed to ', trim(new_varname)
        call lib_log(APP_LIBGMM,APP_DEBUG,app_msg)
        gmm_rename = 0
    end function gmm_rename


    !> Cycle rename a list of labels
    integer function gmm_shuffle(taglist)
        use app
        use iso_fortran_env, only : int64

        implicit none

        character(len = *), intent(in) :: taglist(:)

        integer :: i, nkeys, ier
        integer(kind = int64), dimension(:), allocatable :: key_list
        integer(kind = int64) :: temp_key
        ! FIXME: Why do we have this magic value!?
        character(len = GMM_MAXNAMELENGTH) :: tempname = '2121_Trans-Canada_Dorval_H9P-1J3'

        logical ok, valide

        gmm_shuffle = GMM_ERROR

        ! Epuration de la liste... On elimine les chaines de caracteres vides
        temp_key = GMM_KEY_NOT_FOUND
        call find_directory_entry(tempname, temp_key)
        if (temp_key /= GMM_KEY_NOT_FOUND) then
            call lib_log(APP_LIBGMM,APP_FATAL,'gmm_shuffle: Temporary variable used for swapping should not exist')
            gmm_shuffle = GMM_ERROR
        endif

        allocate(key_list(size(taglist)))
        nkeys = size(taglist)
        i = 0
        ok = .true.
        do while (ok .and. i <= nkeys)
            i = i + 1
            if (i <= nkeys) then
                if (0 == len_trim(taglist(i))) then
                    ok = .false.
                    i = i - 1
                endif
            endif
        enddo

        if (i < nkeys) then
            nkeys = i
        endif

        do i = 1, nkeys
            call find_directory_entry(taglist(i), key_list(i))
        enddo

        valide = .false.
        do i = 1, nkeys
            if (key_list(i) /= GMM_KEY_NOT_FOUND) then
                valide = .true.
                exit
            endif
        enddo

        if (.not. valide) then
            call lib_log(APP_LIBGMM,APP_ERROR,'gmm_shuffle: None of the fields int the list exist')
            gmm_shuffle = GMM_ERROR
            deallocate(key_list)
            return
        endif
        select case (nkeys)
            case (2)
                if (key_list(1) == GMM_KEY_NOT_FOUND) then
                    ier = gmm_rename(taglist(2), taglist(1))
                elseif (key_list(2) == GMM_KEY_NOT_FOUND) then
                    ier = gmm_rename(taglist(1), taglist(2))
                else
                    ier = gmm_rename(taglist(1), tempname)
                    ier = gmm_rename(taglist(2), taglist(1))
                    ier = gmm_rename(tempname, taglist(2))
                endif
            case default
                temp_key = key_list(nkeys)
                do i = 2, nkeys
                    key_list(i) = key_list(i-1)
                enddo
                key_list(1) = temp_key

                if (key_list(nkeys) /= GMM_KEY_NOT_FOUND) then
                    ier = gmm_rename(taglist(nkeys),tempname)
                endif
                do i = nkeys, 2, -1
                    if (key_list(i-1) /= GMM_KEY_NOT_FOUND) then
                        ier = gmm_rename(taglist(i-1), taglist(i))
                    endif
                enddo

                call find_directory_entry(tempname, temp_key)
                if (temp_key /= GMM_KEY_NOT_FOUND) then
                    ier = gmm_rename(tempname, taglist(1))
                endif
        end select

        gmm_shuffle = 0

        deallocate(key_list)
    end function gmm_shuffle


    !> Check if a GMM function result is OK
    logical function gmm_is_ok(errcode)
        implicit none
        integer, intent(in) :: errcode
        gmm_is_ok = (errcode >= 0)
    end function


    !> Check if a GMM function result is an error
    logical function gmm_is_error(errcode)
        implicit none
        integer, intent(in) :: errcode
        gmm_is_error = (errcode < 0)
    end function


    !> Encode/pack type(gmm_metadata) in a basic Fortran type
    !> \author  Yves Chartier, 2008-04
    function gmm_updatemeta(iname, F_meta) result(F_istat)
        use iso_fortran_env, only : int64

        implicit none

        character(len=*), intent(in) :: iname
        type(gmm_metadata), intent(in) :: F_meta

        integer :: F_istat

        integer :: i
        integer(kind = int64) :: key

        key = 0
        call check_directory_entry(iname, key)
        if(cur_page == 0 .or. cur_entry == 0) then
            ! Quick check using key was not successful
            call find_directory_entry(iname, key)
        endif

        if (cur_page == 0 .or. cur_entry == 0) then
            ! Return null entry
            F_istat = GMM_VAR_NOT_FOUND
            return
        endif

        do i = 1, 4
            directory(cur_page)%entry(cur_entry)%l(i)%halo = F_meta%l(i)%halo
            directory(cur_page)%entry(cur_entry)%l(i)%halomax = F_meta%l(i)%halomax
            directory(cur_page)%entry(cur_entry)%l(i)%n = F_meta%l(i)%n
        enddo

    directory(cur_page)%entry(cur_entry)%a%uuid1 = F_meta%a%uuid1
    directory(cur_page)%entry(cur_entry)%a%uuid2 = F_meta%a%uuid2
    directory(cur_page)%entry(cur_entry)%a%flags = F_meta%a%flags

    F_istat = GMM_OK
    end function gmm_updatemeta


    integer function gmm_update_tpi_key2(indx, datatype, key)
        use iso_fortran_env, only : int64

        implicit none
        integer, intent(in) :: indx, datatype
        integer(kind = int64), intent(in) :: key

        select case (datatype)
            case (184)
                gmm_update_tpi_key2 = gmm_update_table_entry184(indx, key)
            case (144)
                gmm_update_tpi_key2 = gmm_update_table_entry144(indx, key)
            case (284)
                gmm_update_tpi_key2 = gmm_update_table_entry284(indx, key)
            case (244)
                gmm_update_tpi_key2 = gmm_update_table_entry244(indx, key)
            case (384)
                gmm_update_tpi_key2 = gmm_update_table_entry384(indx, key)
            case (183)
                gmm_update_tpi_key2 = gmm_update_table_entry183(indx, key)
            case (143)
                gmm_update_tpi_key2 = gmm_update_table_entry143(indx, key)
            case (283)
                gmm_update_tpi_key2 = gmm_update_table_entry283(indx, key)
            case (243)
                gmm_update_tpi_key2 = gmm_update_table_entry243(indx, key)
            case (383)
                gmm_update_tpi_key2 = gmm_update_table_entry383(indx, key)
            case (182)
                gmm_update_tpi_key2 = gmm_update_table_entry182(indx, key)
            case (142)
                gmm_update_tpi_key2 = gmm_update_table_entry142(indx, key)
            case (282)
                gmm_update_tpi_key2 = gmm_update_table_entry282(indx, key)
            case (242)
                gmm_update_tpi_key2 = gmm_update_table_entry242(indx, key)
            case (382)
                gmm_update_tpi_key2 = gmm_update_table_entry382(indx, key)
            case (181)
                gmm_update_tpi_key2 = gmm_update_table_entry181(indx, key)
            case (141)
                gmm_update_tpi_key2 = gmm_update_table_entry141(indx, key)
            case (281)
                gmm_update_tpi_key2 = gmm_update_table_entry281(indx, key)
            case (241)
                gmm_update_tpi_key2 = gmm_update_table_entry241(indx, key)
            case (381)
                gmm_update_tpi_key2 = gmm_update_table_entry381(indx, key)
        end select
    end function gmm_update_tpi_key2


    !> Set the verbosity level
    integer function gmm_verbosity(verbose_level)
        use app
        implicit none
        integer, intent(in) :: verbose_level

        gmm_verbosity=lib_loglevelno(APP_LIBGMM,verbose_level)
    end function gmm_verbosity

end module rmn_gmm

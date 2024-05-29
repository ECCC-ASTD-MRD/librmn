!> \file rmn_fst24_record.F90
!> Encapsulation of FST record information into a derived type 

!> Object oriented interface and functions for the fst24 API
module rmn_fst24_record
    use App
    use f_c_strings_mod
    use rmn_common
    use rmn_meta
    implicit none
    private

    include 'fst24_interface.inc'
#include "fst24_record.hf"

    public :: fst_record_fields, fst_record_c
    public :: fst24_is_default_record_valid, fst24_make_fields, fst24_make_fields_from_string

    integer(C_INT32_T), parameter, public :: FST_TYPE_BINARY         = 0
    integer(C_INT32_T), parameter, public :: FST_TYPE_REAL_OLD_QUANT = 1
    integer(C_INT32_T), parameter, public :: FST_TYPE_UNSIGNED       = 2
    integer(C_INT32_T), parameter, public :: FST_TYPE_CHAR           = 3
    integer(C_INT32_T), parameter, public :: FST_TYPE_SIGNED         = 4
    integer(C_INT32_T), parameter, public :: FST_TYPE_REAL_IEEE      = 5
    integer(C_INT32_T), parameter, public :: FST_TYPE_REAL           = 6
    integer(C_INT32_T), parameter, public :: FST_TYPE_STRING         = 7
    integer(C_INT32_T), parameter, public :: FST_TYPE_COMPLEX        = 8

    integer(C_INT32_T), parameter, public :: FST_TYPE_TURBOPACK      = 128
    integer(C_INT32_T), parameter, public :: FST_TYPE_MAGIC          = 810

    character(len=23), dimension(0:8), parameter :: FST_TYPE_NAMES = [      &
        'FST_TYPE_BINARY        ',    &
        'FST_TYPE_REAL_OLD_QUANT',    &
        'FST_TYPE_UNSIGNED      ',    &
        'FST_TYPE_CHAR          ',    &
        'FST_TYPE_SIGNED        ',    &
        'FST_TYPE_REAL_IEEE     ',    &
        'FST_TYPE_REAL          ',    &
        'FST_TYPE_STRING        ',    &
        'FST_TYPE_COMPLEX       ']

    !> Representation of an FST record. It allows to get and set basic information about the record and its data,
    !> and to easily read, write, search a file
    !>
    !> It contains a (private) copy of itself that is compatible with the C interface so that the C functions can
    !> be called directly on it. Whenever a call to a C function occurs, the attributes of this type are synchronized with the
    !> underlying C version of the record.
    type, public :: fst_record
        type(fst_record_c), private :: c_self !< bind(C) version of this struct, to interface with C implementation

        type(C_PTR) :: data     = C_NULL_PTR  !< Pointer to the data
        type(meta)  :: metadata               !< Metadata object

        integer(C_INT32_T) :: dateo    = -1   !< Origin Date timestamp
        integer(C_INT32_T) :: datev    = -1   !< Valid Date timestamp

        integer(C_INT32_T) :: data_type = -1    !< Data type of elements
        integer(C_INT32_T) :: data_bits = -1    !< Number of bits per element (uncompressed)
        integer(C_INT32_T) :: pack_bits = -1    !< Number of bits per element (compressed)
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

        procedure, pass :: make_c_self => fst24_record_make_c_self !< \private \copydoc fst24_record_make_c_self
        procedure, pass :: from_c_self => fst24_record_from_c_self !< \private \copydoc fst24_record_from_c_self
        procedure, pass :: get_c_ptr => fst24_record_get_c_ptr     !< \private \copydoc fst24_record_get_c_ptr

        procedure, pass :: allocate => fst24_record_allocate                 !< \copydoc fst24_record_new
        procedure, pass :: free => fst24_record_free                    !< \copydoc fst24_record_free
        procedure, pass :: has_same_info => fst24_record_has_same_info  !< \copydoc fst24_record_has_same_info
        procedure, pass :: is_same       => fst24_record_is_same        !< \copydoc fst24_record_is_same
        procedure, pass :: is_descriptor => fst24_record_is_descriptor  !< \copydoc fst24_file_is_descriptor
        procedure, pass :: read          => fst24_record_read           !< \copydoc fst24_record_read
        procedure, pass :: read_metadata => fst24_record_read_metadata  !< \copydoc fst24_record_read_metadata
!        procedure, pass :: get_metadata => fst24_record_get_metadata    !< \copydoc fst24_record_get_metadata
!        procedure, pass :: set_metadata => fst24_record_set_metadata    !< \copydoc fst24_record_set_metadata
        procedure, pass :: copy_metadata => fst24_record_copy_metadata    !< \copydoc fst24_record_copy_metadata
        procedure, pass :: delete        => fst24_record_delete         !< \copydoc fst24_record_delete

        procedure, pass :: print        => rmn_fst24_record_print           !< \copydoc rmn_fst24_record_print
        procedure, pass :: print_short  => rmn_fst24_record_print_short     !< \copydoc rmn_fst24_record_print_short

        !> Obtain a fortran array pointer to the data. If the pointer does not match the type, size and rank of the
        !> data stored in the record, it will be nullified
        generic :: get_data_array => &
            fst24_record_get_data_array_i1_1d, fst24_record_get_data_array_i1_2d, fst24_record_get_data_array_i1_3d,    &
            fst24_record_get_data_array_i2_1d, fst24_record_get_data_array_i2_2d, fst24_record_get_data_array_i2_3d,    &
            fst24_record_get_data_array_i4_1d, fst24_record_get_data_array_i4_2d, fst24_record_get_data_array_i4_3d,    &
            fst24_record_get_data_array_i8_1d, fst24_record_get_data_array_i8_2d, fst24_record_get_data_array_i8_3d,    &
            fst24_record_get_data_array_r4_1d, fst24_record_get_data_array_r4_2d, fst24_record_get_data_array_r4_3d,    &
            fst24_record_get_data_array_r8_1d, fst24_record_get_data_array_r8_2d, fst24_record_get_data_array_r8_3d

        procedure, pass, private ::  &
            fst24_record_get_data_array_i1_1d, fst24_record_get_data_array_i1_2d, fst24_record_get_data_array_i1_3d,    &
            fst24_record_get_data_array_i2_1d, fst24_record_get_data_array_i2_2d, fst24_record_get_data_array_i2_3d,    &
            fst24_record_get_data_array_i4_1d, fst24_record_get_data_array_i4_2d, fst24_record_get_data_array_i4_3d,    &
            fst24_record_get_data_array_i8_1d, fst24_record_get_data_array_i8_2d, fst24_record_get_data_array_i8_3d,    &
            fst24_record_get_data_array_r4_1d, fst24_record_get_data_array_r4_2d, fst24_record_get_data_array_r4_3d,    &
            fst24_record_get_data_array_r8_1d, fst24_record_get_data_array_r8_2d, fst24_record_get_data_array_r8_3d
    end type fst_record

    interface
        pure function base_fst_type(type_flag) result(base_type) bind(C, name = 'base_fst_type_f')
            import :: C_INT32_T
            implicit none
            integer(C_INT32_T), intent(in), value :: type_flag
            integer(C_INT32_T) :: base_type
        end function base_fst_type
        pure function is_type_real(type_flag) result(is_real) bind(C, name = 'is_type_real_f')
            import :: C_INT32_T
            implicit none
            integer(C_INT32_T), intent(in), value :: type_flag
            integer(C_INT32_T) :: is_real
        end function is_type_real
        pure function is_type_complex(type_flag) result(is_complex) bind(C, name = 'is_type_complex_f')
            import :: C_INT32_T
            implicit none
            integer(C_INT32_T), intent(in), value :: type_flag
            integer(C_INT32_T) :: is_complex
        end function is_type_complex
        pure function is_type_turbopack(type_flag) result(is_turbopack) bind(C, name = 'is_type_turbopack_f')
            import :: C_INT32_T
            implicit none
            integer(C_INT32_T), intent(in), value :: type_flag
            integer(C_INT32_T) :: is_turbopack
        end function is_type_turbopack
        pure function is_type_integer(type_flag) result(is_integer) bind(C, name = 'is_type_integer_f')
            import :: C_INT32_T
            implicit none
            integer(C_INT32_T), intent(in), value :: type_flag
            integer(C_INT32_T) :: is_integer
        end function is_type_integer
        pure function has_type_missing(type_flag) result(has_missing) bind(C, name = 'has_type_missing_f')
            import :: C_INT32_T
            implicit none
            integer(C_INT32_T), intent(in), value :: type_flag
            integer(C_INT32_T) :: has_missing
        end function has_type_missing
    end interface

contains
#include "rmn/rmn_fst24_record_get_data_array.hf"

    !> Determine whether another record points to the same as this one (same record in same file)
    !> \return Whether the two instances point to the exact same record. .false. if one (or both) of them
    !> does not point to a particular record
    pure function fst24_record_is_same(this, other) result(is_same)
        implicit none
        class(fst_record), intent(in) :: this   !< fst_record instance
        type(fst_record), intent(in) :: other   !< fst_record to which we are comparing this
        logical :: is_same

        integer(C_INT32_T) :: c_status

        c_status = fst24_record_is_same_c(this % get_c_ptr(), other % get_c_ptr())
        is_same = (c_status == 1)
    end function fst24_record_is_same

    !> Check if an fst_record is a field/reference descriptor
    !> \return TRUE (1) if it is, FALSE (0) otherwise
    pure function fst24_record_is_descriptor(this) result(is_same)
        implicit none
        class(fst_record), intent(in) :: this   !< fst_record instance
        logical :: is_same

        integer(C_INT32_T) :: c_status

        c_status = fst24_record_is_descriptor_c(this % get_c_ptr())
        is_same = (c_status == 1)
    end function fst24_record_is_descriptor

    !> Update c_self with current values from this
    subroutine fst24_record_make_c_self(this)
        implicit none
        class(fst_record), intent(inout) :: this

        this % c_self % data     = this % data
        this % c_self % metadata = this % metadata % json_obj

        this % c_self % dateo = this % dateo
        this % c_self % datev = this % datev

        this % c_self % data_type = this % data_type
        this % c_self % data_bits = this % data_bits
        this % c_self % pack_bits = this % pack_bits
        this % c_self % ni    = this % ni
        this % c_self % nj    = this % nj
        this % c_self % nk    = this % nk

        this % c_self % deet  = this % deet
        this % c_self % npas  = this % npas

        this % c_self % ip1   = this % ip1
        this % c_self % ip2   = this % ip2
        this % c_self % ip3   = this % ip3

        this % c_self % ig1   = this % ig1
        this % c_self % ig2   = this % ig2
        this % c_self % ig3   = this % ig3
        this % c_self % ig4   = this % ig4

        call strncpy_f2c(this % typvar, this % c_self % typvar, 3)
        call strncpy_f2c(this % grtyp, this % c_self % grtyp, 2)
        call strncpy_f2c(this % nomvar, this % c_self % nomvar, 5)
        call strncpy_f2c(this % etiket, this % c_self % etiket, 13)
    end subroutine fst24_record_make_c_self

    !> Update this record with current values from c_self
    subroutine fst24_record_from_c_self(this)
        implicit none
        class(fst_record), intent(inout) :: this

        this % data     = this % c_self % data
        this % metadata % json_obj = this % c_self % metadata

        this % dateo = this % c_self % dateo
        this % datev = this % c_self % datev
                            
        this % data_type = this % c_self % data_type
        this % data_bits = this % c_self % data_bits
        this % pack_bits = this % c_self % pack_bits
        this % ni    = this % c_self % ni
        this % nj    = this % c_self % nj
        this % nk    = this % c_self % nk
                            
        this % deet  = this % c_self % deet
        this % npas  = this % c_self % npas
                            
        this % ip1   = this % c_self % ip1
        this % ip2   = this % c_self % ip2
        this % ip3   = this % c_self % ip3
                            
        this % ig1   = this % c_self % ig1
        this % ig2   = this % c_self % ig2
        this % ig3   = this % c_self % ig3
        this % ig4   = this % c_self % ig4

        call strncpy_c2f(this % typvar, this % c_self % typvar, 2)
        call strncpy_c2f(this % grtyp, this % c_self % grtyp, 1)
        call strncpy_c2f(this % nomvar, this % c_self % nomvar, 4)
        call strncpy_c2f(this % etiket, this % c_self % etiket, 12)
    end subroutine fst24_record_from_c_self

    !> Retrieve the C_PTR to c_self
    pure function fst24_record_get_c_ptr(this) result(ptr)
        implicit none
        class(fst_record), intent(in), target :: this
        type(C_PTR) :: ptr
        ptr = c_loc(this % c_self)
    end function fst24_record_get_c_ptr

    function fst24_record_allocate(this,data,type,size,ni,nj,nk) result(success)
        implicit none
        class(fst_record), intent(inout), target :: this
        integer(C_INT32_T), intent(in) :: type, size, ni, nj, nk
        type(C_PTR), intent(in) :: data
        type(C_PTR) :: c_record
        logical :: success

        c_record = fst24_record_new_c(data,type,size,ni,nj,nk)
        success = .false.
        if (c_associated(c_record)) then
            call this % from_c_self()
            success = .true.
        end if
    end function

    subroutine fst24_record_free(this)
        implicit none
        class(fst_record), intent(inout), target :: this
        integer(C_INT32_T) :: c_status
        logical :: success

        c_status = fst24_record_free_c(c_loc(this%c_self))
     end subroutine

    !> Check whether two records have identical information (except data). This will sync the underlying C struct
    !> \return .true. if the two records have the same information (not data/metadata), .false. otherwise
    function fst24_record_has_same_info(this, other) result(has_same_info)
        implicit none
        class(fst_record), intent(inout) :: this
        type(fst_record),  intent(inout) :: other
        logical :: has_same_info

        integer(C_INT32_T) :: c_status

        has_same_info = .false.
        call this % make_c_self()
        call other % make_c_self()
        c_status = fst24_record_has_same_info_c(this % get_c_ptr(), other % get_c_ptr())
        if (c_status == 1) has_same_info = .true.
    end function fst24_record_has_same_info

    !> \copybrief fst24_read
    !> \return Whether we were able to do the reading
    function fst24_record_read(this,data) result(success)
        implicit none
        class(fst_record), intent(inout) :: this  !< fst_record instance. If must be a valid record already found in a file
        logical :: success

        !> Where to put the data being read (optional). Can also be specified by setting the
        !> `data` attribute of the record being read.
        type(C_PTR), intent(in), optional :: data

        integer(C_INT32_T) :: c_status

        success = .false.
        if (present(data)) this % data = data
        
        call this % make_c_self()
        c_status = fst24_read_record(this % get_c_ptr())
        if (c_status > 0) then
            call this % from_c_self()
            success = .true.
        end if
    end function fst24_record_read

    !> \copybrief fst24_read_metadata
    !> \return .true. If we were able to read the metadata, .false. otherwise
    function fst24_record_read_metadata(this) result(success)
        implicit none
        class(fst_record), intent(inout) :: this !< fst_record instance. If must be a valid record already found in a file
        logical :: success

        type(C_PTR) :: metadata

        success = .false.
        metadata = fst24_read_metadata(this % get_c_ptr())
        if (c_associated(metadata)) then
            call this % from_c_self()
            success = .true.
        end if
    end function fst24_record_read_metadata

    !> \copybrief fst24_record_copy_metadata_c
    !> \return .true. if we were able to copy the metadata, .false. otherwise
    function fst24_record_copy_metadata(this,record,what) result(success)
        implicit none
        class(fst_record), intent(inout) :: this !< fst_record instance. If must be a valid record already found in a file
        type(fst_record), target :: record
        integer(C_INT32_T), intent(in), optional :: what

        logical :: success

        integer(C_INT32_T) :: c_result, what_c

        what_c=FST24_META_ALL
        if (present(what)) then
            what_c=what
        end if

        success = .false.
        c_result = fst24_record_copy_metadata_c(this % get_c_ptr(),record % get_c_ptr(), what_c)
        if (c_result == 1) then
            call this % from_c_self()
            success = .true.
        end if
    end function fst24_record_copy_metadata

    !> \copybrief fst24_delete
    !> \return .true. if we were able to delete the record, .false. otherwise
    function fst24_record_delete(this) result(success)
        implicit none
        class(fst_record), intent(inout) :: this !< fst_record instance, must be a valid record previously found in a file
        logical :: success

        integer(C_INT32_T) :: c_result

        success = .false.
        c_result = fst24_delete(this % get_c_ptr())
        if (c_result == 1) then
            call this % from_c_self()
            success = .true.
        end if
    end function fst24_record_delete

    !> Verify that the C and Fortran definitions of the default fst_record match.
    !> \return Whether the fst_record_c type matches exactly the C-defined fst_record
    function fst24_is_default_record_valid() result(is_valid)
        implicit none
        logical :: is_valid

        integer(C_INT32_T) :: c_result
        type(fst_record_c), target :: to_compare

        is_valid = .false.
        c_result = fst24_validate_default_record(c_loc(to_compare), storage_size(to_compare, kind=int64) / 8_int64)
        if (c_result == 0) is_valid = .true.

        if (.not. is_valid) then
            call Lib_Log(APP_LIBFST, APP_ERROR, 'Default FST record is not valid, check your library versions!')
        end if
    end function fst24_is_default_record_valid

    !> \copybrief fst24_record_print
    !> Causes an update of the underlying C struct
    subroutine rmn_fst24_record_print(this)
        implicit none
        class(fst_record), intent(inout) :: this !< fst_record instance we want to print
        call this % make_c_self()
        call fst24_record_print_c(this % get_c_ptr())
    end subroutine rmn_fst24_record_print

    !> \copybrief fst24_record_print_short
    !> Causes an update of the underlying C struct
    !> Refer to fst_record_fields for the meaning of undocumented parameters
    subroutine rmn_fst24_record_print_short(                                                                      &
            this, prefix, print_header, dateo, datev, datestamps, level, data_type, nijk,                         &
            deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234, typvar, nomvar, etiket, metadata)
        implicit none
        class(fst_record), intent(inout) :: this !< fst_record instance whose information we want to print
        character(len=*), intent(in), optional :: prefix !< [optional] Text we want to add at the start of the line
        logical, intent(in), optional :: print_header !< [optional] Whether we want to print a header above the line to name the fields
        logical, intent(in), optional :: dateo, datev, datestamps, level, data_type, nijk
        logical, intent(in), optional :: deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234
        logical, intent(in), optional :: typvar, nomvar, etiket, metadata

        type(fst_record_fields), target :: fields
        character(len=1), dimension(256) :: prefix_c
        integer(C_INT32_T) :: print_header_c

        call this % make_c_self()

        prefix_c(1) = achar(0)
        if (present(prefix)) then
            call strncpy_f2c(prefix, prefix_c, 255)
        end if

        print_header_c = 0
        if (present(print_header)) then
            if (print_header) print_header_c = 1
        end if

        fields = fst24_make_fields(dateo=dateo, datev=datev, datestamps=datestamps, level=level, data_type=data_type, nijk=nijk, &
                                   deet=deet, npas=npas, ip1=ip1, ip2=ip2, ip3=ip3, decoded_ip=decoded_ip,&
                                   grid_info=grid_info, ig1234=ig1234, typvar=typvar, nomvar=nomvar, etiket=etiket, metadata=metadata)

        call fst24_record_print_short_c(this % get_c_ptr(), c_loc(fields), print_header_c, prefix_c)
    end subroutine rmn_fst24_record_print_short

    !> Create a fst_record_fields struct from optional parameters
    function fst24_make_fields(dateo, datev, datestamps, level, data_type, nijk,                         &
            deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234, typvar, nomvar, etiket, metadata) result(fields)
        implicit none
        logical, intent(in), optional :: dateo, datev, datestamps, level, data_type, nijk
        logical, intent(in), optional :: deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234
        logical, intent(in), optional :: typvar, nomvar, etiket, metadata
        type(fst_record_fields) :: fields

        if (present(dateo)) then
            fields % dateo = 0
            if (dateo) fields % dateo = 1
        end if

        if (present(datev)) then
            fields % datev = 0
            if (datev) fields % datev = 1
        end if

        if (present(datestamps)) then
            fields % datestamps = 0
            if (datestamps) fields % datestamps = 1
        end if

        if (present(level)) then
            fields % level = 0
            if (level) fields % level = 1
        end if

        if (present(data_type)) then
            fields % data_type = 0
            if (data_type) fields % data_type = 1
        end if

        if (present(nijk)) then
            fields % nijk = 0
            if (nijk) fields % nijk = 1
        end if

        if (present(deet)) then
            fields % deet = 0
            if (deet) fields % deet = 1
        end if

        if (present(npas)) then
            fields % npas = 0
            if (npas) fields % npas = 1
        end if

        if (present(ip1)) then
            fields % ip1 = 0
            if (ip1) fields % ip1 = 1
        end if

        if (present(ip2)) then
            fields % ip2 = 0
            if (ip2) fields % ip2 = 1
        end if

        if (present(ip3)) then
            fields % ip3 = 0
            if (ip3) fields % ip3 = 1
        end if

        if (present(decoded_ip)) then
            fields % decoded_ip = 0
            if (decoded_ip) fields % decoded_ip = 1
        end if

        if (present(grid_info)) then
            fields % grid_info = 0
            if (grid_info) fields % grid_info = 1
        end if

        if (present(ig1234)) then
            fields % ig1234 = 0
            if (ig1234) fields % ig1234 = 1
        end if

        if (present(typvar)) then
            fields % typvar = 0
            if (typvar) fields % typvar = 1
        end if

        if (present(nomvar)) then
            fields % nomvar = 0
            if (nomvar) fields % nomvar = 1
        end if

        if (present(etiket)) then
            fields % etiket = 0
            if (etiket) fields % etiket = 1
        end if

        if (present(metadata)) then
            fields % metadata = 0
            if (metadata) fields % metadata = 1
        end if
    end function fst24_make_fields

    function fst24_make_fields_from_string(string) result(fields)
        implicit none
        type(fst_record_fields) :: fields
        character(len=*)        :: string

        if (len_trim(string)>0) then
            fields % dateo = 0
            fields % datev = 0
            fields % datestamps = 0
            fields % level = 0
            fields % data_type = 0
            fields % nijk = 0
            fields % deet = 0
            fields % npas = 0
            fields % ip1 = 0
            fields % ip2 = 0
            fields % ip3 = 0
            fields % decoded_ip = 0
            fields % ig1234 = 0
            fields % grid_info = 0
            fields % typvar = 0
            fields % nomvar = 0
            fields % etiket = 0
            fields % metadata = 0
        endif

        if (index(string,'DATEO')>0) then
            fields % dateo = 1
        end if

        if (index(string,'DATEV')>0) then
           fields % datev = 1
        end if

        if (index(string,'STAMP')>0) then
            fields % datestamps = 1
        end if

        if (index(string,'LEVEL')>0) then
           fields % level = 1
        end if

        if (index(string,'DATYP')>0 .or. index(string, 'DATA_TYPE')>0) then
           fields % data_type = 1
        end if

        if (index(string,'NIJK')>0) then
            fields % nijk = 1
        end if

        if (index(string,'DEET')>0) then
            fields % deet = 1
        end if

        if (index(string,'NPAS')>0) then
            fields % npas = 1
        end if

        if (index(string,'IP1')>0) then
            fields % ip1 = 1
        end if

        if (index(string,'IP2')>0) then
            fields % ip2 = 1
        end if

        if (index(string,'IP3')>0) then
            fields % ip3 = 1
        end if

        if (index(string,'IPS')>0) then
            fields % ip1 = 1
            fields % ip2 = 1
            fields % ip3 = 1
        end if 

        if (index(string,'DECODE')>0) then
            fields % decoded_ip = 1
        end if

        if (index(string,'GRID')>0) then
            fields % grid_info = 1
        end if

        if (index(string,'IGS')>0) then
            fields % ig1234 = 1
        end if

        if (index(string,'TYPVAR')>0) then
            fields % typvar = 1
        end if

        if (index(string,'NOMVAR')>0) then
            fields % nomvar = 1
        end if

        if (index(string,'ETIKET')>0) then
            fields % etiket = 1
        end if

        if (index(string,'META')>0) then
            fields % metadata = 1
        end if

    end function fst24_make_fields_from_string

end module rmn_fst24_record

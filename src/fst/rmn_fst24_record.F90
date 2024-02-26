!> \file rmn_fst24_record.F90
!> Encapsulation of FST record information into a derived type 

module rmn_fst24_record
    use App
    use f_c_strings_mod
    use rmn_common
    implicit none
    private

    include 'fst24_interface.inc'
#include "fst24_record.hf"

    public :: fst_record_fields, fst_record_c
    public :: fst24_is_default_record_valid, fst24_make_fields

    type, public :: fst_record
        type(fst_record_c), private :: c_self !< bind(C) version of this struct, to interface with C implementation

        type(C_PTR) :: data     = C_NULL_PTR
        type(C_PTR) :: metadata = C_NULL_PTR

        integer(C_INT64_T) :: flags    = 0
        integer(C_INT64_T) :: dateo    = -1
        integer(C_INT64_T) :: datev    = -1

        integer(C_INT32_T) :: datyp = -1
        integer(C_INT32_T) :: dasiz = -1
        integer(C_INT32_T) :: npak  = -1
        integer(C_INT32_T) :: ni    = -1
        integer(C_INT32_T) :: nj    = -1
        integer(C_INT32_T) :: nk    = -1

        integer(C_INT32_T) :: deet  = -1
        integer(C_INT32_T) :: npas  = -1

        integer(C_INT32_T) :: ip1   = -1
        integer(C_INT32_T) :: ip2   = -1
        integer(C_INT32_T) :: ip3   = -1

        integer(C_INT32_T) :: ig1   = -1
        integer(C_INT32_T) :: ig2   = -1
        integer(C_INT32_T) :: ig3   = -1
        integer(C_INT32_T) :: ig4   = -1

        character(len=2)  :: typvar = ''
        character(len=1)  :: grtyp  = ''
        character(len=4)  :: nomvar = ''
        character(len=12) :: etiket = ''
    contains

        procedure, pass :: make_c_self => fst24_record_make_c_self
        procedure, pass :: from_c_self => fst24_record_from_c_self
        procedure, pass :: get_c_ptr => fst24_record_get_c_ptr

        procedure, pass :: has_same_info => fst24_record_has_same_info
        procedure, pass :: read          => fst24_record_read
        procedure, pass :: read_metadata => fst24_record_read_metadata

        procedure, pass :: print        => fst24_record_print
        procedure, pass :: print_short  => fst24_record_print_short

    end type fst_record

contains

    !> Fill the fields of c_self with current values
    subroutine fst24_record_make_c_self(this)
        implicit none
        class(fst_record), intent(inout) :: this

        this % c_self % data     = this % data
        this % c_self % metadata = this % metadata

        this % c_self % flags = this % flags
        this % c_self % dateo = this % dateo
        this % c_self % datev = this % datev

        this % c_self % datyp = this % datyp
        this % c_self % dasiz = this % dasiz
        this % c_self % npak  = this % npak
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

    !> Retrieve values from c_self into this
    subroutine fst24_record_from_c_self(this)
        implicit none
        class(fst_record), intent(inout) :: this

        this % data     = this % c_self % data
        this % metadata = this % c_self % metadata

        this % flags = this % c_self % flags
        this % dateo = this % c_self % dateo
        this % datev = this % c_self % datev
                            
        this % datyp = this % c_self % datyp
        this % dasiz = this % c_self % dasiz
        this % npak  = this % c_self % npak
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

    function fst24_record_get_c_ptr(this) result(ptr)
        implicit none
        class(fst_record), intent(in), target :: this
        type(C_PTR) :: ptr
        ptr = c_loc(this % c_self)
    end function fst24_record_get_c_ptr

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

    function fst24_record_read(this) result(success)
        implicit none
        class(fst_record), intent(inout) :: this
        logical :: success

        integer(C_INT32_T) :: c_status

        success = .false.
        c_status = fst24_read(this % get_c_ptr())
        if (c_status > 0) then
            call this % from_c_self()
            success = .true.
        end if
    end function fst24_record_read

    function fst24_record_read_metadata(this) result(success)
        implicit none
        class(fst_record), intent(inout) :: this
        logical :: success

        type(C_PTR) :: metadata

        success = .false.
        metadata = fst24_read_metadata(this % get_c_ptr())
        if (c_associated(metadata)) then
            call this % from_c_self()
            success = .true.
        end if
    end function fst24_record_read_metadata

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

    subroutine fst24_record_print(this)
        implicit none
        class(fst_record), intent(inout) :: this
        call this % make_c_self()
        call fst24_record_print_c(this % get_c_ptr())
    end subroutine fst24_record_print

    subroutine fst24_record_print_short(                                                                            &
            this, prefix, print_header, dateo, datev, datestamps, level, datyp, ni, nj, nk,                         &
            deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234, typvar, nomvar, etiket)
        implicit none
        class(fst_record), intent(inout) :: this
        character(len=*), intent(in), optional :: prefix
        logical, intent(in), optional :: print_header
        logical, intent(in), optional :: dateo, datev, datestamps, level, datyp, ni, nj, nk
        logical, intent(in), optional :: deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234
        logical, intent(in), optional :: typvar, nomvar, etiket

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

        fields = fst24_make_fields(dateo=dateo, datev=datev, datestamps=datestamps, level=level, datyp=datyp, ni=ni,    &
                                   nj=nj, nk=nk, deet=deet, npas=npas, ip1=ip1, ip2=ip2, ip3=ip3, decoded_ip=decoded_ip,&
                                   grid_info=grid_info, ig1234=ig1234, typvar=typvar, nomvar=nomvar, etiket=etiket)

        call fst24_record_print_short_c(this % get_c_ptr(), c_loc(fields), print_header_c, prefix_c)
    end subroutine fst24_record_print_short

    function fst24_make_fields(dateo, datev, datestamps, level, datyp, ni, nj, nk,                         &
            deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234, typvar, nomvar, etiket) result(fields)
        implicit none
        logical, intent(in), optional :: dateo, datev, datestamps, level, datyp, ni, nj, nk
        logical, intent(in), optional :: deet, npas, ip1, ip2, ip3, decoded_ip, grid_info, ig1234
        logical, intent(in), optional :: typvar, nomvar, etiket
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

        if (present(datyp)) then
            fields % datyp = 0
            if (datyp) fields % datyp = 1
        end if

        if (present(ni)) then
            fields % ni = 0
            if (ni) fields % ni = 1
        end if

        if (present(nj)) then
            fields % nj = 0
            if (nj) fields % nj = 1
        end if

        if (present(nk)) then
            fields % nk = 0
            if (nk) fields % nk = 1
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

    end function fst24_make_fields

end module rmn_fst24_record

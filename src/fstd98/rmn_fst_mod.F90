
module rmn_fst
    use iso_c_binding
    use f_c_strings_mod
    implicit none
    private

    public :: fstouv, fstecr, fstfrm, fstlir
    public :: fst_data_length, fstapp, fstckp
    public :: set_missing_value_flags

#include <fstd98/c_fstd98_interface.hf>

    interface
    subroutine set_missing_value_flags(missingFloatVal, missingIntVal, missingUIntVal, missingDoubleVal,    &
                                       missingShortVal, missingUShortVal, missingByteVal, missingUByteVal)  &
            bind (C, name = 'set_missing_value_flags')
        import C_FLOAT, C_INT32_T, C_DOUBLE, C_INT16_T, C_INT8_T
        real(C_FLOAT),      intent(in) :: missingFloatVal
        real(C_DOUBLE),     intent(in) :: missingDoubleVal
        integer(C_INT32_T), intent(in) :: missingIntVal, missingUIntVal
        integer(C_INT16_T), intent(in) :: missingShortVal, missingUShortVal
        integer(C_INT8_T),  intent(in) :: missingByteVal, missingUByteVal
    end subroutine set_missing_value_flags
    end interface

contains

    function fstouv(iun, options) result(status)
        implicit none
        integer(C_INT32_T), intent(in), value :: iun
        character(len=*),   intent(in)        :: options
        integer(C_INT32_T) :: status
        status = c_fstouv(iun, f_c_string(options))
    end function fstouv

    function fstfrm(iun) result(status)
        implicit none
        integer(C_INT32_T), intent(in), value :: iun
        integer(C_INT32_T) :: status
        status = c_fstfrm(iun)
    end function fstfrm

    function fstecr(field, work, npak, iun, date, deet, npas, ni, nj, nk,   &
                    ip1, ip2, ip3, typvar, nomvar, etiket,                  &
                    grtyp, ig1, ig2, ig3, ig4, datyp, rewrite)              &
                result(status)
        implicit none
#define IgnoreTypeKindRank field, work
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
        integer(C_INT), intent(IN), value :: iun
        integer(C_INT), intent(IN) :: npak, date, deet, npas, ni, nj, nk, datyp
        integer(C_INT), intent(IN) :: ip1, ip2, ip3, ig1, ig2, ig3, ig4
        character(len=*), intent(IN) :: typvar, nomvar, etiket, grtyp
        logical, intent(IN) :: rewrite
        integer(C_INT) :: status

        character(len=4)  :: nom
        character(len=2)  :: typ
        character(len=1)  :: gty
        character(len=12) :: eti

        integer(C_INT32_T) :: c_rewrite

        nom = nomvar
        typ = typvar
        gty = grtyp
        eti = etiket

        c_rewrite = 0
        if (rewrite) c_rewrite = 1

        status = c_fstecr(field, work, npak, iun, date, deet, npas, ni, nj, nk,                                     &
                        ip1, ip2, ip3, f_c_string(typ), f_c_string(nom), f_c_string(eti), f_c_string(gty),          &
                        ig1, ig2, ig3, ig4, datyp, c_rewrite)

    end function fstecr

    function fst_data_length(length) result(status)
        implicit none
        integer(C_INT), intent(IN), value :: length
        integer(C_INT) :: status
        status = c_fst_data_length(length)
    end function fst_data_length

    function fstlir(field, iun, ni, nj, nk, datev, etiket, ip1, ip2, ip3, typvar, nomvar) result(handle)
        implicit none
#define IgnoreTypeKindRank field
#define ExtraAttributes 
#include <rmn/IgnoreTypeKindRank.hf>
        integer(C_INT), intent(IN), value :: iun
        integer(C_INT), intent(OUT) :: ni, nj, nk
        integer(C_INT), intent(IN)  :: datev, ip1, ip2, ip3
        character(len=*), intent(IN) :: typvar, nomvar, etiket
        integer(C_INT) :: handle
        character(len=5)  :: nom
        character(len=3)  :: typ
        character(len=13) :: eti
        call strncpy_f2c(typvar, typ, 3)
        call strncpy_f2c(nomvar, nom, 5)
        call strncpy_f2c(etiket, eti, 13)
        handle = c_fstlir(field, iun, ni, nj, nk, datev, eti, ip1, ip2, ip3, typ, nom)
    end function fstlir

    function fstapp(iun, option) result(status)
        implicit none
        integer(C_INT), intent(IN), value :: iun
        character(len=*), intent(IN) :: option
        integer(C_INT) :: status
        status = c_fstapp(iun, f_c_string(option))
    end function fstapp

    function fstckp(iun) result (status)
        implicit none
        integer(C_INT), intent(IN), value :: iun
        integer(C_INT) :: status
        status = c_fstckp(iun)
    end function fstckp
end module rmn_fst

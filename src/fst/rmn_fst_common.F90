
module rmn_fst_common
    use rmn_common
    implicit none

    integer, parameter :: FST_NO   = 0
    integer, parameter :: FST_YES  = 1
    integer, parameter :: FST_SKIP = 2

    interface
    subroutine set_missing_value_flags(missingFloatVal, missingIntVal, missingUIntVal, missingDoubleVal,    &
                                       missingShortVal, missingUShortVal, missingByteVal, missingUByteVal)  &
            bind (C, name = 'set_missing_value_flags')
        import C_FLOAT, C_INT32_T, C_DOUBLE, C_INT16_T, C_INT8_T
        implicit none
        real(C_FLOAT),      intent(in) :: missingFloatVal
        real(C_DOUBLE),     intent(in) :: missingDoubleVal
        integer(C_INT32_T), intent(in) :: missingIntVal, missingUIntVal
        integer(C_INT16_T), intent(in) :: missingShortVal, missingUShortVal
        integer(C_INT8_T),  intent(in) :: missingByteVal, missingUByteVal
    end subroutine set_missing_value_flags

    function get_missing_value_flags(missingFloatVal, missingIntVal, missingUIntVal, missingDoubleVal,    &
                                     missingShortVal, missingUShortVal, missingByteVal, missingUByteVal)  &
            result(status) bind (C, name = 'get_missing_value_flags')
        import C_FLOAT, C_INT32_T, C_DOUBLE, C_INT16_T, C_INT8_T
        implicit none
        real(C_FLOAT),      intent(out) :: missingFloatVal
        real(C_DOUBLE),     intent(out) :: missingDoubleVal
        integer(C_INT32_T), intent(out) :: missingIntVal, missingUIntVal
        integer(C_INT16_T), intent(out) :: missingShortVal, missingUShortVal
        integer(C_INT8_T),  intent(out) :: missingByteVal, missingUByteVal
        integer(C_INT32_T) :: status
    end function get_missing_value_flags

    end interface
end module rmn_fst_common

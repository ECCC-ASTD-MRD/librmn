module rmn_primitives
    use iso_c_binding
    use rmn_fnom
    implicit none
    private

    public :: fnom, fclos
    public :: wkoffit, exfin, exdb

    interface
        function c_wkoffit(filepath, path_length) result(status) bind(C, name = 'c_wkoffit')
            import :: C_CHAR, C_INT, C_INT32_T
            implicit none
            character(C_CHAR), dimension(*), intent(in) :: filepath
            integer(C_INT), intent(in), value :: path_length
            integer(C_INT32_T) :: status
        end function c_wkoffit

        integer function exfin(in_titre, revis, flag)
            implicit none
            !> Title. Only the first 90 characters will be printed
            character(len = *), intent(in) :: in_titre
            !> End message when called from exfin
            character(len = *), intent(in) :: revis
            !> Unused. Kept for backward compatibility
            character(len = *), intent(in) :: flag
        end function exfin

        integer function exdb(in_titre, revis, flag)
            implicit none
            character(len = *) :: in_titre, revis, flag
        end function exdb
    end interface

contains

    function wkoffit(filepath) result(status)
        use iso_c_binding
        implicit none
        character(len=*), intent(in) :: filepath
        integer(C_INT32_T) :: status
        status = c_wkoffit(trim(filepath) // c_null_char, len_trim(filepath))
    end function wkoffit

end module rmn_primitives

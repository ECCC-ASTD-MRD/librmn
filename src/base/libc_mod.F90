module rmn_libc
    use iso_c_binding
    implicit none

    interface
        function c_strncpy(dest, src, count) result(dest_copy) bind(C, name = 'strncpy')
            import :: C_PTR, C_SIZE_T
            implicit none
            type(C_PTR), intent(in), value :: dest, src
            integer(C_SIZE_T), value :: count
            type(C_PTR) :: dest_copy
        end function

        function c_memset(s, byte, n) result(p) bind(C, name='memset')
            import :: C_PTR, C_SIZE_T, C_INT
            implicit none
            type(C_PTR), intent(IN), value :: s
            integer(C_INT), intent(IN), value :: byte
            integer(C_SIZE_T), intent(IN), value :: n
            type(C_PTR) :: p
        end function

        function libc_malloc(sz) result(ptr) BIND(C, name='malloc')
            import :: C_SIZE_T, C_PTR
            implicit none
            integer(C_SIZE_T), intent(IN), value :: sz
            type(C_PTR) :: ptr
        end function

        subroutine libc_free(ptr) BIND(C, name='free')
            import :: C_PTR
            implicit none
            type(C_PTR), intent(IN), value :: ptr
        end subroutine

        !> This may shadow the intrinsic of the same name!
        subroutine exit(status) bind(C, name = 'exit')
            import :: C_INT
            implicit none
            integer(C_INT), value :: status
        end subroutine

        subroutine c_exit(status) bind(C, name = 'exit')
            import :: C_INT
            implicit none
            integer(C_INT), value :: status
        end subroutine
    end interface

    interface
        function int_c_strlen(str) result(strlen) bind(C, name='strlen')
            import :: C_PTR, C_SIZE_T
            implicit none

            type(C_PTR), intent(IN), value :: str
            integer(C_SIZE_T) :: strlen
        end function

        function int_c_strnlen(str, maxlen) result(strlen) bind(C, name='strnlen')
            import :: C_PTR, C_CHAR, C_SIZE_T
            implicit none

            type(C_PTR), intent(IN), value :: str
            integer(C_SIZE_T), intent(IN), value :: maxlen
            integer(C_SIZE_T) :: strlen
        end function
    end interface

    private :: int_c_strlen, int_c_strnlen

    contains
        function c_strlen(str) result(strlen)
            import :: C_PTR, C_SIZE_T
            implicit none

            type(C_PTR), intent(IN), value :: str
            integer(C_SIZE_T) :: strlen

            if (c_associated(str)) then
                strlen = int_c_strlen(str)
            else
                strlen = 0
            end if
        end function

        function c_strnlen(str, maxlen) result(strlen)
            import :: C_PTR, C_CHAR, C_SIZE_T
            implicit none

            type(C_PTR), intent(IN), value :: str
            integer(C_SIZE_T), intent(IN), value :: maxlen
            integer(C_SIZE_T) :: strlen

            if (c_associated(str)) then
                strlen = int_c_strnlen(str, maxlen)
            else
                strlen = 0
            end if
        end function
end module rmn_libc

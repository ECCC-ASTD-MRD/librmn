
module rmn_libc
    use iso_c_binding
    implicit none
    interface
        function c_strnlen(str, maxlen) result(strlen) bind(C, name='strnlen')
            import :: C_CHAR, C_SIZE_T
            implicit none
            character(C_CHAR), dimension(*), intent(IN) :: str
            integer(C_SIZE_T), intent(IN), value :: maxlen
            integer(C_SIZE_T) :: strlen
        end function c_strnlen

        function c_strlen(str) result(strlen) bind(C, name='strlen')
            import :: C_PTR, C_SIZE_T
            implicit none
            type(C_PTR), intent(IN), value :: str
            integer(C_SIZE_T) :: strlen
        end function c_strlen

        function c_strncpy(dest, src, count) result(dest_copy) bind(C, name = 'strncpy')
            import :: C_PTR, C_SIZE_T
            implicit none
            type(C_PTR), intent(in), value :: dest, src
            integer(C_SIZE_T), value :: count
            type(C_PTR) :: dest_copy
        end function c_strncpy

        function memset(s, byte, n) result(p) bind(C, name='memset')
            import :: C_PTR, C_SIZE_T, C_INT
            implicit none
            type(C_PTR), intent(IN), value :: s
            integer(C_INT), intent(IN), value :: byte
            integer(C_SIZE_T), intent(IN), value :: n
            type(C_PTR) :: p
        end function memset

        function libc_malloc(sz) result(ptr) BIND(C, name='malloc')
            import :: C_SIZE_T, C_PTR
            implicit none
            integer(C_SIZE_T), intent(IN), value :: sz
            type(C_PTR) :: ptr
        end function libc_malloc

        subroutine libc_free(ptr) BIND(C, name='free')
            import :: C_PTR
            implicit none
            type(C_PTR), intent(IN), value :: ptr
        end subroutine libc_free

        subroutine exit(status) bind(C, name = 'exit')
            import :: C_INT
            implicit none
            integer(C_INT), value :: status
        end subroutine exit

        subroutine c_exit(status) bind(C, name = 'exit')
            import :: C_INT
            implicit none
            integer(C_INT), value :: status
        end subroutine c_exit
    end interface
end module rmn_libc

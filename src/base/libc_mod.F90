
module rmn_libc
    use iso_c_binding
    implicit none
    interface
        subroutine exit(status) bind(C, name = 'exit')
            import :: C_INT
            implicit none
            integer(C_INT), value :: status
        end subroutine exit
    end interface
end module rmn_libc

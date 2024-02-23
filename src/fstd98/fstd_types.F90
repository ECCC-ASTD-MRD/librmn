
module rmn_fstd_types
    use rmn_common
    implicit none

    ! character(len=32), dimension(9), parameter :: FST_TYPE_NAMES = ['bit', 'fortran real', 'unsigned int', 'byte', 'signed int', 'real', 'float16', 'string', 'complex']
    integer(C_INT32_T), parameter :: FST_TYPE_BINARY    = 0
    integer(C_INT32_T), parameter :: FST_TYPE_FREAL     = 1
    integer(C_INT32_T), parameter :: FST_TYPE_UNSIGNED  = 2
    integer(C_INT32_T), parameter :: FST_TYPE_FCHAR     = 3
    integer(C_INT32_T), parameter :: FST_TYPE_SIGNED    = 4
    integer(C_INT32_T), parameter :: FST_TYPE_REAL      = 5
    integer(C_INT32_T), parameter :: FST_TYPE_IEEE_16   = 6
    integer(C_INT32_T), parameter :: FST_TYPE_STRING    = 7
    integer(C_INT32_T), parameter :: FST_TYPE_COMPLEX   = 8

    integer(C_INT32_T), parameter :: FST_TYPE_TURBOPACK = 128
    integer(C_INT32_T), parameter :: FST_TYPE_MAGIC     = 810
end module rmn_fstd_types

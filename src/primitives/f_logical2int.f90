!>  handler function call in C to store/retreive fortran logical
subroutine f_logical2int(dest, src, n)
    implicit none

    integer :: n
    integer, dimension(n) :: dest
    logical, dimension(n) :: src

    dest = 0
    where(src) dest = 1
end subroutine f_logical2int


!  handler function call in C to store/retreive fortran logical
subroutine f_int2logical(dest, src, n)
    implicit none

    integer :: n
    integer, dimension(n) :: src
    logical, dimension(n) :: dest

    dest = src .ne. 0
end subroutine f_int2logical


!  handler function call in C to store/retreive fortran logical
subroutine f_logical_move(dest, src, n)
  implicit none

  integer :: n
  logical, dimension(n) :: dest, src

  dest = src
end subroutine
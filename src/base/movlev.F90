!> \file


!> Copy array content
subroutine movlev(src, dst, nb)
    implicit none

    !> Number of items in the array
    integer, intent(in) :: nb
    !> Source array
    integer, dimension(nb), intent(in) :: src
    !> Destination array
    integer, dimension(nb), intent(out) :: dst

    integer :: i

    do i = 1, nb
        dst(i) = src(i)
    end do
end subroutine movlev


!> Copy array content
subroutine move3216(src, dst, nb)
    use rmn_common
    implicit none

    !> Number of items in the array
    integer, intent(in) :: nb
    !> Source array
    integer, dimension(nb), intent(in) :: src
    !> Destination array
    integer, dimension(nb), intent(out) :: dst

#if defined(Little_Endian)
    ! Little endian
    integer :: i

    do i = 1, nb
        dst(i) = ior( ishft(src(i), 16), iand( ishft(src(i), -16), 65535 ) )
    end do
#else
    ! Big endian
    call movlev(src, dst, nb)
#endif
end subroutine


!> Copy array content
subroutine move832(src, dst, nb)
    use rmn_common
    implicit none

    !> Number of items in the array
    integer, intent(in) :: nb
    !> Source array
    integer, dimension(nb), intent(in) :: src
    !> Destination array
    integer, dimension(nb), intent(out) :: dst

#if defined(Little_Endian)
    integer :: i
    ! Little endian
    do i = 1, nb
        dst(i) = ior(ior(ior(ishft(iand(src(i), 255), 24), &
                                iand(ishft(src(i),   8), 16711680)), &
                                iand(ishft(src(i),  -8), 65280)), &
                                iand(ishft(src(i), -24), 255))
    end do
#else
    ! Big endian
    call movlev(src, dst, nb)
#endif
end subroutine

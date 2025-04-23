subroutine lorenzo_mask_fill(fld, masque, ni, nj, methode)
    implicit none

    integer, intent(in) :: ni, nj
    real, dimension(:, :),  intent(inout) :: fld(ni, nj)
    integer, dimension(:, :), intent(in) :: masque(ni, nj)
    !> Method: 1 for lorenzo, 2 for minimum, 3 for horizontal fill
    integer, intent(in) :: methode

    integer :: i, j, ii, last_i
    real :: rmin, rmax

    rmin = minval(fld)
    rmax = maxval(fld)

    select case (methode)
    case(1)
        do j = 2, nj
            do i = 2, ni
                if (masque(i, j) == 0) then
                    fld(i, j) = fld(i - 1, j) + fld(i, j - 1) - fld(i - 1, j - 1)
                    if (fld(i, j) < rmin) fld(i, j) = rmin
                    if (fld(i, j) > rmax) fld(i, j) = rmax
                endif
            enddo
        enddo
    case(2)
        do j = 1, nj
            do i = 1, ni
                if (masque(i, j) == 0) then
                    fld(i, j) = rmin
                endif
            enddo
        enddo
    case(3)
        do j = 1, nj
            i = 1
            last_i = 1
            if (masque(i, j) == 0) then  ! On recherche le 1e point non nul
                do
                    i = i + 1
                    if (masque(i, j) /= 0) exit
                    if (i == ni) then
                        fld(1:ni, j) = rmin
                        exit
                    endif
                enddo
                if (i < ni) then
                    do ii = last_i, i - 1
                        fld(ii, j) = fld(i, j)
                    enddo
                endif
            endif

            do
                i = i + 1
                if (i >= ni + 1) exit
                if (masque(i, j) == 0) then
                    fld(i, j) = fld(last_i, j)
                endif
                last_i = i
            enddo
        enddo
    end select
end subroutine lorenzo_mask_fill
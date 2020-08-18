!> Copier le contenu de src dans dst
!
!> @param[in] src Source pour la copie
!> @param[out] dst Destination pour la copie
!> @param[in] Taille du tableau
subroutine movlev(src, dst, nb)
    implicit none

    integer, intent(in) :: nb
    integer, dimension(nb), intent(in) :: src
    integer, dimension(nb), intent(out) :: dst

    integer :: i

    do i = 1, nb
        dst(i) = src(i)
    end do
end subroutine movlev


!> Copier le contenu de src dans dst
!
!> @param[in] src Source pour la copie
!> @param[out] dst Destination pour la copie
!> @param[in] Taille du tableau
subroutine move3216(src, dst, nb)
    implicit none

    integer, intent(in) :: nb
    integer, dimension(nb), intent(in) :: src
    integer, dimension(nb), intent(out) :: dst

    integer :: i

    integer(kind = 4) :: litend = 1
    integer(kind = 2), dimension(2) :: little
    equivalence (little(1), litend)

    if (little(1) == 1) then
        ! Little endian
        do i = 1, nb
            dst(i) = ior( ishft(src(i), 16), iand( ishft(src(i), -16), 65535 ) )
        end do
    else
        ! Big endian
        call movlev(src, dst, nb)
    end if
end subroutine


!> Copier le contenu de src dans dst
!
!> @param[in] src Source pour la copie
!> @param[out] dst Destination pour la copie
!> @param[in] Taille du tableau
subroutine move832(src, dst, nb)
    implicit none

    integer, intent(in) :: nb
    integer, dimension(nb), intent(in) :: src
    integer, dimension(nb), intent(out) :: dst

    integer :: i

    integer(kind = 4) :: litend = 1
    integer(kind = 2), dimension(2) :: little
    equivalence (little(1), litend)

    if (little(1) == 1) then
        ! Little endian
        do i = 1, nb
            dst(i) = ior(ior(ior(ishft(iand(src(i), 255), 24), &
                                 iand(ishft(src(i),   8), 16711680)), &
                                 iand(ishft(src(i),  -8), 65280)), &
                                 iand(ishft(src(i), -24), 255))
        end do
    else
        ! Big endian
        call movlev(src, dst, nb)
    end if
end subroutine

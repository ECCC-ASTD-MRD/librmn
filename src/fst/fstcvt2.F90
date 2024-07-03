!> Convert between hollerith and character
integer function fstcvt2( nom, typ, etik, grtp, cnom, ctyp, cetik, cgrtp, holacar)
    implicit none

    !> Nomvar (Hollerith *4)
    integer, intent(inout) :: nom
    !> Typvar (Hollerith *2)
    integer, intent(inout) :: typ
    !> Etiket (Hollerith *12)
    integer, intent(inout) :: etik(3)
    !> Grtyp (Hollerith *2)
    integer, intent(inout) :: grtp
    !> Nomvar
    character(len = *), intent(inout) :: cnom
    !> Typvar
    character(len = *), intent(inout) :: ctyp
    !> Etiket
    character(len = *), intent(inout) :: cetik
    !> Grtyp
    character(len = *), intent(inout) :: cgrtp
    !> Convert from Hollerith to character if true, convert from character to Hollerith if false
    logical, intent(in) :: holacar

    integer :: i

    Fstcvt2 = 0
    if (holacar) then
        ! transfer string d'une location hollerith en caractere
        if (nom.eq. -1) then
            cnom = ' '
        else
            if (len(cnom) .gt. 2) then
                write(cnom,'(a4)') nom
            else
                write(cnom,'(a2)') nom
            endif
        endif
        if (typ.eq. -1) then
            ctyp = ' '
        else
            if (len(ctyp) .gt. 1) then
                write(ctyp,'(a2)') typ
            else
                write(ctyp,'(a1)') typ
            endif
        endif
        if (grtp.eq. -1) then
            cgrtp = ' '
        else
            write(cgrtp,'(a1)') grtp
        endif
        cetik = ' '
        if (etik(1).eq. -1) then
            cetik = ' '
        else
            if (len(cetik) .gt. 8) then
                write(cetik, '(3a4)')(etik(i),i=1,3)
            else
                write(cetik,'(2a4)')(etik(i),i=1,2)
            endif
        endif
    else
        ! transfer string d'une location caractere en hollerith*
        read(cnom,'(a4)') nom
        if (cnom.eq. ' ') then
            nom = -1
        endif
        read(ctyp,'(a2)') typ
        if (ctyp.eq. ' ') then
            typ = -1
        endif
        read(cgrtp,'(a1)') grtp
        if (cgrtp.eq. ' ') then
            grtp = -1
        endif
        if (len(cetik) .gt. 8) then
            read(cetik, '(3a4)') (etik(i),i=1,3)
        else
            read(cetik,'(2a4)') (etik(i),i=1,2)
        endif
        if (cetik.eq. ' ') then
            etik(1) = -1
        endif
    endif
end

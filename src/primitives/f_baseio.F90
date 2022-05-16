! RMNLIB - Library of useful routines for C and FORTRAN programming
! Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
!                          Environnement Canada
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.



integer function qqqf7op(iun, name, lrec, rndflag, unfflag, lmult)
    implicit none
    integer, intent(in) :: iun
    character(len = *), intent(in) :: name
    integer, intent(in) :: lrec
    integer, intent(in) :: rndflag
    integer, intent(in) :: unfflag
    !> No longer used, but kept for backward compatibility
    integer, intent(in) :: lmult

    integer :: lng

    integer :: i
    logical :: opened
    integer :: d77mult
    integer :: stat
    integer, parameter :: scrap = 42

    ! Figure out the value of d77mult
    i = 100
    opened = .true.

    ! Find an unused unit number
    do while(opened .and. i > 1)
        i = i - 1
        inquire (unit = i, opened = opened, iostat = stat)
    enddo

    open(unit = i, ACCESS = 'DIRECT', FORM = 'UNFORMATTED', STATUS = 'SCRATCH', RECL = 1)
    d77mult = 4                       ! this value if write generates an error
    write(i, rec = 1, err = 66) scrap ! there will be an error if d77mult needs to be 4 (recl in bytes)
    d77mult = 1                       ! no error, recl had room for 16 bytes, recl is in words
66  close(unit = i)


    qqqf7op = 0
    lng = len(name)
    ! print *,'opening file ',name(1:lng),' as unit ',iun
    ! print *,'lrec=',lrec,' flags = ',rndflag,unfflag
    ! print *,'len=',lng
    if (rndflag == 1) then
        if (unfflag == 1) then
            ! print *,'ACCESS=DIRECT,RECL=',lrec
            OPEN(iun, FILE = name(1:lng), ACCESS = 'DIRECT', RECL = lrec * d77mult, ERR = 77)
        else
            ! print *,'ACCESS=DIRECT,RECL=',lrec*4
            OPEN(iun, FILE = name(1:lng), ACCESS = 'DIRECT', FORM = 'FORMATTED', RECL = lrec * 4, ERR = 77)
        endif
    else
        if ((name(1:lng) == 'input') .or. (name(1:lng) == '$input') &
            .or. (name(1:lng)=='output') .or. (name(1:lng)=='$output') &
            .or. (name(1:lng)=='$in') .or. (name(1:lng)=='$out')) &
        then
            ! print *,' STDIN or STDOUT'
            return
        else
            if (unfflag == 1) then
                ! print *,'UNFORMATTED open'
                OPEN(iun, FILE = name(1:lng), FORM = 'UNFORMATTED', ERR = 77)
            else
                ! print *,'FORMATTED open'
                OPEN(iun, FILE=name(1:lng), FORM = 'FORMATTED', ERR = 77)

            endif
        endif
    endif
    return

77  continue
    qqqf7op = -1
end


integer function ftnclos(iun)
    implicit none
    integer, intent(in) :: iun

    ftnclos = 0
    CLOSE(iun)
end

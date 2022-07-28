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

!> Get timestamp
!> \author M. Valin
!> \date 1983
!> Se sert du fichier fortran 88 et le retourne (return) apres usage.
integer function iopdatm(flag)
    implicit none

    ! CHAINE DE CARACTERES (7), flag PEUT ETRE
    ! - 'NON' AUQUEL CAS IOPDATM RENVOIE 2010101011.
    ! - UNE CLE DE 7 CARACTERES OU MOINS CORRESPONDANT
    !   A D'AUTRES "DATE TIME STAMP".
    ! - UN NOMBRE DE 7 CHifFRES YYJJJZZ OU YY EST L'ANNEE,
    !   JJJ LE JOUR DANS L'ANNEE (1 A 366) ET ZZ L'HEURE
    !   (00 - 24).
    ! - IOPDATM RENVOIE ALORS LE DATE TIME STAMP QUI
    !   CORRESPOND A CETTE DATE ET CETTE HEURE.
    ! - UN NOM DE FICHier. IOPDATM LIRA LE PREMier MOT DE CE
    !   FICHier EN LE CONSIDERANT COMME UN DATE TIME STAMP.
    !   SI IOPDATM RENCONTRE UNE ERREUR, LE "STAMP" RENVOYE
    !   SERA 1010101011.
    character(len = *), intent(in) :: flag

    integer :: istamp, jd, iyr, imon, iday, istmp, length, iun
    integer :: ier
    character(len = 10) :: ifflg, iquoi, idnt
    character(len = 128) :: datarep
    integer, external :: fnom
    integer, external :: fclos
    external system_time
    external :: jdatec
    external :: datec
    external :: newdate
    integer :: i1, i2
    character(len = 26) :: upper, lower

    upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    lower = 'abcdefghijklmnopqrstuvwxyz'
    iun = 88
    ifflg = flag
    do i1 = 1, len(ifflg)
        i2 = index(lower, ifflg(i1:i1))
        if (i2 > 0) ifflg(i1:i1) = upper(i2:i2)
    enddo
    read(ifflg,'(A7)') iquoi
    if (ifflg == 'OUI') iquoi = 'OPRUN'
    istamp = 010101011
    if (ifflg == 'NON') then
        istamp = 010101011
    else if (ifflg == 'NOW') then
        call system_time(i1, i2)
        call newdate(istamp, i1, i2, 3)
    else
        read(ifflg, '(I10)', err = 100) istamp
        if (istamp < 9936624) then  ! it is in YYJJJHH format
        iyr = 1900 + istamp / 100000
        if (iyr < 1950) iyr = iyr + 100
            call jdatec(jd, iyr, 1, 1)
            call datec(jd + MOD(istamp / 100, 1000) - 1, iyr, imon, iday)
            call newdate(istamp, iyr * 10000 + imon * 100 + iday, MOD(istamp, 100) * 1000000, 3)
        endif
        goto 101
100     continue
        istamp = 010101011
        call getenv('CMC_OCMPATH',datarep)
        length = len_trim(datarep)
        if (length > 0) then
            iun = 0
            ier = fnom(iun, datarep(1:length) // '/datafiles/data/uspmadt', 'SEQ+FTN+FMT',0)
        else
            call getenv('AFSISIO',datarep)
            length = len_trim(datarep)
            iun = 0
            ier = fnom(iun, datarep(1:length) // '/datafiles/data/uspmadt', 'SEQ+FTN+FMT',0)
        endif
200     read(iun,'(A7,1X,I9)', end = 300) idnt, istmp
        if (idnt == iquoi) then
            istamp = istmp
        else
            goto 200
        endif
300     close(iun)
101     continue
        if (istamp == 010101011) then
            open(iun, file = flag, form = 'FORMATTED')
            read(iun, '(I9)', end = 400) istamp
400         close(iun)
            ier = fclos(iun)
        endif
    endif

    iopdatm = istamp
end function iopdatm

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


!> \file


!> Get the number of bits to use and a data type in order to insert tblval in a data block
integer function qrbnbdt(nbit, datyp, tblval, tbldim)
    use app
    use rmn_burp, only: ercmpr
    implicit none

    !> Number of bits to preserve for each value
    integer, intent(inout) :: nbit
    !> Data type for compaction
    integer, intent(inout) :: datyp
    !> Number of entries and tblval
    integer, intent(in) :: tbldim
    !> Values to write (nele*nval*nt)
    integer, intent(in) :: tblval(tbldim)

    !> \return 0 on success, error code if the required precision can not be done

    !     methode: - trouver min,max et max(abs(min),max)) de tblval
    !                voir si entiers signes requis,
    !                calculer nombre de bits requis
    !                on ne retourne jamais moins de precision que
    !                ce que l'usager a demande.

    integer :: tblmax, tblmin, erreur

    ! SUPVAL:TABLEAU DE REFERENCE POUR TROUVER LE NOMBRE DE BITS REQUIS
    integer, dimension(32), parameter :: SUPVAL = [                                                 &
               1,        2,         4,         8,        16,         32,          64,         128,  &
             256,      512,      1024,      2048,      4096,       8192,       16384,       32768,  &
           65536,   131072,    262144,    524288,   1048576,    2097152,     4194304,     8388608,  &
        16777216, 33554432,  67108864, 134217728, 268435456,  536870912,  1073741824,  2147483647   &
    ]
    integer :: i

    erreur = 0
    if (nbit <= 0) nbit = 1

    ! on retourne si datyp et nbit == mode transparent ou si datatyp == 0
    if (((datyp == 2) .and. (nbit == 32)) .or. (datyp == 0)) then
        qrbnbdt = 0
        return
    endif

    ! si datyp >=6 (real, real*8, complex, complex*8) on met le nombre de bits a 32
    if (datyp >= 6) then
        nbit = 32
        qrbnbdt = 0
        return
    endif

    ! si datyp = caracteres, nbit = 8
    if (datyp == 3 .or. datyp == 5) then
        nbit = 8
        qrbnbdt = 0
        return
    endif

    ! trouver le min et le max du champ
    tblmax = maxval(tblval)
    tblmin = minval(tblval)

    ! determiner la valeur abs maximale et si entiers signes sont requis
    if (tblmin < -1) then
        tblmax = max(tblmax, abs( tblmin ))
        datyp = 4
    endif

    ! determiner le nombre de bits a utiliser
    ! si la valeur maximale occupe tous les bits requis, ou si il y ades valeurs manquantes (=-1), on rajoute 1 bit
    if (tblmax >= SUPVAL(nbit)) then
        do i = nbit + 1, 32
            if (tblmax < SUPVAL(i)) then
                nbit = i
                if (tblmax < SUPVAL(i) - 1) nbit = nbit - 1
                goto 100
            endif
        enddo
        write(app_msg,*) 'QRBNDT: On code avec NBIT=32 et DATYP=2'
        call Lib_Log(APP_LIBFST, APP_WARNING, app_msg)       
        erreur = ercmpr
    endif
100 continue

    ! s'assurer que les parametres sont valables:
    ! si nbit = 32 ==> datyp = 2
    ! si datyp = 4, on alloue un bit de plus pour le signe
    if (datyp == 4) then
        nbit = nbit + 1
        if (nbit > 31) then
            nbit = 32
            datyp = 2
            write(app_msg, *) 'QRBNDT: On code VALEURS<0 avec NBIT=32 et DATYP='
            call Lib_Log(APP_LIBFST, APP_WARNING, app_msg)
            erreur = ercmpr
        endif
    else
        nbit = min(nbit, 32)
    endif

    qrbnbdt = erreur
end

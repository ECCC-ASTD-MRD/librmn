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


!> Initialize conversion table
integer function qrbsct(tableau, tabdim, nelelu)
    use app
    use rmn_burp, only: badtbl, rpetitif, erbtab, erelem
    implicit none

    !> Second dimension of the conversion table
    integer, intent(in) :: tabdim
    !> Conversion table. tableau(1, :) = Element code, tableau(2, :) = scaling factor, tableau(3, :) = reference value
    integer, intent(out) :: tableau(3, tabdim)
    !> Number of elements read
    integer, intent(out) :: nelelu

    integer, external :: mrbcov
    integer :: maxelm, i, iun, pathlng, travail, ier, io_status
    character(len=128) ligne
    character(len=256) path, path1

#include <rmn/fnom.hf>
    ! For PUTBIT
#include <ftnmacros.hf>

    call getenv('MA_TABLEBURP_PERSONNELLE', path1)
    pathlng = len_trim( path1 )
    if (pathlng > 0) then
        write(*, *) ' ***********************ATTENTION***********************'
        write(*, *) ' *                                                     *'
        write(*, *) ' *   ON N''UTILISE PAS LE FICHIER TABLEBURP OFFICIEL   *'
        write(*, *) ' *                                                     *'
        write(*, *) ' *******************************************************'

        badtbl = 1
    else
        call getenv('CMCCONST', path)
        pathlng = len_trim( path )
        path1 = path(1:pathlng) // '/table_b_bufr'
    endif

    qrbsct = -1
    iun = 0
    qrbsct = fnom(iun, path1, 'FTN+FMT+R/O', 0)
    if (qrbsct /= 0) then
        write(app_msg, *) 'QRBSCT: Erreur d''ouverture du fichier TABLEBURP'
        call lib_log(app_libfst, app_error, app_msg)
        qrbsct = erbtab
        return
    endif

    ! lire le nombre d'elements presents et le nombre d'elements a convertir dans le fichier bufr_b
    read(iun, *) maxelm, nelelu

    ! lecture de toutes les entrees de la table bufrb
    i = 0
    do
        read(iun,'(a128)', iostat = io_status) ligne
        if (io_status /= 0) exit
        if ((ligne(1:1) == '*') .or. (ligne(1:1) == '#')) cycle

        ! conversion des nom de variables a des entiers de 16 bits
        read(ligne(1:6),'(i6)') travail
        travail = mrbcov( travail )

        ! verifier si l'element est repetitif. si oui, on allume le bit correspondant au no d'element dans rpetitif
        if (index('Mm', ligne(85:85)) /= 0 ) PUTBIT(rpetitif, 1, travail, 1)

        if (ligne(51:51) /= '*') then
            i = i + 1
            if (i > tabdim) then
                write(app_msg,*) 'QRBSCT: Tableau pour la lecture trop petit, consulte un specialiste'
                call lib_log(app_libfst, app_error, app_msg)
                qrbsct = erbtab
                return
            endif
            tableau(1, i) = travail
            read(ligne(64:66), '(i3)') tableau(2, i)
            read(ligne(67:77), '(i11)') tableau(3, i)
        endif
    end do

    if (nelelu /= i) then
        write(app_msg,*) 'QRBSCT: NELELU <> nombre d''entree dans TABLEBURP'
        call lib_log(app_libfst, app_info, app_msg)
        qrbsct = erelem
        nelelu = i
    endif

    ier = fclos( iun )
    qrbsct = 0
end

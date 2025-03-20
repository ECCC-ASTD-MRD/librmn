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


!> Open a burp file
integer function mrfopn(iun, inmode)
    use app
    use rmn_burp, only: npritot, nauxtot, enforc8, erfmod, erfrap, nonoftb, &
        bpdate, bpdrcv, bpdx, bpdy, bpelev, bpflgs, bpheur, bpidtp, bplati, bplong, bpmin, bpnblk, bpoars, bprunn, &
        bpsti1, bpsti2, bpsti3, bpsti4, bpsti5, bpsti6, bpsti7, bpsti8, bpsti9, &
        ldate, ldrcv, ldx, ldy, lelev, lflgs, lheur, lidtp, llati, llong, lmin, lnblk, loars, lrunn, &
        lsti1, lsti2, lsti3, lsti4, lsti5, lsti6, lsti7, lsti8, lsti9
    implicit none

    !> Unit number
    integer, intent(in) :: iun
    !> Open mode (read, create, append)
    character(len = *), intent(in) :: inmode

    !     initialiser les descripteurs de clefs et creer un fichier
    !     burp ou ouvrir un fichier burp deja existant.
    !     mrfopn retourne le nombre d'enregistrements actifs
    !     contenus dans le fichier.

    integer, external :: xdfopn, xdfcle, xdfsta, mrfopr, mrfopc
    external genvdt8

    integer :: nombre, stat, prii, pri(2,npritot), aux(2,nauxtot), ier, auxx
    character(len = 4) :: appl, versn
    character(len = 6) :: mode

    logical, save :: initdone = .false.

    !     verifier si le fichier est ouvert dans un mode permis
    if (.not. initdone) then
        call genvdt8(enforc8)
        initdone = .true.
    endif
    mrfopn = -1
    mode = inmode
    if (index(mode, 'WRITE') .NE. 0 .OR. index(mode, 'R-W') .NE. 0) then
        write(app_msg, *) 'MRFOPN: Seul les modes READ, CREATE et APPEND sont permis'
        call lib_log(app_libfst, app_error, app_msg)
        mrfopn = erfmod
        return
    endif

    ! Initialiser les clef si mode = 'CREATE'
    if (index(mode, 'CREATE') .ne. 0) then
        ier = 0

        ! Definition des clefs primaires
        ier = ier + xdfcle('STI1', bpsti1, lsti1, 33, pri(1,1),  pri(2,1))
        ier = ier + xdfcle('STI2', bpsti2, lsti2, 33, pri(1,2),  pri(2,2))
        ier = ier + xdfcle('STI3', bpsti3, lsti3, 33, pri(1,3),  pri(2,3))
        ier = ier + xdfcle('STI4', bpsti4, lsti4, 33, pri(1,4),  pri(2,4))
        ier = ier + xdfcle('STI5', bpsti5, lsti5, 33, pri(1,5),  pri(2,5))
        ier = ier + xdfcle('STI6', bpsti6, lsti6, 33, pri(1,6),  pri(2,6))
        ier = ier + xdfcle('STI7', bpsti7, lsti7, 33, pri(1,7),  pri(2,7))
        ier = ier + xdfcle('STI8', bpsti8, lsti8, 33, pri(1,8),  pri(2,8))
        ier = ier + xdfcle('STI9', bpsti9, lsti9, 33, pri(1,9),  pri(2,9))
        ier = ier + xdfcle('FLGS', bpflgs, lflgs, 00, pri(1,10), pri(2,10))
        ier = ier + xdfcle('LATI', bplati, llati, 00, pri(1,11), pri(2,11))
        ier = ier + xdfcle('LONG', bplong, llong, 00, pri(1,12), pri(2,12))
        ier = ier + xdfcle('DATE', bpdate, ldate, 00, pri(1,13), pri(2,13))
        ier = ier + xdfcle('DX',   bpdx,   ldx,   00, pri(1,14), pri(2,14))
        ier = ier + xdfcle('IDTP', bpidtp, lidtp, 00, pri(1,15), pri(2,15))
        ier = ier + xdfcle('DY',   bpdy,   ldy,   00, pri(1,16), pri(2,16))
        ier = ier + xdfcle('HEUR', bpheur, lheur, 00, pri(1,17), pri(2,17))
        ier = ier + xdfcle('MIN',  bpmin,  lmin,  00, pri(1,18), pri(2,18))
 
        ! Definition des clefs auxiliaires
        ier = ier + xdfcle('NBLK', bpnblk, lnblk, 00, aux(1,1), aux(2,1))
        ier = ier + xdfcle('OARS', bpoars, loars, 00, aux(1,2), aux(2,2))
        ier = ier + xdfcle('ELEV', bpelev, lelev, 00, aux(1,3), aux(2,3))
        ier = ier + xdfcle('DRCV', bpdrcv, ldrcv, 00, aux(1,4), aux(2,4))
        ier = ier + xdfcle('RUNN', bprunn, lrunn, 00, aux(1,5), aux(2,5))
        if (ier .lt. 0) return
    else
        if (index(mode, 'APPEND') .ne. 0) mode = 'R-W'
    endif

    ! Ouverture du fichier
    nombre = xdfopn(iun, mode, pri, npritot, aux, nauxtot, 'BRP0')
    if (nombre .lt. 0) then
        mrfopn = nombre
        return
    endif

    ! Obtenir les informations concernant le fichier.
    mrfopn = xdfsta(iun, stat, 0, prii, 0, auxx, 0, versn, appl)
    IF ((INDEX(VERSN, 'XDF').EQ.0) .OR. ((INDEX(APPL,'BRP0').EQ.0) .AND. (INDEX(APPL,'bRp0') .EQ. 0))) THEN
        write(app_msg,*) 'MRFOPN: Le fichier n''est pas un fichier rapport'
        call lib_log(app_libfst,app_warning,app_msg)
        mrfopn = erfrap
        return
    endif

    ! S'assurer que le fichier a ete cree en utilisant la bonne table burp
    if (index(appl,'bRp0') .ne. 0) then
        write(app_msg,*) 'MRFOPN: Fichier cree avec TABLEBURP non-officielle'
        call lib_log(app_libfst,app_warning,app_msg)
        mrfopn = nonoftb
    endif

    if (index(mode, 'CREATE') .ne. 0) then
        write(app_msg, "('MRFOPN: UNITE = ',I3,' Fichier rapport est cree')") iun
        call lib_log(app_libfst,app_trivial,app_msg)
    endif

    write(app_msg, "('MRFOPN: UNITE = ',I3,' Fichier rapport est ouvert')") iun
    call lib_log(app_libfst,app_trivial,app_msg)

    mrfopn = nombre
END

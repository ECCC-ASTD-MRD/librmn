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


!> Print the content of a burp file
integer function mrfvoi( iun )
    use rmn_burp, only: enforc8, npridef, nauxdef, maxfil
    implicit none

    !> file unit number
    integer, intent(in) :: iun

    !> \return 0 on success, error code otherwise

    external :: rah2char
    integer, external :: xdfsta,  xdfprm, qdfind, xdfloc, xdfopn, xdfcls, qqqfnom

    character(*), parameter :: page_header_fmt = "('1  MRFVOI  UNITE  ', I3, '  NOM ', A, T86, '  PAGE ', I3)"
    character(*), parameter :: stat_fmt = "(A38, I10)"

    integer :: temps, long, stat(12), &
            indfic, handle, typrec, nlign, addr, j, &
            nklprim, ncprm, pridef(2, npridef), &
            nbpages, lengr, auxdef(2, nauxdef), &
            klprim(npridef), cprm(npridef)
    character(len = 9) :: stnid
    character(len = 4) :: vers, applic
    character(len = 50) :: nomfic,  typfic
    logical :: onferme
    integer :: date, mois, annee, aa, mm, jj

    ncprm = npridef
    do j = 1, ncprm
        cprm(j) = -1
    end do
    onferme = .false.
    mrfvoi  = -1
    indfic  = qdfind( iun )
    if (indfic > maxfil) then
        onferme = .true.
        mrfvoi = xdfopn(iun, 'READ', pridef, npridef, auxdef, nauxdef, 'BURP')
        if (mrfvoi < 0) return
    endif

    ! obtenir le nom du fichier
    mrfvoi = qqqfnom(iun, nomfic, typfic, lengr)

    ! obtenir les statistiques se rapportant au fichier
    mrfvoi  = xdfsta(iun, stat, 12, pridef, npridef, auxdef, nauxdef, vers, applic)
    nklprim = stat(7)

    ! obtenir les parametres descripteurs
    handle  = 0
    handle  = xdfloc(iun, handle, cprm, ncprm)
    nlign   = 60
    nbpages = 1
    do while (handle >= 0)
        mrfvoi = xdfprm(handle, addr, long, typrec, klprim, nklprim)
        ! enregistrement valide?
        if (typrec /= 255) then
            ! extraire les clefs primaires
            do j = 1, 9
                call rah2char(stnid(j:j), klprim(j), 1)
            end do
            ! commencer nouvelle page?
            if (nlign == 60) then
                write(*, page_header_fmt) iun, nomfic, nbpages
                write(*, "('0  STATION   LATI   LONG     DX     DY   FLGS(HEX)   DATE   TEMPS   IDTYP   LONGUEUR  ADRESSE '/)")
                nlign   = 0
                nbpages = nbpages + 1
            endif

            temps = (klprim(17) * 100) + klprim(18)
            date = klprim(13)
            if ((mod((date / 100), 100) > 12) .or. (enforc8)) then

            ! retourner la date en format aaaammjj
            aa = mod((date / 10000), 100)
            mm = mod((date / 100), 100)
            jj = mod(date, 100)
            annee = 1900 + aa + (((mm - 1) / 12) * 100)
            mois = 1 + mod(mm - 1, 12)
            date = (annee * 10000) + (mois * 100) + jj
            endif
            write(*, "(' ', A9, 1X, 4(I6, 1X), 4X, Z6.6, 1X, I8, 3X, I4, 3X, I3, 3X, I8, 1X, I10)") &
                stnid, klprim(11), klprim(12), klprim(14), klprim(16), klprim(10), date, temps, klprim(15), long, addr
            nlign = nlign + 1
        endif
        handle = xdfloc(iun, handle, cprm, ncprm)
    end do

    ! fermer le fichier si on l'a ouvert
    if (onferme) mrfvoi = xdfcls( iun )

    ! ecrire les statisttiques se rapportant au fichier
    if (nlign > 46) write(*, page_header_fmt) iun, nomfic, nbpages
    write(*, "('0 STATISTIQUES'//)")
    write(*, stat_fmt) ' TAILLE DU FICHIER                   ', stat(1)
    write(*, stat_fmt) ' NOMBRE DE REECRITURES               ', stat(2)
    write(*, stat_fmt) ' NOMBRE D''EXTENSIONS                ', stat(3)
    write(*, stat_fmt) ' NOMBRE D''EFFACEMENTS               ', stat(11)
    write(*, stat_fmt) ' NOMBRE D''ENREGISTREMENTS VALIDES   ', stat(12)
    write(*, stat_fmt) ' TAILLE DU PLUS GROS ENREGISTREMENT  ', stat(6)
    write(*, "(/' N.B. DIMENSIONS ET ADRESSES EN UNITES DE 64 BITS'//)")
    mrfvoi = 0
end

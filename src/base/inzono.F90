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


!> Initialisation des tables d'infomations de controle pour diagnostiques zonaux
subroutine inzono(poids, rang, theta, ndeltat, deltat, mode, dznsrf, zsurfac, &
                  dznprf, zprofil, latmin, rot, iun, s, etikx, idayo, ni, nj, nk)
    use rmn_fst98
    implicit none
    !> Numero de fichier standard ou l'on ecrit
    integer, intent(inout) :: iun
    !> Dimension horizontale de la grille du modele
    integer, intent(in) :: ni
    !> Deuxieme dimension horizontale de la grille
    integer, intent(in) :: nj
    !> Nombre de niveaux du modele
    integer, intent(in) :: nk
    !> Plus grand cercle de latitude inscrit dans la grille
    integer, intent(in) :: latmin
    !> Angle de rotation de l'axe des X de la grille
    integer, intent(in) :: rot
    !> Nombre de pas de temps d'accumulations
    integer, intent(in) :: ndeltat
    !> Nombre de seconde entre chaque pas de temps
    integer, intent(in) :: deltat
    !> Sauve les moyennes, la somme des moyennes, les deux
    integer, intent(in) :: mode

    !> Numero de la bande pour position des accumulateurs
    integer, intent(in) :: rang(ni * nj)
    !> Date stamp lu de analev
    integer, intent(in) :: idayo
    !> Nombre de variables de surface
    integer, intent(in) :: dznsrf
    !> Nombre de variables de surface
    integer, intent(in) :: dznprf
    !> Variables de surface demandees
    integer, intent(in) :: zsurfac(dznsrf)
    !> Variables de profil demandees
    integer, intent(in) :: zprofil(dznprf)
    !> Poids relatifs des points de grille pour extraction
    real, intent(inout) :: poids(ni * nj)
    !> Angles de rotation de la grille p/r Greenwich
    real, intent(in) :: theta(ni * nj)
    !> Niveaux du modele
    real, intent(in) :: s(nk)
    !> Etiket de l'experience
    character(len = *), intent(in) :: etikx

    !---------------------------------------------------------------------------
    ! Declarations
    !---------------------------------------------------------------------------

    integer :: complet
    integer :: nbin, somnk

    integer, parameter :: maxvar = 256

    ! Nom/propiete/position des variables a traiter.
    integer, dimension(maxvar + 2) :: var
    integer, save :: nvar

    character(len = 4), dimension(maxvar), save :: listvar
    integer, dimension(maxvar), save :: propvar
    integer, dimension(0:maxvar), save :: posvar

    ! Declarations des variables statiques.
    integer, parameter :: nombrec = 14

    integer, dimension(nombrec), save :: tabctl


    ! Parametres servant a l'allocation de memoire dynamique.
    ! pWk   - espace de travail pour fichier standard
    real, dimension (:), allocatable :: wk

    ! Drapeaux logiques.
    logical, save :: tourne = .true.

    ! Declaration des fonctions fstxxx et de leurs parametres.
    integer :: ifrm, iecr, inbr, ierr, nil, exfin
    external exfin, qqexit, strgr4a

#include <rmn/fnom.hf>

    character(len = 1) :: typvar
    character(len = 4) :: etivar
    character(len = 2) :: nomvar
    character(len = 8) :: etiket
    character(len = 1) :: grtyp

    integer, save :: dateo, deet, datyp, npak

    integer, save :: ip1 = 0
    integer, save :: ip2 = 0
    integer, save :: ip20 = 0
    integer, save :: ip3 = 0

    integer, save :: ig1 = 0
    integer, save :: ig2 = 0
    integer, save :: ig3 = 0
    integer, save :: ig4 = 0

    integer, save :: nbits = 24
    integer, save :: npas = 0

    integer :: rewrit

    ! Variables de travail non statiques.
    integer :: nic, njc, nkc, ii, jj, kk

    !---------------------------------------------------------------------------
    ! Body
    !---------------------------------------------------------------------------

    complet = 0
    tourne  = .true.
    if (rot .eq. 0) tourne = .false.

    dateo   = idayo
    deet    = deltat
    npak    = -nbits

    tabctl(1) = ndeltat
    tabctl(2) = deltat
    tabctl(3) = mode
    tabctl(4) = ni
    tabctl(5) = nj
    tabctl(6) = nk

    ! Encoder les variables de surface
    posvar(0) = 1
    do ii = 1, dznsrf
        write(listvar(ii), '(A4)') zsurfac(ii)
        call strgr4a(listvar(ii), var(ii), 0, 3)
        ! Ces calculs seront repris dans mzonxst
        propvar(ii) = 0
        if (listvar(ii)(1:1) .eq. '.') propvar(ii) = 1
        posvar(ii) = ii + 1
    end do

    ! Encode les variables de profil
    kk = dznsrf
    do ii = 1, dznprf
        kk = kk + 1
        write(listvar(kk), '(A4)') zprofil(ii)
        call strgr4a(listvar(kk), var(kk), 0, 3)
        ! Ces calculs seront repris dans mzonxst
        propvar(kk) = 0
        if (listvar(kk)(1:1) .eq. '.') propvar(kk) = 1
        posvar(kk) = kk + ii * (nk - 1) + 1
    end do

    nvar = dznsrf + dznprf
    ! somme des deux
    print *, 'NVAR=', nvar
    do ii = 1, nvar
        print *, listvar(II), propvar(II), posvar(II)
    end do

    somnk = posvar(nvar) - 1
    print *, 'SOMNK= ', somnk

    ! Determiner nbin
    nbin = 0
    do jj = 1, nj
        do ii = 1, ni
            if (nbin .lt. rang(ii + (jj - 1) * ni)) nbin = rang(ii + (jj - 1) * ni)
        end do
    end do
    print *,'NBIN= ',nbin

    ! Encode la tiquette
    etivar = etikx(1:4)
    call strgr4a(etivar, var(nvar + 1), 0, 3)
    etivar = etikx(5:8)
    call strgr4a(etivar, var(nvar + 2), 0 ,3)
    print *, 'ETIKX= ', etikx

    ! Ecrire le reste des tables attn. maximum de 14
    tabctl(7)  = nbin
    tabctl(8)  = somnk
    tabctl(9)  = complet
    tabctl(10) = LATMIN
    tabctl(11) = rot
    tabctl(12) = 0
    tabctl(13) = 0
    tabctl(14) = 0

    ierr = fnom(iun, 'noutzon', 'STD+RND', 0)
    inbr = fstouv(iun, ' RND ')

    ! Allouer la memoire pour les vecteurs de travail
    allocate(wk(ni * nj))

    ! Definir les parametres de stockage du fichier standard
    datyp  = 2
    rewrit = 0
    typvar = '+'
    etiket = 'CONTROLE'
    grtyp  = 'X'
    if (.not. tourne) grtyp  = 'G'

    ! Ecrire l'information de controle dans iun.
    nomvar = 'T/'
    nic = nombreC
    njc = 1
    nkc = 1
    iecr = fstecr(tabctl, wk, -32, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        write(6, 6001)
        nil = exfin('Zonecri', 'Erreur 2', 'NON')
        call qqexit(2)
    end if

    ! Ecrire LISTVAR. NV + 2 est contenu dans NIC.
    datyp = 3
    nomvar = 'V/'
    nic = (nvar + 2) * 4
    njc = 1
    nkc = 1
    iecr = fstecr(var, wk, -32, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip2, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        write(6, 6002)
        nil = exfin('Zonecri', 'Erreur 3', 'NON')
        call qqexit(3)
    end if

    ! Ecrire POSVAR. posvar(0) = 1 par definition.
    datyp = 2
    nomvar = 'P/'
    nic = nvar + 1
    njc = 1
    nkc = 1
    iecr = fstecr(posvar, wk, -32, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip2, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr.lt.0) then
        write(6, 6003)
        nil = exfin('Zonecri', 'Erreur 4', 'NON')
        call qqexit(4)
    end if

    ! Ecrire les champs grilles.
    !      Ecrire PDS.
    datyp = 1
    nomvar = 'W/'
    nic = ni
    njc = nj
    nkc = 1
    iecr = fstecr(poids, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        write(6, 6007)
        nil = exfin('Zonecri', 'Erreur 7', 'NON')
        call qqexit( 7 )
    end if

    if (tourne) then
        do jj = 1, nj
            do ii = 1, ni
                poids(ii + (jj - 1) * ni) = sin(theta(ii + (jj - 1) * ni))
            end do
        end do
    else
        do jj = 1, nj
            do ii = 1, ni
                poids(ii + (jj - 1) * ni) = -1.
            end do
        end do
    end if

    ! Ecrire SINT.
    datyp = 1
    nomvar = 'S/'
    nic = ni
    njc = nj
    nkc = 1
    iecr = fstecr(poids, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        write(6, 6004)
        nil = exfin('Zonecri', 'Erreur 4', 'NON')
        call qqexit(4)
    end if

    if (tourne) then
        do jj = 1, nj
            do ii = 1, ni
                poids(ii + (jj - 1) * ni) = cos(theta(ii + (jj - 1) * ni))
            end do
        end do
    else
        do jj = 1, nj
            do ii = 1, ni
                poids(ii + (jj - 1) * ni) = 0.
            end do
        end do
    end if

    ! Ecrire COST.
    nomvar = 'C/'
    nic = ni
    njc = nj
    nkc = 1
    iecr = fstecr(poids, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        write(6, 6005)
        nil = exfin('Zonecri', 'Erreur 5', 'NON')
        call qqexit(5)
    end if

    ! Ecrire BIN
    nomvar = 'B/'
    datyp = 2
    nic = ni
    njc = nj
    nkc = 1
    iecr = fstecr(rang, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        write(6, 6006)
        nil = exfin('Zonecri', 'Erreur 6', 'NON')
        call qqexit(6)
    end if

    ! Ecrire les niveaux sigma dans iun.
    nomvar = 'S^'
    datyp = 1
    nic = nk
    njc = 1
    nkc = 1
    iecr = fstecr(S, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etikx, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        write(6, 6010)
        nil = exfin('Zonecri', 'Erreur 10', 'NON')
        call qqexit(10)
    end if

    ! Ecrire les latitudes dans iun.
    if (.not. tourne) then
        nomvar = 'L^'
        nic    =   nbin
        njc    =   1
        nkc    =   1

        iecr   = fstecr( theta , wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
            typvar, nomvar, etikx, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
        if (iecr .lt. 0) then
            write(6, 6011)
            nil = exfin('Zonecri', 'Erreur 11', 'NON')
            call qqexit(11)
        end if
    end if

    ! Liberer la memoire dynamique.
    deallocate(wk)

    ! Fermer le fichier iun.
    ifrm = fstfrm(iun)

    return

6001 Format(' Unable to write "T/" control table before closing down.')
6002 Format(' Unable to write "V/" variable list before closing down.')
6003 Format(' Unable to write "P/" position list before closing down.')
6004 Format(' Unable to write "S/" sin array before closing down.')
6005 Format(' Unable to write "C/" cos array before closing down.')
6006 Format(' Unable to write "B/" bin array before closing down.')
6007 Format(' Unable to write "W/" weights array before closing down.')
6010 Format(' Unable to write "S^" sigma array before closing down.')
6011 Format(' Unable to write "L^" theta array before closing down.')

end subroutine



!> Initialisation des tables d'infomations de controle pour diagnostiques zonaux
subroutine inzono2(poids, rang, theta, ndeltat, deltat, mode, dznsrf, zsurfac, &
                  dznprf, zprofil, latmin, rot, iun, s, etikx, idayo, ni, nj, nk, &
                  lun_out, noutzon)
    use rmn_fst98
    implicit none

    !> Number of points along the X axis
    integer, intent(in) :: ni
    !> Number of points along the Y axis
    integer, intent(in) :: nj
    !> Number of vertical levels
    integer, intent(in) :: nk
    !> Biggest latitude circle within the grid
    integer, intent(in) :: latmin
    !> Rotation angle of the X axis
    integer, intent(in) :: rot
    !> Unit number of the file in which to write
    integer, intent(out) :: iun
    !> Number of timesteps for accumulation
    integer, intent(in) :: ndeltat
    !> Number of seconds between each timestep
    integer, intent(in) :: deltat
    !> Indicate what to save: averages, sum of averages, both
    integer, intent(in) :: mode
    !> Unit number for output messages
    integer, intent(in) :: lun_out
    !> Band number of the position of the accumulators
    integer, intent(in) :: rang(ni * nj)
    !> Date stamp read from analev
    integer, intent(in) :: idayo
    !> Number of surface variables
    integer, intent(in) :: dznsrf
    !> Number of profile variables
    integer, intent(in) :: dznprf
    !> Requested surface variables
    integer, intent(in) :: zsurfac(dznsrf)
    !> Requested profile variables
    integer, intent(in) :: zprofil(dznprf)
    !> Relative weight of the grid points for extraction
    real, intent(inout) :: poids(ni * nj)
    !> Grid rotation angle in regards to Greenwich
    real, intent(in) :: theta(ni * nj)
    !> Model levels
    real, intent(in) :: s(nk)
    !> Experience label
    character(len = *), intent(in) :: etikx
    !> Full pathname of the output file
    character(len = *), intent(in) :: noutzon

    !---------------------------------------------------------------------------
    ! Declarations
    !---------------------------------------------------------------------------

    integer :: complet
    integer :: nbin, somnk

    integer, parameter :: maxvar = 256

    ! Nom/propiete/position des variables a traiter.
    integer, dimension(maxvar + 3) :: var
    integer, save :: nvar

    character(len = 4), dimension(maxvar), save :: listvar
    integer, dimension(maxvar), save :: propvar
    integer, dimension(0:maxvar), save :: posvar

    ! Declarations des variables statiques.
    integer, parameter :: nombrec = 14

    integer, dimension(nombrec), save :: tabctl


    ! Parametres servant a l'allocation de memoire dynamique.
    ! pWk   - espace de travail pour fichier standard
    real, dimension (:), allocatable :: wk

    ! Drapeaux logiques.
    logical, save :: tourne = .true.

    ! Declaration des fonctions fstxxx et de leurs parametres.
    integer :: ifrm, iecr, inbr, ierr, nil, exfin
    external exfin, qqexit, strgr4a

#include <rmn/fnom.hf>

    character(len = 1) :: typvar
    character(len = 4) :: etivar
    character(len = 2) :: nomvar
    character(len = 12) :: etiket
    character(len = 1) :: grtyp

    integer, save :: dateo, deet, datyp, npak

    integer, save :: ip1 = 0
    integer, save :: ip2 = 0
    integer, save :: ip20 = 0
    integer, save :: ip3 = 0

    integer, save :: ig1 = 0
    integer, save :: ig2 = 0
    integer, save :: ig3 = 0
    integer, save :: ig4 = 0

    integer, save :: nbits = 24
    integer, save :: npas = 0

    integer :: rewrit

    ! Variables de travail non statiques.
    integer :: nic, njc, nkc, ii, jj, kk

    !---------------------------------------------------------------------------
    ! Body
    !---------------------------------------------------------------------------

    complet = 0
    tourne  = .true.
    if (rot .eq. 0) tourne = .false.

    dateo   = idayo
    deet    = deltat
    npak    = -nbits

    tabctl(1) = ndeltat
    tabctl(2) = deltat
    tabctl(3) = mode
    tabctl(4) = ni
    tabctl(5) = nj
    tabctl(6) = nk

    ! Encoder les variables de surface
    posvar(0) = 1
    do ii = 1, dznsrf
        write(listvar(ii), '(A4)') zsurfac(ii)
        call strgr4a(listvar(ii), var(ii), 0, 3)
        ! Ces calculs seront repris dans mzonxst
        propvar(ii) = 0
        if (listvar(ii)(1:1) .eq. '.') propvar(ii) = 1
        posvar(ii) = ii + 1
    end do

    ! Encode les variables de profil
    kk = dznsrf
    do ii = 1, dznprf
        kk = kk + 1
        write(listvar(kk), '(A4)') zprofil(ii)
        call strgr4a(listvar(kk), var(kk), 0, 3)
        ! Ces calculs seront repris dans mzonxst
        propvar(kk) = 0
        if (listvar(kk)(1:1) .eq. '.') propvar(kk) = 1
        posvar(kk) = kk + ii * (nk - 1) + 1
    end do

    nvar = dznsrf + dznprf
    if (lun_out .gt. 0) write(lun_out, '(a,i6)') 'NVAR=', nvar
    if (lun_out .gt. 0) write(lun_out, '(a,2i6)') (listvar(ii), propvar(ii), posvar(ii), ii = 1, nvar)

    somnk = posvar(nvar) - 1
    if (lun_out .gt. 0) write(lun_out, '(a,i6)') 'SOMNK= ', somnk

    ! Determiner nbin
    nbin = 0
    do jj = 1, nj
        do ii = 1, ni
            if (nbin .lt. rang(ii + (jj - 1) * ni)) nbin = rang(ii + (jj - 1) * ni)
        end do
    end do
    if (lun_out .gt. 0) write(lun_out,'(a,i6)') 'NBIN= ', nbin

    ! Encode la tiquette
    etivar = etikx(1:4)
    call strgr4a(etivar, var(nvar + 1), 0, 3)
    etivar = etikx(9:12)
    call strgr4a(etivar, var(nvar + 3), 0, 3)
    if (lun_out .gt. 0) write(lun_out,'(a,a)') 'ETIKX= ', etikx

    ! Ecrire le reste des tables attn. maximum de 14
    tabctl(7)  = nbin
    tabctl(8)  = somnk
    tabctl(9)  = complet
    tabctl(10) = LATMIN
    tabctl(11) = rot
    tabctl(12) = 0
    tabctl(13) = 0
    tabctl(14) = 0

    iun = 0
    ierr = fnom(iun, noutzon, 'STD+RND', 0)
    if (ierr .ge. 0) then
        inbr = fstouv(iun, ' RND ')
        if (inbr .lt. 0) then
            if (lun_out .gt. 0) write(lun_out, 6002) inbr
            nil = exfin('Inzono2', 'Erreur 2', 'NON')
            call qqexit(2)
        end if
    else
        if (lun_out .gt. 0) Write(lun_out, 6001) ierr, noutzon
        nil = exfin('Inzono2', 'Erreur 1', 'NON')
        call qqexit(1)
    end if

    ! Allouer la memoire pour les vecteurs de travail
    allocate(wk(ni * nj))

    ! Definir les parametres de stockage du fichier standard
    datyp  = 2
    rewrit = 0
    typvar = '+'
    etiket = 'CONTROLE'
    grtyp  = 'X'
    if (.not. tourne) grtyp  = 'G'

    ! Ecrire l'information de controle dans iun.
    nomvar = 'T/'
    nic = nombreC
    njc = 1
    nkc = 1
    iecr = fstecr(tabctl, wk, -32, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
            if (lun_out .gt. 0) write(lun_out, 6003) iecr
            nil = exfin('Inzono2', 'Erreur 3', 'NON')
            call qqexit(3)
    end if

    ! Ecrire LISTVAR. NV + 3 est contenu dans NIC.
    datyp = 3
    nomvar = 'V/'
    nic = (nvar + 3) * 4
    njc = 1
    nkc = 1
    iecr = fstecr(var, wk, -32, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip2, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        if (lun_out .gt. 0) write(lun_out, 6004) iecr
        nil = exfin('Inzono2', 'Erreur 4', 'NON')
        call qqexit(4)
    end if

    ! Ecrire POSVAR. posvar(0) = 1 par definition.
    datyp = 2
    nomvar = 'P/'
    nic = nvar + 1
    njc = 1
    nkc = 1
    iecr = fstecr(posvar, wk, -32, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip2, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr.lt.0) then
        if (lun_out .gt. 0) write(lun_out, 6005) iecr
        nil = exfin('Inzono2', 'Erreur 5', 'NON')
        call qqexit(5)
    end if

    ! Ecrire les champs grilles.
    !      Ecrire PDS.
    datyp = 1
    nomvar = 'W/'
    nic = ni
    njc = nj
    nkc = 1
    iecr = fstecr(poids, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        if (lun_out .gt. 0) write(lun_out, 6009) iecr
        nil = exfin('Inzono2', 'Erreur 9', 'NON')
        call qqexit(9)
    end if

    if (tourne) then
        do jj = 1, nj
            do ii = 1, ni
                poids(ii + (jj - 1) * ni) = sin(theta(ii + (jj - 1) * ni))
            end do
        end do
    else
        do jj = 1, nj
            do ii = 1, ni
                poids(ii + (jj - 1) * ni) = -1.
            end do
        end do
    end if

    ! Ecrire SINT.
    datyp = 1
    nomvar = 'S/'
    nic = ni
    njc = nj
    nkc = 1
    iecr = fstecr(poids, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        if (lun_out .gt. 0) write(lun_out, 6006) iecr
        nil = exfin('Inzono2', 'Erreur 6', 'NON')
        call qqexit(6)
    end if

    if (tourne) then
        do jj = 1, nj
            do ii = 1, ni
                poids(ii + (jj - 1) * ni) = cos(theta(ii + (jj - 1) * ni))
            end do
        end do
    else
        do jj = 1, nj
            do ii = 1, ni
                poids(ii + (jj - 1) * ni) = 0.
            end do
        end do
    end if

    ! Ecrire COST.
    nomvar = 'C/'
    nic = ni
    njc = nj
    nkc = 1
    iecr = fstecr(poids, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        if (lun_out .gt. 0) write(lun_out, 6007) iecr
        nil = exfin('Inzono2', 'Erreur 7', 'NON')
        call qqexit(7)
    end if

    ! Ecrire BIN
    nomvar = 'B/'
    datyp = 2
    nic = ni
    njc = nj
    nkc = 1
    iecr = fstecr(rang, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        if (lun_out .gt. 0) write(lun_out, 6008) iecr
        nil = exfin('Inzono2', 'Erreur 8', 'NON')
        call qqexit(8)
    end if

    ! Ecrire les niveaux sigma dans iun.
    nomvar = 'S^'
    datyp = 1
    nic = nk
    njc = 1
    nkc = 1
    iecr = fstecr(S, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etikx, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        if (lun_out .gt. 0) write(lun_out, 6010) iecr
        nil = exfin('Inzono2', 'Erreur 10', 'NON')
        call qqexit(10)
    end if

    ! Ecrire les latitudes dans iun.
    if (.not. tourne) then
        nomvar = 'L^'
        nic    =   nbin
        njc    =   1
        nkc    =   1

        iecr   = fstecr( theta , wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
            typvar, nomvar, etikx, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
        if (iecr .lt. 0) then
            if (lun_out .gt. 0) write(lun_out, 6011) iecr
            nil = exfin('Inzono2', 'Erreur 11', 'NON')
            call qqexit(11)
        end if
    end if

    ! Liberer la memoire dynamique.
    deallocate(wk)

    ! Fermer le fichier iun.
    ifrm = fstfrm(iun)
    ierr = fclos(iun)

    return

6001 Format(' Fnom error ',I5,' on file ',A)
6002 Format(' Fstouv error ',I5)
6003 Format(' Unable to write "T/" control table, fstecr error =',I5)
6004 Format(' Unable to write "V/" variable list, fstecr error =',I5)
6005 Format(' Unable to write "P/" position list, fstecr error =',I5)
6006 Format(' Unable to write "S/" sin array, fstecr error =',I5)
6007 Format(' Unable to write "C/" cos array, fstecr error =',I5)
6008 Format(' Unable to write "B/" bin array, fstecr error =',I5)
6009 Format(' Unable to write "W/" weights array, fstecr error =',I5)
6010 Format(' Unable to write "S^" sigma array, fstecr error =',I5)
6011 Format(' Unable to write "L^" theta array, fstecr error =',I5)

end subroutine


!> Initialisation des tables d'infomations de controle pour diagnostiques zonaux
subroutine inzono3(poids, rang, theta, ndeltat, deltat, mode, dznsrf, surfac, &
                  dznprf, profil, latmin, rot, iun, s, etikx, idayo, ni, nj, nk, &
                  nbin, lun_out, noutzon)
    use rmn_fst98
    implicit none

    !> Number of points along the X axis
    integer, intent(in) :: ni
    !> Number of points along the Y axis
    integer, intent(in) :: nj
    !> Number of vertical levels
    integer, intent(in) :: nk
    !> Number of zone old bands from one pole to the other
    integer, intent(in) :: nbin
    !> Biggest latitude circle within the grid
    integer, intent(in) :: latmin
    !> Rotation angle of the X axis
    integer, intent(in) :: rot
    !> Unit number of the file in which to write
    integer, intent(out) :: iun
    !> Number of timesteps for accumulation
    integer, intent(in) :: ndeltat
    !> Number of seconds between each timestep
    integer, intent(in) :: deltat
    !> Indicate what to save: averages, sum of averages, both
    integer, intent(in) :: mode
    !> Unit number for output messages
    integer, intent(in) :: lun_out
    !> Band number of the position of the accumulators
    integer, intent(in) :: rang(ni * nj)
    !> Date stamp read from analev
    integer, intent(in) :: idayo
    !> Number of surface variables
    integer, intent(in) :: dznsrf
    !> Number of profile variables
    integer, intent(in) :: dznprf
    !> Requested surface variables
    character(len = 6), intent(in) :: surfac(dznsrf)
    !> Requested profile variables
    character(len = 6), intent(in) :: profil(dznprf)
    !> Relative weight of the grid points for extraction
    real, intent(inout) :: poids(ni * nj)
    !> Grid rotation angle in regards to Greenwich
    real, intent(in) :: theta(ni * nj)
    !> Model levels
    real, intent(in) :: s(nk)
    !> Experience label
    character(len = *), intent(in) :: etikx
    !> Full pathname of the output file
    character(len = *), intent(in) :: noutzon

    !---------------------------------------------------------------------------
    ! Declarations
    !---------------------------------------------------------------------------

    integer :: complet
    integer :: somnk

    integer, parameter :: maxvar = 256

    ! Nom/propiete/position des variables a traiter.
    integer, save :: nvar
    character(len = 10000), save :: chaine
    character(len = 8), dimension(maxvar + 2), save :: listvar
    integer, dimension(maxvar + 2), save :: propvar
    integer, dimension(0:maxvar + 2), save :: posvar

    ! Declarations des variables statiques.
    integer, parameter :: nombrec = 14

    integer, dimension(nombrec), save :: tabctl


    ! Parametres servant a l'allocation de memoire dynamique.
    real, dimension (:), allocatable :: wk

    ! Drapeaux logiques.
    logical, save :: tourne = .true.

    ! Declaration des fonctions fstxxx et de leurs parametres.
    integer :: ifrm, iecr, inbr, ierr, nil, exfin
    external exfin, qqexit

#include <rmn/fnom.hf>

    character(len = 1) :: typvar
    character(len = 4) :: nomvar
    character(len = 12) :: etiket
    character(len = 1) :: grtyp

    integer, save :: dateo, deet, datyp, npak

    integer, save :: ip1 = 0
    integer, save :: ip2 = 0
    integer, save :: ip20 = 0
    integer, save :: ip3 = 0

    integer, save :: ig1 = 0
    integer, save :: ig2 = 0
    integer, save :: ig3 = 0
    integer, save :: ig4 = 0

    integer, save :: nbits = 24
    integer, save :: npas = 0

    integer :: rewrit

    ! Variables de travail non statiques.
    integer :: nic, njc, nkc, ii, jj, kk, i0, i1

    !---------------------------------------------------------------------------
    ! Body
    !---------------------------------------------------------------------------

    complet = 0
    tourne  = .true.
    if (rot .eq. 0) tourne = .false.

    dateo   = idayo
    deet    = deltat
    npak    = -nbits

    tabctl(1) = ndeltat
    tabctl(2) = deltat
    tabctl(3) = mode
    tabctl(4) = ni
    tabctl(5) = nj
    tabctl(6) = nk

    nvar = dznsrf + dznprf
    if (nvar .gt. maxvar) then
        if (lun_out .gt. 0) write(lun_out, 6012) nvar, maxvar
        nil = exfin('InZono3', 'Erreur 12', 'NON')
        call qqexit(12)
    end if

    ! Encoder les variables de surface
    posvar(0) = 1
    do ii = 1, dznsrf
        listvar(ii) = '        '
        listvar(ii) = surfac(ii)
        ! Ces calculs seront repris dans mzonxst
        propvar(ii) = 0
        if (listvar(ii)(1:1) .eq. '.') propvar(ii) = 1
        posvar(ii) = ii + 1
    end do

    ! Encode les variables de profil
    kk = dznsrf
    do ii = 1, dznprf
        kk = kk + 1
        listvar(kk) = '        '
        listvar(kk) = profil(ii)
        ! Ces calculs seront repris dans mzonxst
        propvar(kk) = 0
        if (listvar(kk)(1:1) .eq. '.') propvar(kk) = 1
        posvar(kk) = kk + ii * (nk - 1) + 1
    end do

    if (lun_out .gt. 0) write(lun_out, '(a,i6)') 'NVAR=', nvar
    if (lun_out .gt. 0) write(lun_out, '(a,2i6)') (listvar(ii), propvar(ii), posvar(ii), ii = 1, nvar)

    somnk = posvar(nvar) - 1
    if (lun_out .gt. 0) write(lun_out, '(a,i6)') 'SOMNK= ', somnk

    if (lun_out .gt. 0) write(lun_out,'(a,i6)') 'NBIN= ', nbin

    ! Encode la tiquette
    listvar(nvar + 1) = etikx(1:8)
    listvar(nvar + 2) = etikx(9:12)

    if (lun_out .gt. 0) write(lun_out,'(a,a)') 'ETIKX= ', etikx

    ! Ecrire le reste des tables attn. maximum de 14
    tabctl(7)  = nbin
    tabctl(8)  = somnk
    tabctl(9)  = complet
    tabctl(10) = LATMIN
    tabctl(11) = rot
    tabctl(12) = 0
    tabctl(13) = 0
    tabctl(14) = 0

    iun = 0
    ierr = fnom(iun, noutzon, 'STD+RND', 0)
    if (ierr .ge. 0) then
        inbr = fstouv(iun, ' RND ')
        if (inbr .lt. 0) then
            if (lun_out .gt. 0) write(lun_out, 6002) inbr
            nil = exfin('InZono3', 'Erreur 2', 'NON')
            call qqexit(2)
        end if
    else
        if (lun_out .gt. 0) Write(lun_out, 6001) ierr, noutzon
        nil = exfin('InZono3', 'Erreur 1', 'NON')
        call qqexit(1)
    end if

    ! Allouer la memoire pour les vecteurs de travail
    allocate(wk(ni * nj))

    ! Definir les parametres de stockage du fichier standard
    datyp  = 2
    rewrit = 0
    typvar = '+'
    etiket = 'CONTROLE'
    grtyp  = 'X'
    if (.not. tourne) grtyp  = 'G'

    ! Ecrire l'information de controle dans iun.
    nomvar = 'T/'
    nic = nombreC
    njc = 1
    nkc = 1
    iecr = fstecr(tabctl, wk, -32, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
            if (lun_out .gt. 0) write(lun_out, 6003) iecr
            nil = exfin('InZono3', 'Erreur 3', 'NON')
            call qqexit(3)
    end if

    ! Ecrire LISTVAR. NV + 2 est contenu dans NIC.
    datyp  = 7
    nomvar = 'VC/'
    nic = (nvar + 2) * 8
    njc = 1
    nkc = 1

    i0 = 1
    i1 = 8
    do  ii = 1, nvar + 2
        chaine(i0:i1) = listvar(ii)
        i0 = i1 + 1
        i1 = i0 + 7
    end do

    iecr = fstecr_s(trim( chaine ), wk, -8 , iun, dateo, deet, npas, nic, njc, nkc, ip1, ip2, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit, len_trim(chaine))
    if (iecr .lt. 0) then
        if (lun_out .gt. 0) write(lun_out, 6004) iecr
        nil = exfin('InZono3', 'Erreur 4', 'NON')
        call qqexit(4)
    end if

    ! Ecrire POSVAR. posvar(0) = 1 par definition.
    datyp = 2
    nomvar = 'P/'
    nic = nvar + 1
    njc = 1
    nkc = 1
    iecr = fstecr(posvar, wk, -32, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip2, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr.lt.0) then
        if (lun_out .gt. 0) write(lun_out, 6005) iecr
        nil = exfin('InZono3', 'Erreur 5', 'NON')
        call qqexit(5)
    end if

    ! Ecrire les champs grilles.
    !      Ecrire PDS.
    datyp = 1
    nomvar = 'W/'
    nic = ni
    njc = nj
    nkc = 1
    iecr = fstecr(poids, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        if (lun_out .gt. 0) write(lun_out, 6009) iecr
        nil = exfin('InZono3', 'Erreur 9', 'NON')
        call qqexit(9)
    end if

    if (tourne) then
        do jj = 1, nj
            do ii = 1, ni
                poids(ii + (jj - 1) * ni) = sin(theta(ii + (jj - 1) * ni))
            end do
        end do
    else
        do jj = 1, nj
            do ii = 1, ni
                poids(ii + (jj - 1) * ni) = -1.
            end do
        end do
    end if

    ! Ecrire SINT.
    datyp = 1
    nomvar = 'S/'
    nic = ni
    njc = nj
    nkc = 1
    iecr = fstecr(poids, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        if (lun_out .gt. 0) write(lun_out, 6006) iecr
        nil = exfin('InZono3', 'Erreur 6', 'NON')
        call qqexit(6)
    end if

    if (tourne) then
        do jj = 1, nj
            do ii = 1, ni
                poids(ii + (jj - 1) * ni) = cos(theta(ii + (jj - 1) * ni))
            end do
        end do
    else
        do jj = 1, nj
            do ii = 1, ni
                poids(ii + (jj - 1) * ni) = 0.
            end do
        end do
    end if

    ! Ecrire COST.
    nomvar = 'C/'
    nic = ni
    njc = nj
    nkc = 1
    iecr = fstecr(poids, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        if (lun_out .gt. 0) write(lun_out, 6007) iecr
        nil = exfin('InZono3', 'Erreur 7', 'NON')
        call qqexit(7)
    end if

    ! Ecrire BIN
    nomvar = 'B/'
    datyp = 2
    nic = ni
    njc = nj
    nkc = 1
    iecr = fstecr(rang, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        if (lun_out .gt. 0) write(lun_out, 6008) iecr
        nil = exfin('InZono3', 'Erreur 8', 'NON')
        call qqexit(8)
    end if

    ! Ecrire les niveaux sigma dans iun.
    nomvar = 'S^'
    datyp = 1
    nic = nk
    njc = 1
    nkc = 1
    iecr = fstecr(S, wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
        typvar, nomvar, etikx, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
    if (iecr .lt. 0) then
        if (lun_out .gt. 0) write(lun_out, 6010) iecr
        nil = exfin('InZono3', 'Erreur 10', 'NON')
        call qqexit(10)
    end if

    ! Ecrire les latitudes dans iun.
    if (.not. tourne) then
        nomvar = 'L^'
        nic    =   nbin
        njc    =   1
        nkc    =   1

        iecr   = fstecr( theta , wk, npak, iun, dateo, deet, npas, nic, njc, nkc, ip1, ip20, ip3, &
            typvar, nomvar, etikx, grtyp, ig1, ig2, ig3, ig4, datyp, rewrit)
        if (iecr .lt. 0) then
            if (lun_out .gt. 0) write(lun_out, 6011) iecr
            nil = exfin('InZono3', 'Erreur 11', 'NON')
            call qqexit(11)
        end if
    end if

    ! Liberer la memoire dynamique.
    deallocate(wk)

    ! Fermer le fichier iun.
    ifrm = fstfrm(iun)
    ierr = fclos(iun)

    return

 6001 Format(' Fnom error ',I5,' on file ',A)
 6002 Format(' Fstouv error ',I5)
 6003 Format(' Unable to write "T/" control table, fstecr error =',I5)
 6004 Format(' Unable to write "VC/" variable list, fstecr error =',I5)
 6005 Format(' Unable to write "P/" position list, fstecr error =',I5)
 6006 Format(' Unable to write "S/" sin array, fstecr error =',I5)
 6007 Format(' Unable to write "C/" cos array, fstecr error =',I5)
 6008 Format(' Unable to write "B/" bin array, fstecr error =',I5)
 6009 Format(' Unable to write "W/" weights array, fstecr error =',I5)
 6010 Format(' Unable to write "S^" sigma array, fstecr error =',I5)
 6011 Format(' Unable to write "L^" theta array, fstecr error =',I5)
 6012 Format(' NVAR ',I4,' greater than MaxVar ',I4)

end subroutine

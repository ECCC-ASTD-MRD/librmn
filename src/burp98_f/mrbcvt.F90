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


module mrb_cvt_sct_tbl
    use rmn_burp, only: maxnele, erbtab, manque, errcell, ncellmax
    implicit none

    logical, save :: premier = .true.
    integer, save :: tableau(3, maxnele)
    integer, save :: nelelu
end module


!> Convert between real and integer
integer function mrbcvt(liste,  tblval, rval, nele, nval, nt, mode)
    use app
    use mrb_cvt_sct_tbl
    implicit none

    !> Number of elements
    integer, intent(in) :: nele
    !> Number of values per element
    integer, intent(in) :: nval
    !> Number of nele * nval sets
    integer, intent(in) :: nt
    !> Conversion mode: 0 = bufr code to real value, 1 = real value to bufr code
    integer, intent(in) :: mode
    !> Elements to convert
    integer, intent(in) :: liste(nele)
    !> Bufr codes
    integer, intent(inout) :: tblval(nele, nval, nt)
    !> Real values
    real, intent(inout) :: rval(nele, nval, nt)

    !> A reference table is consulted to find the scaling factor and reference value to apply to the converted variable.
    !> The reference table is initialized by \ref mrbsct which is executed on the first call to \ref mrbcvt

    integer, external :: qrbsct, bufrchr
    external :: qbrptri

    !******************************************************************************
    !       la matrice tableau contient:
    !          tableau(1,i) - code de l'element i
    !          tableau(2,i) - facteur a appliquer a l'element i
    !          tableau(3,i) - valeur de reference a ajouter a l'element i
    !       la variable nelelu indique le nombre d'elements present dans le
    !       fichier bufr
    !
    !       pour coder la valeur d'un element avant un appel a mrfput, on fait
    !       l'operation suivante:
    !       element(i)_code = element(i) * tableau(2,i) - tableau(3,i)
    !
    !       on ne fait aucune conversion lorsque qu'un element est deja code
    !       comme par exemple pour les differents marqueurs.
    !
    !       pour decoder un element on fait l'operation inverse.  dans le cas
    !       des elements ne requerant aucun decodage (e.g. marqueurs), on insere
    !       dans le tableau rval la valeur -1.1e30 ce qui indique a l'usager
    !       qu'il doit consulter le tableau tblval pour obtenir cet elememt
    !*****************************************************************************

    integer i, j, k, l, referen, zerocpl
    real    echele

    mrbcvt = -1
    if( premier ) then
        mrbcvt = qrbsct(tableau,maxnele,nelelu)
        if(mrbcvt .eq. -erbtab) return

        ! trier le tableau par element en ordre croissant
        ! (necessaire pour la recherche binaire que l'on fait plus tard)
        call qbrptri(tableau, 3, nelelu)
        premier = .false.
    endif

    zerocpl = not( 0 )

    do i = 1, nele
        ! trouver l'index j pointant a l'element dans tableau
        echele  = 1.0
        referen = 0
        j = bufrchr(liste(i), tableau, nelelu)
        if(j .gt. 0) then
            echele  = 10.0 ** tableau(2,j)
            referen = tableau(3,j)
            ! print *,'debug+ echele=',echele,' tableau(2,j)=',tableau(2,j)
            ! print *,'debug+ referen=',referen

            ! conversion de code bufr a unites cmc
            if(mode .eq. 0) then
                do l = 1, nt
                    do k = 1, nval
                        if(tblval(i,k,l) .eq. zerocpl) then
                            rval(i,k,l) = manque
                        else
                            if(tblval(i,k,l) .lt. 0) tblval(i,k,l) = tblval(i,k,l) + 1
                            rval(i,k,l) = float(tblval(i,k,l) + referen)/echele
                        endif
                    enddo
                enddo
            else
                ! codage bufr
                do l = 1, nt
                    do k = 1, nval
                        if(rval(i,k,l) .eq. manque) then
                            tblval(i,k,l) = zerocpl
                        else
                            tblval(i,k,l) = nint(rval(i,k,l) * echele) - referen
                            if(tblval(i,k,l) .lt. 0) tblval(i,k,l) = tblval(i,k,l) - 1
                        endif
                    enddo
                enddo
            endif
        endif
    enddo
    mrbcvt = 0
end


!> Initialize the user's conversion table
integer function mrbsct(tblusr, neleusr)
    use app
    use mrb_cvt_sct_tbl
    implicit none

    integer, intent(in) :: neleusr
    integer, intent(inout) :: tblusr(3, neleusr)

    !> Add user defined elements to the standard table. If the standard table was not yep initialized, \ref qrbset is first called.

    external qrbsct, bufrchr, qbrptri
    integer  qrbsct, bufrchr
    integer :: i, j

    mrbsct = -1
    if( premier ) then
        mrbsct = qrbsct(tableau,maxnele,nelelu)
        if (mrbsct .eq. -erbtab) return

    ! trier le tableau par element en ordre croissant
    ! (necessaire pour la recherche binaire que l'on fait plus tard)
        call qbrptri(tableau, 3, nelelu)
        premier = .false.
    endif

    ! ajouter le tableau de l'usager a la fin du tableau standard apres l'avoir trie
    call qbrptri(tblusr, 3, neleusr)

    do j = 1, neleusr
        nelelu = nelelu + 1
        do i = 1, 3
            tableau(i,nelelu) = tblusr(i,j)
        enddo
    enddo
    mrbsct = 0
end


!> Remplir un tableau a partir de tableburp
integer FUNCTION MRBTBL(TBLBURP, NSLOTS, NELE)
    use app
    use mrb_cvt_sct_tbl
    IMPLICIT NONE
    INTEGER  NELE, NSLOTS,TBLBURP(NSLOTS, NELE)

    !     SOUS-PROGRAMME SERVANT A REMPLIR LE TABLEAU TBLBURP
    !     A PARTIR DES DESCRIPTIONS D'ELEMENTS TROUVEES DANS
    !     LE FICHIER TABLEBURP.  POUR CHAQUE ELEMENT,
    !     ON RETOURNE:
    !        - FACTEUR D'ECHELLE
    !        - VALEUR DE REFERENCE
    !        - SI L'ELEMENT EST CONVERTISSABLE OU NON
    !          0 - non convertissable
    !          1 - convertissable
    !
    !ARGUMENTS:
    !     NELE      ENTREE        - NOMBRE D'ELEMENTS A TRAITER
    !     TBLBURP   ENTREE CONTIENT LES CODES D'ELEMENTS
    !        "      SORTIE CONTIENT LES PARAMETRES DE CHAQUE ELEMENT
    !
    !     ARANGEMENT DE TBLBURP:
    !
    !     ----------------------------------------------------------
    !     | code elem 16 bits | echelle | reference | convertissable |
    !     |                   |         |           |                |
    !              .               .          .             .
    !              .               .          .             .
    !              .               .          .             .

    integer, external :: qrbsct, bufrchr
    external :: qbrptri

    integer :: i, j

    mrbtbl = -1

    ! s'assurer que la valeur de nslots est bonne
    if(nslots .ne. ncellmax) then
        write(app_msg,*) 'MRBTBL: Dimension NCELL incorrecte'
        call lib_log(app_libfst,app_warning,app_msg)
        mrbtbl = errcell
        return
    endif

    if( premier ) then
        mrbtbl = qrbsct(tableau,maxnele,nelelu)
        if (mrbtbl .eq. -erbtab) return

    ! trier le tableau par element en ordre croissant
    ! (necessaire pour la recherche binaire que l'on fait plus tard)
        call qbrptri(tableau, 3, nelelu)
        premier = .false.
    endif

    ! traiter chaque element du tableau tblburp
    do i = 1, nele
    ! trouver l'index j pointant a l'element dans tableau
        j = bufrchr(tblburp(1,i), tableau, nelelu)
        if(j .gt. 0) then
            tblburp(2,i) = tableau(2,j)
            tblburp(3,i) = tableau(3,j)
            tblburp(4,i) = 1
        else
            tblburp(4,i) = 0
        endif
    enddo
    mrbtbl = 0
end

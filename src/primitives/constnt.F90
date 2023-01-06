!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */


!> \brief Lire le fichier de constantes communes du CMC-RPN
subroutine constnt(valeur, flag, nom, mode0)
    implicit none

    real :: valeur
    integer :: flag, mode0
    character(len = *) :: nom

    integer :: ierror

    external dummy_do_nothing_routine

    call constnt_x(valeur, flag, nom, mode0, dummy_do_nothing_routine, -1, -1, -1, ierror)
end subroutine constnt


!> \brief This routine does absolutely nothing.  Meant to be used when a callback is required.
subroutine dummy_do_nothing_routine()
    return
end subroutine dummy_do_nothing_routine


!> \brief Lire le fichier de constantes communes du CMC-RPN
!> La valeur de l'argument "mode" indique le mode d'utilisation de la routine.
!> L'utilisateur peut avoir une liste imprimee complete du fichier, obtenir la
!> valeur d'une constante ou peut ajouter ou modifier, localement au travail,
!> une constante. Ce module peut etre utilise en mode mpi et propager les tables
!> à ses partenaires.
subroutine constnt_x(valeur, flag, nom, mode0, bcast_mpi, datatype, root, comm, ierror)
    use, intrinsic :: iso_fortran_env, only : real64
    use app
    implicit none

    !> Valeur de la constante demandée, ajoutée ou modifiée
    real, intent(inout) :: valeur
    integer :: flag
    integer :: mode0
    integer :: datatype
    integer :: root
    integer :: comm
    !> Code d'erreur retourné par RPN_COMM_BCST_X
    integer, intent(out) :: ierror
    character(len = *), intent(in) :: nom

    !> Nombre mximum de constantes dans le fichier
    integer, parameter :: maxcns = 200

!FICHIERS
!     ACCES AU FICHIER DE CONSTANTES SE FAIT A L'INTERIEUR MEME
!     DE LA SOUS-ROUTINE.

!ARGUMENTS
!     FLAG (S) - SI FLAG=0, LA DEMANDE FUT UN INSUCCES, I.E.
!                LA CONSTANTE N'EXISTE PAS POUR LE CAS OU ON VEUT
!                OBTENIR OU MODIFIER UNE VALEUR, OU LA CONSTANTE
!                QU'ON DESIRE AJOUTER EXISTE DEJA.
!              - SI FLAG=1, LA DEMANDE FUT UN SUCCES.
!     NOM (E)  - NOM DE LA CONSTANTE DONT ON DESIRE OBTENIR,
!                AJOUTER OU MODIFIER LA VALEUR.
!                MAXIMUM DE 8 CARACTERES.  EX. 'OMEGA'
!     MODE (E) - SI MODE=0, ON VEUT OBTENIR LA VALEUR D'UNE CONSTANTE
!              - SI MODE=1, ON VEUT UN IMPRIME COMPLET DU FICHIER
!              - SI MODE=2, ON VEUT AJOUTER (LOCALEMENT AU TRAVAIL)
!                           UNE CONSTANTE
!              - SI MODE=3, ON VEUT MODIFIER (LOCALEMENT AU TRAVAIL)
!                           UNE CONSTANTE.
!              - SI MODE=4, ON VEUT PROPAGER LE CONTENU DES TABLES
!                           VIA L'ARGUMENT BCAST_MPI. 
!              par MODE, on entend mod(MODE0,100)
!              MODE0 >  100 : VALEUR est un REAL*8
!              MODE0 <= 100 : VALEUR est un REAL*4
!     BCAST_MPI (E)   L'ARGUMENT BCAST_MPI DOIT ETRE RPN_COMM_BCST_X
!     DATATYPE,ROOT,COMM (E) :  ne servent plus 


    external bcast_mpi
    integer mode
#include <rmn/fnom.hf>

!NOTE
!     SI ON AJOUTE UNE CONSTANTE (MODE=2), NE PAS OUBLIER DE
!     MODIFIER EN CONSEQUENCE LE COMDECK CONSDYN OU CONSPHY
!      QUI RESIDE DANS LA LIBRAIRIE 'PHYSIQUE' ET/OU 'DYNAMIQUE',
!     AINSI QUE LE PARAMETER (NBRE=..) DANS LA ROUTINE INCTDYN
!     ET DANS LA ROUTINE INCTPHY (ROUTINE QUI INITIALISE
!     LE COMMON 'CTESDYN' OU 'CTESPHY'.
!     avant de proceder a un appel en MODE 4, il faut que le "root pe" ait fait 
!     un appel en mode 0/1/2/3 pour faire initialiser les tables

!     ces tours de passe-passe sont necessaires pour eviter une reference 
!     externe a la librairie MPI, ce qui rendrait sa presence obligatoire
!     pour tout appel a constnt/constnt_x et constituerait un ennui serieux
!     pour tout programme non MPI qui se servirait de ces routines

    real(kind = real64) :: valeur8
    pointer (pv8, valeur8)
    integer :: i, ncns, ier, iunread, count, dsize
    integer, dimension(2) :: istrt, iend
    logical :: fexist

    real(kind = real64), dimension(maxcns) :: val

    character(len = 8) :: tname
    character(len = 8), dimension(maxcns) :: name
    character(len = 42) :: text

    common /constnt_pdata/ istrt, name, val, ncns, iend
    data name /maxcns * ' '/

    flag = 0

    mode = mod(mode0, 100)
    pv8  = loc(valeur)
    if (mode == 4) then
        ! taille d'un "integer"
        dsize = ( loc(istrt(2)) - loc(istrt(1)) )
        ! nombre d' "integers" dans /constnt_pdata/
        count = ( loc(iend(1)) - loc(istrt(1)) ) / dsize
        call bcast_mpi(istrt, count, dsize, root, comm, ierror)
        ! ca donne  (root et comm seront ignores)
        ! call RPN_COMM_bcst_x(istrt, COUNT, dsize, ROOT, COMM, IERROR)
        ! et par ricochet
        ! call RPN_COMM_bcast (istrt, count * dsize / 4, "MPI_INTEGER", 0, "grid", err)
        return
    end if
    if (name(1) == ' ') then
        ! lire les tables en provenance du fichier approprie
        iunread = 0
        inquire(file = './constantes', exist = fexist)
        if (fexist) then
            ier = fnom(iunread, 'constantes', 'FTN+SEQ+FMT', 0)
        else
            ier = fnom(iunread, '@thermoconsts', 'FTN+SEQ+FMT', 0)
        end if

        ! Le fichier de constantes n'a pas été trouvé
        if (ier .ne. 0) return

        if (mode == 1) then
            write(6, 600)
            write(6, 602)
        end if

        do i = 1, maxcns
            read(iunread, '(2X,A8,2X,E20.13,2X,A42)', end = 2, err = 3) name(i), val(i), text
            if (mode == 1) write(6, 605) name(i), val(i), text
            ncns = i
        enddo
 2     flag = 1

 3     ier = fclos(iunread)

        if (mode == 1) return
    end if

    ! Fin de l'initialisation de name et val

    tname = nom

    ! option  mode=0, aller chercher la valeur d'une constante
    if (mode == 0) then
        flag = 0
        if (mode0 < 100) then
            valeur = 0
         else
            valeur8 = 0
         endif
         do i = 1, ncns
            if (tname == name(i)) then
                flag = 1
                if (mode0 < 100) then
                    valeur = val(i)
                else
                    valeur8 = val(i)
                end if
                return
            end if
         end do

    ! option  mode=2, ajouter une constante
    else if (mode == 2)then
        flag = 0
        ! verifier si constante existe deja
        do i = 1, ncns
            if (tname == name(i)) then
                write(app_msg, 620) tname
                call lib_log(APP_LIBRMN,APP_WARNING,app_msg)
620             format(/, 5X, 'constnt_x: LA CTE', 1X, A8, 1X, 'EXISTE DEJA', /)
                ! return avec flag = 0 ?
                stop
            end if
        end do
        if (ncns < maxcns) then
            flag = 1
            ncns = ncns + 1
            if (mode0 < 100) then
                val(ncns) = valeur
            else
                val(ncns) = valeur8
            end if
            name(ncns) = tname
         else
            write(app_msg, 625)
            call lib_log(APP_LIBRMN,APP_ERROR,app_msg)
625         FORMAT(/, 5X, 'constnt_x: NB DE CONSTANTES DEPASSE LA LIMITE', /)
            ! return avec flag = 0 ?
            stop
        end if

        ! option  mode=3,  modifier la valeur d'une constante
        else if (mode == 3) then
            flag = 0
            ! verifier si constante existe
            do i = 1, ncns
                if (tname == name(i)) then
                    flag = 1
                    if (mode0 < 100) then
                        val(i) = valeur
                    else
                        val(i) = valeur8
                    endif
                    exit
                end if
            end do
            if (flag == 0) then
                write(app_msg, 630) tname
                call lib_log(APP_LIBRMN,APP_WARNING,app_msg)
                stop
            endif
      endif

600   FORMAT(1H1, 10X, 'LISTE DES CONSTANTES COMMUNES CMC-RPN', ///)
602   FORMAT(2X, 'NOM', 17X, 'VALEUR', 8X, 'DESCRIPTION', 10X, 'UNITE', //)
605   FORMAT(2X, A8, 2X, E20. 13, 2X, A42)
630   FORMAT(/, 5X, 'constnt_x: LA CTE A MODIFIER', 1X, A8, 1X, "N'EXISTE PAS", /)
end

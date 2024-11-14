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


!> Get the number of day since the beginning of the current year
real function jjulien(DEET, NPAS, IDATE)
    ! use rmn_common
    use iso_fortran_env, only : real64
    use rmn_date
    implicit none

    !> Taille du pas-de-temps en seconde
    real, intent(in) :: DEET
    !> Nombre de pas de temps depuis IDATE
    integer, intent(in) :: NPAS
    !> DATE TIME STAMP CMC valide
    integer, intent(in) :: IDATE

    !     Description:
    !      Cette fonction calcule le nombre de jour (ordinal) 
    !      depuis le debut de l'annee en cours, utilisant la 
    !      date d'analyse, le nombre de pas de temps effectues 
    !      depuis cette date et la taille du pas de temps.

    ! Si DEET et/ou NPAS sont nuls, le jour correspondant a IDATE est tout de meme retourne.

    integer             :: jour, mois, annee
    integer             :: jdebut, jfin, datim, is1(2), is2
    real(kind = real64) :: heures
    integer :: dummy

    external :: datmgp, jdatec

    ! Calculer le nombre d'heures depuis le debut de l'integration.
    If ( DEET <= 0 .or. NPAS <= 0 ) Then
        heures = 0.0
    Else
        heures = dble(npas) / ( 3600.0 / dble( deet ) )
    End If

    ! Determiner le date-time-stamp correspondant.
    call incdatr( datim, IDATE,  heures )

    ! Extraire l'annee, le mois, le jour et l'heure correspondante.

    ! In mode -3, newdate only uses the first element of its second parameter, but
    ! to make Intel 2024.2 happy with "-warn all", it must be declared as an array
    dummy = newdate(datim, is1, is2, -3)

    heures = is2 / 1000000
    annee  = is1(1) / 10000
    jour   = mod(is1(1), 100)
    mois   = mod(is1(1) / 100, 100)

    ! Trouver le jour julien du debut de cette annee de meme que le jour julien final.
    call jdatec( jdebut, annee,  01,   01  )
    call jdatec( jfin,   annee, mois, jour )

    ! jjulien est la difference de ces deux jours.
    heures  = heures / 24.0
    jjulien = jfin - jdebut + 1 + heures
End

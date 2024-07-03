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


!> \brief Fermer un fichier BURP
!! \author Jame Caveen
!! \date 1990
INTEGER FUNCTION mrfcls( iun )

    use app
    use rmn_burp_defi
    use rmn_burpopt
    IMPLICIT NONE

    !> Numéro d'unité associé au fichier
    INTEGER, INTENT(IN) :: iun

#include <rmn/codes.cdk>

    integer, external :: qdfmsig
    integer, external :: xdfcls

    mrfcls = 0
    IF (badtbl /= 0) THEN
        mrfcls = qdfmsig(iun, 'bRp0')
    END IF
    IF (mrfcls < 0) THEN
        RETURN
    END IF

    mrfcls = xdfcls( iun )
    IF (mrfcls < 0) THEN 
        RETURN
    END IF

    write(app_msg, *) 'MRFCLS: Unite ', iun, ' fichier rapport est ferme'
    call lib_log(app_libfst, app_trivial, app_msg)
END

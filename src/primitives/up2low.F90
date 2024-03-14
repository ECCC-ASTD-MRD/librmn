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

!> Convert string inStr from upper case to lower case, output in outStr
!>
!> This subroutine assumes the ASCII character set!
SUBROUTINE up2low(inStr, outStr)
    IMPLICIT NONE

    CHARACTER(len=*), intent(IN) :: inStr
    CHARACTER(len=*), intent(OUT) :: outStr

    INTEGER :: I, L, L2

    L = MIN(LEN(inStr), LEN(outStr))
    L2 = LEN(outStr)
    DO I = 1, L
        IF ((ICHAR(inStr(I:I)) >= 65) .AND. (ICHAR(inStr(I:I)) <= 90)) THEN
            outStr(I:I) = CHAR(ICHAR(inStr(I:I)) + 32)
        ELSE
            outStr(I:I) = inStr(I:I)
        ENDIF
    END DO

    DO I = L + 1, L2
        outStr(I:I) = ' '
    END DO
END

!> Convert string inStr from lower case to upper case, output in outStr
!>
!> This subroutine assumes the ASCII character set!
SUBROUTINE low2up(inStr, outStr)
    IMPLICIT NONE

    CHARACTER(len=*), intent(IN) :: inStr
    CHARACTER(len=*), intent(OUT) :: outStr

    INTEGER :: I, L, L2

    L = MIN(LEN(inStr), LEN(outStr))
    L2 = LEN(outStr)
    DO I = 1, L
        IF ((ICHAR(inStr(I:I)) >= 97) .AND. (ICHAR(inStr(I:I)) <= 122)) THEN
            outStr(I:I) = CHAR(ICHAR(inStr(I:I)) - 32)
        ELSE
            outStr(I:I) = inStr(I:I)
        ENDIF
    END DO

    DO I = L + 1, L2
          outStr(I:I) = ' '
    END DO
END

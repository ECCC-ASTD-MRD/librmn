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


#include <ftnmacros.hf>


!> Compact or decompact real numbers
!> For IEEE format (1 sign, 8 exponant 32 mantisse)
subroutine ieeepak(IFLD, IPK, NI, NJ, NPAK, serpas, mode)
    use app
    IMPLICIT NONE
    !> Size along the X axis
    integer, intent(in) :: NI
    !> Size along the Y axis
    integer, intent(in) :: NJ
    !> Unpacked integer array
    integer, intent(inout) :: IFLD(0:*)
    !> Packed integer array
    integer, intent(inout)  :: IPK(*)
    !> | Value  | Description                      |
    !> | -----: | :------------------------------- |
    !> |    > 0 | Compaction density               |
    !> | 0 or 1 | No compaction                    |
    !> |    < 0 | Number of bits to keep per items |
    integer, intent(in) :: NPAK
    !> Not used!
    integer, intent(in) :: serpas
    !> 1 to pack, 2 to unpack
    integer, intent(in) :: mode

    ! NOMBRE DE MOTS DE L'ENREGISTREMENT COMPACTE
    INTEGER :: NWR
    ! NOMBRE DE BITS UTILISES PAR ITEM
    INTEGER :: NBITS


    INTEGER :: I, INDX, ITEMP, LEFT, ITEM, NINJ, INEXT
    INTEGER :: RBITS, RLEFT, ISAVE

    IF (NPAK .GT. 1) THEN
        NBITS = MAX(1, BITMOT / NPAK)
    ELSE IF (NPAK .LT. 0) THEN
        NBITS = MIN(64, -NPAK)
    ELSE
        NBITS = BITMOT
    ENDIF

    NINJ = NI * NJ

    RBITS = RMASK(NBITS)
    NWR = (NINJ * NBITS + BITMOT - 1) / BITMOT

#if defined (ALL64)
    IF (NBITS .NE. 32) THEN
        write(app_msg,*) 'IEEEPAK: NBITS must be 32, NBITS=', nbits
        call lib_log(APP_LIBRMN,APP_ERROR,app_msg)
        stop 'IEEEPAK' 
    ENDIF
    IF (mode .EQ. 1) THEN
        ! COMPACTER
        CALL MOVR8R4(IFLD,IPK,NINJ)
    ELSE
        ! DECOMPACTER
        CALL MOVR4R8(IPK,IFLD,NINJ)
    ENDIF
#else

    IF (mode .EQ. 1) THEN
        ! COMPACTER
        IF (NBITS .GE. BITMOT) THEN
            IF (NBITS .EQ. BITMOT) THEN
                DO I = 0, NWR - 1
                    IPK(I+1) = IFLD(I)
                END DO
            ELSEIF (NBITS .EQ. 64) THEN
                CALL MOVE6432(IFLD,IPK,NINJ)
            ELSE
                write(app_msg,*) 'IEEEPAK: NBITS must be <=32 or 64, NBITS=', nbits
                call lib_log(APP_LIBRMN,APP_ERROR,app_msg)
                stop 'IEEEPAK' 
            ENDIF
        ELSE
            INDX = 0
            LEFT = BITMOT
            ITEMP = 0
            DO I = 0, NINJ - 1
            ITEM = IAND(RSHIFT(IFLD(I),32-NBITS) , RBITS)
            IF (NBITS .LE. LEFT) THEN
                ITEMP = IOR(LYSHIFT(ITEMP,NBITS) , ITEM)
                LEFT = LEFT - NBITS
            ELSE
                INDX = INDX+1
                IPK(INDX) = IOR(LYSHIFT(ITEMP,LEFT), RSHIFT(ITEM,NBITS-LEFT))
                ITEMP = IAND(ITEM , RMASK(NBITS-LEFT))
                LEFT = LEFT - NBITS + BITMOT
            ENDIF
            END DO

            IF (LEFT .LT. BITMOT) THEN
            INDX = INDX + 1
            IPK(INDX) = IOR(LYSHIFT(ITEMP,LEFT), IAND(IPK(INDX),RMASK(LEFT)))
            ENDIF
        ENDIF

    ELSE
        ! DECOMPACTER
        IF (NBITS .GE. BITMOT) THEN
            IF (NBITS .EQ. BITMOT) THEN
                DO I = 0, NWR - 1
                    IFLD(I) = IPK(I+1)
                END DO
            ELSEIF (NBITS .EQ. 64) THEN
                CALL MOVE6432(IPK,IFLD,NINJ)
            ELSE
                write(app_msg,*) 'IEEEPAK: NBITS must be <=32 or 64, NBITS=', nbits
                call lib_log(APP_LIBRMN,APP_ERROR,app_msg)
                stop 'IEEEPAK' 
            ENDIF
        ELSE
            ITEMP = 0
            ISAVE = IPK(1+ITEMP)
            DO I = NWR, 2, -1
            IFLD(I-NWR+NINJ-1) = IPK(I+ITEMP)
            END DO
            INDX = (NINJ-NWR) + 1
            LEFT = BITMOT
            ITEMP = LXSHIFT(ISAVE,BITMOT-LEFT)
            INEXT = IFLD(INDX)

            DO I = 0, NINJ - 1
            IF (NBITS .LE. LEFT) THEN
                IFLD(I) = IAND(RYSHIFT(ITEMP,BITMOT-NBITS),RBITS)
                ITEMP = LYSHIFT(ITEMP,NBITS)
                LEFT = LEFT - NBITS
            ELSE
                RLEFT = RMASK(NBITS-LEFT)
                IFLD(I)=IOR(IAND(IAND(RYSHIFT(ITEMP,BITMOT-NBITS),RBITS), &
        &            NOT(RLEFT)),IAND(RYSHIFT(INEXT,BITMOT-(NBITS-LEFT)), &
        &            RLEFT))
                INDX = INDX+1
                ITEMP = LYSHIFT(INEXT,NBITS-LEFT)
                INEXT = IFLD(INDX)
                LEFT = LEFT - NBITS + BITMOT
            ENDIF
            IFLD(I) = LSHIFT(IFLD(I),32-NBITS)
            END DO
        ENDIF
    ENDIF
#endif
END subroutine

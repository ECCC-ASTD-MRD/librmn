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
 
#define NKLEMAX 1024


module ccard_private
    USE ISO_C_BINDING
    IMPLICIT NONE
    PROCEDURE(), pointer, save :: callback => NULL()
end module ccard_private


subroutine CCARD_callback(p)
    USE ccard_private
    IMPLICIT NONE
    external p

    call C_F_PROCPOINTER(C_FUNLOC(p), callback)
end subroutine


!> Parse command line; retreive key values and positionnal arguments
!>
!> If a key name is present in the command line without '=' or followed by a value,
!> the corresponding value in def is given for that key.  If the name is fallowed
!> by a value, that value is given to the key.  Otherwise, the key is given the
!> corresponding value from val
!>
!> All the arguments preceding the first key and following '--' are handled as
!> positionnal arguments.
!>
!> Values can be given in the followin manner:
!>
!>     -key value
!>     -key val1:val2:val3:...
!>     -key 2:3:-4
!>     -key =-2:3:4
!>     -key =-22
!>     -key=-22:33:...
!>
!> When '-h' or '-H' is present in the command line, ccard prints the calling convention
!>
!> If there is an error in the command line, it will be printed along with the
!> keys and their current value
subroutine ccard(incle, def, val, nbkeys, ii)
    use app
    use ccard_private
    IMPLICIT NONE

    !> Number of keys
    INTEGER, intent(in) :: nbkeys
    !> If > 0, return the number of positional arguments.  If -111 or 111, stop on error.  If <= 0, no value returned but will not stop on error
    INTEGER, intent(inout) :: ii
    !> Key names
    CHARACTER(len = *), dimension(nbkeys), intent(in) :: incle
    !> Second default values
    CHARACTER(len = *), dimension(nbkeys), intent(in) :: def
    !> Final key values
    CHARACTER(len = *), dimension(nbkeys), intent(out) :: val

    INTEGER qqqobm, QQQOENV
    EXTERNAL qqqobm, QQQSAP, low2up, QQQTRNS
    EXTERNAL QQQOENV, qqexit
    INTEGER i, pos, j, posc, idx, cllng, posmoin, posmoinc
    ! TYPE DE CLE -1=MINUS, 1=MAJUS, 0=PAREIL
    INTEGER cltype(NKLEMAX)
    CHARACTER * 50 cleup, cle(NKLEMAX), cletemp
    CHARACTER * 8192 argup, ARG, ccard_args, ARGtemp
    CHARACTER * 60 keyName
    LOGICAL pasfini, plante
    INTEGER status, interne
    Integer largenv

    character(len = *), parameter :: errorFmt = "(a,i4.4,a)"

    ! SI abs(II) = 111, LE PROGRAMME ARRETE DES LA premierE ERREUR RENCONTREE SINON, ON CONTINUE
    plante = abs(ii) == 111
    call getenv('CCARD_OPT', ccard_args)
    largenv = len_trim(ccard_args)
    IF (largenv > 0) THEN
        IF (ccard_args(1:largenv) == 'ABORT') THEN
            plante = .true.
        END IF
    END IF
    call getenv('CCARD_ARGS', ccard_args)
    largenv = len_trim(ccard_args)

    ! INITIALISER LE VECTEUR DE TYPE DE CLEFS
    DO i = 1, nbkeys
        cletemp = incle(i)
        IF (TRIM(incle(i)) /= TRIM(cletemp)) THEN
            write(app_msg, "(a,i4,a)") 'ccard: key name #', i, ' > 50 character limit'
            call lib_log(APP_LIBRMN,APP_ERROR,app_msg)
            if (plante) call qqexit(21)
        END IF
        CALL low2up(incle(i), cleup)
        DO j = 50, 1, -1
            IF (cleup(j:j) /= ' ') THEN
                idx = j
                EXIT
            END IF
        END DO
 
        IF (cleup(idx:idx) == '.') THEN
            cltype(i) = 0
            cleup(idx:idx) = ' '
        ELSE IF (cleup(idx:idx) == '_') THEN
            cltype(i) = -1
            cleup(idx:idx) = ' '
        ELSE IF(cleup(idx:idx) == ':') THEN
            cltype(i) = 2
            cleup(idx:idx) = ' '
        ELSE
            cltype(i) = 1
        END IF
        cle(i) = cleup
    END DO

    ! TROUVER LA POSITION DE LA premierE CLEF - DANS LA LISTE CLE

    posmoin = 1
    posmoinc = 1
    DO j = 1, nbkeys
        IF (cle(j) == '-') THEN
            posmoinc = j
            posmoin  = j
            EXIT
        END IF
    END DO

    pos = 1
    posc = 1
    interne = 0
    pasfini = .TRUE.
    DO WHILE (pasfini)
        if (largenv > 0) then
            status = qqqoenv(arg, ccard_args, largenv)
        else
            status = qqqobm(arg, cltype(pos))
        end if

        IF (status == 1) THEN
            ! argup is a keyname
            CALL low2up(arg, argup)
            DO j = 1, nbkeys
                cleup = cle(j)
                IF (trim(argup) == trim(cleup)) THEN
                    posc = j
                    pos = j
                    CALL qqqtrns(val(pos), def(pos), cltype(pos))
                    ! print *,'Debug+ ccard cleup=',cleup,' val(pos)=',val(pos)
                    write(keyname, errorFmt) '%%' // trim(cle(posc)), posc - pos, '%%'
                    call c_set_appl_var(keyName, val(pos))
                    EXIT
                END IF
            END DO

            IF (j > nbkeys) THEN
                cllng = len_trim(argup)
                write(app_msg,*) 'ccard: Unknow key ', argup(1:cllng)
                call lib_log(APP_LIBRMN,APP_ERROR,app_msg)
                CALL qqqsap(cle, def, val, nbkeys)
                IF (plante) THEN
                    CALL qqexit(22)
                END IF
            END IF

        ELSE IF (status == 2) THEN
            ! argup is a value for a key
            IF (posc /= 0 .AND. cle(pos) == cle(posc)) THEN
                CALL qqqtrns(argtemp, arg, cltype(posc))
                VAL(posc) = ARGtemp
                ! print *,'Debug+ ccard cle(posc)=',cle(posc),' ARGtemp=', trim(ARGtemp)
                ! print *,'Debug+ ccard val(posc)=',val(posc)
                write(keyname, errorFmt) '%%' // trim(cle(posc)), posc - pos, '%%'
                call c_set_appl_var(keyname, argtemp)
                posc = posc + 1
            ELSE
                call lib_log(APP_LIBRMN,APP_ERROR,'ccard: List overflow')
                CALL qqqsap(cle, def, val, nbkeys)
                IF (plante) THEN
                    CALL qqexit(23)
                ENDIF
            END IF

        ELSE IF (status == 3) THEN
            ! argup is a positionnal argument
            IF (posmoinc /= 0 .AND. cle(posmoin) == cle(posmoinc)) THEN
                CALL qqqtrns(argtemp, arg, cltype(posmoinc))
                val(posmoinc) = argtemp
                write(keyname, errorFmt) '%%' // trim(cle(posmoinc)), posmoinc - pos, '%%'
                call c_set_appl_var(keyname, argtemp)
            ELSE
                call lib_log(APP_LIBRMN,APP_ERROR,'ccard: List overflow or positionnal mode not allowed')
                CALL qqqsap(cle, def, val, nbkeys)
                IF (plante) THEN
                    CALL qqexit(24)
                END IF
            END IF
            posmoinc = posmoinc + 1
            interne = interne + 1

        ELSE IF (status == 5) THEN
            ! Help printout requested
            CALL qqqsap(cle, def, val, nbkeys)
            if (associated(callback)) then
                print *, ''
                call callback
            end if
            CALL qqexit(0)
        ELSE
            pasfini = .FALSE.
        END IF
     END DO

    ! RETOURNER LE NOMBRE D'ARGUMENTS ASSOCIES A LA CLEF - SI DEMANDE
    IF (ii > 0) THEN
        ii = interne
    END IF
END SUBROUTINE


!> Get key name or value of an argument
!> ＼return The return value has the followin meaning:
!> | Value | Description                     |
!> | ----: | :------------------------------ |
!> |     0 | Everything done                 |
!> |     1 | arg is a key name               |
!> |     2 | arg is a value to give to a key |
!> |     3 | arg is a positionnal argument   |
!> |     5 | Help printout requested         |
INTEGER FUNCTION qqqobm(arg, cltype)
    IMPLICIT NONE

    !> Key name or value
    CHARACTER(len = 8192), intent(out) :: arg
    integer, intent(in) :: cltype

    LOGICAL :: pasdcle
    LOGICAL :: pudcle
    LOGICAL :: premier
    INTEGER :: argnum
    INTEGER :: iindex
    INTEGER :: indfin
    INTEGER :: narg
    INTEGER :: i
    INTEGER :: indeb
    INTEGER :: j
    CHARACTER(len = 8192) :: argup
    character(len = 1) :: delim

    SAVE argup
    SAVE pasdcle, pudcle, premier, argnum, narg, iindex, indfin
    DATA pasdcle, pudcle, premier /.TRUE., .FALSE., .TRUE./
    DATA argnum, narg, iindex, indfin /0, 0, 0, 0/


    if (cltype == 2) then
        delim = '='
    else
        delim = ':'
    endif

    ! Shitty loop condition used as a simple replacement for GOTOs and labels
    DO WHILE (.TRUE.)
        qqqobm = 0
        DO i = 1, 8192
            arg(i:i) = ' '
        END DO

        IF (premier) THEN
            narg = COMMAND_ARGUMENT_COUNT()
            premier = .FALSE.
        END IF

        IF (iindex >= indfin) THEN
            ! ALLER CHERCHER UN ARGUMENT
            argnum = argnum + 1
            IF (argnum > narg)   RETURN

            DO I = 1, 8192
                argup(I:I) = ' '
            END DO

            CALL GETARG(argnum, argup)
            IF (((narg == 1) .AND. ((INDEX(argup, '-H ') /= 0) .OR. (INDEX(argup,'-h ') /= 0) &
        &          .OR. (INDEX(argup,'-HELP') /= 0) .OR. (INDEX(argup,'-help') /= 0) ))) THEN
                qqqobm = 5
                RETURN
            END IF

            iindex = 1

            ! TROUVER LA FIN DE argup
            indfin = len_trim(argup)
        END IF

        ! EXTRAIRE UN NOM DE CLEF OU UNE VALEUR
        IF ((iindex == 1) .AND.  (argup(1:1) == '-') .AND. (.NOT. pudcle)) THEN
            ! NOM DE CLEF
            pasdcle = .FALSE.
            qqqobm = 1
            iindex = 2
            IF (argup(2:2) == '-') THEN
                pudcle = .TRUE.
                ! CHERCHER PROCHAIN ARG POSITIONEL
            END IF
            EXIT
        ELSE
            ! VALEUR A DONNER
            IF (pudcle .OR. pasdcle) THEN
                ! ARGUMENT POSITIONEL
                qqqobm = 3
                arg = argup
                iindex = indfin
                RETURN
            ELSE
                qqqobm = 2
            END IF
            IF ((argup(iindex:iindex) == delim) .OR. (argup(iindex:iindex) == '=')) then
                iindex = iindex + 1
            END IF
            EXIT
        END IF
    END DO

    indeb = iindex
    j = 1
    DO I = indeb, indfin
        IF ((argup(i:i) == '=') .OR. (argup(i:i) == delim)) THEN
            EXIT
        ELSE
            arg(j:j) = argup(i:i)
            iindex = iindex + 1
            j = j + 1
        END IF
    END DO
END


!**S/P  QQQOENV - OBTENIR UN NOM DE CLEF OU UNE VALEUR D'UN ARGUMENT
!                 A PARTIR D'UNE VARIABLE D'ENVIRONNEMENT CONTENANT LA
!                 SEQUENCE D'APPEL COMPLETE

INTEGER FUNCTION QQQOENV(ARGP,ccard_args,L)
        IMPLICIT NONE
        CHARACTER * 8192 ARGP, ccard_args
        INTEGER L

!AUTEUR          M. Lepine,  Fevrier 2003

!OBJET(QQQOENV)
!  FONCTION QUI PERMET D'ALLER CHERCHER LES ARGUMENTS D'UNE ==ENCE
!  D'APPEL A UN PROGRAMME ET D'EN EXTRAIRE LES NOMS DE CLEFS ET LES
!  VALEURS A DONNER A CES CLEFS.
!  LA FONCTION QQQOENV RETOURNE:
!       - UNE VALEUR DE 1 SI ARG CONTIENT UN NOM DE CLEF
!       - UNE VALEUR DE 2 SI ARG CONTIENT UNE VALEUR A DONNER A UNE CLEF
!       - UNE VALEUR DE 3 SI ARG CONTIENT UN ARGUMENT POSITIONEL
!       - UNE VALEUR DE 5 SI ON DEMANDE LA ==ENCE D'APPEL
!       - UNE VALEUR DE 0 LORSQUE TOUT EST FINI

!ARGUMENT:
!         ARGP    SORTIE     NOM DE CLEF OU VALEUR RETOURNE

!*
        Integer pos, i, ic, indfin
        Logical debut, pudcle
        character c, quotechar
        character * 8192 arg
        SAVE        pos,debut
        DATA        debut, pudcle /.true., .false./

        if (debut) then
          pos = 1
          debut = .false.
        endif

 100    continue
        i = 1
        arg = ' '
        argp = ' '

        if (pos > L) then
           qqqoenv = 0        ! termine, fin de la sequence d'appel
           return
        endif

        c = ccard_args(pos:pos)
        getarg: DO                    ! obtenir la prochaine cle, valeur
          if ((pos > L) .or. (c == ' ').or.(c == ':')) EXIT getarg
          if ((c /= '''') .and. (c /= '"')) then
            arg(i:i) = c
            pos = pos +1
            i = i + 1
            if (pos <= L) c = ccard_args(pos:pos)
          else
            quotechar = c
            pos = pos + 1
            c = ccard_args(pos:pos)
            quote: DO
                       if (c == quotechar) EXIT quote
                  if (pos > L) then
                  print *,'CCARD, qqqoenv error: unmatched quote'
                  EXIT quote
                endif
                arg(i:i) = c
                 i = i + 1
                pos = pos +1
                if (pos <= L) c = ccard_args(pos:pos)
            END DO quote
            pos = pos +1
            if (pos <= L) c = ccard_args(pos:pos)
          endif
        END DO getarg

        indfin = len_trim(arg)

        if ((arg(1:1) == '-') .and. (.not. pudcle)) then
           qqqoenv = 1           ! nom de cle
           ic = 2                ! position de copie apres '-'
           if (arg(2:2) == '-') then   ! -- passage en mode positionel
              pudcle = .true.
              goto 100                    ! prochain argument positionel
           endif
        else
           if (pudcle) then                ! argument positionel
              qqqoenv = 3
              ic = 1
           else
              qqqoenv = 2                  ! valeur
              ic = 1
           endif
           if (arg(1:1) == '=') ic = 2  ! passer le caractere d'escape
        endif
        argp = arg(ic:indfin)
        if ((qqqoenv == 1) .and. (indfin == 2) .and. &
     & ((arg(1:2) == '-h') .or. (arg(1:2) == '-H') .or. &
     &  (arg(1:5) == '-help') .or. (arg(1:5) == '-HELP') )) then
          qqqoenv = 5                      ! sequence d'appel demande
        endif
        pos = pos + 1     ! positionnement au debut du prochain argument

        return
END FUNCTION


!> Coy input to ouput while applying specified case conversion
SUBROUTINE qqqtrns(sorti, entre, traduction)
    !> Converted text
    CHARACTER(LEN = *), INTENT(OUT)  :: sorti
    !> Input text
    CHARACTER(LEN = *), INTENT(IN) :: entre
    !> If 1, convert to uppercace.  If -1 convert to lowercase.  Otherwise, copy as-is.
    INTEGER, INTENT(IN) :: traduction

    EXTERNAL low2up, up2low

    IF (traduction == 1) THEN
        CALL low2up(entre, sorti)
    ELSE IF (traduction == -1) THEN
        CALL up2low(entre, sorti)
    ELSE
        sorti = entre
    END IF
END SUBROUTINE


!> Print program argument syntax along with default values
SUBROUTINE qqqsap(cle, def, val, nbkeys)
    !> Number of keys
    INTEGER, INTENT(IN) :: nbkeys
    !> Kay names
    CHARACTER(LEN = *), DIMENSION(nbkeys), INTENT(IN) :: cle
    !> Second default values
    CHARACTER(LEN = *), DIMENSION(nbkeys), INTENT(IN) :: def
    !> First default values or current values
    CHARACTER(LEN = *), DIMENSION(nbkeys), INTENT(IN) :: val

    CHARACTER(LEN = 8192) :: lenom
    INTEGER :: i, nomlng, clelng, deflng, valng
    CHARACTER(LEN = 128) :: clepre
    INTEGER :: repcle

    ! ON OBTIENT LE NOM DU PROGRAMME APPELANT
    CALL GETARG(0, lenom)

    WRITE(6, *) ' *** SEQUENCE D''APPEL ***'

    nomlng = len_trim(lenom)
    WRITE(6,*) lenom(1:nomlng)
    clepre = ' '
    repcle = 0

    DO i = 1, nbkeys
        IF (trim(clepre) == trim(cle(i))) then
            repcle = repcle + 1
        ELSE
            IF (repcle > 0) THEN
                print '(A,I4,A)', '     [repeated', repcle, ' time(s)]'
            END IF
            repcle = 0
            clepre = trim(cle(i))
            clelng = len_trim(cle(i))
            deflng = len_trim(def(i))
            valng = len_trim(VAL(i))
            WRITE(6, "('     -',A,' [',A,':',A,']')") cle(i)(1:clelng), VAL(i)(1:valng), def(i)(1:deflng)
        END IF
    END DO
    IF (repcle > 0) THEN
        print '(A,I4,A)', '     [repeated', repcle, ' more time(s)]'
    END IF
END SUBROUTINE

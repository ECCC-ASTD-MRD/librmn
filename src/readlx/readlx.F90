!> \file


!> \todo A lot of global variables in theses modules are not initialized. Is that OK?
!> (there was no "DATA" statement for them in the common blocks)
module readlx_parmadr
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    save

    !> Number of arguments defined for the latest readlx call
    integer :: narg
    integer :: nprm
    integer :: ndopes

    integer, parameter :: max_dope = 41

    integer, dimension(max_dope) :: dope
    integer, dimension(max_dope + 1)  :: dopea
    integer, dimension(101) :: dopes
    integer, dimension(101) :: parm = 0
    integer(kind = int64), dimension(max_dope) :: adr = 0
end module readlx_parmadr


module readlx_qlxbuff
    implicit none
    save

    integer :: NC = 1
    integer :: LAST = 0
    integer :: INPFILE = 5
    integer :: NERR, SKIPFLG, CURREC, READREC, TMPFILE
    logical :: EOFL = .false.

    character(len = 101) :: INLINE = ' '
end module readlx_qlxbuff


module readlx_qlxfmt
    implicit none
    save

    character(len = 20) :: LINEFMT
    integer :: KARMOT = 04
end module readlx_qlxfmt


module readlx_nrdlx
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    save

    INTEGER, dimension(3:3, 256) :: ITAB(3:3, 256) = 0
    integer :: NENTRY = 0
    character(len = 8), dimension(256) :: NAMES = ' '
    integer(kind = int64), dimension(2, 256) :: IPTADR = 0
end module readlx_nrdlx


module readlx_remote
    use iso_c_binding, only: c_int32_t, c_int64_t, c_ptr, c_funptr, c_f_procpointer
    implicit none
contains
    integer(C_INT32_t) function remote_call(fn, args) BIND(C,name='RemoteCall')
        abstract interface
            integer(C_INT32_t) function machin( &
                                    a00,a01,a02,a03,a04,a05,a06,a07,a08,a09, &
                                    a10,a11,a12,a13,a14,a15,a16,a17,a18,a19, &
                                    a20,a21,a22,a23,a24,a25,a26,a27,a28,a29, &
                                    a30,a31,a32,a33,a34,a35,a36,a37,a38,a39, &
                                    a40) BIND(C)
            use iso_c_binding, only: c_int32_t, c_ptr
            type(C_PTR), intent(IN), value :: &
                                        a00,a01,a02,a03,a04,a05,a06,a07,a08,a09, &
                                        a10,a11,a12,a13,a14,a15,a16,a17,a18,a19, &
                                        a20,a21,a22,a23,a24,a25,a26,a27,a28,a29, &
                                        a30,a31,a32,a33,a34,a35,a36,a37,a38,a39, &
                                        a40
            end function machin
        end interface

        integer(C_INT64_T), intent(IN) :: fn
        integer(C_INT64_T), dimension(0:40), intent(IN) :: args
        procedure(machin), pointer :: fptr
        type(C_FUNPTR) :: cptr
        type(C_PTR), dimension(0:40) :: v
        integer :: i

        do i = 0, 40
            v(i) = transfer(args(i), v(i))
        enddo
        cptr = transfer(fn, cptr)
        call c_f_procpointer(cptr, fptr)
        remote_call = fptr(v(00), v(01), v(02), v(03), v(04), v(05), v(06), v(07), v(08), v(09), &
                        v(10), v(11), v(12), v(13), v(14), v(15), v(16), v(17), v(18), v(19), &
                        v(20), v(21), v(22), v(23), v(24), v(25), v(26), v(27), v(28), v(29), &
                        v(30), v(31), v(32), v(33), v(34), v(35), v(36), v(37), v(38), v(39), &
                        v(40))
    end function
end module readlx_remote


module qlx_token
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    character(len = 80), save :: token

    logical, save :: inexpr
    !> Number of characters in token
    integer, save :: len
    !> Token type: alpha numeric key, integer, real, string, symbol
    integer, save :: typ
    !> Value of the number contained in the token
    real, save :: zval

    integer(kind = int64), save :: jval64
end module


!> Get contents at address(subscript) (assuming a 32 bit item)
subroutine get_value_at_address(address, subscript, content)
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    !> Base memory address
    integer(kind = int64), intent(IN) :: address
    !> Element index (32 bit stride)
    integer, intent(IN) :: subscript
    !> Memory content at specified address
    integer, intent(OUT) :: content

    integer :: val
    pointer(pval, val(*))

    pval = transfer(address,pval)
    ! print *,'entering get_value_at_address, subscript =',subscript
    content = val(subscript)
    ! print *,'exiting get_value_at_address'
end subroutine


!> Set contents at address(subscript) (assuming a 32 bit item)
subroutine set_value_at_address(address, subscript, content)
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    !> Base memory address
    integer(kind = int64), intent(IN) :: address
    !> Element index (32 bit stride)
    integer, intent(IN) :: subscript
    !> Value to write at specified address
    integer, intent(IN) :: content
    integer :: val
    pointer(pval, val(*))

    pval = transfer(address, pval)
    ! print *,'entering set_value_at_address, subscript =',subscript
    val(subscript) = content
    ! print *,'exiting set_value_at_address'
end subroutine


!> Get value of indexed array component
subroutine qlx_adi2(kle, ind, valeur, err)
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    character(len = *), intent(in) :: kle
    integer, intent(in) :: ind
    integer, intent(out) :: valeur
    logical, intent(out) :: err

    integer, external :: qlx_dtyp
    external :: qlx_fnd

    integer(kind = int64) :: locvar, loccnt
    integer :: limite, ityp, iz, indx
    integer, dimension(1024) :: mem
    pointer(pmem, mem)
    real :: z

    iz = ind
    if (qlx_dtyp(iz) == 1) then
        indx = iz
    else
        z = transfer(iz, z)
        indx = nint(z)
    endif
    call qlx_fnd(kle, locvar, loccnt, limite, ityp)
    if (ityp /= 0 .and. ityp /= 1) then
        err = .true.
    endif
    if (indx > limite .or. indx <= 0) then
        err = .true.
    endif
    if (.not. err) then
        pmem = locvar
        valeur = mem(indx)
    endif
end


!> Get subscript then build memory address
integer(kind = int64) function qlx_adr(kle, err)
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    character(len = *), intent(in) :: kle
    logical, intent(out) :: err

    external :: qlx_err, qlx_fnd, qlx_ind

    integer :: limits, ityp, ind
    integer(kind = int64) :: loccnt, locvar8
    integer :: vari
    pointer(locvar, vari(*))

    call qlx_ind(ind, err)
    if (.not. err) then
        call qlx_fnd(kle, locvar8, loccnt, limits, ityp)
        locvar = transfer(locvar8, locvar)
        if (ind <= limits .and. ityp >= 0 .and. ityp <= 1) then
            qlx_adr = loc(vari(ind))
        else
            err = .true.
            call qlx_err(21017, 'qlx_adr')
            qlx_adr = 0
        endif
    else
        qlx_adr = 0
    endif
end


!> Prend les tokens qui suivent le signe = et separes par des virgules pour les placer a l'adresse val
SUBROUTINE qlx_asg(VAL, ICOUNT, LIMIT, ERR)
    use, intrinsic :: iso_fortran_env, only: int64
    use readlx_qlxfmt
    use qlx_token, only: len, typ, zval, token, jval64
    implicit none

    !> Adresse de la clé cible
    integer(kind = int64), intent(in) :: val
    !> Nombre de mots déposés
    integer, intent(inout) :: icount
    !> Nombre maximal de mots disponibles
    integer, intent(in) :: limit
    !> Indicateur d'erreur
    logical, intent(out) :: err

    EXTERNAL :: qlx_err, qlx_ind, qlx_tok, qlx_xpr, qlx_val, get_value_at_address, set_value_at_address

    INTEGER JVAL
    pointer(pjval,jval)

    INTEGER IND, JLEN, qlx_val
    INTEGER OLDTYP, ITEMP(80), IREPCN
    LOGICAL IAREP, FIN
    INTEGER :: I, J

    pjval = LOC(ZVAL)
    IND = 1
    OLDTYP = 4
    FIN = .FALSE.
    IAREP = .FALSE.
    IREPCN = 1
    JLEN = 0
    CALL qlx_ind(IND, ERR)

    IF (.NOT.ERR) THEN
        CALL qlx_tok
    ENDIF

    IF (TOKEN(1:2) == '= ' .AND. typ == 4 .AND. .NOT. ERR) THEN
        DO WHILE (.NOT.ERR .AND. .NOT.FIN)
            CALL qlx_tok
            IF ((typ == 4) .AND. (TOKEN(1:1) == '(')) THEN
                CALL qlx_xpr(ERR)
                IF (ERR) THEN
                    EXIT
                ENDIF
            ENDIF
            IF (typ == 8) THEN
                call get_value_at_address(jval64, 1, JVAL)
            ELSE
                IF (typ == 1 .AND. OLDTYP == 4) THEN
                    ITEMP(1) = JVAL
                    JLEN = 1
                ELSE
                    IF (typ == 2 .AND. OLDTYP == 4) THEN
                        itemp(1) = transfer(zval, itemp(1))
                        JLEN = 1
                    ELSE
                        IF (typ == 3 .AND. OLDTYP == 4) THEN
                            JLEN = (LEN + KARMOT - 1) / KARMOT
                            READ(TOKEN, LINEFMT)(ITEMP(J), J=1, JLEN)
                        ELSE
                            IF (typ == 4) THEN
                                IF (TOKEN(1:2) == '% ') THEN
                                    IF (OLDTYP == 1 .AND.(.NOT.IAREP)) THEN
                                        IREPCN = ITEMP(1)
                                        IF (IREPCN > 0) THEN
                                            IAREP = .TRUE.
                                            JLEN = 0
                                        ELSE
                                            CALL qlx_err(21001, 'qlx_asg')
                                            ERR = .TRUE.
                                        ENDIF
                                    ELSE
                                        CALL qlx_err(21002, 'qlx_asg')
                                        ERR = .TRUE.
                                    ENDIF
                                ELSE
                                    IF (TOKEN(1:2) == ', ' .OR.TOKEN(1:2) == '$ ') THEN
                                        IF ((IREPCN * MAX(JLEN, 1) + IND) > LIMIT + 1) THEN
                                            CALL qlx_err(21003, 'qlx_asg')
                                            ERR = .TRUE.
                                        ELSE
                                            DO I = 1, IREPCN
                                                DO J = 1, JLEN
                                                    call set_value_at_address(VAL, IND + J - 1, ITEMP(J))
                                                END DO
                                                IND = IND + MAX(JLEN, 1)
                                            END DO
                                            IREPCN = 1
                                            IAREP = .FALSE.
                                            JLEN = 0
                                            ICOUNT = IND-1
                                        ENDIF
                                        FIN = TOKEN(1:1) == '$'
                                    ELSE
                                        CALL qlx_err(21004, 'qlx_asg')
                                        ERR = .TRUE.
                                    ENDIF
                                ENDIF
                            ELSE
                                IF (typ == 0 .AND. OLDTYP == 4) THEN
                                    JLEN = 1
                                    ITEMP(1) = qlx_val(TOKEN(1:8), ERR)
                                ELSE
                                    CALL qlx_err(21005, 'qlx_asg')
                                    ERR = .TRUE.
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            OLDTYP = typ
        END DO
    ELSE
        CALL qlx_err(21006, 'qlx_asg')
        ERR = .TRUE.
    ENDIF
END


!> Remettre un caractère dans une ligne de texte à la position courante et reculer le pointeur du caractère courant
SUBROUTINE qlx_bak(ICAR)
    use readlx_qlxbuff
    implicit none

    !> Caractère à remettre dans la ligne de texte
    character(len = 1), intent(in) :: ICAR

    EXTERNAL :: qlx_err

    IF (NC > 1) THEN
        INLINE(NC - 1 : NC - 1) = ICAR
        NC = NC - 1
    ELSE
        CALL qlx_err(81007, 'qlx_bak')
    ENDIF
END


SUBROUTINE qlx_call(SUB, ICOUNT, LIMITS, ERR)
    use, intrinsic :: iso_fortran_env, only: int64
    use readlx_parmadr
    use readlx_qlxfmt
    use readlx_remote
    use qlx_token, only: len, typ, zval, token
    implicit none

    integer(kind = int64) :: SUB, ICOUNT

    INTEGER JVAL
    pointer(pjval, jval)

    EXTERNAL :: qlx_err, qlx_tok, qlx_flsh, set_value_at_address

    integer, external :: rmtcall
    EXTERNAL qlx_adr
    INTEGER, external ::  qlx_val
    INTEGER :: LIM1, LIM2, JLEN, PREVI
    integer(kind = int64) :: LOCDUM, qlx_adr
    character(len = 8) :: KLE
    integer(kind = int64) :: icount64

    LOGICAL :: ERR, FIN, INLIST

    integer :: I, J, NPRM0, LIMITS, JUNK

    pjval = LOC(ZVAL)
    FIN = .FALSE.
    INLIST = .FALSE.
    LOCDUM = LOC(PARM(1))
    NDOPES = 0
    DO I = 1, max_dope
        DOPE(I) = 0
        DOPEA(I) = 0
        ADR(I) = LOCDUM
    END DO
    NARG = 0
    NPRM = 0
    NPRM0 = 0
    PREVI = 4

    CALL qlx_tok
    IF (typ /= 4 .AND. TOKEN(1:1) /= '(') THEN
        CALL qlx_err(81018, 'qlx_call')
        ERR = .TRUE.
    ENDIF

    DO WHILE (.NOT. ERR .AND. .NOT. FIN)
        CALL qlx_tok
        IF (PREVI == 4) THEN
            IF (typ == 0) THEN
                KLE = TOKEN(1:8)
                PREVI = 7
                IF (INLIST) THEN
                    NPRM = MIN(NPRM + 1, 101)
                    PARM(NPRM) = qlx_val(KLE, ERR)
                ELSE
                    NARG = MIN(NARG + 1, 41)
                    ADR(NARG) = qlx_adr(KLE, ERR)
                    DOPEA(NARG) = NDOPES + 1
                    NPRM0 = NPRM - 1
                ENDIF
                NDOPES = MIN(NDOPES + 1, 101)
                DOPES(NDOPES) = typ + 1 * 256 + (NPRM - NPRM0) * 256 * 256
                DOPE(NARG) = DOPE(NARG) + 1
            ELSE
                IF (typ == 1 .OR. typ == 2) THEN
                    NPRM = MIN(NPRM + 1, 101)
                    PARM(NPRM) = JVAL
                    PREVI = 7
                    IF (.NOT. INLIST) THEN
                        NARG = MIN(NARG + 1, 41)
                        ADR(NARG) = LOC(PARM(NPRM))
                        DOPEA(NARG) = NDOPES + 1
                        NPRM0 = NPRM - 1
                    ENDIF
                    NDOPES = MIN(NDOPES + 1, 101)
                    DOPES(NDOPES) = typ + 1 * 256 + (NPRM - NPRM0) * 256 * 256
                    DOPE(NARG) = DOPE(NARG) + 1
                ELSE
                    IF (typ  == 3) THEN
                        JLEN = MIN((LEN + KARMOT - 1) / KARMOT, 101 - NPRM)
                        IF (.NOT. INLIST) THEN
                            NARG = MIN(NARG + 1, 41)
                            ADR(NARG) = LOC(PARM(NPRM + 1))
                            DOPEA(NARG) = NDOPES + 1
                            NPRM0 = NPRM
                        ENDIF
                        READ(TOKEN, LINEFMT) (PARM(J + NPRM), J = 1, JLEN)
                        NDOPES = MIN(NDOPES + 1, 101)
                        DOPES(NDOPES) = typ + LEN * 256 + (NPRM - NPRM0 + 1) * 256 * 256
                        NPRM = MIN(NPRM + JLEN, 101)

                        DOPE(NARG) = DOPE(NARG) + JLEN
                        PREVI = 7
                    ELSE
                        IF (typ == 4 .AND. TOKEN(1:1) == '[' .AND. .NOT.INLIST) THEN
                            INLIST = .TRUE.
                            PREVI = 4
                            NARG = MIN(NARG + 1, 41)
                            ADR(NARG) = LOC(PARM(NPRM + 1))
                            DOPEA(NARG) = NDOPES + 1
                            NPRM0 = NPRM
                        ELSE
                            IF (typ == 4 .AND. TOKEN(1:1) == ')' .AND.NARG == 0) THEN
                                FIN = .TRUE.
                            ELSE
                                CALL qlx_err(81019, 'qlx_call')
                                ERR = .TRUE.
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
        ELSE
            IF (typ == 4 .AND. (TOKEN(1:1) == ',' .OR. TOKEN(1:1) == ')')) THEN
                FIN = TOKEN(1:1) == ')'
                PREVI = 4
            ELSE
                IF (typ == 4 .AND. TOKEN(1:1) == ']' .AND. INLIST) THEN
                    INLIST = .FALSE.
                ELSE
                    CALL qlx_err(81020, 'qlx_call')
                    ERR = .TRUE.
                ENDIF
            ENDIF
        ENDIF
    END DO
    DOPEA(NARG + 1) = NDOPES + 1
    IF (.NOT. ERR) THEN
        LIM1 = LIMITS / 100
        LIM2 = MOD(LIMITS, 100)
        IF (NARG > 40 .OR. NPRM > 100 .OR. NDOPES > 100) THEN
            CALL qlx_err(81021, 'qlx_call')
            ERR = .TRUE.
        ELSE
            IF (NARG < LIM1 .OR. NARG > LIM2) THEN
                CALL qlx_err(81022, 'qlx_call')
                ERR = .TRUE.
            ELSE
                icount64 = ICOUNT
                call set_value_at_address(icount64, 1, NARG)
                junk = remote_call(SUB, ADR)
                call set_value_at_address(icount64, 1, 0)
                CALL qlx_flsh('$')
            ENDIF
        ENDIF
    ENDIF
END


!> Retourne un caractere a la fois d'une ligne
function qlx_chr()
    use App
    use readlx_qlxbuff
#if __INTEL_LLVM_COMPILER >= 20250001
    use ifport, only : abort
#endif
    implicit none

    external :: qlx_err

    character(len = 8), parameter :: SKIPMSG(0:3) = ['<<    >>', '<<SKIP>>', '<<SKIP>>', '<< ** >>']

    character(len = 1) :: qlx_chr
    logical :: comment
    integer :: prtflag

    IF (NC <= LAST) THEN
        qlx_chr = INLINE(NC:NC)
        NC = NC + 1
    ELSE
         IF (.NOT. EOFL) THEN
1           CONTINUE
            IF (READREC > CURREC) THEN
               READREC = 0
            ENDIF
            IF (READREC == 0) THEN
               READ(INPFILE, '(A80)', END = 10) INLINE(21:100)
               CURREC = CURREC + 1
               WRITE(TMPFILE, '(A80)', REC = CURREC) INLINE(21:100)
            ELSE
               READ(TMPFILE, '(A80)', REC = READREC) INLINE(21:100)
               READREC = READREC + 1
            ENDIF
            INLINE(1:20) = ' '
            COMMENT = .FALSE.
            PRTFLAG = SKIPFLG
            IF (INLINE(21:21) == 'C' .OR. INLINE(21:21) == '*' .OR.INLINE(21:21)  == '#') THEN
               IF (PRTFLAG ==  0) THEN
                  COMMENT = .TRUE.
                  PRTFLAG = 3
               ELSE
                  COMMENT = .TRUE.
               ENDIF
            ENDIF
            WRITE(app_msg, '(1X, A8, 1X, A80)') SKIPMSG(PRTFLAG), INLINE(21:100)
            call Lib_Log(APP_LIBRMN, APP_INFO, app_msg)
            IF ((INLINE == ' ') .OR. (COMMENT)) THEN
               GOTO 1
            ENDIF
            LAST = 100
            DO WHILE (LAST > 21 .AND. INLINE(LAST:LAST) == ' ')
                LAST = LAST - 1
            END DO
            IF (INLINE(LAST:LAST)  == '_') THEN
               LAST = LAST - 1
            ELSE
               IF (INLINE(LAST:LAST) /= ',') THEN
                  LAST = LAST + 1
                  INLINE(LAST:LAST) ='$'
               ENDIF
            ENDIF
            qlx_chr = INLINE(21:21)
            NC = 22
         ELSE
            CALL qlx_err(81008, 'qlx_chr')
            CALL ABORT()
         ENDIF
      ENDIF
      RETURN
10    INLINE = ' END$'
      qlx_chr = ' '
      EOFL = .TRUE.
      LAST = 5
      NC = 2
END


SUBROUTINE qlx_dbg
    use app
    use readlx_qlxbuff

    WRITE(app_msg, *) 'qlx_dbg: NC=', NC, 'LAST=', LAST, 'INPFILE=', INPFILE
    call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
    WRITE(app_msg, '(1X,A101)')INLINE(1:101)
    call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
END


!> Get item data type
integer function qlx_dtyp(item)
    implicit none

    integer, intent(in) :: item

    if (abs(item) <= 2147483647) then
        qlx_dtyp = 1
    else
        qlx_dtyp = 2
    endif
end

!> Print error message
!>
!> Terminates execution if error is fatal
subroutine qlx_err(code, module)
    use app
    use readlx_qlxbuff
    implicit none

    !> Code d'erreur
    integer, intent(in) :: code
    !> Module dans lequel l'erreur est survenue
    character(len = *), intent(in) :: module

    integer, parameter :: nb_msg_lvl = 9
    integer, parameter :: msg_lvl(nb_msg_lvl) = [APP_INFO, APP_ALWAYS, 0, 0, 0, 0, 0, APP_FATAL, APP_SYSTEM]

    integer, parameter :: nb_msg = 23
    character(len = 35), parameter :: msg(nb_msg) = [ &
        "REPETITION NEGATIF                ", &
        "NB DE FOIS DEJA VU OU NON ENTIER  ", &
        "LA LIMITE EST DEPASSEE            ", &
        "OPERATEUR MAL PLACE               ", &
        "TOKEN MAL PLACE                   ", &
        "IL MANQUE LE SIGNE EGAL           ", &
        "DEBORDEMENT DU TAMPON D ENTREE    ", &
        "FIN DU FICHIER DEPASSEE           ", &
        "INDICE NEGATIF, NUL OU NON ENTIER ", &
        "MANQUE LE CROCHET DROIT           ", &
        "TABLE DES SYMBOLES PLEINE         ", &
        "LIMITE > 99999                    ", &
        "MAUVAIS CODE DE TYPE              ", &
        "TOKEN DOUTEUX                     ", &
        "CLE MAL UTILISEE                  ", &
        "PAS TROUVE LA CLE                 ", &
        "INDICE HORS LIMITE OU MAUVAISE CLE", &
        "( ATTENDU                         ", &
        "OPERANDE DEMANDEE                 ", &
        ", OU ) ATTENDU                    ", &
        "LA PILE D ARGUMENTS DEBORDE       ", &
        "TROP OU PAS ASSEZ D'ARGUMENTS     ", &
        "ADRESSE INVALIDE                  "]

    integer :: msg_lvl_idx, msg_idx
    integer :: i

    nerr = nerr + 1

    msg_lvl_idx = min(code / 10000, nb_msg_lvl)
    if (msg_lvl_idx < 1) msg_lvl_idx = 1

    msg_idx = min( mod(code, 1000), nb_msg)
    if (msg_idx < 1) msg_idx = 1

    write(app_msg, "(A7, ': RLX', I3.3, ' - ', A40)") module, msg_idx, msg(msg_idx)

    call lib_log(APP_LIBRMN, msg_lvl(msg_lvl_idx), app_msg)
    write(app_msg, '(1x,a)') inline(21:last)
    call lib_log(APP_LIBRMN, APP_VERBATIM, app_msg)
    write(app_msg, '(1X,101A1)') (' ', I = 1, NC-22), '^'
    call lib_log(APP_LIBRMN, APP_VERBATIM, app_msg)
end


!> Retourne le premier caractère d'une linge de text qui soit égal à l'argument
subroutine qlx_flsh(icar)
    implicit none

    !> caratère à chercher
    character(len = 1), intent(in) :: icar

    character(len = 1), external :: qlx_chr

    do while (qlx_chr() /= icar)
        continue
    enddo
end


!> Get variable address for key
subroutine qlx_fnd(key, locvar, loccnt, limits, ityp)
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    character(len = *), intent(in) :: key
    integer(kind = int64), intent(out) :: locvar
    integer(kind = int64), intent(out) :: loccnt
    integer, intent(out) :: limits
    integer, intent(out) :: ityp

    character(len = 8), parameter :: clef(12) = &
        ['END     ', 'IF      ', 'ELSE    ', 'ENDIF   ', &
         'WHILE   ', 'ENDWHILE', 'ENDDATA ', 'ENDCASE ', &
         'ENDREAD ', '@PRINT  ', '@DEFINE ', '@UNDEF  ']

    integer, external :: qlx_nvar, qlx_undf, qlx_prnt
    external :: low2up, qlx_look

    character(len = 8) :: ikey
    integer, save :: dummy
    integer :: pos, i

    locvar = 0
    loccnt = 0
    limits = 0
    ityp = -1
    call low2up(key, ikey)

    pos = 0
    do i = 1, 12
        if ( ikey ==  clef(i) ) then
            pos = i
            exit
        endif
    end do
    select case (pos)
    case(0)
        call qlx_look(locvar, ikey, loccnt, limits, ityp)
    case(1)
        ityp = 10
    case(2)
        ityp = 3
    case(3)
        ityp = 4
    case(4)
        ityp = 5
    case(5)
        ityp = 6
    case(6)
        ityp = 7
    case(7)
        ityp = 11
    case(8)
        ityp = 12
    case(9)
        ityp = 13
    case(10)
        ityp = 2
        locvar = loc(qlx_prnt)
        loccnt = loc(dummy)
        limits = 202
    case(11)
        ityp = 2
        locvar = loc(qlx_nvar)
        loccnt = loc(dummy)
        limits = 202
    case(12)
        ityp = 2
        locvar = loc(qlx_undf)
        loccnt = loc(dummy)
        limits = 101
    end select
end


subroutine qlx_ind(ind, err)
    use qlx_token, only: typ, zval, token
    implicit none

    integer, intent(out) :: ind
    logical, intent(out) :: err

    external :: qlx_skp, qlx_tok, qlx_err, qlx_bak

    integer jval
    pointer(pjval, jval)
    character(len = 1) :: qlx_skp
    character(len = 1) :: ic

    pjval = loc(zval)
    ind = 1
    ic = qlx_skp(' ')

    if (ic == '[') then
        call qlx_tok
        if (((typ == 1) .or.(typ == 0)) .and. jval > 0) then
            ind = jval
        else
            call qlx_err(21009, 'qlx_ind')
            err = .true.
        endif
        if (.not.err) then
            call qlx_tok
            if (token(1:1) /= ']' .or. typ /= 4) then
                call qlx_err(21010, 'qlx_ind')
                err = .true.
            endif
        endif
    else
        call qlx_bak(ic)
    endif
end


!> DECLARATION DES CLES ET DE LEUR TYPE
SUBROUTINE qqlx_ins(ivar, key, icount, limits, ityp, xtern)
    use readlx_nrdlx
    implicit none

    INTEGER, INTENT(IN) :: ivar
    CHARACTER(len = *), intent(in) :: key
    INTEGER, INTENT(OUT):: icount
    INTEGER, INTENT(IN) :: limits
    INTEGER, INTENT(IN) :: ityp

    EXTERNAL :: xtern, low2up, qlx_err

    ! CONSTRUIT UNE TABLE CONTENANT LA CLE(IKEY), L'ADRESSE DES
    ! VALEURS IVAR(MAXIMUM DE 'LIMITS')ET DU NOMBRE DE VALEURS(ICOUNT),
    ! LE NOMBRE MAXIMUM DE VALEURS, ET LE TYPE DE SYMBOLES.

    CHARACTER(len = 8) :: ikey
    INTEGER :: ipnt

    ! TROUVER LA CLE
    CALL low2up(key, ikey)
    ipnt = NENTRY
    DO WHILE (ipnt > 0)
        if (ikey /= NAMES(ipnt)) then
            ipnt = ipnt - 1
        else
            exit
        end if
    END DO

    IF (ipnt == 0) THEN
        nentry = nentry + 1
        ipnt = nentry
    ENDIF

    IF (ipnt == 256) THEN
        CALL qlx_err(10011, 'QLXINS')
    ENDIF

    IF (limits < 0 .OR. limits > 99999) THEN
        CALL qlx_err(20012, 'QLXINS')
        RETURN
    ENDIF

    IF (ityp < 0 .OR. ityp > 13)THEN
        CALL qlx_err(20013, 'QLXINS')
        RETURN
    ENDIF

    icount = 0
    NAMES(ipnt) = ikey
    IF (ityp ==  2) THEN
        IPTADR(1, ipnt) = LOC(xtern)
    ELSE
        IPTADR(1, ipnt) = LOC(ivar)
    ENDIF
    ITAB(3, ipnt) = IOR(limits, ishft(ityp, 24))
    IPTADR(2, ipnt) = LOC(icount)
END SUBROUTINE qqlx_ins


!> Variable lookup
subroutine qlx_look(ivar, key, icount, limits, ityp)
    use readlx_nrdlx
    implicit none

    character(len = *), intent(in) :: key
    integer(kind = int64), intent(out) :: ivar
    integer(kind = int64), intent(out) :: icount
    integer, intent(out) :: limits
    integer, intent(out) :: ityp

    external :: low2up

    integer :: ipnt
    character(len = 8) ikey

    ! trouver la cle
    call low2up(key, ikey)
    ipnt = nentry
    do while (ipnt > 0)
        if (ikey /= names(ipnt)) then
            ipnt = ipnt - 1
        else
            exit
        end if
    end do
    if (ipnt ==  0) then
        ivar = 0
        icount = 0
        limits = 0
        ityp = -1
        return
    endif

    ! decortiquer les parametres de la cle
    ivar = iptadr(1, ipnt)
    icount = iptadr(2, ipnt)
    limits = iand(itab(3, ipnt), ishft(-1, -(32-(24))))
    ityp = ishft(itab(3, ipnt), -(24))
end


!> Undefine a variable
subroutine qlx_udf2(key)
    use readlx_nrdlx
    implicit none

    character(len = *), intent(in) :: key

    integer :: ipnt

    integer :: i
    character(len = 8) :: ikey

    ! trouver la cle
    ikey = key
    ipnt = nentry
    do while (ipnt >  0 .and. ikey /= names(ipnt))
        ipnt = ipnt - 1
    end do
    if (ipnt  ==  0) then
        return
    endif
    do i = ipnt, nentry-1
        iptadr(1, i) = iptadr(1, i+1)
        itab(3, i) = itab(3, i+1)
        iptadr(2, i) = iptadr(2, i+1)
        names(i)  = names(i+1)
    end do
    nentry = nentry - 1
end


subroutine qlx_dtb
    use readlx_nrdlx
    implicit none

    integer :: i
    PRINT *, ' NAMES, LOCVAR, TYPE/LIMITS, LOCCOUNT'
    DO I = 1, NENTRY
        PRINT '(2X, A8, 3Z22)', NAMES(I), IPTADR(1, I), ITAB(3, I), IPTADR(2, I)
    END DO
END


!> Reconstituer un nombre entier, reel ou octal
INTEGER FUNCTION qlx_num(IB, LENG)
    implicit none

    character(len = *), intent(INOUT) :: IB
    INTEGER, intent(INOUT) :: LENG

    !ARGUMENT
    !        qlx_num    RETOURNE   2   reel
    !        (S)                   1   entier
    !                              6   entier octal
    !                              5   ERREUR
    !
    !        IB(*)     IB(1) EST LE PREMIER CHIFFRE DU NOMBRE.
    !        (E)       LA TABLE IB CONTIENT LE NOMBRE.
    !
    !        LENG      NOMBRE DE CARACTERES DANS LE NOMBRE(ENTIER OU REEL)
    !        (S)

    INTEGER :: J, ILX

    EXTERNAL ::  qlx_chr, qlx_bak

    character(len=1) I, CTMP, qlx_chr

    IF (IB(1:1) == '.') THEN
        ILX = 1                                ! real number
    ELSE
        ILX = 0                                ! potentially an integer
    ENDIF
    I = qlx_chr()

    DO WHILE (I >= '0' .AND. I <= '9' )        ! collect a digit stream
        LENG = MIN(21, LENG+1)
        IB(LENG:LENG) = I
        I = qlx_chr()
    END DO
    IF (I == '.' .AND. IB(1:1) /= '.') THEN    ! decimal period, not in column 1
        ILX = 1
        LENG = MIN(21, LENG+1)
        IB(LENG:LENG) = I
        I = qlx_chr()
        DO WHILE (I >= '0' .AND. I <= '9')     ! digit stream after the period
            LENG = MIN(21, LENG+1)
            IB(LENG:LENG) = I
            I = qlx_chr()
        END DO
    END IF

      IF (I == 'E' ) THEN                      ! E after number
         IF (ILX == 0) THEN
            LENG=MIN(21, LENG+1)
            IB(LENG:LENG)='.'
         ENDIF
         ILX = 1                               ! definitely a real number
         LENG = MIN(21, LENG+1)
         IB(LENG:LENG) = I
         I = qlx_chr()
         IF ( (I >= '0' .AND. I <= '9') .OR. (I == '+') .OR. (I == '-') ) THEN
            LENG = MIN(21, LENG + 1)
            IB(LENG:LENG) = I
            I = qlx_chr()
            DO WHILE (I >= '0' .AND. I <= '9')  ! more digits
                LENG = MIN(21, LENG + 1)
                IB(LENG:LENG) = I
                I = qlx_chr()
            END DO
         ENDIF
      ENDIF

      IF (LENG >= 21) THEN
         qlx_num = 5                              ! bad number
      ELSE
         IF (ILX == 0) THEN
            IF (I /= 'B') THEN
               qlx_num = 1                        ! integer
            ELSE
               qlx_num = 6                        ! octal
               I = qlx_chr()
               DO J = LENG, 1, -1
                  IF (IB(J:J) > '7') THEN
                     qlx_num = 5                  ! bad number
                  ENDIF
                  CTMP = IB(J:J)
                  IB(20 - LENG + J : 20 - LENG + J) = CTMP
               END DO
               DO J = 1, 20 - LENG
                  IB(J:J) = '0'
               END DO
               LENG = 20
            ENDIF
         ELSE
            IF (LENG > 1) THEN
               IF (IB(LENG:LENG) == '.') THEN
                  qlx_num = 2                     ! real number
               ELSE
                  IF (IB(LENG:LENG) >= '0' .AND. IB(LENG:LENG) <= '9') THEN
                     qlx_num = 2                  ! real number
                  ELSE
                     qlx_num = 5                  ! bad number
                  ENDIF
               ENDIF
            ELSE
               qlx_num = 5
            ENDIF
         ENDIF
      ENDIF
      CALL qlx_bak(I)
END


subroutine qlx_nvar(key, nw)
    use, intrinsic :: iso_fortran_env, only: int64
    use readlx_qlxfmt
    implicit none

    integer, intent(in) :: key(*)
    integer, intent(in) :: nw

    external :: qlx_look, qlx_err, qlxins
    integer, external :: argdims

    ! This variable MUST have the save attribute, since qqlx_ins saves its address
    integer, save :: sc(1024)
    integer, save :: nsc = 1

    integer :: dummy
    integer :: limits, j, ityp
    character(len = 8) :: ikey
    integer(kind = int64) :: ivar, icount

    write(ikey, linefmt) (key(j), j = 1, argdims(1))
    call qlx_look(ivar, ikey, icount, limits, ityp)
    if (ityp /= -1) then
        return
    endif
    if (nsc + nw > 1024 + 1) then
        call qlx_err(21011, 'qlx_nvar')
        return
    endif
    call qlxins(sc(nsc), ikey, dummy, nw, 1)
    nsc = nsc + nw
end


!> Appliquer un operateur numerique ou logique
SUBROUTINE qlx_opr(TOKENS, NTOKEN, TOKTYPE, OPRTR, ERR)
    use, intrinsic :: iso_fortran_env, only: int32, int64
    implicit none

    INTEGER, INTENT(inout) :: NTOKEN
    integer(kind = int64), INTENT(inout) :: TOKENS(NTOKEN)
    INTEGER, INTENT(inout) :: TOKTYPE(NTOKEN)
    INTEGER, INTENT(in) :: OPRTR
    LOGICAL, INTENT(inout) :: ERR

    EXTERNAL :: get_value_at_address, set_value_at_address

    INTEGER :: IZ1, IZ2, IR1, tok32, MINOPER
    REAL :: Z1, Z2, R1
    pointer(pz1, Z1)
    pointer(pz2, Z2)
    pointer(pr1, R1)
    LOGICAL :: REALOP
    integer(kind = int32) :: TOK
    POINTER(PTOK, TOK(*))

    pz1 = LOC(IZ1)
    IZ1 = 0
    pz2 = LOC(IZ2)
    IZ2 = 0
    pr1 = LOC(IR1)

    IF (ERR) THEN
        RETURN
    ENDIF
    IF (OPRTR == 4 .OR. OPRTR == 17) THEN
        MINOPER = 1
    ELSE
        MINOPER = 2
    ENDIF
    IF (NTOKEN < MINOPER) THEN
        ERR = .TRUE.
        RETURN
    ENDIF
    IF (TOKTYPE(NTOKEN) > 0) THEN
        call get_value_at_address(TOKENS(NTOKEN), 1, tok32)
        TOKENS(NTOKEN) = tok32
        TOKTYPE(NTOKEN) = 0
    ENDIF
    IF (OPRTR /= 2 .AND. OPRTR /= 17 .AND. OPRTR /= 21 .AND. OPRTR /= 4) THEN
        IF (TOKTYPE(NTOKEN-1) > 0) THEN
            call get_value_at_address(TOKENS(NTOKEN-1), 1, tok32)
            TOKENS(NTOKEN-1) = tok32
            TOKTYPE(NTOKEN-1) = 0
        ENDIF
    ENDIF
    REALOP = ABS(TOKENS(NTOKEN)) > 2147483647
    IZ1 = int(TOKENS(NTOKEN))
    IF (OPRTR /= 2 .AND. OPRTR /= 17 .AND. OPRTR /= 4) THEN
        REALOP = REALOP .OR. ABS(TOKENS(NTOKEN-1)) > 2147483647
        IZ2 = int(TOKENS(NTOKEN-1))
        IF (REALOP) THEN
            IF (ABS(IZ1) <= 2147483647) THEN
            Z1 = int(TOKENS(NTOKEN))
            ENDIF
            IF (ABS(IZ2) <= 2147483647) THEN
            Z2 = int(TOKENS(NTOKEN-1))
            ENDIF
        ENDIF
    ENDIF
    IR1 = 0

    select case(OPRTR)
    case(1)
        ERR = .TRUE.
        RETURN
    case(2)
        IF (TOKENS(NTOKEN) <= 0 .OR. TOKTYPE(NTOKEN-1) <= 0 .OR. REALOP) THEN
            ERR = .TRUE.
            RETURN
        ENDIF
        IF (TOKENS(NTOKEN) >= TOKTYPE(NTOKEN-1)) THEN
            ERR = .TRUE.
            RETURN

        ENDIF
        PTOK = TOKENS(NTOKEN-1)
        TOKENS(NTOKEN-1) = tok(TOKENS(NTOKEN))
        NTOKEN = NTOKEN - 1
        TOKTYPE(NTOKEN) = 0
        RETURN
    case(3)               ! unary +
        RETURN
    case(4)               ! unary -
        IF (REALOP) THEN
            R1 = -Z1
        ELSE
            IR1 = -IZ1
        ENDIF
    case(5)               ! **
        IF (REALOP) THEN
            R1 = Z2**Z1
        ELSE
            IR1 = IZ2**IZ1
        ENDIF
    case(6)               ! *
        IF (REALOP) THEN
            R1 = Z2*Z1
        ELSE
            IR1 = IZ2*IZ1
        ENDIF
    case(7)               ! /
        IF (REALOP) THEN
            R1 = Z2/Z1
        ELSE
            IR1 = IZ2/IZ1
        ENDIF
    case(8)               ! binary +
        IF (REALOP) THEN
            R1 = Z2+Z1
        ELSE
            IR1 = IZ2+IZ1
        ENDIF
    case(9)               ! binary -
        IF (REALOP) THEN
            R1 = Z2-Z1
        ELSE
            IR1 = IZ2-IZ1
        ENDIF
    case(10)              ! binary <
        IF (REALOP) THEN
            IF (Z2 < Z1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ELSE
            IF (IZ2 < IZ1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ENDIF
    case(11)              ! binary >
        IF (REALOP) THEN
            IF (Z2 > Z1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ELSE
            IF (IZ2 > IZ1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ENDIF
    case(12)              ! binary ==
        IF (REALOP) THEN
            IF (Z2 == Z1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ELSE
            IF (IZ2 == IZ1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ENDIF
    case(13)              ! binary <=
        IF (REALOP) THEN
            IF (Z2 <= Z1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ELSE
            IF (IZ2 <= IZ1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ENDIF
    case(14)              ! binary >=
        IF (REALOP) THEN
            IF (Z2 >= Z1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ELSE
            IF (IZ2 >= IZ1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ENDIF
    case(15)              ! binary <>
        IF (REALOP) THEN
            IF (Z2 /= Z1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ELSE
            IF (IZ2 /= IZ1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ENDIF
    case(16)              ! binary ><
        IF (REALOP) THEN
            IF (Z2 /= Z1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ELSE
            IF (IZ2 /= IZ1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ENDIF
    case(17)              ! unary NOT
        IF (REALOP) THEN
            ERR = .TRUE.
        ELSE
            IR1 =NOT(IZ1)
        ENDIF
    case(18)              ! binary AND
        IF (REALOP) THEN
            ERR = .TRUE.
        ELSE
            IR1 = IAND(IZ2, IZ1)
        ENDIF
    case(19)              ! binary OR
        IF (REALOP) THEN
            ERR = .TRUE.
        ELSE
            IR1 = IOR(IZ2, IZ1)
        ENDIF
    case(20)              ! binary XOR
        IF (REALOP) THEN
            ERR = .TRUE.
        ELSE
            IR1 = IEOR(IZ2, IZ1)
        ENDIF
    case(21)              ! :=
        IF (TOKTYPE(NTOKEN-1) <= 0) THEN
            ERR = .TRUE.
            RETURN
        ENDIF
        tok32 = int(TOKENS(NTOKEN))
        call set_value_at_address(TOKENS(NTOKEN-1), 1, tok32)
        !          TOKENS(NTOKEN) = tok32
        NTOKEN = NTOKEN - 1
        RETURN
    end select
    NTOKEN = NTOKEN + 1 - MINOPER
    TOKENS(NTOKEN) = IR1
    TOKTYPE(NTOKEN) = 0
END


!> Get operator priority
integer function qlx_pri_l(opr, leftpri)
    implicit none

    character(len = *), intent(in) :: opr
    logical, intent(in) :: leftpri

    integer, parameter :: maxoper = 23
    character(len = 4), parameter :: liste(maxoper) = [ &
        ')   ', ']   ', 'U+  ', 'U-  ', '**  ', '*   ', '/   ', '+   ', &
        '-   ', '<   ', '>   ', '==  ', '<=  ', '>=  ', '<>  ', '><  ', &
        'NOT ', 'AND ', 'OR  ', 'XOR ', ':=  ', '(   ', '[   ']
    integer, parameter :: pri(maxoper) = [ &
           150,    150,    101,    101,     91,     81,     81,     71, &
            71,     61,     61,     61,     61,     61,     61,     61, &
            51,     41,     41,     41,     10,      1,      1]

    integer :: i

    do i = 1, maxoper
        if (liste(i) == opr) then
            if (leftpri) then
                qlx_pri_l = i + pri(i) * 100
            else
                qlx_pri_l = i + (pri(i) - mod(pri(i), 2)) * 100
            endif
            return
        endif
    end do
    qlx_pri_l = 0
end


!> Evaluer la priorite d'un operateur (right priority)
integer function qlx_pri(opr)
    implicit none

    character(len = *), intent(in) :: opr

    integer, external :: qlx_pri_l

    qlx_pri = qlx_pri_l(opr, .FALSE.)
end


!> Evaluer la priorite d'un operateur (left priority)
integer function qlx_pril(opr)
    implicit none

    character(len=*) opr

    integer, external :: qlx_pri_l

    qlx_pril = qlx_pri_l(opr, .TRUE.)
end


subroutine qlx_prnt(quoi, comment)
    use app
    use readlx_qlxfmt, only: karmot, linefmt
    implicit none

    integer, intent(in) :: quoi(*)
    integer, intent(in) :: comment(*)

    integer, external :: argdims

    character(len = 120) fmt
    integer :: i, l1, l2

    l1 = argdims(1)
    l2 = min(120 / karmot, argdims(2))
    if (l1 < 1 .or. l2 < 1) then
        return
    endif
    write(fmt, linefmt)(comment(i), i = 1, l2)
    write(app_msg, fmt)(quoi(i), i = 1, l1)
    call lib_log(APP_LIBFST, APP_VERBATIM, app_msg)
end


!> Conversion a notation postfixe (reverse polish notation)
SUBROUTINE qlx_rpn(TOK, TOKENS, MAXTKNS, NTOKEN, TOKTYPE, PILEOP, MAXOPS, NOPER, ERR)
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    character(len = *), intent(in) :: TOK
    INTEGER, INTENT(in) :: MAXTKNS
    integer(kind = int64), INTENT(inout) :: TOKENS(MAXTKNS)
    INTEGER, INTENT(inout) :: TOKTYPE(MAXTKNS)
    INTEGER, INTENT(inout) :: NTOKEN
    INTEGER, INTENT(in) :: MAXOPS
    INTEGER, INTENT(inout) :: NOPER
    LOGICAL, INTENT(inout) :: ERR

    INTEGER, EXTERNAL :: qlx_pri, qlx_pril
    EXTERNAL :: qlx_opr

    character(len = 4) :: TOKEN
    character(len = 4) :: PILEOP(MAXOPS)

    IF (ERR) THEN
        RETURN
    ENDIF

    TOKEN = TOK
    IF (TOKEN == '(' .OR. TOKEN == '[') THEN
        NOPER = MIN(NOPER+1 , MAXOPS)
        PILEOP(NOPER) = TOKEN
    ELSE
    IF (TOKEN == ')') THEN
        DO WHILE (PILEOP(NOPER) /= '(' .AND. PILEOP(NOPER) /= '[' .AND. PILEOP(NOPER) /= '$')
            CALL qlx_opr(TOKENS, NTOKEN, TOKTYPE, MOD(qlx_pri(PILEOP(NOPER)), 100), ERR)
            NOPER = NOPER - 1
        ENDDO
        IF (PILEOP(NOPER) == '(') THEN
            NOPER = NOPER-1
        ELSE
            ERR = .TRUE.
        ENDIF
    ELSE
    IF (TOKEN == ']') THEN
        DO WHILE (PILEOP(NOPER) /= '(' .AND. PILEOP(NOPER) /= '[' .AND. PILEOP(NOPER) /= '$')
            CALL qlx_opr(TOKENS, NTOKEN, TOKTYPE, MOD(qlx_pri(PILEOP(NOPER)), 100), ERR)
            NOPER = NOPER - 1
        ENDDO
        IF (PILEOP(NOPER) == '[') THEN
            CALL qlx_opr(TOKENS, NTOKEN, TOKTYPE, MOD(qlx_pri(']'), 100), ERR)
            NOPER = NOPER-1
        ELSE
            ERR = .TRUE.
        ENDIF
    ELSE
    IF (TOKEN == '$') THEN
        DO WHILE (PILEOP(NOPER) /= '(' .AND. PILEOP(NOPER) /= '[' .AND. PILEOP(NOPER) /= '$')
            CALL qlx_opr(TOKENS, NTOKEN, TOKTYPE, MOD(qlx_pri(PILEOP(NOPER)), 100), ERR)
            NOPER = NOPER - 1
        ENDDO
        IF (PILEOP(NOPER) == '$') THEN
            NOPER = NOPER-1
        ELSE
            ERR = .TRUE.
        ENDIF
    ELSE
        DO WHILE (qlx_pril(PILEOP(NOPER)) > qlx_pri(TOKEN))
            CALL qlx_opr(TOKENS, NTOKEN, TOKTYPE, MOD(qlx_pri(PILEOP(NOPER)), 100), ERR)
            NOPER = NOPER -1
        ENDDO
        NOPER = MIN(NOPER+1 , MAXOPS)
        PILEOP(NOPER) = TOKEN
    ENDIF
    ENDIF
    ENDIF
    ENDIF
END


!> Get the first character other than the specified one from a line
character(len = 1) function qlx_skp(icar)
    implicit none

    !> Character to ignore
    character(len = 1), intent(in) :: icar

    external qlx_chr

    character(len = 1) :: ctmp, qlx_chr

    ctmp = qlx_chr()
    do while (ctmp == icar)
        ctmp = qlx_chr()
    end do
    qlx_skp = ctmp
end


!> Decomposer une ligne de texte en tokens de differents types, identifie la longueur du token et son type.
SUBROUTINE qlx_tok
    use, intrinsic :: iso_fortran_env, only: int64
    use readlx_qlxfmt
    use qlx_token, only: len, typ, zval, token, inexpr
    implicit none

    EXTERNAL :: qlx_chr, qlx_num, qlx_err, qlx_bak, qlx_fnd, get_value_at_address

    integer(kind = int64) :: LOCVAR, LOCCNT

    character(len = 1) :: IC, qlx_chr
    INTEGER :: qlx_num, ival, leng, limits
    INTEGER :: JSIGN, ITYP
    INTEGER :: JVAL
    pointer(pjval, JVAL)

    pjval = LOC(ZVAL)
    IVAL = -1
    JSIGN = 0
    TOKEN = ' '

    IC = qlx_chr()
    DO WHILE (.NOT.(IC /= ' '))
        IC = qlx_chr()
    END DO

    LENG = 1
    TOKEN(1:1) = IC
    IF ( (IC >= 'A'.AND.IC <= 'Z') .OR. IC == '@' .OR. IC == '_' .OR. (IC >=  'a' .AND. IC <=  'z') ) THEN
        IC = qlx_chr()
        DO WHILE ( (IC >= 'A' .AND.IC  <= 'Z').OR. (IC >= '0' .AND. IC <= '9') .OR. (IC >=  'a' .AND. IC <=  'z') )
            LENG = MIN(81, LENG+1)
            TOKEN(LENG:LENG) = IC
            IC = qlx_chr()
        ENDDO
        IF (LENG > 8) THEN
            typ = 3                 ! string ( non delimited )
        ELSE
            typ = 0                 ! short string, possibly a key
        ENDIF
        CALL qlx_bak(IC)
    ELSE
    IF (IC == '''' .OR. IC == '"') THEN
        LENG = 0
        LENG = MIN(80, LENG + 1)
        TOKEN(LENG:LENG) = qlx_chr()
        DO WHILE (.NOT.(TOKEN(LENG:LENG) ==  IC))
            LENG = MIN(80, LENG + 1)
            TOKEN(LENG:LENG) = qlx_chr()
        END DO
        TOKEN(LENG:LENG) = ' '
        LENG = LENG -1
        IF (IC  == '"') THEN
            LENG = MIN(LENG, KARMOT)
        ENDIF
        typ = 3                    ! string ( delimited )
    ELSE
    IF ( (IC >= '0' .AND. IC <= '9') .OR. (IC == '.') ) THEN
        typ = qlx_num(TOKEN, LENG)  ! 1/2/5/6
        JSIGN = 1
    ELSE
    IF ( (IC == '+' .OR. IC == '-') .AND. (.NOT.INEXPR) ) THEN
        IF (IC == '+') THEN
            JSIGN = 1
        ELSE
            JSIGN = -1
        ENDIF
        IC = qlx_chr()
        IF ((IC >= '0' .AND. IC <= '9').OR. IC == '.') THEN
            TOKEN(1:1)=IC
            typ = qlx_num(TOKEN, LENG)  ! 1/2/5/6
        ELSE
            CALL qlx_bak(IC)
            typ = 4              ! special char, possibly operator
        ENDIF
    ELSE
    IF (IC == '*') THEN
        typ = 4                  ! operator (1 or 2 chars)
        IC = qlx_chr()
        IF (IC == '*') THEN
            LENG = 2
            TOKEN = '**'
        ELSE
            CALL qlx_bak(IC)
        ENDIF
    ELSE
    IF (IC == '<' .OR. IC == '>' .OR. IC == '=' .OR. IC == ':') THEN
        typ = 4                  ! operator (1 or 2 chars)
        IC = qlx_chr()
        IF (IC == '<' .OR. IC == '>' .OR. IC == '=') THEN
            LENG = 2
            TOKEN(2:2) = IC
        ELSE
            CALL qlx_bak(IC)
        ENDIF
    ELSE
        typ = 4                  ! operator
    ENDIF
    ENDIF
    ENDIF
    ENDIF
    ENDIF
    ENDIF

    IF ( (LENG > 80) .OR. (typ == 5) ) THEN
        TOKEN = 'SCRAP'
        typ = 5
        CALL qlx_err(21014, 'qlx_tok')
    ENDIF
    IF (typ == 1) THEN         ! integer
        READ(TOKEN, '(I20)')JVAL
        JVAL = SIGN(JVAL, JSIGN)
    ELSE
    IF (typ == 2) THEN         ! float
        READ(TOKEN, '(G20.3)')ZVAL
        ZVAL = SIGN(ZVAL, FLOAT(JSIGN))
    ELSE
    IF (typ == 6) THEN          ! octal constant
        READ(TOKEN, '(O20)')JVAL
        typ = 1                 ! integer
        JVAL = SIGN(JVAL, JSIGN)
    ENDIF
    ENDIF
    ENDIF
    IF (typ == 0) THEN
        CALL qlx_fnd(TOKEN(1:8), LOCVAR, LOCCNT, LIMITS, ITYP)
        IF (ITYP  ==  -1) THEN
            typ = 3
            LENG = MIN(LENG, KARMOT)
        ELSE
        IF ( (ITYP  ==  0) .OR. (ITYP  ==  1) ) THEN
            call get_value_at_address(LOCVAR, 1, JVAL)
        ELSE
            JVAL = -1
        ENDIF
        ENDIF
    ENDIF
    LEN = LENG
END


subroutine qlx_undf(ikey)
    use readlx_qlxfmt
    implicit none

    integer, intent(in) :: ikey(*)

    integer, external :: argdims
    external :: qlx_udf2

    character(len = 8) ckey
    integer :: i

    write(ckey, '(2 a4)') (ikey(i), i = 1, argdims(1))
    call qlx_udf2(ckey)
end


integer function qlx_val(kle, err)
    implicit none

    character(len = *), intent(in) :: kle
    logical, intent(out) :: err

    external :: qlx_ind, qlx_adi2

    integer :: ind, val

    call qlx_ind(ind, err)

    val = 0
    if (.not. err) then
        call qlx_adi2(kle, ind, val, err)
    endif
    qlx_val = val
end


!> Traiter une expression arithmetique ou logique
SUBROUTINE qlx_xpr(ERR)
    use app
    use rmn_common
    use qlx_token, only: typ, token, inexpr, zval, jval64
    implicit none

    LOGICAL, INTENT(out) :: ERR

    INTEGER JVAL
    pointer(pjval,JVAL)

    INTEGER, PARAMETER :: MAXTKNS = 65
    INTEGER, PARAMETER :: MAXOPS = 30
    INTEGER TOKTYPE(MAXTKNS), NTOKEN
    integer(kind = int64) :: TOKENS(MAXTKNS)
    INTEGER NOPER
    integer(kind = int64) :: LOCVAR, LOCCNT
    character(len=4) :: PILEOP(MAXOPS)
    LOGICAL UNARY, FINI, FIRST
    INTEGER PLEV, BLEV, LIMITES, ITYP

    INTEGER, EXTERNAL :: qlx_pri
    EXTERNAL :: qlx_err, qlx_tok, qlx_fnd, qlx_bak, qlx_rpn

    pjval = LOC(ZVAL)
    INEXPR = .TRUE.
    NTOKEN = 0
    PLEV = 0
    BLEV = 0
    UNARY = .TRUE.
    ERR = .FALSE.
    FINI = .FALSE.
    FIRST = .TRUE.
    NOPER = 1
    PILEOP(1) ='$'

    DO WHILE ( .NOT.FINI .AND. NTOKEN < MAXTKNS .AND. NOPER < MAXOPS .AND. .NOT.ERR)
        IF (.NOT.FIRST) THEN
            CALL qlx_tok
        ENDIF
        FIRST = .FALSE.
        IF (typ == 0) THEN
            NTOKEN = NTOKEN + 1
            CALL qlx_fnd(TOKEN(1:8), LOCVAR, LOCCNT, LIMITES, ITYP)
            IF (ITYP /= 0 .AND. ITYP /= 1) THEN
                ERR = .TRUE.
            ENDIF
            TOKENS(NTOKEN) = LOCVAR
            TOKTYPE(NTOKEN) = LIMITES + 1
            IF (.NOT. UNARY) THEN
                ERR = .TRUE.
            ENDIF
            UNARY = .FALSE.
        ELSE
        IF (typ == 1 .OR. typ == 2) THEN
            NTOKEN = NTOKEN + 1
            TOKENS(NTOKEN) = JVAL
            TOKTYPE(NTOKEN) = 0
            IF (.NOT. UNARY) THEN
                ERR = .TRUE.
            ENDIF
            UNARY = .FALSE.
        ELSE
        IF (qlx_pri(TOKEN(1:4)) > 0) THEN
            IF (TOKEN(1:2) == '( ') THEN
                PLEV = PLEV + 1
            ELSE
                IF (TOKEN(1:2) == ') ') THEN
                    PLEV = PLEV - 1
                ELSE
                    IF (TOKEN(1:2) == '[ ') THEN
                        BLEV = BLEV + 1
                    ELSE
                        IF (TOKEN(1:2) == '] ') THEN
                            BLEV = BLEV - 1
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            IF (PLEV < 0 .OR. BLEV < 0) THEN
                FINI = .TRUE.
                CALL qlx_bak(TOKEN(1:1))
                EXIT
            ENDIF
            IF (UNARY) THEN
                IF (TOKEN(1:2) == '+ ') THEN
                    TOKEN(1:2) = 'U+'
                ELSE
                    IF (TOKEN(1:2) == '- ') THEN
                        TOKEN(1:2) = 'U-'
                    ELSE
                        IF (TOKEN(1:2) /= '( ' .AND. TOKEN(1:2) /= '[ ') THEN
                            ERR = .TRUE.
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
            UNARY = TOKEN(1:1) /= ')' .AND. TOKEN(1:1) /= ']'
            CALL qlx_rpn(TOKEN, TOKENS, MAXTKNS, NTOKEN, TOKTYPE, PILEOP, MAXOPS, NOPER, ERR)
        ELSE
        IF (TOKEN(1:1) == ',' .OR. TOKEN(1:1) == '$' .OR. TOKEN(1:2) == ':=') THEN
            CALL qlx_rpn('$', TOKENS, MAXTKNS, NTOKEN, TOKTYPE, PILEOP, MAXOPS, NOPER, ERR)
            FINI = .TRUE.
            CALL qlx_bak(TOKEN(1:1))
        ELSE
            WRITE(app_msg, '(A8,A)')TOKEN(1:8), ' IS INVALID'
            call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
            ERR = .TRUE.
        ENDIF
        ENDIF
        ENDIF
        ENDIF
    END DO

    IF (PLEV > 0 .OR. .NOT.FINI .OR. BLEV > 0   .OR. NTOKEN /= 1 ) THEN
        ERR = .TRUE.
    ENDIF
    INEXPR = .FALSE.
    IF (.NOT.ERR) THEN
        TOKEN = ' '
        JVAL = int(TOKENS(1))
        IF (TOKTYPE(1) > 0) THEN
            typ = 8      ! adresse
            jval64 = TOKENS(1)
        ELSE
            IF (ABS(JVAL) <= 2147483647) THEN
                typ =1
            ELSE
                typ =2
            ENDIF
        ENDIF
    ENDIF
    IF (ERR) THEN
        CALL qlx_err(81005, 'QLXEXPR')
    ENDIF
END


!> Set readlx option
subroutine qlxopt(option, val)
    use app
    use readlx_qlxfmt

    !> Option to set
    character(len = *), intent(in) :: option
    !> Value to set the option to
    integer, intent(in) :: val

    if (option(1:6) ==  'carmot') then
        karmot = val
        write(linefmt, '(a, i2, a)') '(25 a', karmot, ')'
    else
        write(app_msg, *) 'Option (', option, ') unknown'
        call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
    endif
end


!> Get argument length in number of words
integer function argdims(arg_idx)
    use readlx_parmadr
    implicit none

    !> Argument index for the latest readlx call
    integer, intent(in) :: arg_idx

    if (arg_idx <= narg) then
        argdims = dope(arg_idx)
    else
        argdims = 0
    endif
end


!> Get dope list of argument
integer function argdope(arg_idx, list, list_size)
    use readlx_parmadr

    integer, intent(in) :: arg_idx
    integer, intent(in) :: list_size
    integer, intent(out) :: list(list_size)

    integer :: i, base

    if (arg_idx >  narg) then
        argdope = 0
    else
        base = dopea(arg_idx)
        argdope = dopea(arg_idx + 1) - dopea(arg_idx)
        do i = 1, min(dopea(arg_idx + 1) - dopea(arg_idx), list_size)
            list(i) = dopes(base + i - 1)
        end do
    endif
end


!> Déclaration des routines
SUBROUTINE qlxinx(xtern, key, icount, limits, ityp)
    use app
    implicit none

    !> Nom de la fonction à appeler
    EXTERNAL :: xtern, qlx_err, qqlx_ins

    !> Chaine de caractères du jetton
    CHARACTER(LEN = *) :: key
    !> Nombre d'occurences
    INTEGER, INTENT(OUT) :: icount
    !> Limites? Doit être entre 0 et 99999
    INTEGER :: limits
    !> Option mystère devant être entre 0 et 13
    INTEGER :: ityp

    INTEGER :: idum

    IF (ityp /= 2) THEN
        CALL lib_log(APP_LIBRMN, APP_ERROR, 'qlxinx ne peut etre utilise pour ityp <> 2')
        CALL qlx_err(81013, 'QLXINS')
        STOP
    ENDIF
    CALL qqlx_ins(idum, key, icount, limits, ityp, xtern)
END


!> Interface de qlxins
subroutine lexins(ivar, icle, nb, limit, typ)
    implicit none

    integer, intent(in) :: ivar
    character(len = *), intent(in) :: icle
    integer, intent(out) :: nb
    integer, intent(in) :: limit
    integer, intent(in) :: typ

    external :: qlxins

    character(len = 8) :: kle

    write(kle, '(a8)') icle
    call qlxins(ivar, kle, nb, limit, typ)
end


!> Declare keys
subroutine qlxins(ivar, key, icount, limits, ityp)
    use app
    implicit none

    integer, intent(in) :: ivar
    character(len = *), intent(in) :: key
    integer, intent(out) :: icount
    integer, intent(in) :: limits
    integer, intent(in) :: ityp

    external :: readlx
    external :: qlx_err, qqlx_ins

    if (ityp == 2) then
        call lib_log(APP_LIBRMN, APP_ERROR, 'qlxinx doit etre utilise quand ityp = 2, au lieu de QLXINS')
        call qlx_err(81013, 'QLXINS')
        stop
    else
        call qqlx_ins(ivar, key, icount, limits, ityp, readlx)
    endif
end


!> Interprete de directives
SUBROUTINE readlx(UNIT, KEND, KERR)
    use app
    use rmn_common
    use rmn_fnom
    use readlx_qlxbuff
    use readlx_qlxfmt
    use qlx_token, only: typ, token, inexpr, zval
    implicit none

    !> Numéro d'unité d'entrée
    INTEGER, INTENT(in) :: UNIT
    !> 0 : Pas de problème
    INTEGER, INTENT(out) :: KEND

    INTEGER, INTENT(inout) :: KERR

    EXTERNAL :: qlx_nvar, qlx_prnt, qlx_undf
    EXTERNAL :: qlxinx, qlx_bak, qlx_err, qlx_tok, qlx_fnd, qlx_asg, qlx_call, qlx_xpr, qlx_flsh
    external :: get_value_at_address, set_value_at_address

    INTEGER, PARAMETER :: MAXSTRU = 20
    INTEGER, PARAMETER :: NXTELSE(0:2) = [1, 0, 2]
    INTEGER, PARAMETER :: NEXTIF(0:2) = [0, 2, 2]

    integer(kind = int64) :: LOCCNT, LOCVAR
    Integer :: IICNT
    INTEGER :: LIMITS, ITYP
    LOGICAL :: FIN, ERR
    INTEGER :: STYPE(MAXSTRU), SKIPF(MAXSTRU)
    INTEGER :: READBSE(MAXSTRU)
    INTEGER :: NSTRUC, ier
    character(len = 128) :: nomscra
    integer(kind = int64) :: jval64
    INTEGER :: KERRMAX
    INTEGER :: IDUM

    INTEGER :: JVAL
    pointer(pjval, JVAL)

    pjval = LOC(ZVAL)
    WRITE(LINEFMT, '(A,I2,A)') '(25 A', KARMOT, ')'

    KERRMAX = 999999
    IF (KERR < 0 ) THEN
        KERRMAX = MIN(ABS(KERR), KERRMAX)
    ENDIF
    ! print *,"==========================readlx NEW=========================="
    NC = 1
    LAST = 0
    INPFILE = UNIT
    EOFL = .FALSE.
    NERR = 0
    FIN = .FALSE.
    INEXPR = .FALSE.
    STYPE(1) = 0
    SKIPF(1) = 0
    NSTRUC = 1
    CURREC = 0
    READREC = 0
    READBSE(1) = 0
    nomscra = 'XXXXQLX'
    tmpfile = 0
    ier = fnom(tmpfile, nomscra, 'D77+SCRATCH+FMT', 20)
    CALL qlxinx(qlx_prnt, 'PRINT', IDUM, 0202, 2)
    CALL qlxinx(qlx_nvar, 'DEFINE', IDUM, 0202, 2)
    CALL qlxinx(qlx_undf, 'UNDEF', IDUM, 0101, 2)

    DO WHILE (.NOT.FIN .AND. NERR < KERRMAX .AND. NSTRUC < MAXSTRU)
        SKIPFLG = SKIPF(NSTRUC)
        ERR = .FALSE.
        CALL qlx_tok
        IF (typ == 0) THEN
            CALL qlx_fnd(TOKEN, LOCVAR, LOCCNT, LIMITS, ITYP)
            IF (ITYP == 1 .AND. SKIPF(NSTRUC) == 0) THEN
                call get_value_at_address(LOCCNT, 1, IICNT)
                CALL qlx_asg(LOCVAR, IICNT, LIMITS, ERR)
                call set_value_at_address(LOCCNT, 1, IICNT)
            ELSE
                IF (ITYP == 2 .AND. SKIPF(NSTRUC) == 0) THEN
                    CALL qlx_call(LOCVAR, LOCCNT, LIMITS, ERR)
                ELSE
                    IF (ITYP == 3) THEN
                        NSTRUC = NSTRUC + 1
                        STYPE(NSTRUC) = ITYP
                        SKIPF(NSTRUC) = NEXTIF(SKIPF(NSTRUC-1))
                        IF (SKIPF(NSTRUC) == 0) THEN
                            CALL qlx_tok
                            IF (TOKEN(1:1) /= '$') THEN
                                CALL qlx_xpr(ERR)
                                IF (ERR) THEN
                                    EXIT
                                ENDIF
                                IF (typ == 8) THEN
                                    jval64 = JVAL
                                    call get_value_at_address(jval64, 1, JVAL)
                                ENDIF
                                IF (IAND(JVAL, ishft(-1, 32-(16))) == 0) THEN
                                    SKIPF(NSTRUC) = 1
                                ENDIF
                            ELSE
                                CALL qlx_bak('$')
                            ENDIF
                        ENDIF
                        CALL qlx_flsh('$')
                    ELSE
                        IF (ITYP == 4) THEN
                            IF (STYPE(NSTRUC) /= 3) THEN
                                EXIT
                            ENDIF
                            STYPE(NSTRUC) = ITYP
                            SKIPF(NSTRUC) = NXTELSE(SKIPF(NSTRUC))
                            CALL qlx_flsh('$')
                        ELSE
                            IF (ITYP == 5) THEN
                                IF (STYPE(NSTRUC) /= 3 .AND. STYPE(NSTRUC) /= 4) THEN
                                    EXIT
                                ENDIF
                                SKIPF(NSTRUC) = 0
                                NSTRUC = NSTRUC - 1
                                CALL qlx_flsh('$')
                            ELSE
                                IF (ITYP == 6) THEN
                                    NSTRUC = NSTRUC + 1
                                    STYPE(NSTRUC) = ITYP
                                    SKIPF(NSTRUC) = NEXTIF(SKIPF(NSTRUC-1))
                                    IF (READREC /= 0) THEN
                                        READBSE(NSTRUC) = READREC -1
                                    ELSE
                                        READBSE(NSTRUC) = CURREC
                                    ENDIF
                                    IF (SKIPF(NSTRUC) == 0) THEN
                                        CALL qlx_tok
                                        IF (TOKEN(1:1) /= '$') THEN
                                            CALL qlx_xpr(ERR)
                                            IF (ERR) THEN
                                                EXIT
                                            ENDIF
                                            IF (typ == 8) THEN
                                                jval64 = JVAL
                                                call get_value_at_address(jval64, 1, JVAL)
                                            ENDIF
                                            IF (IAND(JVAL, ishft(-1, 32-(16))) == 0) THEN
                                                SKIPF(NSTRUC) = 1
                                            ENDIF
                                        ELSE
                                            CALL qlx_bak('$')
                                        ENDIF
                                    ENDIF
                                    CALL qlx_flsh('$')
                                ELSE
                                    IF (ITYP == 7) THEN
                                        IF (STYPE(NSTRUC) /= 6) THEN
                                            EXIT
                                        ENDIF
                                        IF (SKIPF(NSTRUC)  ==  0) THEN
                                            READREC = READBSE(NSTRUC)
                                        ENDIF
                                        SKIPF(NSTRUC) = 0
                                        NSTRUC = NSTRUC - 1
                                        CALL qlx_flsh('$')
                                    ELSE
                                        IF (ITYP >= 10 .AND. ITYP <= 13 .AND. SKIPF(NSTRUC) == 0) THEN
                                            KERR = NERR
                                            KEND = ITYP-10
                                            FIN = .TRUE.
                                        ELSE
                                            IF (SKIPF(NSTRUC) /= 0) THEN
                                                CALL qlx_flsh('$')
                                            ELSE
                                                CALL qlx_err(21015, 'readlx')
                                                ERR = .TRUE.
                                            ENDIF
                                        ENDIF
                                    ENDIF
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
        ELSE
            CALL qlx_err(21016, 'readlx')
            ERR = .TRUE.
        ENDIF
        IF (ERR.AND.(TOKEN(1:1) /= '$'.OR. typ /= 4)) THEN
            CALL qlx_flsh('$')
        ENDIF
    ENDDO

    IF (NSTRUC > 1) THEN
        call lib_log(APP_LIBRMN, APP_ERROR, 'readlx: Error within if then else bloc structure')
        KERR = NERR + 1
        KEND = -1
    ENDIF

    IER = FCLOS(TMPFILE)
END

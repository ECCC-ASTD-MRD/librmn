
! TODO A lot of global variables in theses modules are not initialized. Is that OK?
! (there was no "DATA" statement for them in the common blocks)
module readlx_parmadr
    use rmn_common
    implicit none
    save

    INTEGER :: NARG, NPRM, NDOPES
    integer, dimension(41)  :: DOPE
    integer, dimension(42)  :: DOPEA
    integer, dimension(101) :: DOPES
    integer, dimension(101) :: PARM = 0
    integer(kind = int64), dimension(41)  :: ADR = 0
end module readlx_parmadr

module readlx_qlxbuff
    implicit none
    save

    integer :: NC = 1
    integer :: LAST = 0
    integer :: INPFILE = 5
    integer :: NERR, SKIPFLG, CURREC, READREC, TMPFILE
    logical :: EOFL = .false.

    character(len=101) :: INLINE = ' '
end module readlx_qlxbuff

module readlx_qlxfmt
    implicit none
    save

    character(len=20) :: LINEFMT
    integer :: KARMOT = 04
end module readlx_qlxfmt

module readlx_nrdlx
    use rmn_common
    implicit none
    save

    INTEGER, dimension(3:3, 256) :: ITAB(3:3, 256) = 0
    integer :: NENTRY = 0
    character(len=8), dimension(256) :: NAMES = ' '
    integer(kind = int64), dimension(2, 256) :: IPTADR = 0
end module readlx_nrdlx

module readlx_remote
    use rmn_common
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
            use ISO_C_BINDING
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

! get contents at address(subscript) (assuming a 32 bit item)
subroutine get_value_at_address(address, subscript, content)
    use rmn_common
    implicit none

    integer(kind = int64), intent(IN) :: address   ! memory address
    integer, intent(IN) :: subscript               ! subscript
    integer, intent(OUT) :: content                ! output from memory
    integer :: val
    pointer(pval, val(*))

    pval = transfer(address,pval)
    ! print *,'entering get_value_at_address, subscript =',subscript
    content = val(subscript)
    ! print *,'exiting get_value_at_address'
end subroutine

! set contents at address(subscript) (assuming a 32 bit item)
subroutine set_value_at_address(address, subscript, content)
    use rmn_common
    implicit none

    integer(kind = int64), intent(IN) :: address   ! memory address
    integer, intent(IN) :: subscript               ! subscript
    integer, intent(IN) :: content                 ! value to set to
    integer :: val
    pointer(pval, val(*))

    pval = transfer(address, pval)
    ! print *,'entering set_value_at_address, subscript =',subscript
    val(subscript) = content
    ! print *,'exiting set_value_at_address'
end subroutine

!> Get value of indexed array component
! SUBROUTINE QLXADI(KLE, IND, VALEUR, TYPE, ERR)
SUBROUTINE qlx_adi2(KLE, IND, VALEUR, ERR)
    use rmn_common
    implicit none

    !     INTEGER IND, VALEUR, TYPE
    INTEGER IND, VALEUR
    LOGICAL ERR
    character(len=*) KLE

    INTEGER, EXTERNAL :: qlx_dtyp
    EXTERNAL :: qlx_fnd

    integer(kind = int64) LOCVAR, LOCCNT
    INTEGER LIMITE, ITYP, IZ, INDX
    integer, dimension(1024) :: mem
    pointer(pmem, mem)
    REAL Z

    IZ = IND
    IF (qlx_dtyp(IZ) == 1) THEN
        INDX = IZ
    ELSE
        z = transfer(iz, z)
        INDX = NINT(Z)
    ENDIF
    CALL qlx_fnd(KLE, LOCVAR, LOCCNT, LIMITE, ITYP)
    IF (ITYP.NE.0 .AND. ITYP.NE.1) THEN
        ERR = .TRUE.
    ENDIF
    IF (INDX > LIMITE .OR. INDX <= 0) THEN
        ERR = .TRUE.
    ENDIF
    IF (.NOT.ERR) THEN
        pmem = LOCVAR
        VALEUR = mem(INDX)
    ENDIF

END

!> Get subscript then build memory address
integer(kind = int64) FUNCTION qlx_adr(KLE, ERR)
    use rmn_common
    implicit none

    character(len=*) KLE
    LOGICAL ERR

    EXTERNAL :: qlx_err, qlx_fnd, qlx_ind

    INTEGER LIMITS, ITYP, IND
    integer(kind = int64) :: LOCCNT, locvar8
    integer :: VARI
    POINTER (LOCVAR, VARI(*))

    CALL qlx_ind(IND, ERR)
    IF (.NOT. ERR) THEN
        CALL qlx_fnd(KLE, LOCVAR8, LOCCNT, LIMITS, ITYP)
        LOCVAR = transfer(locvar8, LOCVAR)
        IF (IND <= LIMITS .AND. ITYP >= 0 .AND. ITYP <= 1) THEN
            qlx_adr = LOC(VARI(IND))
        ELSE
            ERR = .TRUE.
            CALL qlx_err(21017, 'qlx_adr')
            qlx_adr = 0
        ENDIF
    ELSE
        qlx_adr = 0
    ENDIF
END

!> Prend les tokens qui suivent le signe = et separes par des virgules pour les placer a l'adresse val
SUBROUTINE qlx_asg(VAL, ICOUNT, LIMIT, ERR)
    use rmn_common
    use readlx_qlxfmt
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

    COMMON /qlx_tok1/ LEN, TYPE, ZVAL, INEXPR
    LOGICAL INEXPR
    INTEGER LEN, TYPE, JVAL
    REAL ZVAL
    pointer(pjval,jval)

    COMMON/qlx_tok2/TOKEN
    character(len=80) TOKEN

    COMMON /qlx_tok3/ jval64
    integer(kind = int64) :: jval64

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

    IF (TOKEN(1:2) == '= ' .AND. TYPE == 4 .AND. .NOT. ERR) THEN
        DO WHILE (.NOT.ERR .AND. .NOT.FIN)
            CALL qlx_tok
            IF ((TYPE == 4) .AND. (TOKEN(1:1) == '(')) THEN
                CALL qlx_xpr(ERR)
                IF (ERR) THEN
                    EXIT
                ENDIF
            ENDIF
            IF (TYPE == 8) THEN
                call get_value_at_address(jval64, 1, JVAL)
            ELSE
                IF (TYPE == 1 .AND. OLDTYP == 4) THEN
                    ITEMP(1) = JVAL
                    JLEN = 1
                ELSE
                    IF (TYPE == 2 .AND. OLDTYP == 4) THEN
                        itemp(1) = transfer(zval, itemp(1))
                        JLEN = 1
                    ELSE
                        IF (TYPE == 3 .AND. OLDTYP == 4) THEN
                            JLEN = (LEN + KARMOT - 1) / KARMOT
                            READ(TOKEN, LINEFMT)(ITEMP(J), J=1, JLEN)
                        ELSE
                            IF (TYPE == 4) THEN
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
                                IF (TYPE == 0 .AND. OLDTYP == 4) THEN
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
            OLDTYP = TYPE
        END DO
    ELSE
        CALL qlx_err(21006, 'qlx_asg')
        ERR = .TRUE.
    ENDIF
END

! Remettre un caractère dans une ligne de texte à la position courante et reculer le pointeur du caractère courant
SUBROUTINE qlx_bak(ICAR)
    use readlx_qlxbuff
    implicit none

    !> Caractère à remettre dans la ligne de texte
    character(len = 1), intent(in) :: ICAR

    EXTERNAL :: qlx_err

    IF (NC > 1) THEN
        INLINE(NC-1:NC-1)=ICAR
        NC=NC-1
    ELSE
        CALL qlx_err(81007, 'qlx_bak')
    ENDIF
END

SUBROUTINE qlx_call(SUB, ICOUNT, LIMITS, ERR)
    use rmn_common
    use readlx_parmadr
    use readlx_qlxfmt
    use readlx_remote
    implicit none

    integer(kind = int64) :: SUB, ICOUNT

    COMMON /qlx_tok1/ LEN, TYPE, ZVAL, INEXPR
    LOGICAL INEXPR
    INTEGER LEN, TYPE, JVAL
    REAL ZVAL
    pointer(pjval,jval)

    COMMON/qlx_tok2/TOKEN
    character(len=80) TOKEN

    EXTERNAL :: qlx_err, qlx_tok, qlx_flsh, set_value_at_address

    integer, external :: rmtcall
    EXTERNAL qlx_adr, qlx_val
    INTEGER  qlx_val
    INTEGER LIM1, LIM2, JLEN, PREVI
    integer(kind = int64) LOCDUM, qlx_adr
    character(len=8) KLE
    integer(kind = int64) icount64

    LOGICAL ERR, FIN, INLIST

    integer :: I, J, NPRM0, LIMITS, JUNK

    pjval = LOC(ZVAL)
    FIN  = .FALSE.
    INLIST = .FALSE.
    LOCDUM = LOC(PARM(1))
    NDOPES = 0
    DO I = 1, 41
        DOPE(I) = 0
        DOPEA(I) = 0
        ADR(I) = LOCDUM
    END DO
    NARG = 0
    NPRM = 0
    NPRM0 = 0
    PREVI =4

    CALL qlx_tok
    IF (TYPE.NE.4 .AND. TOKEN(1:1).NE.'(') THEN
        CALL qlx_err(81018, 'qlx_call')
        ERR = .TRUE.
    ENDIF

    DO WHILE (.NOT. ERR .AND. .NOT.FIN)
        CALL qlx_tok
        IF (PREVI  == 4) THEN
            IF (TYPE  == 0) THEN
                KLE = TOKEN(1:8)
                PREVI =7
                IF (INLIST) THEN
                    NPRM = MIN(NPRM+1, 101)
                    PARM(NPRM) = qlx_val(KLE, ERR)
                ELSE
                    NARG = MIN(NARG+1, 41)
                    ADR(NARG) = qlx_adr(KLE, ERR)
                    DOPEA(NARG) = NDOPES + 1
                    NPRM0 = NPRM - 1
                ENDIF
                NDOPES = MIN(NDOPES+1, 101)
                DOPES(NDOPES) = TYPE + 1 * 256 + (NPRM-NPRM0) * 256 * 256
                DOPE(NARG) = DOPE(NARG) + 1
            ELSE
                IF (TYPE == 1 .OR. TYPE == 2) THEN
                    NPRM = MIN(NPRM+1, 101)
                    PARM(NPRM) = JVAL
                    PREVI =7
                    IF (.NOT. INLIST) THEN
                        NARG = MIN(NARG+1, 41)
                        ADR(NARG) = LOC(PARM(NPRM))
                        DOPEA(NARG) = NDOPES + 1
                        NPRM0 = NPRM - 1
                    ENDIF
                    NDOPES = MIN(NDOPES+1, 101)
                    DOPES(NDOPES) = TYPE + 1 * 256 + (NPRM-NPRM0)*256*256
                    DOPE(NARG) = DOPE(NARG) + 1
                ELSE
                    IF (TYPE  == 3) THEN
                        JLEN = MIN((LEN+KARMOT-1) / KARMOT , 101 - NPRM)
                        IF (.NOT. INLIST) THEN
                            NARG = MIN(NARG+1, 41)
                            ADR(NARG) = LOC(PARM(NPRM+1))
                            DOPEA(NARG) = NDOPES + 1
                            NPRM0 = NPRM
                        ENDIF
                        READ(TOKEN, LINEFMT) (PARM(J+NPRM), J=1, JLEN)
                        NDOPES = MIN(NDOPES+1, 101)
                        DOPES(NDOPES) = TYPE + LEN * 256 + (NPRM-NPRM0+1)*256 *256
                        NPRM = MIN(NPRM+JLEN, 101)

                        DOPE(NARG) = DOPE(NARG) + JLEN
                        PREVI =7
                    ELSE
                        IF (TYPE == 4 .AND. TOKEN(1:1) == '[' .AND. .NOT.INLIST) THEN
                            INLIST = .TRUE.
                            PREVI =4
                            NARG = MIN(NARG+1, 41)
                            ADR(NARG) = LOC(PARM(NPRM+1))
                            DOPEA(NARG) = NDOPES + 1
                            NPRM0 = NPRM
                        ELSE
                            IF (TYPE == 4 .AND. TOKEN(1:1) == ')' .AND.NARG == 0) THEN
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
            IF (TYPE == 4 .AND. (TOKEN(1:1) == ',' .OR. TOKEN(1:1) == ')')) THEN
                FIN = TOKEN(1:1) == ')'
                PREVI = 4
            ELSE
                IF (TYPE == 4 .AND. TOKEN(1:1) == ']' .AND. INLIST) THEN
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
        IF (NARG > 40 .OR. NPRM > 100 .OR. NDOPES  >  100) THEN
            CALL qlx_err(81021, 'qlx_call')
            ERR = .TRUE.
        ELSE
            IF (NARG < LIM1 .OR. NARG > LIM2) THEN
                CALL qlx_err(81022, 'qlx_call')
                ERR = .TRUE.
            ELSE
                icount64 = ICOUNT
                call set_value_at_address(icount64, 1, NARG)
!                 junk = rmtcall(SUB, ADR)
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
#if __INTEL_LLVM_COMPILER == 20250001
    use ifport, only : abort
#endif
    implicit none

    external :: qlx_err

    character(len=1) qlx_chr

    character(len=8) SKIPMSG(0:3)
    LOGICAL COMMENT
    INTEGER PRTFLAG
    DATA SKIPMSG/'<<    >>', '<<SKIP>>', '<<SKIP>>', '<< ** >>'/

    IF (NC <= LAST) THEN
        qlx_chr = INLINE(NC:NC)
        NC = NC + 1
    ELSE
         IF (.NOT. EOFL) THEN
1           CONTINUE
            IF (READREC > CURREC) THEN
               READREC=0
            ENDIF
            IF (READREC == 0) THEN
               READ(INPFILE, '(A80)', END = 10)INLINE(21:100)
               CURREC = CURREC + 1
               WRITE(TMPFILE, '(A80)', REC=CURREC)INLINE(21:100)
            ELSE
               READ(TMPFILE, '(A80)', REC=READREC)INLINE(21:100)
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
            WRITE(app_msg, '(1X, A8, 1X, A80)')   SKIPMSG(PRTFLAG), INLINE(21:100)
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
               IF (INLINE(LAST:LAST) .NE. ',') THEN
                  LAST = LAST + 1
                  INLINE(LAST:LAST) ='$'
               ENDIF
            ENDIF
            qlx_chr=INLINE(21:21)
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

!> Type of a data item
FUNCTION qlx_dtyp(ITEM)
    INTEGER qlx_dtyp
    INTEGER ITEM
    IF (ABS(ITEM) <= 2147483647) THEN
        qlx_dtyp =1
    ELSE
        qlx_dtyp =2
    ENDIF
END

!> Imprime des messages d'erreur.
!>
!> Termine l'excution si l'erreur est fatale
SUBROUTINE qlx_err(CODE, MODULE)
    use app
    use readlx_qlxbuff
    implicit none

    !> Code d'erreur
    INTEGER CODE
    !> Module dans lequel l'erreur est survenue
    character(len=*) MODULE

    INTEGER DESTI, MT, ME
    INTEGER TYPE(9)
    character(len=40) MSG(50)
    INTEGER :: I

    DATA MSG(  1) /'REPETITION NEGATIF'/
    DATA MSG(  2) /'NB DE FOIS DEJA VU OU NON ENTIER'/
    DATA MSG(  3) /'LA LIMITE EST DEPASSEE'/
    DATA MSG(  4) /'OPERATEUR MAL PLACE'/
    DATA MSG(  5) /'TOKEN MAL PLACE'/
    DATA MSG(  6) /'IL MANQUE LE SIGNE EGAL'/
    DATA MSG(  7) /'DEBORDEMENT DU TAMPON D ENTREE'/
    DATA MSG(  8) /'FIN DU FICHIER DEPASSEE'/
    DATA MSG(  9) /'INDICE NEGATIF, NUL OU NON ENTIER'/
    DATA MSG( 10) /'MANQUE LE CROCHET DROIT'/
    DATA MSG( 11) /'TABLE DES SYMBOLES PLEINE'/
    DATA MSG( 12) /'LIMITE > 99999'/
    DATA MSG( 13) /'MAUVAIS CODE DE TYPE'/
    DATA MSG( 14) /'TOKEN DOUTEUX'/
    DATA MSG( 15) /'CLE MAL UTILISEE'/
    DATA MSG( 16) /'PAS TROUVE LA CLE'/
    DATA MSG( 17) /'INDICE HORS LIMITE OU MAUVAISE CLE'/
    DATA MSG( 18) /'( ATTENDU'/
    DATA MSG( 19) /'OPERANDE DEMANDEE'/
    DATA MSG( 20) /', OU ) ATTENDU'/
    DATA MSG( 21) /'LA PILE D ARGUMENTS DEBORDE'/
    DATA MSG( 22) /'TROP OU PAS ASSEZ D''ARGUMENTS'/
    DATA MSG( 23) /'ADRESSE INVALIDE'/

    DATA TYPE( 1) /APP_INFO/
    DATA TYPE( 2) /APP_ALWAYS/
    DATA TYPE( 3) /0/
    DATA TYPE( 4) /0/
    DATA TYPE( 5) /0/
    DATA TYPE( 6) /0/
    DATA TYPE( 7) /0/
    DATA TYPE( 8) /APP_FATAL/
    DATA TYPE( 9) /APP_SYSTEM/

    MT = CODE / 10000
    NERR = NERR + 1
    ME = MOD(CODE, 1000)
    DESTI = MOD(CODE/1000, 10)

    write(app_msg, 600) MODULE, ME, MSG(ME)
600   FORMAT(A7,': RLX',I3.3,' - ',A40)

    call lib_log(APP_LIBRMN, TYPE(MT), app_msg)
    write(app_msg, '(1X,A)') INLINE(21:LAST)
    call lib_log(APP_LIBRMN, APP_VERBATIM, app_msg)
    write(app_msg, '(1X,101A1)') (' ', I=1, NC-22), '^'
    call lib_log(APP_LIBRMN, APP_VERBATIM, app_msg)
END

!> Retourne le premier caractère d'une linge de text qui soit égal à l'argument
subroutine qlx_flsh(ICAR)
    implicit none

    !> Caratère à chercher
    character(len=1) ICAR

    EXTERNAL qlx_chr
    character(len=1) qlx_chr

    do while (qlx_chr() /= ICAR)
        continue
    enddo
END

subroutine qlx_fnd(key, locvar, loccnt, limits, ityp)
    use rmn_common
    implicit none

    character(len = *), intent(in) :: key
    integer(kind = int64), intent(out) :: locvar
    integer(kind = int64), intent(out) :: loccnt
    integer, intent(out) :: limits
    integer, intent(out) :: ityp

    ! retrouve, a partir de la cle ikey, l'adresse de ivar, icount.

    integer, external :: qlx_nvar, qlx_undf, qlx_prnt
    external :: low2up, qlx_look
    character(len = 8) :: ikey
    character(len = 8), dimension(12) :: clef
    integer, save :: dummy
    integer :: pos, I

    DATA CLEF /'END', 'IF', 'ELSE', 'ENDIF', 'WHILE', 'ENDWHILE', 'ENDDATA', 'ENDCASE', 'ENDREAD', '@PRINT', '@DEFINE', '@UNDEF'/

    LOCVAR = 0
    LOCCNT = 0
    LIMITS = 0
    ITYP = -1
    CALL LOW2UP(KEY, IKEY)

    POS = 0
    DO I = 1, 12
        IF ( IKEY ==  CLEF(I) ) THEN
            POS = I
            EXIT
        ENDIF
    END DO
    select case (POS)
    case(0)
        CALL qlx_look(LOCVAR, IKEY, LOCCNT, LIMITS, ITYP)
    case(1)
        ITYP = 10
    case(2)
        ITYP = 3
    case(3)
        ITYP = 4
    case(4)
        ITYP = 5
    case(5)
        ITYP = 6
    case(6)
        ITYP = 7
    case(7)
        ITYP = 11
    case(8)
        ITYP = 12
    case(9)
        ITYP = 13
    case(10)
        ITYP = 2
        LOCVAR = LOC(qlx_prnt)
        LOCCNT = LOC(DUMMY)
        LIMITS = 202
    case(11)
        ITYP = 2
        LOCVAR = LOC(qlx_nvar)
        LOCCNT = LOC(DUMMY)
        LIMITS = 202
    case(12)
        ITYP = 2
        LOCVAR = LOC(qlx_undf)
        LOCCNT = LOC(DUMMY)
        LIMITS = 101
    end select
END


SUBROUTINE qlx_ind(IND, ERR)
    use app
    implicit none

    INTEGER IND
    LOGICAL ERR
    COMMON /qlx_tok1/ LEN, TYPE, ZVAL, INEXPR
    LOGICAL INEXPR
    INTEGER LEN, TYPE, JVAL
    REAL ZVAL
    pointer(pjval,JVAL)

    COMMON/qlx_tok2/TOKEN
    character(len=80) TOKEN

    EXTERNAL :: qlx_skp, qlx_tok, qlx_err, qlx_bak
    character(len=1) qlx_skp
    character(len=1) IC

    pjval = LOC(ZVAL)
    IND=1
    IC=qlx_skp(' ')

    IF (IC == '[') THEN
        CALL qlx_tok
        IF (((TYPE == 1) .OR.(TYPE == 0)) .AND. JVAL > 0) THEN
            IND=JVAL
        ELSE
            CALL qlx_err(21009, 'qlx_ind')
            ERR = .TRUE.
        ENDIF
        IF (.NOT.ERR) THEN
            CALL qlx_tok
            IF (TOKEN(1:1).NE.']' .OR. TYPE.NE.4) THEN
                CALL qlx_err(21010, 'qlx_ind')
                ERR = .TRUE.
            ENDIF
        ENDIF
    ELSE
        CALL qlx_bak(IC)
    ENDIF
END

!> DECLARATION DES CLES ET DE LEUR TYPE
SUBROUTINE qqlx_ins(ivar, key, icount, limits, ityp, xtern)
    use rmn_common
    use readlx_nrdlx
    implicit none

    INTEGER, INTENT(IN) :: ivar
    CHARACTER(len = *)  :: key
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
    DO WHILE (ipnt >  0 .AND. ikey /= NAMES(ipnt))
        ipnt = ipnt - 1
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

! variable lookup
SUBROUTINE qlx_look(IVAR, KEY, ICOUNT, LIMITS, ITYP)
    use rmn_common
    use readlx_nrdlx
    implicit none

    integer(kind = int64) :: ivar, icount
    INTEGER               :: ITYP, LIMITS
    character(len=*)      :: KEY

    external :: low2up

    INTEGER :: IPNT
    character(len=8) ikey

    ! TROUVER LA CLE
    CALL LOW2UP(KEY, IKEY)
    IPNT = NENTRY
    DO WHILE (IPNT >  0 .AND. IKEY .NE. NAMES(IPNT))
        IPNT = IPNT - 1
    END DO
    IF (IPNT ==  0) THEN
        ITYP = -1
        IVAR = 0
        ICOUNT = 0
        LIMITS = 0
        RETURN
    ENDIF

    ! DECORTIQUER LES PARAMETRES DE LA CLE
    IVAR = IPTADR(1, IPNT)
    ICOUNT = IPTADR(2, IPNT)
    LIMITS = IAND(ITAB(3, IPNT), ishft(-1, -(32-(24))))
    ITYP = ishft(ITAB(3, IPNT), -(24))
end


! subroutine QLXUDF(IVAR, KEY) (undefine a variable)
subroutine qlx_udf2(KEY)
    use rmn_common
    use readlx_nrdlx
    implicit none
!     integer(kind = int64) :: ivar
    character(len=*) KEY

    INTEGER IPNT

    integer :: i
    character(len=8) IKEY

    ! TROUVER LA CLE
    IKEY = KEY
    IPNT = NENTRY
    DO WHILE (IPNT >  0 .AND. IKEY.NE.NAMES(IPNT))
        IPNT = IPNT - 1
    END DO
    IF (IPNT  ==  0) THEN
        RETURN
    ENDIF
    DO I = IPNT, NENTRY-1
        IPTADR(1, I) = IPTADR(1, I+1)
        ITAB(3, I) = ITAB(3, I+1)
        IPTADR(2, I) = IPTADR(2, I+1)
        NAMES(I)  = NAMES(I+1)
    END DO
    NENTRY = NENTRY - 1
END

subroutine qlx_dtb
    use rmn_common
    use readlx_nrdlx
    implicit none

    integer :: i
    PRINT *, ' NAMES, LOCVAR, TYPE/LIMITS, LOCCOUNT'
    DO I = 1, NENTRY
        PRINT 101, NAMES(I), IPTADR(1, I), ITAB(3, I), IPTADR(2, I)
    END DO
101   FORMAT (2X,A8,3Z22)
END


!> Reconstituer un nombre entier, reel ou octal
INTEGER FUNCTION qlx_num(IB, LENG)
    implicit none

    character(len=*), intent(INOUT) :: IB
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
    IF (I == '.' .AND. IB(1:1).NE.'.') THEN    ! decimal period, not in column 1
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
6           LENG = MIN(21, LENG + 1)
            IB(LENG:LENG) = I
            I = qlx_chr()
            IF (I >= '0' .AND. I <= '9') THEN  ! more digits
               GOTO 6
            ENDIF
         ENDIF
      ENDIF

      IF (LENG >= 21) THEN
         qlx_num=5                              ! bad number
      ELSE
         IF (ILX == 0) THEN
            IF (I.NE.'B') THEN
               qlx_num=1                        ! integer
            ELSE
               qlx_num=6                        ! octal
               I=qlx_chr()
               DO J = LENG, 1, -1
                  IF (IB(J:J) > '7') THEN
                     qlx_num=5                  ! bad number
                  ENDIF
                  CTMP = IB(J:J)
                  IB(20-LENG+J:20-LENG+J)=CTMP
               END DO
               DO J = 1, 20 - LENG
                  IB(J:J)='0'
               END DO
               LENG=20
            ENDIF
         ELSE
            IF (LENG > 1) THEN
               IF (IB(LENG:LENG) == '.') THEN
                  qlx_num=2                     ! real number
               ELSE
                  IF (IB(LENG:LENG) >= '0' .AND. IB(LENG:LENG) <= '9') THEN
                     qlx_num=2                  ! real number
                  ELSE
                     qlx_num=5                  ! bad number
                  ENDIF
               ENDIF
            ELSE
               qlx_num=5
            ENDIF
         ENDIF
      ENDIF
      CALL qlx_bak(I)
END


SUBROUTINE qlx_nvar(KEY, NW)
    use rmn_common
    use readlx_qlxfmt
    implicit none

    INTEGER NW
    INTEGER KEY(*)

    EXTERNAL :: qlx_look, qlx_err, qlxins
    INTEGER, EXTERNAL :: argdims

    INTEGER, SAVE :: SC(1024), NSC

    INTEGER :: DUMMY, LIMITS, J, ITYP
    character(len=8) IKEY
    integer(kind = int64) IVAR, ICOUNT

    SAVE DUMMY
    DATA NSC /1/
    DATA DUMMY /0/

    WRITE(IKEY, LINEFMT) (KEY(J), J=1, argdims(1))
    CALL qlx_look(IVAR, IKEY, ICOUNT, LIMITS, ITYP)
    IF (ITYP /= -1) THEN
        RETURN
    ENDIF
    IF (NSC + NW > 1024 + 1) THEN
        CALL qlx_err(21011, 'qlx_nvar')
        RETURN
    ENDIF
    CALL qlxins(SC(NSC), IKEY, DUMMY, NW, 1)
    NSC = NSC + NW
END


!> Appliquer un operateur numerique ou logique
SUBROUTINE qlx_opr(TOKENS, NTOKEN, TOKTYPE, OPRTR, ERR)
    use rmn_common
    implicit none

    !       INTEGER NTOKEN, OPRTR, TOKENS(NTOKEN), TOKTYPE(NTOKEN)
    INTEGER NTOKEN, OPRTR, TOKTYPE(NTOKEN)
    integer(kind = int64) :: TOKENS(NTOKEN)
    LOGICAL ERR

    EXTERNAL :: get_value_at_address, set_value_at_address

    INTEGER :: IZ1, IZ2, IR1, tok32, MINOPER
    REAL   Z1,  Z2,  R1
    pointer(pz1,Z1)
    pointer(pz2,Z2)
    pointer(pr1,R1)
    LOGICAL REALOP
    integer(kind = int32) :: TOK
    POINTER (PTOK, TOK(*))

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
    IF (OPRTR.NE.2 .AND. OPRTR.NE.17   .AND. OPRTR.NE.21 .AND. OPRTR.NE.4) THEN
        IF (TOKTYPE(NTOKEN-1) > 0) THEN
            call get_value_at_address(TOKENS(NTOKEN-1), 1, tok32)
            TOKENS(NTOKEN-1) = tok32
            TOKTYPE(NTOKEN-1) = 0
        ENDIF
    ENDIF
    REALOP = ABS(TOKENS(NTOKEN)) > 2147483647
    IZ1 = int(TOKENS(NTOKEN))
    IF (OPRTR.NE.2 .AND. OPRTR.NE.17 .AND. OPRTR.NE.4) THEN
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
            IF (Z2.NE.Z1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ELSE
            IF (IZ2.NE.IZ1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ENDIF
    case(16)              ! binary ><
        IF (REALOP) THEN
            IF (Z2.NE.Z1) THEN
            IR1 =ishft(-1, 32-(32))
            ENDIF
        ELSE
            IF (IZ2.NE.IZ1) THEN
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

!  FONCTION  qlx_pri_l EVALUER LA PRIORITE D'UN OPERATEUR
INTEGER FUNCTION qlx_pri_l(OPR, LEFTPRI)
    implicit none

    character(len=*) OPR
    LOGICAL LEFTPRI

    !     INTEGER qlx_pril
    INTEGER, PARAMETER :: MAXOPER = 23
    INTEGER PRI(MAXOPER)
    INTEGER :: I
    character(len=4) LISTE(MAXOPER), OPRTR
    SAVE LISTE, PRI
    DATA LISTE/   ')' ,   ']' ,   'U+' ,   'U-', '**' ,   '*' ,     &
    '/' ,   '+', '-' ,   '<' ,   '>' ,   '==', '<=' ,   '>=' ,     &
    '<>' ,   '><', 'NOT',   'AND',   'OR' ,   'XOR', ':=' ,   '('  &
    ,   '[' /
    DATA PRI  /  150 ,   150 ,   101  ,   101, 91 ,   81 ,   81  ,  &
        71, 71 ,   61 ,   61  ,   61, 61 ,   61 ,   61  ,   61, 51 ,   &
        41 ,   41  ,   41, 10 ,   1 ,   1   /
    OPRTR = OPR

    DO I = 1, MAXOPER
        IF (OPRTR == LISTE(I)) THEN
            IF (LEFTPRI) THEN
                qlx_pri_l = I + PRI(I)*100
            ELSE
                qlx_pri_l = I + (PRI(I)-MOD(PRI(I), 2))*100
            ENDIF
            RETURN
        ENDIF
    END DO
    qlx_pri_l = 0
END

!> Evaluer la priorite d'un operateur (right priority)
integer function qlx_pri(opr)
    implicit none

    character(len=*) opr

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

SUBROUTINE qlx_prnt(QUOI, COMMENT)
    use app
    use readlx_qlxfmt
    implicit none

    INTEGER QUOI(*), COMMENT(*)
    character(len=120) FMT
    INTEGER, external :: argdims
    integer :: I, L1, L2

    L1 = argdims(1)
    L2 = MIN(120/KARMOT, argdims(2))
    IF (L1 < 1 .OR. L2 < 1) THEN
        RETURN
    ENDIF
    WRITE(FMT, LINEFMT)(COMMENT(I), I=1, L2)
    ! WRITE(6, FMT)(QUOI(I), I=1, L1)
    WRITE(app_msg, FMT)(QUOI(I), I=1, L1)
    call lib_log(APP_LIBFST, APP_VERBATIM, app_msg)
END

!> Conversion a notation postfixe (reverse polish notation)
SUBROUTINE qlx_rpn(TOK, TOKENS, MAXTKNS, NTOKEN, TOKTYPE, PILEOP, MAXOPS, NOPER, ERR)
    use rmn_common
    implicit none

    character(len=*) TOK
    INTEGER MAXTKNS, NTOKEN, MAXOPS, NOPER
    !     INTEGER TOKENS(MAXTKNS), TOKTYPE(MAXTKNS)
    INTEGER TOKTYPE(MAXTKNS)
    integer(kind  = int64) :: TOKENS(MAXTKNS)
    LOGICAL ERR

    INTEGER, EXTERNAL :: qlx_pri, qlx_pril
    EXTERNAL :: qlx_opr

    character(len=4) TOKEN
    character(len=4) PILEOP(MAXOPS)

    IF (ERR) THEN
        RETURN
    ENDIF

    TOKEN = TOK
    IF (TOKEN == '(' .OR. TOKEN == '[') THEN
        NOPER = MIN(NOPER+1 , MAXOPS)
        PILEOP(NOPER) = TOKEN
    ELSE
    IF (TOKEN == ')') THEN
        DO WHILE (PILEOP(NOPER) .NE.'(' .AND. PILEOP(NOPER) .NE.'[' .AND. PILEOP(NOPER) .NE.'$')
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
        DO WHILE (PILEOP(NOPER) .NE.'(' .AND. PILEOP(NOPER) .NE. '[' .AND. PILEOP(NOPER) .NE.'$')
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
        DO WHILE (PILEOP(NOPER) .NE.'(' .AND. PILEOP(NOPER) .NE.'[' .AND. PILEOP(NOPER) .NE.'$')
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

!> Retourne le premier caractere d'une ligne de texte, différent de icar
FUNCTION qlx_skp(ICAR)
    implicit none

    character(len=1) qlx_skp
    !> Caratère à ignorer
    character(len=1), intent(in) :: ICAR

    EXTERNAL qlx_chr
    character(len=1) :: CTMP, qlx_chr

    CTMP = qlx_chr()
    do while (CTMP == ICAR)
        CTMP = qlx_chr()
    end do
    qlx_skp = CTMP
END


!> Decomposer une ligne de texte en tokens de differents types, identifie la longueur du token et son type.
SUBROUTINE qlx_tok
    use rmn_common
    use readlx_qlxfmt
    implicit none

    ! ARGUMENTS
    ! TOKEN
    ! (S)

    ! LEN       NOMBRE DE CARACTERE DANS UN TOKEN
    ! (S)

    ! TYPE      TYPE DU TOKEN(CLE ALPHANUMERIQUE, NOMBRE
    ! (S)       ENTIER OU REEL, CHAINE DE CARACTERE OU SYMBOLE).

    ! JVAL, ZVAL LES VALEURS D'UN NOMBRE ENTIER OU REEL,
    ! (S)       CONTENU DANS UN TOKEN.

    INTEGER JSIGN, ITYP
    COMMON /qlx_tok1/ LEN, TYPE, ZVAL, INEXPR
    LOGICAL INEXPR
    INTEGER LEN, TYPE, JVAL
    REAL ZVAL
    !     EQUIVALENCE (ZVAL, JVAL)
    pointer(pjval,JVAL)

    COMMON /qlx_tok2/ TOKEN
    character(len=80) TOKEN

    EXTERNAL :: qlx_chr, qlx_num, qlx_err, qlx_bak, qlx_fnd, get_value_at_address

    integer(kind = int64) :: LOCVAR, LOCCNT

    character(len=1) IC, qlx_chr
    INTEGER :: qlx_num, ival, leng, limits

    pjval = LOC(ZVAL)
    IVAL = -1
    JSIGN = 0
    TOKEN = ' '

    IC = qlx_chr()
    DO WHILE (.NOT.(IC .NE. ' '))
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
            TYPE = 3                 ! string ( non delimited )
        ELSE
            TYPE = 0                 ! short string, possibly a key
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
        TYPE = 3                    ! string ( delimited )
    ELSE
    IF ( (IC >= '0' .AND. IC <= '9') .OR. (IC == '.') ) THEN
        TYPE = qlx_num(TOKEN, LENG)  ! 1/2/5/6
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
            TYPE = qlx_num(TOKEN, LENG)  ! 1/2/5/6
        ELSE
            CALL qlx_bak(IC)
            TYPE = 4              ! special char, possibly operator
        ENDIF
    ELSE
    IF (IC == '*') THEN
        TYPE = 4                  ! operator (1 or 2 chars)
        IC = qlx_chr()
        IF (IC == '*') THEN
            LENG = 2
            TOKEN = '**'
        ELSE
            CALL qlx_bak(IC)
        ENDIF
    ELSE
    IF (IC == '<' .OR. IC == '>' .OR. IC == '=' .OR. IC == ':') THEN
        TYPE = 4                  ! operator (1 or 2 chars)
        IC = qlx_chr()
        IF (IC == '<' .OR. IC == '>' .OR. IC == '=') THEN
            LENG = 2
            TOKEN(2:2) = IC
        ELSE
            CALL qlx_bak(IC)
        ENDIF
    ELSE
        TYPE = 4                  ! operator
    ENDIF
    ENDIF
    ENDIF
    ENDIF
    ENDIF
    ENDIF

    IF ( (LENG > 80) .OR. (TYPE == 5) ) THEN
        TOKEN = 'SCRAP'
        TYPE = 5
        CALL qlx_err(21014, 'qlx_tok')
    ENDIF
    IF (TYPE == 1) THEN         ! integer
        READ(TOKEN, '(I20)')JVAL
        JVAL = SIGN(JVAL, JSIGN)
    ELSE
    IF (TYPE == 2) THEN         ! float
        READ(TOKEN, '(G20.3)')ZVAL
        ZVAL = SIGN(ZVAL, FLOAT(JSIGN))
    ELSE
    IF (TYPE == 6) THEN          ! octal constant
        READ(TOKEN, '(O20)')JVAL
        TYPE = 1                 ! integer
        JVAL = SIGN(JVAL, JSIGN)
    ENDIF
    ENDIF
    ENDIF
    IF (TYPE == 0) THEN
        CALL qlx_fnd(TOKEN(1:8), LOCVAR, LOCCNT, LIMITS, ITYP)
        IF (ITYP  ==  -1) THEN
            TYPE = 3
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


SUBROUTINE qlx_undf(IKEY)
    use rmn_common
    use readlx_qlxfmt
    implicit none

    INTEGER IKEY(*)

    INTEGER, EXTERNAL :: argdims
    EXTERNAL :: qlx_udf2

    character(len=8) CKEY
    INTEGER :: I

    WRITE(CKEY, 101) (IKEY(I), I=1, argdims(1))
101   FORMAT(2 A4)
    CALL qlx_udf2(CKEY)
END


FUNCTION qlx_val(KLE, ERR)
    implicit none

    INTEGER qlx_val

    character(len=*) KLE
    LOGICAL ERR

    EXTERNAL :: qlx_ind, qlx_adi2

    !     INTEGER IND, VAL, DUM
    INTEGER IND, VAL

    CALL qlx_ind(IND, ERR)

    VAL = 0
    IF (.NOT. ERR) THEN
        CALL qlx_adi2(KLE, IND, VAL, ERR)
    ENDIF
    qlx_val = VAL
END


!> Traiter une expression arithmetique ou logique
SUBROUTINE qlx_xpr(ERR)
    use app
    use rmn_common
    implicit none

    LOGICAL ERR
    COMMON /qlx_tok1/ LEN, TYPE, ZVAL, INEXPR
    LOGICAL INEXPR
    INTEGER LEN, TYPE, JVAL
    REAL ZVAL
    pointer(pjval,JVAL)

    COMMON /qlx_tok2/ TOKEN
    character(len=80) TOKEN

    COMMON /qlx_tok3/ jval64
    integer(kind = int64) :: jval64

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
        IF (TYPE == 0) THEN
            NTOKEN = NTOKEN + 1
            CALL qlx_fnd(TOKEN(1:8), LOCVAR, LOCCNT, LIMITES, ITYP)
            IF (ITYP.NE.0 .AND. ITYP.NE.1) THEN
                ERR = .TRUE.
            ENDIF
            TOKENS(NTOKEN) = LOCVAR
            TOKTYPE(NTOKEN) = LIMITES + 1
            IF (.NOT. UNARY) THEN
                ERR = .TRUE.
            ENDIF
            UNARY = .FALSE.
        ELSE
        IF (TYPE == 1 .OR. TYPE == 2) THEN
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
                IF (TOKEN(1:2).NE.'( ' .AND. TOKEN(1:2).NE.'[ ') THEN
                    ERR = .TRUE.
                ENDIF
                ENDIF
                ENDIF
            ENDIF
            UNARY = TOKEN(1:1).NE.')' .AND. TOKEN(1:1).NE.']'
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

    IF (PLEV > 0 .OR. .NOT.FINI .OR. BLEV > 0   .OR. NTOKEN.NE.1 ) THEN
        ERR = .TRUE.
    ENDIF
    INEXPR = .FALSE.
    IF (.NOT.ERR) THEN
        TOKEN = ' '
        JVAL = int(TOKENS(1))
        IF (TOKTYPE(1) > 0) THEN
            TYPE = 8      ! adresse
            jval64 = TOKENS(1)
        ELSE
        IF (ABS(JVAL) <= 2147483647) THEN
            TYPE =1
        ELSE
            TYPE =2
        ENDIF
        ENDIF
    ENDIF
    IF (ERR) THEN
        CALL qlx_err(81005, 'QLXEXPR')
    ENDIF
END

!> Passage d'options a readlx
SUBROUTINE qlxopt(OPTION, VAL)
    use app
    use readlx_qlxfmt

    character(len=*) OPTION
    INTEGER VAL

    IF (OPTION(1:6) ==  'CARMOT') THEN
        KARMOT = VAL
        WRITE(LINEFMT, '(A,I2,A)') '(25 A', KARMOT, ')'
    ELSE
        WRITE(app_msg, *) 'Option (', OPTION, ') unknown'
        call lib_log(APP_LIBRMN, APP_ERROR, app_msg)
    ENDIF
END

!> LONGUEUR D'ARGUMENTS (APPEL VIA readlx)
FUNCTION argdims(N)
      use rmn_common
      use readlx_parmadr
      INTEGER argdims
      INTEGER N

!OBJET(argdims)
!         RENVOYER LA LONGUEUR EN NOMBRE DE MOTS DE L'ARGUMENT
!         N DU DERNIER APPEL EFFECTUE VIA readlx

!ARGUMENTS
! IN      N     NUMERO D'ORDRE DE L'ARGUMENT DANS LA LISTE

    IF (N  <=  NARG) THEN
        argdims = DOPE(N)
    ELSE
        argdims = 0
    ENDIF
END

!> GET DOPE LIST OF ARGUMENT NARG
FUNCTION argdope(N, LISTE, ND)
    use rmn_common
    use readlx_parmadr

    INTEGER argdope
    INTEGER N, ND
    INTEGER LISTE(ND)

    INTEGER I, BASE

    IF (N >  NARG) THEN
        argdope = 0
    ELSE
        BASE = DOPEA(N)
        argdope = DOPEA(N+1) - DOPEA(N)
        DO I = 1, MIN(DOPEA(N + 1) - DOPEA(N), ND)
            LISTE(I) = DOPES(BASE + I - 1)
        END DO
    ENDIF
END


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
SUBROUTINE lexins(IVAR, ICLE, NB, LIMIT, TYP)
    implicit none

    INTEGER :: IVAR, ICLE, NB, LIMIT, TYP

    EXTERNAL :: QLXINS

    character(len=8) :: KLE

    WRITE(KLE, '(A8)') ICLE
    CALL qlxins(IVAR, KLE, NB, LIMIT, TYP)
END

!> DECLARATION DES CLES
SUBROUTINE qlxins(ivar, key, icount, limits, ityp)
    use app
    IMPLICIT NONE

    INTEGER :: ivar
    CHARACTER(LEN = *) :: key
    INTEGER, INTENT(OUT) :: icount
    INTEGER :: limits
    INTEGER :: ityp

    integer, EXTERNAL :: readlx
    EXTERNAL :: qlx_err, qqlx_ins

    IF (ityp == 2) THEN
        CALL lib_log(APP_LIBRMN, APP_ERROR, 'qlxinx doit etre utilise quand ityp = 2, au lieu de QLXINS')
        CALL qlx_err(81013, 'QLXINS')
        STOP
    ELSE
        CALL qqlx_ins(ivar, key, icount, limits, ityp, readlx)
    ENDIF
END

!> Interprete de directives
SUBROUTINE readlx(UNIT, KEND, KERR)
    use app
    use rmn_common
    use readlx_qlxbuff
    use readlx_qlxfmt
    implicit none

    !> Numéro d'unité d'entrée
    INTEGER, INTENT(in) :: UNIT
    !> 0 : Pas de problème
    INTEGER, INTENT(out) :: KEND

    INTEGER, INTENT(inout) :: KERR

    COMMON /qlx_tok1/ LEN, TYPE, ZVAL, INEXPR
    LOGICAL INEXPR
    INTEGER LEN, TYPE, JVAL
    REAL ZVAL
    pointer(pjval,JVAL)

    COMMON/qlx_tok2/TOKEN
    character(len=80) TOKEN

    EXTERNAL :: qlx_nvar, qlx_prnt, qlx_undf
    EXTERNAL :: qlxinx, qlx_bak, qlx_err, qlx_tok, qlx_fnd, qlx_asg, qlx_call, qlx_xpr, qlx_flsh
    external :: get_value_at_address, set_value_at_address

#include <rmn/fnom.hf>
    integer(kind = int64) :: LOCCNT, LOCVAR
    Integer IICNT
    INTEGER LIMITS, ITYP
    LOGICAL FIN, ERR
    INTEGER, PARAMETER :: MAXSTRU = 20
    INTEGER NXTELSE(0:2), NEXTIF(0:2), STYPE(MAXSTRU), SKIPF(MAXSTRU)
    INTEGER READBSE(MAXSTRU)
    INTEGER NSTRUC, ier
    character(len=128) nomscra
    integer(kind = int64) :: jval64
    INTEGER :: KERRMAX
    INTEGER :: IDUM


    DATA NXTELSE / 1, 0, 2/
    DATA NEXTIF  / 0, 2, 2/

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
        IF (TYPE == 0) THEN
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
                            IF (TOKEN(1:1).NE.'$') THEN
                                CALL qlx_xpr(ERR)
                                IF (ERR) THEN
                                    EXIT
                                ENDIF
                                IF (TYPE == 8) THEN
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
                            IF (STYPE(NSTRUC).NE.3) THEN
                                EXIT
                            ENDIF
                            STYPE(NSTRUC) = ITYP
                            SKIPF(NSTRUC) = NXTELSE(SKIPF(NSTRUC))
                            CALL qlx_flsh('$')
                        ELSE
                            IF (ITYP == 5) THEN
                                IF (STYPE(NSTRUC).NE.3 .AND. STYPE(NSTRUC).NE.4) THEN
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
                                    IF (READREC.NE. 0) THEN
                                        READBSE(NSTRUC) = READREC -1
                                    ELSE
                                        READBSE(NSTRUC) = CURREC
                                    ENDIF
                                    IF (SKIPF(NSTRUC) == 0) THEN
                                        CALL qlx_tok
                                        IF (TOKEN(1:1).NE.'$') THEN
                                            CALL qlx_xpr(ERR)
                                            IF (ERR) THEN
                                                EXIT
                                            ENDIF
                                            IF (TYPE == 8) THEN
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
                                        IF (STYPE(NSTRUC).NE.6) THEN
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
                                            IF (SKIPF(NSTRUC).NE.0) THEN
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
        IF (ERR.AND.(TOKEN(1:1).NE.'$'.OR. TYPE.NE.4)) THEN
            CALL qlx_flsh('$')
        ENDIF
    ENDDO

    IF (NSTRUC > 1) THEN
        call lib_log(APP_LIBRMN, APP_ERROR, 'readlx: Error within if then else bloc structure')
        KERR = NERR + 1
        KEND = -1
    ENDIF

    CLOSE(TMPFILE, STATUS='DELETE')
END

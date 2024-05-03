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
  integer(C_INT32_t) function remote_call(fn, args) BIND(C)
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
subroutine get_content_of_address(address, subscript, content)
  use rmn_common
  integer(kind = int64), intent(IN) :: address   ! memory address
  integer, intent(IN) :: subscript               ! subscript
  integer, intent(OUT) :: content                ! output from memory
  integer :: val
  pointer(pval, val(*))

  pval = address
  content = val(subscript)
end subroutine

! set contents at address(subscript) (assuming a 32 bit item)
subroutine set_content_of_address(address, subscript, content)
  use rmn_common
  integer(kind = int64), intent(IN) :: address   ! memory address
  integer, intent(IN) :: subscript               ! subscript
  integer, intent(IN) :: content                 ! value to set to
  integer :: val
  pointer(pval, val(*))

  pval = address
  val(subscript) = content
end subroutine

!> LONGUEUR D'ARGUMENTS (APPEL VIA READLX)
FUNCTION ARGDIMS(N)
      use rmn_common
      use readlx_parmadr
      INTEGER ARGDIMS
      INTEGER N

!OBJET(ARGDIMS)
!         RENVOYER LA LONGUEUR EN NOMBRE DE MOTS DE L'ARGUMENT
!         N DU DERNIER APPEL EFFECTUE VIA READLX

!ARGUMENTS
! IN      N     NUMERO D'ORDRE DE L'ARGUMENT DANS LA LISTE

    IF (N  <=  NARG) THEN
        ARGDIMS = DOPE(N)
    ELSE
        ARGDIMS = 0
    ENDIF
END


!> GET DOPE LIST OF ARGUMENT NARG
FUNCTION ARGDOPE(N, LISTE, ND)
    use rmn_common
    use readlx_parmadr

    INTEGER ARGDOPE
    INTEGER N, ND
    INTEGER LISTE(ND)

    INTEGER I, BASE

    IF (N >  NARG) THEN
        ARGDOPE = 0
    ELSE
        BASE = DOPEA(N)
        ARGDOPE = DOPEA(N+1) - DOPEA(N)
        DO I = 1, MIN(DOPEA(N + 1) - DOPEA(N), ND)
            LISTE(I) = DOPES(BASE + I - 1)
        END DO
    ENDIF
END


!> Interface de qlxins
SUBROUTINE LEXINS(IVAR, ICLE, NB, LIMIT, TYP)
    INTEGER :: IVAR, ICLE, NB, LIMIT, TYP

    character(len=8) :: KLE

    WRITE(KLE, '(A8)') ICLE
    CALL qlxins(IVAR, KLE, NB, LIMIT, TYP)
END

!> Get value of indexed array component
! SUBROUTINE QLXADI(KLE, IND, VALEUR, TYPE, ERR)
SUBROUTINE QLXADI2(KLE, IND, VALEUR, ERR)
    use rmn_common
!     INTEGER IND, VALEUR, TYPE
    INTEGER IND, VALEUR
    LOGICAL ERR
    character(len=*) KLE

    INTEGER  QLXDTYP
    EXTERNAL QLXDTYP
    integer(kind = int64) LOCVAR, LOCCNT
    INTEGER LIMITE, ITYP, IZ, INDX
    integer, dimension(1024) :: mem
    pointer(pmem, mem)
    REAL Z

    IZ = IND
    IF (QLXDTYP(IZ) == 1) THEN
        INDX = IZ
    ELSE
        z = transfer(iz, z)
        INDX = NINT(Z)
    ENDIF
    CALL QLXFND(KLE, LOCVAR, LOCCNT, LIMITE, ITYP)
    IF (ITYP.NE.0 .AND. ITYP.NE.1) THEN
        ERR = .TRUE.
    ENDIF
    IF (INDX > LIMITE .OR. INDX <= 0) THEN
        ERR = .TRUE.
    ENDIF
    IF (.NOT.ERR) THEN
        pmem = LOCVAR
        VALEUR = mem(INDX)
!         CALL PEEK(LOCVAR, INDX, VALEUR)
    ENDIF
END

!> Get subscript then build memory address
integer(kind = int64) FUNCTION QLXADR(KLE, ERR)
    use rmn_common

    character(len=*) KLE
    LOGICAL ERR
    INTEGER LIMITS, ITYP
    integer(kind = int64) :: LOCCNT, locvar8
    POINTER (LOCVAR, VARI(*))

    CALL QLXIND(IND, ERR)

    IF (.NOT. ERR) THEN
        CALL QLXFND(KLE, LOCVAR8, LOCCNT, LIMITS, ITYP)
!         call make_cray_pointer(LOCVAR, locvar8)
        LOCVAR = transfer(locvar8, LOCVAR)
        IF (IND <= LIMITS .AND. ITYP >= 0 .AND. ITYP <= 1) THEN
            QLXADR = LOC(VARI(IND))
        ELSE
            ERR = .TRUE.
            CALL QLXERR(21017, 'QLXADR')
            QLXADR = 0
        ENDIF
    ELSE
        QLXADR = 0
    ENDIF
END

!> Prend les tokens qui suivent le signe = et separes par des virgules pour les placer a l'adresse val
SUBROUTINE QLXASG(VAL, ICOUNT, LIMIT, ERR)
    use rmn_common
    use readlx_qlxfmt
    !> Adresse de la clé cible
    integer(kind = int64), intent(in) :: val
    !> Nombre de mots déposés
    integer, intent(inout) :: icount
    !> Nombre maximal de mots disponibles
    integer, intent(in) :: limit
    !> Indicateur d'erreur
    logical, intent(out) :: err

    COMMON /QLXTOK1/ LEN, TYPE, ZVAL, INEXPR
    LOGICAL INEXPR
    INTEGER LEN, TYPE, JVAL
    REAL ZVAL
    pointer(pjval,jval)
    integer(kind = int64) :: jval64

    COMMON/QLXTOK2/TOKEN
    character(len=80) TOKEN

    INTEGER IND, JLEN, QLXVAL
    INTEGER OLDTYP, ITEMP(80), IREPCN
    LOGICAL IAREP, FIN

    pjval = LOC(ZVAL)
    IND = 1
    OLDTYP = 4
    FIN = .FALSE.
    IAREP = .FALSE.
    IREPCN = 1
    JLEN = 0
    CALL QLXIND(IND, ERR)

    IF (.NOT.ERR) THEN
        CALL QLXTOK
    ENDIF

    IF (TOKEN(1:2) == '= ' .AND. TYPE == 4 .AND. .NOT. ERR) THEN
        DO WHILE (.NOT.ERR .AND. .NOT.FIN)
            CALL QLXTOK
            IF ((TYPE == 4) .AND. (TOKEN(1:1) == '(')) THEN
                CALL QLXXPR(ERR)
                IF (ERR) THEN
                    EXIT
                ENDIF
            ENDIF
            IF (TYPE == 8) THEN
!                 call get_content_of_address(JVAL, 1, JVAL)
                jval64 = jval
                call get_content_of_address(jval64, 1, JVAL)
            ELSE
                IF (TYPE == 1 .AND. OLDTYP == 4) THEN
                    ITEMP(1) = JVAL
                    JLEN = 1
                ELSE
                    IF (TYPE == 2 .AND. OLDTYP == 4) THEN
!                         TEMP(1) = ZVAL
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
                                            CALL QLXERR(21001, 'QLXASG')
                                            ERR = .TRUE.
                                        ENDIF
                                    ELSE
                                        CALL QLXERR(21002, 'QLXASG')
                                        ERR = .TRUE.
                                    ENDIF
                                ELSE
                                    IF (TOKEN(1:2) == ', ' .OR.TOKEN(1:2) == '$ ') THEN
                                        IF ((IREPCN * MAX(JLEN, 1) + IND) > LIMIT + 1) THEN
                                            CALL QLXERR(21003, 'QLXASG')
                                            ERR = .TRUE.
                                        ELSE
                                            DO I = 1, IREPCN
                                                DO J = 1, JLEN
                                                    call set_content_of_address(VAL, IND + J - 1, ITEMP(J))
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
                                        CALL QLXERR(21004, 'QLXASG')
                                        ERR = .TRUE.
                                    ENDIF
                                ENDIF
                            ELSE
                                IF (TYPE == 0 .AND. OLDTYP == 4) THEN
                                    JLEN = 1
                                    ITEMP(1) = QLXVAL(TOKEN(1:8), ERR)
                                ELSE
                                    CALL QLXERR(21005, 'QLXASG')
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
        CALL QLXERR(21006, 'QLXASG')
        ERR = .TRUE.
    ENDIF
END

! Remettre un caractère dans une ligne de texte à la position courante et reculer le pointeur du caractère courant
SUBROUTINE QLXBAK(ICAR)
    use readlx_qlxbuff
    !> Caractère à remettre dans la ligne de texte
    character(len = 1), intent(in) :: ICAR

    IF (NC > 1) THEN
        INLINE(NC-1:NC-1)=ICAR
        NC=NC-1
    ELSE
        CALL QLXERR(81007, 'QLXBAK')
    ENDIF
END

SUBROUTINE QLXCALL(SUB, ICOUNT, LIMITS, ERR)
    use rmn_common
    use readlx_parmadr
    use readlx_qlxfmt
    use readlx_remote

    integer(kind = int64) :: SUB, ICOUNT

    COMMON /QLXTOK1/ LEN, TYPE, ZVAL, INEXPR
    LOGICAL INEXPR
    INTEGER LEN, TYPE, JVAL
    REAL ZVAL
    pointer(pjval,jval)

    COMMON/QLXTOK2/TOKEN
    character(len=80) TOKEN

    integer, external :: rmtcall
    EXTERNAL QLXADR, QLXVAL
    INTEGER  QLXVAL
    INTEGER LIM1, LIM2, JLEN, PREVI
    integer(kind = int64) LOCDUM, QLXADR
    character(len=8) KLE
    integer(kind = int64) icount64

    LOGICAL ERR, FIN, INLIST

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

    CALL QLXTOK
    IF (TYPE.NE.4 .AND. TOKEN(1:1).NE.'(') THEN
        CALL QLXERR(81018, 'QLXCALL')
        ERR = .TRUE.
    ENDIF

    DO WHILE (.NOT. ERR .AND. .NOT.FIN)
        CALL QLXTOK
        IF (PREVI  == 4) THEN
            IF (TYPE  == 0) THEN
                KLE = TOKEN(1:8)
                PREVI =7
                IF (INLIST) THEN
                    NPRM = MIN(NPRM+1, 101)
                    PARM(NPRM) = QLXVAL(KLE, ERR)
                ELSE
                    NARG = MIN(NARG+1, 41)
                    ADR(NARG) = QLXADR(KLE, ERR)
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
                                CALL QLXERR(81019, 'QLXCALL')
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
                    CALL QLXERR(81020, 'QLXCALL')
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
            CALL QLXERR(81021, 'QLXCALL')
            ERR = .TRUE.
        ELSE
            IF (NARG < LIM1 .OR. NARG > LIM2) THEN
                CALL QLXERR(81022, 'QLXCALL')
                ERR = .TRUE.
            ELSE
                icount64 = ICOUNT
                call set_content_of_address(icount64, 1, NARG)
!                 junk = rmtcall(SUB, ADR)
                junk = remote_call(SUB, ADR)
                call set_content_of_address(icount64, 1, 0)
                CALL QLXFLSH('$')
            ENDIF
        ENDIF
    ENDIF
END

!> Retourne un caractere a la fois d'une ligne
function QLXCHR()
    use App
    use readlx_qlxbuff
    character(len=1) QLXCHR

    character(len=8) SKIPMSG(0:3)
    LOGICAL COMMENT
    INTEGER PRTFLAG
    DATA SKIPMSG/'<<    >>', '<<SKIP>>', '<<SKIP>>', '<< ** >>'/

    IF (NC <= LAST) THEN
        QLXCHR = INLINE(NC:NC)
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
            QLXCHR=INLINE(21:21)
            NC = 22
         ELSE
            CALL QLXERR(81008, 'QLXCHR')
            CALL ABORT
         ENDIF
      ENDIF
      RETURN
10    INLINE = ' END$'
      QLXCHR = ' '
      EOFL = .TRUE.
      LAST = 5
      NC = 2
END

SUBROUTINE QLXDBG
    use app
    use readlx_qlxbuff

    WRITE(app_msg, *) 'qlxdbg: NC=', NC, 'LAST=', LAST, 'INPFILE=', INPFILE
    call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
    WRITE(app_msg, '(1X,A101)')INLINE(1:101)
    call lib_log(APP_LIBRMN, APP_DEBUG, app_msg)
END

!> Type of a data item
FUNCTION QLXDTYP(ITEM)
    INTEGER QLXDTYP
    INTEGER ITEM
    IF (ABS(ITEM) <= 2147483647) THEN
        QLXDTYP =1
    ELSE
        QLXDTYP =2
    ENDIF
END

!> Imprime des messages d'erreur.
!>
!> Termine l'excution si l'erreur est fatale
SUBROUTINE QLXERR(CODE, MODULE)
    use app
    use readlx_qlxbuff
    !> Code d'erreur
    INTEGER CODE
    !> Module dans lequel l'erreur est survenue
    character(len=*) MODULE

    INTEGER DESTI, MT, ME
    INTEGER TYPE(9)
    character(len=40) MSG(50)
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
subroutine QLXFLSH(ICAR)
    !> Caratère à chercher
    character(len=1) ICAR

    EXTERNAL QLXCHR
    character(len=1) QLXCHR

    do while (QLXCHR() /= ICAR)
        continue
    enddo
END

subroutine qlxfnd(key, locvar, loccnt, limits, ityp)
    use rmn_common

    character(len = *), intent(in) :: key
    integer(kind = int64), intent(out) :: locvar
    integer(kind = int64), intent(out) :: loccnt
    integer, intent(out) :: limits
    integer, intent(out) :: ityp

    ! retrouve, a partir de la cle ikey, l'adresse de ivar, icount.

    integer, external :: qlxnvar, qlxundf, qlxprnt
    external low2up
    character(len = 8) :: ikey
    character(len = 8), dimension(12) :: clef
    integer :: dummy, pos
    save dummy
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
        CALL QLXLOOK(LOCVAR, IKEY, LOCCNT, LIMITS, ITYP)
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
        LOCVAR = LOC(QLXPRNT)
        LOCCNT = LOC(DUMMY)
        LIMITS = 202
    case(11)
        ITYP = 2
        LOCVAR = LOC(QLXNVAR)
        LOCCNT = LOC(DUMMY)
        LIMITS = 202
    case(12)
        ITYP = 2
        LOCVAR = LOC(QLXUNDF)
        LOCCNT = LOC(DUMMY)
        LIMITS = 101
    end select
END


SUBROUTINE QLXIND(IND, ERR)
    use app

    INTEGER IND
    LOGICAL ERR
    COMMON /QLXTOK1/ LEN, TYPE, ZVAL, INEXPR
    LOGICAL INEXPR
    INTEGER LEN, TYPE, JVAL
    REAL ZVAL
    pointer(pjval,JVAL)

    COMMON/QLXTOK2/TOKEN
    character(len=80) TOKEN

    EXTERNAL QLXSKP
    character(len=1) QLXSKP
    character(len=1) IC

    pjval = LOC(ZVAL)
    IND=1
    IC=QLXSKP(' ')

    IF (IC == '[') THEN
        CALL QLXTOK
        IF (((TYPE == 1) .OR.(TYPE == 0)) .AND. JVAL > 0) THEN
            IND=JVAL
        ELSE
            CALL QLXERR(21009, 'QLXIND')
            ERR = .TRUE.
        ENDIF
        IF (.NOT.ERR) THEN
            CALL QLXTOK
            IF (TOKEN(1:1).NE.']' .OR. TYPE.NE.4) THEN
                CALL QLXERR(21010, 'QLXIND')
                ERR = .TRUE.
            ENDIF
        ENDIF
    ELSE
        CALL QLXBAK(IC)
    ENDIF
END

!> Déclaration des routines
SUBROUTINE QLXINX(xtern, key, icount, limits, ityp)
    use app

    !> Nom de la fonction à appeler
    EXTERNAL xtern

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
        CALL lib_log(APP_LIBRMN, APP_ERROR, 'QLXINX ne peut etre utilise pour ityp <> 2')
        CALL QLXERR(81013, 'QLXINS')
        STOP
    ENDIF
    CALL qqlxins(idum, key, icount, limits, ityp, xtern)
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

    integer, EXTERNAL :: READLX

    IF (ityp == 2) THEN
        CALL lib_log(APP_LIBRMN, APP_ERROR, 'QLXINX doit etre utilise quand ityp = 2, au lieu de QLXINS')
        CALL QLXERR(81013, 'QLXINS')
        STOP
    ELSE
        CALL qqlxins(ivar, key, icount, limits, ityp, READLX)
    ENDIF
END

!> DECLARATION DES CLES ET DE LEUR TYPE
SUBROUTINE qqlxins(ivar, key, icount, limits, ityp, xtern)
    use rmn_common
    use readlx_nrdlx

    INTEGER, INTENT(IN) :: ivar
    CHARACTER(len = *) :: key
    INTEGER, INTENT(OUT) :: icount
    INTEGER, INTENT(IN) :: limits
    INTEGER, INTENT(IN) :: ityp

    integer, EXTERNAL :: xtern

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
        CALL QLXERR(10011, 'QLXINS')
    ENDIF

    IF (limits < 0 .OR. limits > 99999) THEN
        CALL QLXERR(20012, 'QLXINS')
        RETURN
    ENDIF

    IF (ityp < 0 .OR. ityp > 13)THEN
        CALL QLXERR(20013, 'QLXINS')
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
END SUBROUTINE qqlxins


SUBROUTINE QLXLOOK(IVAR, KEY, ICOUNT, LIMITS, ITYP)
    use rmn_common
    use readlx_nrdlx
    integer(kind = int64) :: ivar, icount
    INTEGER               :: ITYP, LIMITS
    character(len=*)      :: KEY

    INTEGER               :: IPNT
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


! subroutine QLXUDF(IVAR, KEY)
subroutine QLXUDF2(KEY)
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

subroutine QLXDTB
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
INTEGER FUNCTION QLXNUM(IB, LENG)
    character(len=*), intent(INOUT) :: IB
    INTEGER, intent(INOUT) :: LENG

    !ARGUMENT
    !        QLXNUM    RETOURNE   2   reel
    !        (S)                  1   entier
    !                             6   entier octal
    !                             5   ERREUR
    !
    !        IB(*)     IB(1) EST LE PREMIER CHIFFRE DU NOMBRE.
    !        (E)       LA TABLE IB CONTIENT LE NOMBRE.
    !
    !        LENG      NOMBRE DE CARACTERES DANS LE NOMBRE(ENTIER OU REEL)
    !        (S)

    INTEGER ILX
    EXTERNAL QLXCHR
    character(len=1) I, CTMP, QLXCHR

    IF (IB(1:1) == '.') THEN
        ILX = 1                                ! real number
    ELSE
        ILX = 0                                ! potentially an integer
    ENDIF
    I = QLXCHR()

    DO WHILE (I >= '0' .AND. I <= '9' )        ! collect a digit stream
        LENG = MIN(21, LENG+1)
        IB(LENG:LENG) = I
        I = QLXCHR()
    END DO
    IF (I == '.' .AND. IB(1:1).NE.'.') THEN    ! decimal period, not in column 1
        ILX = 1
        LENG = MIN(21, LENG+1)
        IB(LENG:LENG) = I
        I = QLXCHR()
        DO WHILE (I >= '0' .AND. I <= '9')     ! digit stream after the period
            LENG = MIN(21, LENG+1)
            IB(LENG:LENG) = I
            I = QLXCHR()
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
         I = QLXCHR()
         IF ( (I >= '0' .AND. I <= '9') .OR. (I == '+') .OR. (I == '-') ) THEN
6           LENG = MIN(21, LENG + 1)
            IB(LENG:LENG) = I
            I = QLXCHR()
            IF (I >= '0' .AND. I <= '9') THEN  ! more digits
               GOTO 6
            ENDIF
         ENDIF
      ENDIF

      IF (LENG >= 21) THEN
         QLXNUM=5                              ! bad number
      ELSE
         IF (ILX == 0) THEN
            IF (I.NE.'B') THEN
               QLXNUM=1                        ! integer
            ELSE
               QLXNUM=6                        ! octal
               I=QLXCHR()
               DO J = LENG, 1, -1
                  IF (IB(J:J) > '7') THEN
                     QLXNUM=5                  ! bad number
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
                  QLXNUM=2                     ! real number
               ELSE
                  IF (IB(LENG:LENG) >= '0' .AND. IB(LENG:LENG) <= '9') THEN
                     QLXNUM=2                  ! real number
                  ELSE
                     QLXNUM=5                  ! bad number
                  ENDIF
               ENDIF
            ELSE
               QLXNUM=5
            ENDIF
         ENDIF
      ENDIF
      CALL QLXBAK(I)
END


SUBROUTINE QLXNVAR(KEY, NW)
    use rmn_common
    use readlx_qlxfmt

    INTEGER NW
    INTEGER KEY(*)
    EXTERNAL ARGDIMS
    INTEGER  ARGDIMS
    INTEGER SC(1024), NSC
    SAVE SC, NSC
    INTEGER DUMMY
    character(len=8) IKEY
    integer(kind = int64) IVAR, ICOUNT

    SAVE DUMMY
    DATA NSC /1/
    DATA DUMMY /0/

    WRITE(IKEY, LINEFMT) (KEY(J), J=1, ARGDIMS(1))
    CALL QLXLOOK(IVAR, IKEY, ICOUNT, LIMITS, ITYP)
    IF (ITYP /= -1) THEN
        RETURN
    ENDIF
    IF (NSC + NW > 1024 + 1) THEN
        CALL QLXERR(21011, 'QLXNVAR')
        RETURN
    ENDIF
    CALL qlxins(SC(NSC), IKEY, DUMMY, NW, 1)
    NSC = NSC + NW
END

!> Appliquer un operateur numerique ou logique
SUBROUTINE QLXOPR(TOKENS, NTOKEN, TOKTYPE, OPRTR, ERR)
      use rmn_common
      INTEGER NTOKEN, OPRTR, TOKENS(NTOKEN), TOKTYPE(NTOKEN)
      LOGICAL ERR
      integer (kind = int64) :: token64

      INTEGER IZ1, IZ2, IR1
      REAL   Z1,  Z2,  R1
      pointer(pz1,Z1)
      pointer(pz2,Z2)
      pointer(pr1,R1)
      LOGICAL REALOP
      integer :: TOK
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
         token64 = TOKENS(NTOKEN)
         call get_content_of_address(token64, 1, TOKENS(NTOKEN))
         TOKTYPE(NTOKEN) = 0
      ENDIF
      IF (OPRTR.NE.2 .AND. OPRTR.NE.17   .AND. OPRTR.NE.21 .AND. OPRTR.NE.4) THEN
         IF (TOKTYPE(NTOKEN-1) > 0) THEN
            token64 = TOKENS(NTOKEN-1)
            call get_content_of_address(token64, 1, TOKENS(NTOKEN-1))
            TOKTYPE(NTOKEN-1) = 0
         ENDIF
      ENDIF
      REALOP = ABS(TOKENS(NTOKEN)) > 2147483647
      IZ1 = TOKENS(NTOKEN)
      IF (OPRTR.NE.2 .AND. OPRTR.NE.17 .AND. OPRTR.NE.4) THEN
         REALOP = REALOP .OR. ABS(TOKENS(NTOKEN-1)) > 2147483647
         IZ2 = TOKENS(NTOKEN-1)
         IF (REALOP) THEN
            IF (ABS(IZ1) <= 2147483647) THEN
               Z1 = TOKENS(NTOKEN)
            ENDIF
            IF (ABS(IZ2) <= 2147483647) THEN
               Z2 = TOKENS(NTOKEN-1)
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
         PTOK = LOC(TOKENS(NTOKEN-1))
         TOKENS(NTOKEN-1) = TOK(TOKENS(NTOKEN))
         NTOKEN = NTOKEN - 1
         TOKTYPE(NTOKEN) = 1
         RETURN
      case(3)
         RETURN
      case(4)
         IF (REALOP) THEN
            R1 = -Z1
         ELSE
            IR1 = -IZ1
         ENDIF
      case(5)
         IF (REALOP) THEN
            R1 = Z2**Z1
         ELSE
            IR1 = IZ2**IZ1
         ENDIF
      case(6)
         IF (REALOP) THEN
            R1 = Z2*Z1
         ELSE
            IR1 = IZ2*IZ1
         ENDIF
      case(7)
         IF (REALOP) THEN
            R1 = Z2/Z1
         ELSE
            IR1 = IZ2/IZ1
         ENDIF
      case(8)
         IF (REALOP) THEN
            R1 = Z2+Z1
         ELSE
            IR1 = IZ2+IZ1
         ENDIF
      case(9)
         IF (REALOP) THEN
            R1 = Z2-Z1
         ELSE
            IR1 = IZ2-IZ1
         ENDIF
      case(10)
         IF (REALOP) THEN
            IF (Z2 < Z1) THEN
               IR1 =ishft(-1, 32-(32))
            ENDIF
         ELSE
            IF (IZ2 < IZ1) THEN
               IR1 =ishft(-1, 32-(32))
            ENDIF
         ENDIF
      case(11)
         IF (REALOP) THEN
            IF (Z2 > Z1) THEN
               IR1 =ishft(-1, 32-(32))
            ENDIF
         ELSE
            IF (IZ2 > IZ1) THEN
               IR1 =ishft(-1, 32-(32))
            ENDIF
         ENDIF
      case(12)
         IF (REALOP) THEN
            IF (Z2 == Z1) THEN
               IR1 =ishft(-1, 32-(32))
            ENDIF
         ELSE
            IF (IZ2 == IZ1) THEN
               IR1 =ishft(-1, 32-(32))
            ENDIF
         ENDIF
      case(13)
         IF (REALOP) THEN
            IF (Z2 <= Z1) THEN
               IR1 =ishft(-1, 32-(32))
            ENDIF
         ELSE
            IF (IZ2 <= IZ1) THEN
               IR1 =ishft(-1, 32-(32))
            ENDIF
         ENDIF
      case(14)
         IF (REALOP) THEN
            IF (Z2 >= Z1) THEN
               IR1 =ishft(-1, 32-(32))
            ENDIF
         ELSE
            IF (IZ2 >= IZ1) THEN
               IR1 =ishft(-1, 32-(32))
            ENDIF
         ENDIF
      case(15)
         IF (REALOP) THEN
            IF (Z2.NE.Z1) THEN
               IR1 =ishft(-1, 32-(32))
            ENDIF
         ELSE
            IF (IZ2.NE.IZ1) THEN
               IR1 =ishft(-1, 32-(32))
            ENDIF
         ENDIF
      case(16)
         IF (REALOP) THEN
            IF (Z2.NE.Z1) THEN
               IR1 =ishft(-1, 32-(32))
            ENDIF
         ELSE
            IF (IZ2.NE.IZ1) THEN
               IR1 =ishft(-1, 32-(32))
            ENDIF
         ENDIF
      case(17)
         IF (REALOP) THEN
            ERR = .TRUE.
         ELSE
            IR1 =NOT(IZ1)
         ENDIF
      case(18)
         IF (REALOP) THEN
            ERR = .TRUE.
         ELSE
            IR1 = IAND(IZ2, IZ1)
         ENDIF
      case(19)
         IF (REALOP) THEN
            ERR = .TRUE.
         ELSE
            IR1 = IOR(IZ2, IZ1)
         ENDIF
      case(20)
         IF (REALOP) THEN
            ERR = .TRUE.
         ELSE
            IR1 = IEOR(IZ2, IZ1)
         ENDIF
      case(21)
         IF (TOKTYPE(NTOKEN-1) <= 0) THEN
            ERR = .TRUE.
            RETURN
         ENDIF
         token64 = TOKENS(NTOKEN-1)
         call set_content_of_address(token64, 1, TOKENS(NTOKEN))
         NTOKEN = NTOKEN - 1
         RETURN
      end select
      NTOKEN = NTOKEN + 1 - MINOPER
      TOKENS(NTOKEN) = IR1
      TOKTYPE(NTOKEN) = 0
END


!> Passage d'options a readlx
SUBROUTINE QLXOPT(OPTION, VAL)
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

!  FONCTION  QLXPRI_L EVALUER LA PRIORITE D'UN OPERATEUR
INTEGER FUNCTION QLXPRI_L(OPR, LEFTPRI)
    character(len=*) OPR
    LOGICAL LEFTPRI
!     INTEGER QLXPRIL
    PARAMETER (MAXOPER=23)
    INTEGER PRI(MAXOPER)
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
                QLXPRI_L = I + PRI(I)*100
            ELSE
                QLXPRI_L = I + (PRI(I)-MOD(PRI(I), 2))*100
            ENDIF
            RETURN
        ENDIF
    END DO
    QLXPRI_L = 0
END

!> Evaluer la priorite d'un operateur (right priority)
integer function qlxpri(opr)
    implicit none
    character(len=*) opr
    integer, external :: qlxpri_l
    qlxpri = qlxpri_l(opr, .FALSE.)
end

!> Evaluer la priorite d'un operateur (left priority)
integer function qlxpril(opr)
    implicit none
    character(len=*) opr
    integer, external :: qlxpri_l
    qlxpril = qlxpri_l(opr, .TRUE.)
end

SUBROUTINE QLXPRNT(QUOI, COMMENT)
    use readlx_qlxfmt

    INTEGER QUOI(*), COMMENT(*)
    character(len=120) FMT
    INTEGER ARGDIMS
    L1 = ARGDIMS(1)
    L2 = MIN(120/KARMOT, ARGDIMS(2))
    IF (L1 < 1 .OR. L2 < 1) THEN
        RETURN
    ENDIF
    WRITE(FMT, LINEFMT)(COMMENT(I), I=1, L2)
    WRITE(6, FMT)(QUOI(I), I=1, L1)
END

!> Conversion a notation postfixe
SUBROUTINE QLXRPN(TOK, TOKENS, MAXTKNS, NTOKEN, TOKTYPE, PILEOP, MAXOPS, NOPER, ERR)
    character(len=*) TOK
    INTEGER MAXTKNS, NTOKEN, MAXOPS, NOPER
    INTEGER TOKENS(MAXTKNS), TOKTYPE(MAXTKNS)
    EXTERNAL QLXPRI, QLXPRIL
    INTEGER  QLXPRI, QLXPRIL
    LOGICAL ERR
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
                CALL QLXOPR(TOKENS, NTOKEN, TOKTYPE, MOD(QLXPRI(PILEOP(NOPER)), 100), ERR)
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
                    CALL QLXOPR(TOKENS, NTOKEN, TOKTYPE, MOD(QLXPRI(PILEOP(NOPER)), 100), ERR)
                    NOPER = NOPER - 1
                ENDDO
                IF (PILEOP(NOPER) == '[') THEN
                    CALL QLXOPR(TOKENS, NTOKEN, TOKTYPE, MOD(QLXPRI(']'), 100), ERR)
                    NOPER = NOPER-1
                ELSE
                    ERR = .TRUE.
                ENDIF
            ELSE
                IF (TOKEN == '$') THEN
                    DO WHILE (PILEOP(NOPER) .NE.'(' .AND. PILEOP(NOPER) .NE.'[' .AND. PILEOP(NOPER) .NE.'$')
                        CALL QLXOPR(TOKENS, NTOKEN, TOKTYPE, MOD(QLXPRI(PILEOP(NOPER)), 100), ERR)
                        NOPER = NOPER - 1
                    ENDDO
                    IF (PILEOP(NOPER) == '$') THEN
                        NOPER = NOPER-1
                    ELSE
                        ERR = .TRUE.
                    ENDIF
                ELSE
                    DO WHILE (QLXPRIL(PILEOP(NOPER)) > QLXPRI(TOKEN))
                        CALL QLXOPR(TOKENS, NTOKEN, TOKTYPE, MOD(QLXPRI(PILEOP(NOPER)), 100), ERR)
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
FUNCTION QLXSKP(ICAR)
    character(len=1) QLXSKP
    !> Caratère à ignorer
    character(len=1), intent(in) :: ICAR

    EXTERNAL QLXCHR
    character(len=1) :: CTMP, QLXCHR

    CTMP = QLXCHR()
    do while (CTMP == ICAR)
        CTMP = QLXCHR()
    end do
    QLXSKP = CTMP
END


!> Decomposer une ligne de texte en tokens de differents types, identifie la longueur du token et son type.
SUBROUTINE QLXTOK
    use rmn_common
    use readlx_qlxfmt

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
    COMMON /QLXTOK1/ LEN, TYPE, ZVAL, INEXPR
    LOGICAL INEXPR
    INTEGER LEN, TYPE, JVAL
    REAL ZVAL
!     EQUIVALENCE (ZVAL, JVAL)
    pointer(pjval,JVAL)

    COMMON /QLXTOK2/ TOKEN
    character(len=80) TOKEN

    integer(kind = int64) :: LOCVAR, LOCCNT
    EXTERNAL QLXCHR, QLXNUM
    character(len=1) IC, QLXCHR
    INTEGER  QLXNUM

    pjval = LOC(ZVAL)
    IVAL = -1
    JSIGN = 0
    TOKEN = ' '

    IC = QLXCHR()
    DO WHILE (.NOT.(IC .NE. ' '))
        IC = QLXCHR()
    END DO

    LENG = 1
    TOKEN(1:1) = IC
    IF ( (IC >= 'A'.AND.IC <= 'Z') .OR. IC == '@' .OR. IC == '_' .OR. (IC >=  'a' .AND. IC <=  'z') ) THEN
        IC = QLXCHR()
        DO WHILE ( (IC >= 'A' .AND.IC  <= 'Z').OR. (IC >= '0' .AND. IC <= '9') .OR. (IC >=  'a' .AND. IC <=  'z') )
            LENG = MIN(81, LENG+1)
            TOKEN(LENG:LENG) = IC
            IC = QLXCHR()
        ENDDO
        IF (LENG > 8) THEN
            TYPE = 3
        ELSE
            TYPE = 0
        ENDIF
        CALL QLXBAK(IC)
    ELSE
        IF (IC == '''' .OR. IC == '"') THEN
            LENG = 0
            LENG = MIN(80, LENG + 1)
            TOKEN(LENG:LENG) = QLXCHR()
            DO WHILE (.NOT.(TOKEN(LENG:LENG) ==  IC))
                LENG = MIN(80, LENG + 1)
                TOKEN(LENG:LENG) = QLXCHR()
            END DO
            TOKEN(LENG:LENG) = ' '
            LENG = LENG -1
            IF (IC  == '"') THEN
                LENG = MIN(LENG, KARMOT)
            ENDIF
            TYPE = 3
        ELSE
            IF ( (IC >= '0' .AND. IC <= '9') .OR. (IC == '.') ) THEN
                TYPE = QLXNUM(TOKEN, LENG)
                JSIGN = 1
            ELSE
                IF ( (IC == '+' .OR. IC == '-') .AND. (.NOT.INEXPR) ) THEN
                    IF (IC == '+') THEN
                        JSIGN = 1
                    ELSE
                        JSIGN = -1
                    ENDIF
                    IC = QLXCHR()
                    IF ((IC >= '0' .AND. IC <= '9').OR. IC == '.') THEN
                        TOKEN(1:1)=IC
                        TYPE = QLXNUM(TOKEN, LENG)
                    ELSE
                        CALL QLXBAK(IC)
                        TYPE = 4
                    ENDIF
                ELSE
                    IF (IC == '*') THEN
                        TYPE = 4
                        IC = QLXCHR()
                        IF (IC == '*') THEN
                            LENG = 2
                            TOKEN = '**'
                        ELSE
                            CALL QLXBAK(IC)
                        ENDIF
                    ELSE
                        IF (IC == '<' .OR. IC == '>' .OR. IC == '=' .OR. IC == ':') THEN
                            TYPE = 4
                            IC = QLXCHR()
                            IF (IC == '<' .OR. IC == '>' .OR. IC == '=') THEN
                                LENG = 2
                                TOKEN(2:2) = IC
                            ELSE
                                CALL QLXBAK(IC)
                            ENDIF
                        ELSE
                            TYPE = 4
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
    ENDIF

    IF ( (LENG > 80) .OR. (TYPE == 5) ) THEN
        TOKEN = 'SCRAP'
        TYPE = 5
        CALL QLXERR(21014, 'QLXTOK')
    ENDIF
    IF (TYPE == 1) THEN
        READ(TOKEN, '(I20)')JVAL
        JVAL = SIGN(JVAL, JSIGN)
    ELSE
        IF (TYPE == 2) THEN
            READ(TOKEN, '(G20.3)')ZVAL
            ZVAL = SIGN(ZVAL, FLOAT(JSIGN))
        ELSE
            IF (TYPE == 6) THEN
                READ(TOKEN, '(O20)')JVAL
                TYPE = 1
                JVAL = SIGN(JVAL, JSIGN)
            ENDIF
        ENDIF
    ENDIF
    IF (TYPE == 0) THEN
        CALL QLXFND(TOKEN(1:8), LOCVAR, LOCCNT, LIMITS, ITYP)
        IF (ITYP  ==  -1) THEN
            TYPE = 3
            LENG = MIN(LENG, KARMOT)
        ELSE
            IF ( (ITYP  ==  0) .OR. (ITYP  ==  1) ) THEN
                call get_content_of_address(LOCVAR, 1, JVAL)
            ELSE
                JVAL = -1
            ENDIF
        ENDIF
    ENDIF
    LEN = LENG
END


SUBROUTINE QLXUNDF(IKEY)
    use rmn_common
    use readlx_qlxfmt

    INTEGER IKEY(*)
    character(len=8) CKEY
    INTEGER ARGDIMS
!     integer(kind = int64) :: SCRAP

    WRITE(CKEY, 101) (IKEY(I), I=1, ARGDIMS(1))
101   FORMAT(2 A4)
    CALL QLXUDF2(CKEY)
END


FUNCTION QLXVAL(KLE, ERR)
    INTEGER QLXVAL

    character(len=*) KLE
    LOGICAL ERR
!     INTEGER IND, VAL, DUM
    INTEGER IND, VAL

    CALL QLXIND(IND, ERR)

    VAL = 0
    IF (.NOT. ERR) THEN
        CALL QLXADI2(KLE, IND, VAL, ERR)
    ENDIF
    QLXVAL = VAL
END


!> Traiter une expression arithmetique ou logique
SUBROUTINE QLXXPR(ERR)
    use app
    use rmn_common

    LOGICAL ERR
    COMMON /QLXTOK1/ LEN, TYPE, ZVAL, INEXPR
    LOGICAL INEXPR
    INTEGER LEN, TYPE, JVAL
    REAL ZVAL
    pointer(pjval,JVAL)

    COMMON /QLXTOK2/ TOKEN
    character(len=80) TOKEN

    PARAMETER (MAXTKNS=65, MAXOPS=30)
    INTEGER TOKENS(MAXTKNS), TOKTYPE(MAXTKNS), NTOKEN
    INTEGER NOPER
    integer(kind = int64) :: LOCVAR, LOCCNT
    character(len=4) :: PILEOP(MAXOPS)
    LOGICAL UNARY, FINI, FIRST
    INTEGER PLEV, QLXPRI
    EXTERNAL QLXPRI

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
            CALL QLXTOK
        ENDIF
        FIRST = .FALSE.
        IF (TYPE == 0) THEN
            NTOKEN = NTOKEN + 1
            CALL QLXFND(TOKEN(1:8), LOCVAR, LOCCNT, LIMITES, ITYP)
            IF (ITYP.NE.0 .AND. ITYP.NE.1) THEN
                ERR = .TRUE.
            ENDIF
!             TOKENS(NTOKEN) = LOCVAR
            TOKENS(NTOKEN) = 0              ! FATAL ERROR, no way to store an address in 32 bits
            call lib_log(APP_LIBRMN, APP_ERROR, 'impossible de stocker 64 bits dans 32 bits')
            ERR = .TRUE.
            CALL QLXERR(81023, 'QLXEXPR')
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
                IF (QLXPRI(TOKEN(1:4)) > 0) THEN
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
                        CALL QLXBAK(TOKEN(1:1))
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
                    CALL QLXRPN(TOKEN, TOKENS, MAXTKNS, NTOKEN, TOKTYPE, PILEOP, MAXOPS, NOPER, ERR)
                ELSE
                    IF (TOKEN(1:1) == ',' .OR. TOKEN(1:1) == '$' .OR. TOKEN(1:2) == ':=') THEN
                        CALL QLXRPN('$', TOKENS, MAXTKNS, NTOKEN, TOKTYPE, PILEOP, MAXOPS, NOPER, ERR)
                        FINI = .TRUE.
                        CALL QLXBAK(TOKEN(1:1))
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
        JVAL = TOKENS(1)
        IF (TOKTYPE(1) > 0) THEN
            TYPE =8
        ELSE
            IF (ABS(JVAL) <= 2147483647) THEN
                TYPE =1
            ELSE
                TYPE =2
            ENDIF
        ENDIF
    ENDIF
    IF (ERR) THEN
        CALL QLXERR(81005, 'QLXEXPR')
    ENDIF
END

!> Interprete de directives
SUBROUTINE READLX(UNIT, KEND, KERR)
    use app
    use rmn_common
    use readlx_qlxbuff
    use readlx_qlxfmt

    !> Numéro d'unité d'entrée
    INTEGER, INTENT(in) :: UNIT
    !> 0 : Pas de problème
    INTEGER, INTENT(out) :: KEND

    INTEGER, INTENT(inout) :: KERR

    COMMON /QLXTOK1/ LEN, TYPE, ZVAL, INEXPR
    LOGICAL INEXPR
    INTEGER LEN, TYPE, JVAL
    REAL ZVAL
    pointer(pjval,JVAL)

    COMMON/QLXTOK2/TOKEN
    character(len=80) TOKEN

    EXTERNAL QLXNVAR, QLXPRNT, QLXUNDF

#include <rmn/fnom.hf>
    integer(kind = int64) :: LOCCNT, LOCVAR
    Integer IICNT
    INTEGER LIMITS, ITYP
    LOGICAL FIN, ERR
    PARAMETER (MAXSTRU=20)
    INTEGER NXTELSE(0:2), NEXTIF(0:2), STYPE(MAXSTRU), SKIPF(MAXSTRU)
    INTEGER READBSE(MAXSTRU)
    INTEGER NSTRUC, ier
    character(len=128) nomscra
    integer(kind = int64) :: jval64

    DATA NXTELSE / 1, 0, 2/
    DATA NEXTIF  / 0, 2, 2/

    pjval = LOC(ZVAL)
    WRITE(LINEFMT, '(A,I2,A)') '(25 A', KARMOT, ')'
    KERRMAX = 999999

    IF (KERR < 0 ) THEN
        KERRMAX = MIN(ABS(KERR), KERRMAX)
    ENDIF
print *,"==========================READLX NEW=========================="
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
    CALL QLXINX(QLXPRNT, 'PRINT', IDUM, 0202, 2)
    CALL QLXINX(QLXNVAR, 'DEFINE', IDUM, 0202, 2)
    CALL QLXINX(QLXUNDF, 'UNDEF', IDUM, 0101, 2)

    DO WHILE (.NOT.FIN .AND. NERR < KERRMAX .AND. NSTRUC < MAXSTRU)
        SKIPFLG = SKIPF(NSTRUC)
        ERR = .FALSE.
        CALL QLXTOK
        IF (TYPE == 0) THEN
            CALL QLXFND(TOKEN, LOCVAR, LOCCNT, LIMITS, ITYP)
            IF (ITYP == 1 .AND. SKIPF(NSTRUC) == 0) THEN
                call get_content_of_address(LOCCNT, 1, IICNT)
                CALL QLXASG(LOCVAR, IICNT, LIMITS, ERR)
                call set_content_of_address(LOCCNT, 1, IICNT)
            ELSE
                IF (ITYP == 2 .AND. SKIPF(NSTRUC) == 0) THEN
                    CALL QLXCALL(LOCVAR, LOCCNT, LIMITS, ERR)
                ELSE
                    IF (ITYP == 3) THEN
                        NSTRUC = NSTRUC + 1
                        STYPE(NSTRUC) = ITYP
                        SKIPF(NSTRUC) = NEXTIF(SKIPF(NSTRUC-1))
                        IF (SKIPF(NSTRUC) == 0) THEN
                            CALL QLXTOK
                            IF (TOKEN(1:1).NE.'$') THEN
                                CALL QLXXPR(ERR)
                                IF (ERR) THEN
                                    EXIT
                                ENDIF
                                IF (TYPE == 8) THEN
                                    jval64 = JVAL
                                    call get_content_of_address(jval64, 1, JVAL)
                                ENDIF
                                IF (IAND(JVAL, ishft(-1, 32-(16))) == 0) THEN
                                    SKIPF(NSTRUC) = 1
                                ENDIF
                            ELSE
                                CALL QLXBAK('$')
                            ENDIF
                        ENDIF
                        CALL QLXFLSH('$')
                    ELSE
                        IF (ITYP == 4) THEN
                            IF (STYPE(NSTRUC).NE.3) THEN
                                EXIT
                            ENDIF
                            STYPE(NSTRUC) = ITYP
                            SKIPF(NSTRUC) = NXTELSE(SKIPF(NSTRUC))
                            CALL QLXFLSH('$')
                        ELSE
                            IF (ITYP == 5) THEN
                                IF (STYPE(NSTRUC).NE.3 .AND. STYPE(NSTRUC).NE.4) THEN
                                    EXIT
                                ENDIF
                                SKIPF(NSTRUC) = 0
                                NSTRUC = NSTRUC - 1
                                CALL QLXFLSH('$')
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
                                        CALL QLXTOK
                                        IF (TOKEN(1:1).NE.'$') THEN
                                            CALL QLXXPR(ERR)
                                            IF (ERR) THEN
                                                EXIT
                                            ENDIF
                                            IF (TYPE == 8) THEN
                                                jval64 = JVAL
                                                call get_content_of_address(jval64, 1, JVAL)
                                            ENDIF
                                            IF (IAND(JVAL, ishft(-1, 32-(16))) == 0) THEN
                                                SKIPF(NSTRUC) = 1
                                            ENDIF
                                        ELSE
                                            CALL QLXBAK('$')
                                        ENDIF
                                    ENDIF
                                    CALL QLXFLSH('$')
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
                                        CALL QLXFLSH('$')
                                    ELSE
                                        IF (ITYP >= 10 .AND. ITYP <= 13 .AND. SKIPF(NSTRUC) == 0) THEN
                                            KERR = NERR
                                            KEND = ITYP-10
                                            FIN = .TRUE.
                                        ELSE
                                            IF (SKIPF(NSTRUC).NE.0) THEN
                                                CALL QLXFLSH('$')
                                            ELSE
                                                CALL QLXERR(21015, 'READLX')
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
            CALL QLXERR(21016, 'READLX')
            ERR = .TRUE.
        ENDIF
        IF (ERR.AND.(TOKEN(1:1).NE.'$'.OR. TYPE.NE.4)) THEN
            CALL QLXFLSH('$')
        ENDIF
    ENDDO

    IF (NSTRUC > 1) THEN
        call lib_log(APP_LIBRMN, APP_ERROR, 'readlx: Error within if then else bloc structure')
        KERR = NERR + 1
        KEND = -1
    ENDIF

    CLOSE(TMPFILE, STATUS='DELETE')
END

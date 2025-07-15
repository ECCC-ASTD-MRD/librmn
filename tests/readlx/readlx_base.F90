
module yyy
    use app
    use rmn_common
    implicit none

    integer, save :: NTAB0,NTAB1,NTAB2,NTAB3,NSUB1,NSUB2,NSUB3,NNN

contains

    subroutine print_x(num, vals, index)
        implicit none
        integer, intent(in) :: num
        integer, dimension(num), target, intent(in) :: vals
        integer, intent(in) :: index

        integer, external :: argdope
        integer, dimension(5) :: liste
        character(len=50) :: fmt
        integer :: ND

        write(fmt, '(A, I1, A)') '(3X,Z10.8,I10,Z20.16,', num, 'Z10.8)'
        write(app_msg, fmt) vals(1), vals(1), transfer(c_loc(vals), 0_int64), vals(:)
        call App_Log(APP_INFO, app_msg)

        ND = argdope(index, liste, 5)
        write(fmt, '(A, I1, A)') '(1X," ND=",I2," LISTE=",', ND, 'Z12.8)'
        write(app_msg, fmt) ND, liste(1:ND)
        call App_Log(APP_INFO, app_msg)

    end subroutine print_x

    SUBROUTINE SUB1(A, B, C, D)
        implicit none
        INTEGER, target :: A(*), B(*), C(*), D(*)
        INTEGER, external :: argdims

        call App_Log(APP_INFO, ' PASSE PAR SUB1')
        write(app_msg, '(A, I5)') ' NB D ARGUMENTS =',NSUB1
        call App_Log(APP_INFO, app_msg)
        if (NSUB1 >= 1) call print_x(argdims(1), A, 1)
        if (NSUB1 >= 2) call print_x(argdims(2), B, 2)
        if (NSUB1 >= 3) call print_x(argdims(3), C, 3)
        if (NSUB1 >= 4) call print_x(argdims(4), D, 4)
        call App_Log(APP_INFO, ' ==========================')

    END SUBROUTINE SUB1

    SUBROUTINE SUB2(A,B,C,D,E)
        implicit none
        INTEGER, target :: A,B,C,D,E
        integer(C_INT64_T) :: dummy_addr

        call App_Log(APP_INFO, ' PASSE PAR SUB2')
        write(app_msg, '(A, I10)') ' NB D ARGUMENTS =',NSUB2
        call App_Log(APP_INFO, app_msg)
        if (NSUB2 == 1) then
            goto 1
        else if (NSUB2 == 2) then
            goto 2
        else if (NSUB2 == 3) then
            goto 3
        else if (NSUB2 == 4) then
            goto 4
        else if (NSUB2 == 5) then
            goto 5
        else
            write(app_msg, '(A, I10)') 'SUB2: Invalid number of arguments: ', NSUB1
            call app_log(APP_ERROR, app_msg)
            error stop 1
        end if
    5   write(app_msg, 9) E, transfer(c_LOC(E), dummy_addr)
        call App_Log(APP_INFO, app_msg)
    4   write(app_msg, 9) D, transfer(c_LOC(D), dummy_addr)
        call App_Log(APP_INFO, app_msg)
    3   write(app_msg, 9) C, transfer(c_LOC(C), dummy_addr)
        call App_Log(APP_INFO, app_msg)
    2   write(app_msg, 9) B, transfer(c_LOC(B), dummy_addr)
        call App_Log(APP_INFO, app_msg)
    1   write(app_msg, 9) A, transfer(c_LOC(A), dummy_addr)
        call App_Log(APP_INFO, app_msg)

    9   format(I10,2x,Z16.16)
    END SUBROUTINE SUB2

    SUBROUTINE YOUPI
        use app
        implicit none
        call App_Log(APP_INFO, ' <><><> Y O U P I <><><>')
    END SUBROUTINE YOUPI
end module

PROGRAM YOYO
    use ISO_C_BINDING
    use yyy
    use app
    use rmn_libc, only: program_exit => exit
    implicit none
    integer, dimension(10) :: TAB0,TAB1,TAB2,TAB3
    integer :: dummy, INDICE

    external :: qlxins, qlxinx, readlx

    integer :: KND, KRR, i
    character(len=4096) :: input_file

    if(command_argument_count() < 1) then
        call App_Log(APP_ERROR, 'At least one command line argument is needed')
        call program_exit(1)
    endif
    call GET_COMMAND_ARGUMENT(1 , input_file)
    i = app_loglevel('INFO')

    CALL qlxins(TAB0(1), 'TAB0', NTAB0, 9, 1)      ! up to 9 values, writable
    CALL qlxins(TAB1(1), 'TAB1', NTAB1, 9, 1)      ! up to 9 values, writable
    CALL qlxins(INDICE,  'IND' , NNN,   1, 1)      ! up to 1 value, writable
    CALL qlxins(TAB2(1), 'TAB2', NTAB2, 4, 1)      ! up to 4 values, writable
    CALL qlxins(TAB3(1), 'TAB3', NTAB3, 7, 1)      ! up to 7 values, writable
    CALL qlxins(22,   'CONST22', dummy, 1, 0)      ! up to 1 value, constant
    CALL qlxins(55,   'CONST55', dummy, 1, 0)      ! up to 1 value, constant

    CALL qlxinx(SUB1,  'SUB1', NSUB1, 0104, 2)    ! 1 to 4 argments, callable
    CALL qlxinx(SUB2,  'SUB2', NSUB2, 0305, 2)    ! 3 to 5 argments, callable
    CALL qlxinx(YOUPI, 'SUB3', NSUB3, 0000, 2)    ! NO argument, callable
    !
    !   write(*,77) LOC(sub1),LOC(sub2)
    ! 77  format(' *** debug sub1 = ',z16.16,' sub2 = ',z16.16)
    !   PRINT *,' Avant readlx - input=inp_readlx'
    !   IER = FNOM(5,'INP_READLX','SEQ',0)
    open(5, file=trim(input_file), form='FORMATTED', status = 'OLD', err = 666)
    CALL readlx(5,KND,KRR)
    write(app_msg, '(A,I3,A,10A4)') 'NTAB1 =', NTAB1, ' TAB1 = ',(tab1(i), i=1,NTAB1)
    call App_Log(APP_INFO, app_msg)
    write(app_msg, '(A,I3,A,10A4)') 'NTAB2 =', NTAB2, ' TAB2 = ',(tab2(i), i=1,NTAB2)
    call App_Log(APP_INFO, app_msg)
    write(app_msg, '(A,I3,A,10A4)') 'NTAB3 =', NTAB3, ' TAB3 = ',(tab3(i), i=1,NTAB3)
    call App_Log(APP_INFO, app_msg)
    write(app_msg, '(A, I4, I4)') ' APRES readlx - KND,KRR ',KND,KRR
    call App_Log(APP_INFO, app_msg)

    stop

666 continue ! Open error
    call App_Log(APP_ERROR, 'Unable to open file')
    error stop 1

END PROGRAM

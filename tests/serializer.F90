! Copyright (C) 2022  Environnement et Changement climatique Canada
!
! This is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this software; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.
!
! Authors:
!     M. Valin,   Recherche en Prevision Numerique, 2020-2022
!     V. Magnoux, Recherche en Prevision Numerique, 2020-2022

! test extended macros
#define JAR_EXTENDED_MACROS 1
! include serializer macros
#include <rmn/serializer.hf>

module jar_test_types
    implicit none

    type :: machin1
        integer, dimension(3) :: ip
        integer               :: date
        character(len=4)      :: nom
        character(len=2)      :: typ
        character(len=2)      :: tmp
    end type machin1

    type :: machin2
        integer               :: l1
        integer               :: l2
        character(len=2)      :: unit
    end type machin2

    interface is_same_machin
        module procedure is_same_machin1
        module procedure is_same_machin2
    end interface

    contains

    function is_same_machin1(a, b) result(ok)
        implicit none
        type(machin1), intent(IN) :: a,b
        logical :: ok
        ok = all(a%ip == b%ip) .and. a%date == b%date .and. a%nom == b%nom .and. a%typ == b%typ .and. a%tmp == b%tmp
        if(ok) then
            print *,'SUCCESS 1'
        else
            print *,'ERROR 1'
        endif
    end function is_same_machin1

    function is_same_machin2(a, b) result(ok)
        implicit none
        type(machin2), intent(IN) :: a,b
        logical :: ok
        ok = a%l1 == b%l1 .and. a%l2 == b%l2 .and. a%unit == b%unit
        if(ok) then
            print *,'SUCCESS 2'
        else
            print *,'ERROR 2'
        endif
    end function is_same_machin2
end module jar_test_types

module jar_test_procedures
    implicit none

    contains

    subroutine test_pickling
        use jar_module
        use jar_test_types
        implicit none
        logical :: old_debug
        JAR_DECLARE(my_jar)
        integer :: ok, i
        ! integer(JAR_ELEMENT) :: num_elem
        type(machin1) :: a1, x1
        type(machin2), dimension(4) :: a2, x2
        integer(JAR_ELEMENT), dimension(:), pointer :: blind_array
        ! type(C_PTR) :: c_blind_array
        logical :: success

        character(len=16) :: char_in, char_out

        ! print *,'==========================================================='

        ! num_elem = JAR_ELEM_COUNT(a1)
        ! print *,'each machin1 scalar item will use',num_elem,' cells in jar'
        ! num_elem = JAR_ELEM_COUNT(a2)
        ! print *,'a machin2 array / array section will use',num_elem,' cells per array element in jar'

        old_debug = my_jar%debug(.true.)
        if (JAR_VALID(my_jar)) then
            print *,'ERROR:   jar is valid before creation'
            error stop 1
        end if

        success = JAR_CREATE(my_jar, 4096)
        if (.not. JAR_VALID(my_jar)) then
            print *, 'ERROR while creating jar'
            error stop 1
        end if

        a1 = machin1([0,1,2], -123456, 'xxxx', 'yy', 'zz')
        success = JAR_PUT_ITEM(my_jar, a1)
        if (.not. success) then
            print *, 'ERROR: Unable to put object in jar'
            error stop 1
        end if

        do i = 1, size(a2)
            a2(i) = machin2(500+i, 600+i, 'X_')
        enddo
        success = JAR_PUT_ITEMS(my_jar, a2(2:3))
        if (.not. success) then
            print *, 'ERROR: Unable to put object 2 in jar'
            error stop 1
        end if

        JAR_DATA(my_jar, blind_array)   ! Retrieve fortran pointer to jar data
        ! print 2,'before get        ',blind_array(my_jar%low()+1:my_jar%high()),-1
        x1 = machin1([-1,-1,-1], 999999, '    ', '  ', '  ')
        success = JAR_GET_ITEM(my_jar, x1)
        ! print *,'       ',a1
        ! print *,'x1    =',x1
        success = is_same_machin(a1,x1) .and. success
        if (.not. success) then
            print *, 'ERROR: Extracting object 1 from jar'
            error stop 1
        end if

        ! print 1,'(test_pickling) my_jar : num_elem, size, avail =',num_elem, my_jar%usable(), my_jar%avail()
        ! print 2,'after get #1        ',blind_array(my_jar%low()+1:my_jar%high()),-1

        x2 = machin2(-1, -1, '**')
        success = JAR_GET_ITEMS(my_jar, x2(1:2))
        ! print *,'       ',a2(2)
        ! print *,'x2(1) =',x2(1)
        ! print *,'       ',a2(3)
        ! print *,'x2(2) =',x2(2)
        success = is_same_machin(a2(2),x2(1)) .and. success
        success = is_same_machin(a2(3),x2(2)) .and. success
        if (.not. success) then
            print *, 'ERROR: Extracting object 2 from jar'
            error stop 1
        end if

        ! print 1,'(test_pickling) my_jar : num_elem, size, avail =',num_elem, my_jar%usable(), my_jar%avail()
        ! print 2,'after get #2        ',blind_array(my_jar%low()+1:my_jar%high()),-1
        call my_jar%print_data(15_8)

        ! Testing different lengths for character chains
        char_in = 'abcdefghijklmnop'
        do i = 1, 16

            if (my_jar % get_num_avail() > 0) then
                print *, 'ERROR: jar should be empty'
                error stop 1
            end if

            success = JAR_PUT_STRING(my_jar, char_in(1:i))
            if (.not. success) then
                print *, 'ERROR: Unable to put characters in jar', i
                error stop 1
            end if

            char_out = ''
            success = JAR_GET_STRING(my_jar, char_out(1:i))
            ! print '(A, I2, 1X, L1, 1X, A, 1X, A, 1X, A)', 'INFO: ', i, success, char_in(1:i), char_out(1:i), char_out
            if (.not. success .or. char_in(1:i) .ne. char_out(1:i)) then
                print '(A, I2, 1X, L1, 1X, A, 1X, A, 1X, A)', 'ERROR: Unable to correctly get characters from jar',         &
                    i, success, '"' // char_in(1:i) // '"', '"' // char_out(1:i) // '"', '"' // char_out // '"'
                call my_jar % print_data(200_8)
                error stop 1
            end if
        end do

        ! disguise jar as integer array, test that it can be regenerated after pass-through
        ! here -> pass_through -> level2
        ! check that finalize does not release jas data space
        ! blind_array => my_jar%array()     ! get Fortran pointer to jar contents
        JAR_DATA(my_jar, blind_array)
        print *,'DIAG: before pass_through, blind_array size is', size(blind_array)
        call pass_through(blind_array, size(blind_array))  ! send integer array and its dimension
        print *,'DIAG: after pass_through'

        print *,''
        JAR_RESET(my_jar)
        call my_jar%print_data(20_8)
        print 1,'(test_pickling) my_jar reset : size, avail =', my_jar%get_size(), my_jar%get_num_avail()

        success = JAR_PUT_ITEM_AT(my_jar, a1, 2_8) .and. success                    ! skip one position, start injectiong at 2 rather than 1
        print 1,'(test_pickling) my_jar : size, avail =', my_jar%get_size(), my_jar%get_num_avail()
        call my_jar%print_data(20_8)
        success = JAR_PUT_ITEMS_AT(my_jar, a2(2:4), my_jar%get_top()+2) .and. success  ! skip one position, start at top + 2
        ! num_elem = my_jar%put(a2(2:4), storage_size(a2(2:4))*size(a2(2:4)), where=my_jar%high()+2 )
        print 1,'(test_pickling) my_jar : size, avail =', my_jar%get_size(), my_jar%get_num_avail()
        call my_jar%print_data(20_8)

        print 2,'before get        ', blind_array(my_jar%get_bot()+1:my_jar%get_top()), -1
        x1 = machin1([-1,-1,-1], 999999, '    ', '  ', '  ')
        success = JAR_GET_ITEM_AT(my_jar, x1, 2_8) .and. success                    ! skip one position, start injectiong at 2 rather than 1
        success = is_same_machin(a1,x1) .and. success
        if (.not. success) then
            print *, 'ERROR: Could not put/get object 1 at specific position in jar'
            error stop 1
        end if

        ! num_elem = my_jar%get( x1, storage_size(x1), where=2 )
        print *,'         ',a1
        print *,'x1      =',x1
        print 2,'after get #1        ',blind_array(my_jar%get_bot()+1:my_jar%get_top()),-1
        x2 = machin2(-1, -1, '**')
        success = JAR_GET_ITEMS_AT(my_jar, x2(1:2), my_jar%get_bot() + 2) .and. success       ! skip one position, start at bot + 2 rather than bot +1
        success = is_same_machin(a2(2),x2(1)) .and. success
        success = is_same_machin(a2(3),x2(2)) .and. success
        ! num_elem = my_jar%get(x2(1:2), storage_size(x2(1:2))*size(x2(1:2)), where=num_elem+2 )
        if (.not. success) then
            print *, 'ERROR: Could not put/get object 2 at specific position in jar'
            error stop 1
        end if
        print *,'       ',a2(2)
        print *,'x2(1) =',x2(1)
        print *,'       ',a2(3)
        print *,'x2(2) =',x2(2)
        print 2,'after get #2        ',blind_array(my_jar%get_bot()+1:my_jar%get_top()),-1
        success = JAR_GET_ITEM(my_jar, x2(3:3)) .and. success
        success = is_same_machin(a2(4),x2(3)) .and. success
        if (.not. success) then
            print *, 'ERROR: Could not get object 2 from jar'
            error stop 1
        end if
        print *,'       ',a2(4)
        print *,'x2(3) =',x2(3)
        print 2,'after get #3        ',blind_array(my_jar%get_bot()+1:my_jar%get_top()),-1

        ok = JAR_FREE(my_jar)
        if(.not. JAR_VALID(my_jar)) print *,'SUCCESS: jar is not valid after free'
        if(JAR_VALID(my_jar))       print *,'ERROR:   jar is valid after free'

        if(success) then
            print *,'============= TEST is a SUCCESS ============='
            return
        else
            print *,'============= ERRORS detected ============='
            error stop 1
        endif

1       format(A,10I8)
2       format(A15,30Z9.8)

        stop
    end subroutine test_pickling

    !> Create jar with given array as data (no copy/reallocation) and extract the data in a separate function
    subroutine pass_through(blind_array, n)    !  integer array inbound, jar outbound
        use jar_module
        implicit none
        integer, intent(IN) :: n
        integer(JAR_ELEMENT), dimension(n), intent(IN) :: blind_array
        type(jar) :: my_jar
        logical :: ok

        ! print *,'DIAG(pass_through) :blind_array size is',size(blind_array)

        ok = my_jar%shape_with(blind_array, size(blind_array))     ! jar data pointing to incoming integer array
        if (.not. ok) then
            print *, 'ERROR: Failed to shape jar'
            error stop 1
        end if

        call level2(my_jar)                                   ! pass jar down
    end subroutine pass_through

    !> Extract 1 machin1 and 2 machin2 from the given jar
    subroutine level2(my_jar)    ! receives jar, recreated from integer array by pass_through
        use jar_module
        use jar_test_types
        implicit none
        type(jar), intent(INOUT) :: my_jar
        type(machin1) :: x1
        type(machin2), dimension(4) :: x2
        logical :: success

        print *,'DIAG(level2) :'
        call my_jar%print_data(20_8)
        success = JAR_GET_ITEM(my_jar, x1)
        print *,'x1    =',x1
        success = JAR_GET_ITEMS(my_jar, x2(1:2)) .and. success
        print *,'x2(1) =',x2(1)
        print *,'x2(2) =',x2(2)
        print *,'DIAG(level2) exiting'
        if (.not. success) then
            print *, 'ERROR: Unable to extract stuff from jar'
            error stop 1
        end if

    end subroutine level2

    subroutine test_free
        use jar_module
        implicit none
        type(jar) :: myjar
        logical :: old_debug, scrap
        logical :: ok
        integer :: status

        old_debug = myjar%debug(.false.)
        print *,'old debug mode =',old_debug
        old_debug = myjar%debug(.true.)
        print *,'old debug mode =',old_debug
        scrap = myjar%debug(.true.)
        print *,'old debug mode =',scrap
        print *,''

        ok = myjar%new(640)
        print *,'(test_free) myjar%new() =',ok, myjar%get_size()
        status = myjar%free()
        if (status .ne. 0) then
            print *, 'ERROR: did not free data'
        end if
        status = myjar%free()
        if (status .eq. 0) then
            print *, 'ERROR: jar data should have been freed already'
        end if
        print *,''

        call test_jar_1
        print *,''
        old_debug = myjar%debug(.false.)
        print *,''
        print *,'test_free: old debug mode =',old_debug
        old_debug = myjar%debug(.false.)
        print *,'test_free: old debug mode =',old_debug
      
    end subroutine test_free

    subroutine test_jar_1
        use jar_module
        implicit none
        logical :: ok
        type(jar) :: myjar

        ok = myjar%new(1280)
        call test_jar_finalize()
        print *,''

        block
          type(jar) :: myjar          ! overrrides myjar in main scope
          ok = myjar%new(128)
          print *,'myjar%new() =',ok, myjar%get_size()
          ! ok = myjar%free()
          print *,'jar going out of scope in block'
        end block
        print *,''

        if (myjar % get_size() < 0) then
            print *, 'ERROR: myjar should be usable at this point'
            error stop 1
        end if 
        ! print *,'freeing jar at end of test_jar_1, capacity =',myjar%usable()
        ! ok = myjar%free()
        ! if(ok .ne. 0) print *,'WARNING: jar data already freed'
        print *,'myjar%new() test_jar_1 =',ok, myjar%get_size()
        print *,'jar going out of scope in test_jar_1'
    end subroutine test_jar_1

    subroutine test_jar_finalize
        use jar_module
        implicit none
        type(jar) :: myjar
        logical :: ok

        ok = myjar%new(1024)
        ! print *,'myjar%new(1024) =',ok, myjar%get_size()
        if(myjar%get_size() .ne. 1024) then
            print *,'ERROR: unexpected size', myjar % get_size()
            error stop 1
        end if
        print *,'jar going out of scope in subroutine test_jar_finalize'
    end subroutine test_jar_finalize

end module jar_test_procedures

program test_jar
    use jar_test_procedures
    implicit none
    call test_free()
    call test_pickling()
end program

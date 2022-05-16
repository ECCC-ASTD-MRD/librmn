! Copyright (C) 2021  Environnement et Changement climatique Canada
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
! Author:
!     M. Valin,   Recherche en Prevision Numerique, 2021
!
module f_c_strings_mod
    use ISO_C_BINDING

    private :: c_strnlen, c_strlen
    public  :: memset
    interface
        function c_strnlen(str, maxlen) result(strlen) bind(C, name='strnlen')
            import :: C_CHAR, C_SIZE_T
            implicit none
            character(C_CHAR), dimension(*), intent(IN) :: str
            integer(C_SIZE_T), intent(IN) :: maxlen
            integer(C_SIZE_T) :: strlen
        end function c_strnlen

        function c_strlen(str) result(strlen) bind(C, name='strlen')
            import :: C_PTR, C_SIZE_T
            implicit none
            type(C_PTR), intent(IN) :: str
            integer(C_SIZE_T) :: strlen
        end function c_strlen

        function memset(s, byte, n) result(p) bind(C, name='memset')
            import :: C_PTR, C_SIZE_T, C_INT
            implicit none
            type(C_PTR), intent(IN), value :: s
            integer(C_INT), intent(IN), value :: byte
            integer(C_SIZE_T), intent(IN), value :: n
            type(C_PTR) :: p
        end function memset
    end interface

    interface c_f_strpointer
        module procedure c_f_strpointer1
        module procedure c_f_strpointer2
    end interface

    public  :: strncpy_c2f, strncpy_f2c

    private :: allocate_string, clone_string, duplicate_from_fstring
    private :: delete_string, deallocate_string
    private :: get_string_pointer, is_string_owner, is_string_valid
    type :: C_STRING
        private
            character(C_CHAR), dimension(:), pointer :: string => NULL()
            logical :: owner = .false.      ! true if i own the storage (if i allocated it) ?
            contains
            procedure, PASS(this) :: allocate_string                 ! alllocate new space (or reuse existing space if large neough)
            procedure, PASS(this) :: duplicate_from_fstring          ! new string with copy of contents of a character array
            procedure, PASS(this) :: clone_string                    ! new string pointing to an existing one (no local allocaation)
            GENERIC :: new => allocate_string, clone_string, duplicate_from_fstring

            procedure, PASS(this) :: str => get_string_pointer       ! get pointer to string contents
            procedure, PASS(this) :: del => deallocate_string        ! release string space if owner
            procedure, PASS(this) :: own => is_string_owner          ! do i ownn the storage space (did i allocated it) ?
            procedure, PASS(this) :: valid => is_string_valid        ! string points to existing stoirage
            final :: delete_string             ! deallocate string data when going out of scope
    end type

    contains

    !> Create a new string of length at least n
    function allocate_string(this, n) result(ok)
        implicit none
        class(C_STRING), intent(INOUT) :: this
        integer, intent(IN) :: n

        logical :: ok

        ok = .true.
        if(associated(this%string)) then
            if(size(this%string) >= n+1) return       ! string is large enough, return
            if(this%owner) then
                deallocate(this%string)               ! too small, deallocate. then reallocate
                allocate(this%string(n+1))
            else
                ok = .false.                          ! not owner of storage, and storage is too small
            endif
        else
            allocate(this%string(n+1))
            this%owner = .true.
        endif
    end function allocate_string

    !> Clone a string (copy by pointing)
    function clone_string(this, new) result(ok)
        implicit none
        class(C_STRING), intent(IN) :: this
        type(C_STRING), intent(OUT) :: new

        logical :: ok

        if(associated(this%string)) then
            new%string => this%string
            new%owner = .false.
            ok = .true.
        else
            ok = .false.
        endif
    end function clone_string

    !> Take a copy of a fortran character(len=*) string
    function duplicate_from_fstring(this, fstring) result(ok)
        implicit none
        class(C_STRING), intent(INOUT) :: this
        character(len=*), intent(IN) :: fstring

        logical :: ok
        integer :: length, i

        length = len(fstring)
        ok = allocate_string(this, length)
        if(ok) then
            do i=1,length
                this%string(i) = fstring(i:i)
            enddo
        endif
    end function duplicate_from_fstring

    !> Deallocate string data (also used as finalize subroutine)
    subroutine delete_string(s)
        implicit none
        type(C_STRING), intent(INOUT) :: s

        if(associated(s%string)) then
            if(s%owner) then                 ! deallocate only if owner of storage
                deallocate(s%string)
                s%string => NULL()
                s%owner = .false.
            endif
        endif
    end subroutine delete_string

    !> Deallocate a string
    subroutine deallocate_string(this)
        implicit none
        class(C_STRING), intent(INOUT) :: this

        call delete_string(this)
    end subroutine deallocate_string

    !> String contents accessor
    function get_string_pointer(this) result(s)
        implicit none
        class(C_STRING), intent(IN) :: this
        character(C_CHAR), dimension(:), pointer :: s

        s => this%string
    end function get_string_pointer

    !> String owns the storage space
    function is_string_owner(this) result(ok)
        implicit none
        class(C_STRING), intent(IN) :: this

        logical :: ok

        ok = this%owner
    end function is_string_owner

    ! Validate string
    function is_string_valid(this) result(ok)
        implicit none
        class(C_STRING), intent(IN) :: this
        logical :: ok

        ok = associated(this%string)
    end function is_string_valid

    !> Copy a Fortran string into a NULL terminated character array of at most n characters
    subroutine strncpy_f2c(f_str, c_str, n)
        implicit none
        character(len=*), intent(IN) :: f_str
        integer, intent(IN), value :: n
        character(C_CHAR), dimension(n), intent(OUT) :: c_str

        integer :: clen, i
        clen = min(len(f_str), n-1)              ! C string can accept at most n-1 bytes
        do i = 1, clen
            c_str(i) = f_str(i:i)                  ! copy string
        enddo
        c_str(clen:clen) = achar(0)              ! add terminating null
    end subroutine strncpy_f2c

    !> Copy a NULL terminated character array of up to n characters into a Fortran string
    subroutine strncpy_c2f(f_str, c_str, n)
        implicit none
        character(len=*), intent(OUT) :: f_str
        integer, intent(IN), value :: n
        character(C_CHAR), dimension(n), intent(IN) :: c_str

        integer(C_SIZE_T) :: flen, clen, i
        flen = len(f_str)
        clen = n
        clen = c_strnlen(c_str, clen)            ! C string cannot be longer than n bytes
        clen = min(flen,clen)                    ! at most flen characters can fir in Fortran string
        do i = 1, clen
            f_str(i:i) = c_str(i)            ! copy string
        enddo
        if (flen > clen) f_str(clen+1:flen) = ' ' ! pad with blanks
    end subroutine strncpy_c2f

#if ! defined FORTRAN_202X_SUPPORTED
    !>Attempt at implementing new C<->Fortran strings from Fortran 202X
    function f_c_string(fstr, asis) result(cstr)
        implicit none
        character(len=*), intent(IN) :: fstr
        logical, intent(IN), optional :: asis
        character(len=:), allocatable :: cstr

        logical :: verbatim

        verbatim = .false.
        if(present(asis)) verbatim = asis
        if(verbatim) then
            cstr = fstr // c_null_char
        else
            cstr = trim(fstr) // c_null_char
        endif
    end function f_c_string

    subroutine c_f_strpointer1(cstrarray, fstrptr, nchars)
        implicit none
        character(C_CHAR), dimension(*), intent(IN), target :: cstrarray
        character(len=:), pointer, intent(OUT) :: fstrptr
        integer, intent(IN), optional :: nchars
        type(C_PTR) :: cstrptr
        integer(C_SIZE_T) :: nc
        character(len=:), pointer :: fptr

        nc = 2000000000
        if(present(nchars)) nc = nchars
            nc = c_strnlen(cstrarray, nc)
            cstrptr = C_LOC(cstrarray(1))
        call c_f_pointer(cstrptr, fptr)
            fstrptr => fptr(1:nc)
    end subroutine c_f_strpointer1

    subroutine c_f_strpointer2(cstrptr, fstrptr, nchars)
        implicit none
        type(C_PTR), intent(IN), value :: cstrptr
        character(len=:), pointer, intent(OUT) :: fstrptr
        integer, intent(IN) :: nchars
        integer(C_SIZE_T) :: nc
        character(len=:), pointer :: fptr

        nc = c_strlen(cstrptr)
        nc = min(nc,nchars)
        call c_f_pointer(cstrptr, fptr)
        fstrptr => fptr(1:nc)
    end subroutine c_f_strpointer2
#endif
end module

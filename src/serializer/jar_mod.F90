!  serializer for FORTRAN programming
!  Copyright (C) 2022  Recherche en Prevision Numerique
!
!  This software is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation,
!  version 2.1 of the License.
!
!  This software is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!> \author M. Valin,   Recherche en Prevision Numerique
!> \author V. Magnoux, Recherche en Prevision Numerique
!> \date 2020-2022
!> \file jar_mod.F90 Fortran data serializer module


! _the Cray Fortran compiler treats loc() as a type(C_PTR), other compilers as integer(C_INTPTR_T)
#if defined(_CRAYFTN)
#define WHAT_TYPE type(C_PTR)
#else
#define WHAT_TYPE integer(C_INTPTR_T)
#endif

module jar_module
    use ISO_C_BINDING
    implicit none

    private

    interface
        function libc_malloc(sz) result(ptr) BIND(C,name='malloc')
        import :: C_SIZE_T, C_PTR
        implicit none
        integer(C_SIZE_T), intent(IN), value :: sz
        type(C_PTR) :: ptr
        end function libc_malloc
        subroutine libc_free(ptr) BIND(C, name='free')
        import :: C_PTR
        implicit none
        type(C_PTR), intent(IN), value :: ptr
        end subroutine libc_free
    end interface

    logical, save, private :: debug_mode = .false.

    integer, parameter, public :: JAR_ELEMENT = C_INT64_T !< We want 64-bit elements in the jars

    !> C interoperable version of jar
    type, public, BIND(C) :: c_jar
        private
        integer(JAR_ELEMENT) :: size = 0             ! capacity of jar
        integer(JAR_ELEMENT) :: top  = 0             ! last posision "written" (cannot write beyond size)
        integer(JAR_ELEMENT) :: bot  = 0             ! last position "read" (cannot read beyond top)
        integer(JAR_ELEMENT) :: opt  = 0             ! option flags (0 owner of data memory, 1 not owner)
        type(C_PTR)          :: ptr = C_NULL_PTR     ! address of actual data
    end type

    !> Same as c_jar, but with type bound procedures
    type, public :: jar
        private
        integer(JAR_ELEMENT) :: size = 0             ! capacity of jar
        integer(JAR_ELEMENT) :: top  = 0             ! last posision "written" (cannot write beyond size)
        integer(JAR_ELEMENT) :: bot  = 0             ! last position "read" (cannot read beyond top)
        integer(JAR_ELEMENT) :: opt  = 0             ! option flags (0 owner of data memory, 1 not owner)
        type(C_PTR)          :: ptr = C_NULL_PTR     ! address of actual data
    contains
        procedure, PASS :: new     => new_jar
        procedure, PASS :: shape   => shape_as_jar
        procedure, PASS :: valid   => valid_jar
        procedure, PASS :: free    => free_jar
        procedure, PASS :: rewind  => rewind_jar
        procedure, PASS :: reset   => reset_jar
        procedure, PASS :: data    => jar_pointer
        procedure, PASS :: array   => jar_contents
        procedure, PASS :: raw_array => jar_contents_full
        procedure, PASS :: usable  => jar_size
        procedure, PASS :: high    => jar_top
        procedure, PASS :: low     => jar_bot
        procedure, PASS :: avail   => jar_avail
        procedure, PASS :: put     => put_into_jar
        procedure, PASS :: get     => get_outof_jar
        procedure, PASS :: print   => print_jar
        procedure, NOPASS :: debug => debug_jars
        final :: final_jar
    end type

    contains

    !> Set debug mode, get previous setting
    function debug_jars(mode) result(old)
        implicit none
        logical, intent(IN), value :: mode       !> .true. , set debug mode, .false. cancel debug mode
        logical :: old                           !> Previous setting

        if (debug_mode .or. mode) print *,'DEBUG: setting debug mode to', mode, ' was', debug_mode
        old        = debug_mode
        debug_mode = mode
    end function debug_jars


    !> Create a new data jar, allocate data storage
    function new_jar(jar_instance, data_size) result(ok)
        implicit none
        class(jar), intent(INOUT) :: jar_instance !> Data jar
        integer, intent(IN), value :: data_size   !> Number of elements in jar
        integer :: ok                             !> 0 if O.K., -1 if error

        integer(C_SIZE_T)    :: dsz
        integer(JAR_ELEMENT) :: dummy_jar_element

        ok = -1
        if (C_ASSOCIATED(jar_instance%ptr)) return       ! error, there is already an allocated data container

        dsz = data_size
        ! size in bytes
        jar_instance%ptr = libc_malloc( dsz * storage_size(dummy_jar_element) / 8)
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return ! malloc failed
        ok = 0

        jar_instance%top  = 0                            ! data jar is empty (no data written)
        jar_instance%bot  = 0                            ! data jar is empty (no data to read)
        jar_instance%opt  = 0                            ! options = 0, jar owns the memory storage
        jar_instance%size = data_size                    ! data jar capacity
    end function new_jar


    !> Transform an integer array into a jar
    function shape_as_jar(jar_instance, array, arraysize) result(ok)
        implicit none
        class(jar), intent(INOUT) :: jar_instance                       !> Data jar instance
        integer(C_INT), intent(IN), value :: arraysize                  !> Number of elements in arrray
        integer(JAR_ELEMENT), dimension(arraysize), intent(IN) :: array !> Input array
        integer :: ok                                                   !> 0 if O.K., -1 if error

        integer(C_INTPTR_T) :: temp

        ok = -1
        if (C_ASSOCIATED(jar_instance%ptr)) return          ! error, there is already an allocated data container

        temp = LOC(array)
        jar_instance%ptr = transfer(temp, jar_instance%ptr)
        ok = 0

        jar_instance%top  = arraysize                    ! data jar is full
        jar_instance%bot  = 0                            ! no data has been read yet
        jar_instance%opt  = 1                            ! options = 1, jar is not the owner of the storage
        jar_instance%size = arraysize                    ! data jar capacity
    end function shape_as_jar


    !> Check if jar is valid (is there is a valid data pointer ?)
    function valid_jar(jar_instance) result(ok)
        implicit none
        class(jar), intent(INOUT) :: jar_instance          !> Data jar instance
        logical :: ok                                      !> .true. if valid, .false. if not

        ok = C_ASSOCIATED(jar_instance%ptr)
    end function valid_jar


    !> Empty, keep allocated space
    subroutine reset_jar(jar_instance)
        implicit none
        class(jar), intent(INOUT) :: jar_instance          !> Data jar instance

        jar_instance%top  = 0                              ! data jar is empty (no data written)
        jar_instance%bot  = 0                              ! data jar is empty (no data read)
    end subroutine reset_jar


    !> Rewind jar for extraction
    subroutine rewind_jar(jar_instance)
        implicit none
        class(jar), intent(INOUT) :: jar_instance          !> Data jar instance

        jar_instance%bot  = 0                              ! no data read yet
    end subroutine rewind_jar


    !> Print jar info
    subroutine print_jar(jar_instance, max_elem)
        implicit none
        class(jar), intent(IN) :: jar_instance               !> Data jar instance
        integer(JAR_ELEMENT), intent(IN), value :: max_elem  !> Maximum number of data elements to print

        integer, dimension(:), pointer :: data

        call C_F_POINTER(jar_instance%ptr, data, [jar_instance%size])
        print '(3I6,(20Z9.8))', jar_instance%size, jar_instance%bot, jar_instance%top, data(1:min(max_elem, jar_instance%top))
    end subroutine print_jar


    !> Get current number of elements available for extraction (high - low) (JAR_ELEMENT units)
    function jar_avail(jar_instance) result(sz)
        implicit none
        class(jar), intent(IN) :: jar_instance             !> Data jar instance
        integer(JAR_ELEMENT) :: sz                         !> Number of elements available data in jar (JAR_ELEMENT units)

        sz = -1
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return
        sz = jar_instance%top - jar_instance%bot
    end function jar_avail


    !> Get current number of elements inserted (written) (JAR_ELEMENT units)
    function jar_top(jar_instance) result(sz)
        implicit none
        class(jar), intent(IN) :: jar_instance             !> Data jar instance
        integer(JAR_ELEMENT) :: sz                         !> Number of elements inserted in jar (JAR_ELEMENT units)

        sz = -1
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return
        sz = jar_instance%top
    end function jar_top

    !> Get current number of elements extracted (read) (JAR_ELEMENT units)
    function jar_bot(jar_instance) result(sz)
        implicit none
        class(jar), intent(IN) :: jar_instance             !> Data jar instance
        integer(JAR_ELEMENT) :: sz                         !> Number of elements extracted from jar (JAR_ELEMENT units)

        sz = -1
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return
        sz = jar_instance%bot
    end function jar_bot


    !> Get maximum capacity of data jar (JAR_ELEMENT units)
    function jar_size(jar_instance) result(sz)
        implicit none
        class(jar), intent(IN) :: jar_instance             !> Data jar instance
        integer(JAR_ELEMENT) :: sz                         !> Jar capacity (JAR_ELEMENT units)

        sz = -1
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return
        sz = jar_instance%size
    end function jar_size


    !> Get C pointer to jar data (1 D array of JAR_ELEMENTs)
    function jar_pointer(jar_instance) result(ptr)
        implicit none
        class(jar), intent(IN) :: jar_instance             !> Data jar instance
        type(C_PTR) :: ptr                                 !> C pointer to jar data

        ptr = C_NULL_PTR
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return
        ptr = jar_instance%ptr
    end function jar_pointer


    !> Get Fortran pointer to jar data (1 D array of JAR_ELEMENTs)
    function jar_contents(jar_instance) result(fp)
        implicit none
        class(jar), intent(IN) :: jar_instance             !> Data jar instance
        integer(JAR_ELEMENT), dimension(:),  pointer :: fp !> Fortran pointer to jar data

        nullify(fp)
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return ! no data pointer, return NULL pointer
        if (jar_instance%top == 0) return                ! empty jar, return NULL pointer

        call C_F_POINTER(jar_instance%ptr, fp, [jar_instance%top])  ! Fortran pointer to array of jar_instance%size JAR_ELEMENTs
    end function jar_contents


    !> Get Fortran pointer to entire jar data array
    function jar_contents_full(jar_instance) result(fp)
        implicit none
        class(jar), intent(IN) :: jar_instance             !> Data jar instance
        integer(JAR_ELEMENT), dimension(:), pointer :: fp  !> Fortran pointer to jar data

        nullify(fp)
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return

        call C_F_POINTER(jar_instance%ptr, fp, [jar_instance%size])
    end function jar_contents_full


    !> Deallocate a jar's data if not already done at finalize (if jar owns it)
    subroutine final_jar(jar_instance)
        implicit none
        type(jar), intent(INOUT) :: jar_instance            !> Data jar instance

        if (C_ASSOCIATED(jar_instance%ptr)) then
        if (debug_mode .and. jar_instance%opt == 0) print *, 'DEBUG(jar finalize): freing jar memory, size =', jar_instance%size
        if (debug_mode .and. jar_instance%opt == 1) print *, 'DEBUG(jar finalize): not owner, not freing jar memory, size =', jar_instance%size
        if (jar_instance%opt == 0) call libc_free(jar_instance%ptr)      ! release storage associated with jar if jar owns it
            jar_instance%ptr = C_NULL_PTR
        else
            if(debug_mode) print *,'DEBUG(jar finalize): nothing to free in jar'
        endif
        jar_instance%top  = 0                             ! data jar is now empty (no data written)
        jar_instance%bot  = 0                             ! data jar is now empty (no data to read)
        jar_instance%opt  = 0                             ! reset ownership flag
        jar_instance%size = 0                             ! data jar cannot store data
    end subroutine final_jar


    !> Deallocate a jar's data space (if jar owns it)
    function free_jar(jar_instance) result(status)
        implicit none
        class(jar), intent(INOUT) :: jar_instance           !> Data jar instance
        integer :: status                                   !> 0 if O.K., -1 if nothing to free

        if (C_ASSOCIATED(jar_instance%ptr)) then
        if (debug_mode .and. jar_instance%opt == 0) print *,'DEBUG(jar free): freing jar memory'
        if (debug_mode .and. jar_instance%opt == 1) print *,'DEBUG(jar free): not owner, not freing jar memory'
        if (jar_instance%opt == 0) call libc_free(jar_instance%ptr)    ! release storage associated with jar if jar owns it
            jar_instance%ptr = C_NULL_PTR                     ! nullify data pointer to avoid accidents
            status = 0
        else
            if (debug_mode) print *,'DEBUG(jar free): nothing to free in jar'
            status = -1                            ! already freed
        endif
        jar_instance%top  = 0                             ! data jar is now empty (no data written)
        jar_instance%bot  = 0                             ! data jar is now empty (no data to read)
        jar_instance%opt  = 0                             ! reset ownership flag
        jar_instance%size = 0                             ! data jar cannot store data
    end function free_jar


#define IgnoreTypeKindRank object
#define ExtraAttributes , target
    !> Add data to jar at top of jar (or at a specific position)
    function put_into_jar(jar_instance, object, size, where) result(sz)
        implicit none
        class(jar), intent(INOUT) :: jar_instance                          !> Data jar instance
#include <IgnoreTypeKindRank.hf>
        integer, intent(IN), value :: size                                 !> Size to insert = storage_size(item) * nb_of_items
        integer(JAR_ELEMENT), intent(IN), optional, value :: where         !> Insertion point (1 = start of jar), optional
        integer(JAR_ELEMENT) :: sz                                         !> Position of last inserted element (-1 if error)

        integer(JAR_ELEMENT) :: intsize, pos
        type(C_PTR) :: temp
        integer(JAR_ELEMENT), dimension(:), pointer :: je, content

        sz = -1
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return

        call C_F_POINTER(jar_instance%ptr, content, [jar_instance%size])   ! pointer to jar data
        if ( present(where) ) then
            pos = where - 1                                                ! insert starting at position "where"
        else
            pos = jar_instance%top                                         ! insert after data currently in jar
        endif
        if (pos < 0) return                                                ! invalid insertion position
        if (pos > jar_instance%top) content(jar_instance%top + 1 : pos) = 0 ! zero fill skipped portion

        intsize = (size + storage_size(content(1)) - 1) / storage_size(content(1))
        if (pos + intsize > jar_instance%size) return                      ! jar would overflow

        temp = C_LOC(object)
        call C_F_POINTER(temp, je, [intsize])                              ! make what into an integer array
        content(pos + 1 : pos + intsize) = je(1 : intsize)                 ! insert into data portion of jar
        jar_instance%top = pos + intsize                                   ! update top of jar position
        sz = jar_instance%top                                              ! number of elements in jar
    end function put_into_jar


#define IgnoreTypeKindRank object
#define ExtraAttributes , target
    !> Get data from jar at current (or specific position)
    function get_outof_jar(jar_instance, object, size, where) result(sz)
        implicit none
        class(jar), intent(INOUT) :: jar_instance                    !> Data jar instance
#include <IgnoreTypeKindRank.hf>
        integer, intent(IN), value :: size                           !> Size to insert = storage_size(item) * nb_of_items
        integer(JAR_ELEMENT), intent(IN), optional, value :: where   !> Extraction point (1 = start of jar), optional
        integer(JAR_ELEMENT) :: sz                                   !> Position of last extracted element (-1 if error)

        integer(JAR_ELEMENT) :: intsize, pos
        type(C_PTR) :: temp
        integer(JAR_ELEMENT), dimension(:), pointer :: je, content

        sz = -1
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return

        call C_F_POINTER(jar_instance%ptr, content, [jar_instance%size]) ! pointer to jar data
        if ( present(where) ) then
            pos = where - 1                                              ! insert at position "where"
        else
            pos = jar_instance%bot                                       ! insert after data currently in jar
        endif
        if (pos < 0) return                                              ! invalid insertion position

        intsize = size / storage_size(content(1))
        if (pos + intsize > jar_instance%top) return                     ! insufficient data in jar

        temp = C_LOC(object)
        call C_F_POINTER(temp, je, [intsize])                            ! make what into an integer array
        je(1 : intsize) = content(pos + 1 : pos + intsize)               ! insert into data portion of jar
        jar_instance%bot = pos + intsize                                 ! update top of jar position
        sz = jar_instance%bot                                            ! position of last extracted element
    end function get_outof_jar

end module jar_module

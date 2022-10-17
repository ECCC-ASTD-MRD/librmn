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

!> \file jar_mod.F90
!> Fortran data serializer module


! _the Cray Fortran compiler treats loc() as a type(C_PTR), other compilers as integer(C_INTPTR_T)
#if defined(_CRAYFTN)
#define WHAT_TYPE type(C_PTR)
#else
#define WHAT_TYPE integer(C_INTPTR_T)
#endif

module rmn_jar
    use ISO_C_BINDING
    implicit none

    private

    interface
        function libc_malloc(sz) result(ptr) BIND(C, name='malloc')
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
        integer(JAR_ELEMENT) :: size_elem = 0             !< capacity of jar, in number of elements
        integer(JAR_ELEMENT) :: top       = 0             !< last posision "written" (cannot write beyond size)
        integer(JAR_ELEMENT) :: bot       = 0             !< last position "read" (cannot read beyond top)
        integer(JAR_ELEMENT) :: opt       = 0             !< option flags (0 owner of data memory, 1 not owner)
        type(C_PTR)          :: ptr       = C_NULL_PTR    !< address of actual data
    end type

    !> Same as c_jar, but with type bound procedures
    type, public :: jar
        private
        integer(JAR_ELEMENT) :: size_elem = 0             !< capacity of jar, in number of elements
        integer(JAR_ELEMENT) :: top       = 0             !< last posision "written" (cannot write beyond size)
        integer(JAR_ELEMENT) :: bot       = 0             !< last position "read" (cannot read beyond top)
        integer(JAR_ELEMENT) :: opt       = 0             !< option flags (0 owner of data memory, 1 not owner)
        type(C_PTR)          :: ptr       = C_NULL_PTR    !< address of actual data
    contains
        generic         :: new            => new_i4, new_i8         !< Generic function for creating a new jar
        procedure, PASS :: new_i4         => jar_new_i4             !< \copydoc jar_new_i4
        procedure, PASS :: new_i8         => jar_new_i8             !< \copydoc jar_new_i8
        procedure, PASS :: shape_with     => jar_shape_with         !< \copydoc jar_shape_with
        procedure, PASS :: is_valid       => jar_is_valid           !< \copydoc jar_is_valid
        procedure, PASS :: free           => jar_free               !< \copydoc jar_free
        procedure, PASS :: reset          => jar_reset              !< \copydoc jar_reset
        procedure, PASS :: raw_data       => jar_raw_pointer        !< \copydoc jar_raw_pointer
        procedure, PASS :: f_array        => jar_contents           !< \copydoc jar_contents
        procedure, PASS :: full_f_array   => jar_contents_full      !< \copydoc jar_contents_full
        procedure, PASS :: get_size       => jar_size               !< \copydoc jar_size
        procedure, PASS :: get_top        => jar_top                !< \copydoc jar_top
        procedure, PASS :: get_bot        => jar_bot                !< \copydoc jar_bot
        procedure, PASS :: get_num_avail  => jar_avail              !< \copydoc jar_avail
        procedure, PASS :: insert         => jar_put_into           !< \copydoc jar_put_into
        procedure, PASS :: insert_string  => jar_put_string_into    !< \copydoc jar_put_string_into
        procedure, PASS :: extract        => jar_get_outof          !< \copydoc jar_get_outof
        procedure, PASS :: extract_string => jar_get_string_outof   !< \copydoc jar_get_string_outof
        procedure, PASS :: print_data     => jar_print_data         !< \copydoc jar_print_data
        procedure, NOPASS :: debug        => debug_jars             !< \copydoc debug_jars
        final :: jar_final
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


    !> Create a new data jar (size specified with an 32-bit integer), allocate data storage
    function jar_new_i4(jar_instance, data_size) result(ok)
        implicit none
        class(jar),         intent(INOUT)     :: jar_instance !> Data jar instance
        integer(C_INT32_T), intent(IN), value :: data_size    !> Number of elements in jar
        logical :: ok                             !> .true. if jar was successfully created, .false. otherwise

        ok = jar_instance % new_i8(int(data_size, kind = 8))
    end function jar_new_i4


    !> Create a new data jar (size specified with a 64-bit integer), allocate data storage
    function jar_new_i8(jar_instance, data_size) result(ok)
        implicit none
        class(jar),         intent(INOUT)     :: jar_instance !> Data jar instance
        integer(C_INT64_T), intent(IN), value :: data_size    !> Number of elements in jar
        logical :: ok                             !> .true. if jar was successfully created, .false. otherwise

        integer(C_SIZE_T)    :: data_size_byte
        integer(JAR_ELEMENT) :: dummy_jar_element

        ok = .false.
        if (C_ASSOCIATED(jar_instance%ptr)) return       ! error, there is already an allocated data container

        data_size_byte = data_size * (storage_size(dummy_jar_element) / 8)
        ! size in bytes
        jar_instance%ptr = libc_malloc(data_size_byte)
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return ! malloc failed
        ok = .true.

        jar_instance%top       = 0                            ! data jar is empty (no data written)
        jar_instance%bot       = 0                            ! data jar is empty (no data to read)
        jar_instance%opt       = 0                            ! options = 0, jar owns the memory storage
        jar_instance%size_elem = data_size                    ! data jar capacity
    end function jar_new_i8


    !> Transform an integer array into a jar
    function jar_shape_with(jar_instance, array, array_size_elem) result(ok)
        implicit none
        class(jar), intent(INOUT) :: jar_instance                       !> Data jar instance
        integer(C_INT), intent(IN), value :: array_size_elem            !> Number of elements in array
        integer(JAR_ELEMENT), dimension(array_size_elem), intent(IN) :: array !> Input array
        logical :: ok                                                   !> .true. if O.K., .false. if error

        integer(C_INTPTR_T) :: temp

        ok = .false.
        if (C_ASSOCIATED(jar_instance%ptr)) return          ! error, there is already an allocated data container

        temp = LOC(array)
        jar_instance%ptr = transfer(temp, jar_instance%ptr)
        ok = .true.

        jar_instance%top       = array_size_elem              ! data jar is full
        jar_instance%bot       = 0                            ! no data has been read yet
        jar_instance%opt       = 1                            ! options = 1, jar is not the owner of the storage
        jar_instance%size_elem = array_size_elem              ! data jar capacity
    end function jar_shape_with


    !> Check if jar is valid (is there is a valid data pointer ?)
    function jar_is_valid(jar_instance) result(ok)
        implicit none
        class(jar), intent(INOUT) :: jar_instance          !> Data jar instance
        logical :: ok                                      !> .true. if valid, .false. if not

        ok = C_ASSOCIATED(jar_instance%ptr)
    end function jar_is_valid


    !> Empty, keep allocated space
    subroutine jar_reset(jar_instance)
        implicit none
        class(jar), intent(INOUT) :: jar_instance          !> Data jar instance

        jar_instance%top  = 0                              ! data jar is empty (no data written)
        jar_instance%bot  = 0                              ! data jar is empty (no data read)
    end subroutine jar_reset


    !> Print jar info
    subroutine jar_print_data(jar_instance, max_elem)
        implicit none
        class(jar), intent(IN) :: jar_instance               !> Data jar instance
        integer(JAR_ELEMENT), intent(IN), value :: max_elem  !> Maximum number of data elements to print

        integer, dimension(:), pointer :: jar_data

        call C_F_POINTER(jar_instance%ptr, jar_data, [jar_instance%size_elem])
        print '(3I6,(20Z9.8))', jar_instance%size_elem, jar_instance%bot, jar_instance%top, jar_data(1:min(max_elem, jar_instance%top))
    end subroutine jar_print_data


    !> Get current number of elements available for extraction (high - low) (JAR_ELEMENT units)
    function jar_avail(jar_instance) result(num_elem)
        implicit none
        class(jar), intent(IN) :: jar_instance             !> Data jar instance
        integer(JAR_ELEMENT) :: num_elem                   !> Number of elements available data in jar (JAR_ELEMENT units)

        num_elem = -1
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return
        num_elem = jar_instance%top - jar_instance%bot
    end function jar_avail


    !> Get current number of elements inserted (written) (JAR_ELEMENT units)
    function jar_top(jar_instance) result(num_elem)
        implicit none
        class(jar), intent(IN) :: jar_instance             !> Data jar instance
        integer(JAR_ELEMENT) :: num_elem                   !> Number of elements inserted in jar (JAR_ELEMENT units)

        num_elem = -1
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return
        num_elem = jar_instance%top
    end function jar_top

    !> Get current number of elements extracted (read) (JAR_ELEMENT units)
    function jar_bot(jar_instance) result(num_elem)
        implicit none
        class(jar), intent(IN) :: jar_instance             !> Data jar instance
        integer(JAR_ELEMENT) :: num_elem                   !> Number of elements extracted from jar (JAR_ELEMENT units)

        num_elem = -1
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return
        num_elem = jar_instance%bot
    end function jar_bot


    !> Get maximum capacity of data jar (JAR_ELEMENT units)
    function jar_size(jar_instance) result(num_elem)
        implicit none
        class(jar), intent(IN) :: jar_instance             !> Data jar instance
        integer(JAR_ELEMENT) :: num_elem                   !> Jar capacity (JAR_ELEMENT units)

        num_elem = -1
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return
        num_elem = jar_instance%size_elem
    end function jar_size


    !> Get C pointer to jar data (1 D array of JAR_ELEMENTs)
    function jar_raw_pointer(jar_instance) result(ptr)
        implicit none
        class(jar), intent(IN) :: jar_instance             !> Data jar instance
        type(C_PTR) :: ptr                                 !> C pointer to jar data

        ptr = C_NULL_PTR
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return
        ptr = jar_instance%ptr
    end function jar_raw_pointer


    !> Get Fortran pointer to jar data (1 D array of JAR_ELEMENTs)
    function jar_contents(jar_instance) result(f_ptr)
        implicit none
        class(jar), intent(IN) :: jar_instance                !> Data jar instance
        integer(JAR_ELEMENT), dimension(:),  pointer :: f_ptr !> Fortran pointer to jar data

        nullify(f_ptr)
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return ! no data pointer, return NULL pointer
        if (jar_instance%top == 0) return                ! empty jar, return NULL pointer

        call C_F_POINTER(jar_instance%ptr, f_ptr, [jar_instance%top])  ! Fortran pointer to array of jar_instance%size_elem JAR_ELEMENTs
    end function jar_contents


    !> Get Fortran pointer to entire jar data array
    function jar_contents_full(jar_instance) result(f_ptr)
        implicit none
        class(jar), intent(IN) :: jar_instance                !> Data jar instance
        integer(JAR_ELEMENT), dimension(:), pointer :: f_ptr  !> Fortran pointer to jar data

        nullify(f_ptr)
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return

        call C_F_POINTER(jar_instance%ptr, f_ptr, [jar_instance%size_elem])
    end function jar_contents_full


    !> Deallocate a jar's data if not already done at finalize (if jar owns it)
    subroutine jar_final(jar_instance)
        implicit none
        type(jar), intent(INOUT) :: jar_instance            !> Data jar instance

        if (C_ASSOCIATED(jar_instance%ptr)) then
        if (debug_mode .and. jar_instance%opt == 0) print *, 'DEBUG(jar finalize): freing jar memory, size =', jar_instance%size_elem
        if (debug_mode .and. jar_instance%opt == 1) print *, 'DEBUG(jar finalize): not owner, not freing jar memory, size =', jar_instance%size_elem
        if (jar_instance%opt == 0) call libc_free(jar_instance%ptr)      ! release storage associated with jar if jar owns it
            jar_instance%ptr = C_NULL_PTR
        else
            if(debug_mode) print *,'DEBUG(jar finalize): nothing to free in jar'
        endif
        jar_instance%top       = 0                      ! data jar is now empty (no data written)
        jar_instance%bot       = 0                      ! data jar is now empty (no data to read)
        jar_instance%opt       = 0                      ! reset ownership flag
        jar_instance%size_elem = 0                      ! data jar cannot store data
    end subroutine jar_final


    !> Deallocate a jar's data space (if jar owns it)
    function jar_free(jar_instance) result(status)
        implicit none
        class(jar), intent(INOUT) :: jar_instance           !> Data jar instance
        integer :: status                                   !> 0 if O.K., -1 if nothing to free

        if (C_ASSOCIATED(jar_instance%ptr)) then
            if (debug_mode .and. jar_instance%opt == 0) print *,'DEBUG(jar free): freing jar memory'
            if (debug_mode .and. jar_instance%opt == 1) print *,'DEBUG(jar free): not owner, not freing jar memory'
            if (jar_instance%opt == 0) call libc_free(jar_instance%ptr)    ! release storage associated with jar if jar owns it
            jar_instance%ptr = C_NULL_PTR          ! nullify data pointer to avoid accidents
            status = 0
        else
            if (debug_mode) print *,'DEBUG(jar free): nothing to free in jar'
            status = -1                            ! already freed
        endif
        jar_instance%top       = 0                 ! data jar is now empty (no data written)
        jar_instance%bot       = 0                 ! data jar is now empty (no data to read)
        jar_instance%opt       = 0                 ! reset ownership flag
        jar_instance%size_elem = 0                 ! data jar cannot store data
    end function jar_free

    !> Insert a string into a jar. This is a wrapper on jar_module::jar_put_into, a string-specific function is needed with certain
    !> compilers (nvhpc, aocc).
    function jar_put_string_into(jar_instance, string, position) result(success)
        implicit none
        class(jar),           intent(INOUT)        :: jar_instance  !> Data jar instance
        character(len=*),     intent(IN)           :: string        !> The string we want to insert in the jar
        integer(JAR_ELEMENT), intent(IN), optional :: position  !> Insertion point (1 = start of jar), optional
        logical :: success                                      !> Whether the operation succeeded
        if (present(position)) then
            success = jar_instance % insert(string, int(storage_size(string), kind = 8), position)
        else
            success = jar_instance % insert(string, int(storage_size(string), kind = 8))
        end if
    end function jar_put_string_into

#define IgnoreTypeKindRank object
#define ExtraAttributes , target
    !> Add data to jar at top of jar (or at a specific position)
    function jar_put_into(jar_instance, object, size_bits, position) result(success)
        implicit none
        class(jar), intent(INOUT) :: jar_instance               !> Data jar instance
#include <rmn/IgnoreTypeKindRank.hf>
        integer(JAR_ELEMENT), intent(IN), value    :: size_bits !> Size to insert in number of bits = storage_size(item) * nb_of_items
        integer(JAR_ELEMENT), intent(IN), optional :: position  !> Insertion point (1 = start of jar), optional
        logical :: success                                      !> Whether the operation succeeded

        integer(JAR_ELEMENT) :: size_elem, insertion_pos
        type(C_PTR) :: temp
        integer(JAR_ELEMENT), dimension(:), pointer :: object_as_array, jar_content

        success = .false.
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return

        call C_F_POINTER(jar_instance%ptr, jar_content, [jar_instance%size_elem])   ! pointer to jar data
        if ( present(position) ) then
            insertion_pos = position - 1                                            ! insert starting at "position"
        else
            insertion_pos = jar_instance%top                                        ! insert after data currently in jar
        endif
        if (insertion_pos < 0) return                                               ! invalid insertion position
        if (insertion_pos > jar_instance%top) jar_content(jar_instance%top + 1 : insertion_pos) = 0 ! zero fill skipped portion

        size_elem = (size_bits + storage_size(jar_content(1)) - 1) / storage_size(jar_content(1))
        if (insertion_pos + size_elem > jar_instance%size_elem) return              ! jar would overflow

        temp = C_LOC(object)
        call C_F_POINTER(temp, object_as_array, [size_elem])                        ! make what into an integer array
        jar_content(insertion_pos + 1 : insertion_pos + size_elem) = object_as_array(1 : size_elem)  ! insert into data portion of jar
        jar_instance%top = insertion_pos + size_elem                                ! update top of jar position
        success = .true.

    end function jar_put_into

    !> Extract a string from a jar. This is just a wrapper on jar_module::jar_get_outof, a string-specific version is needed
    !> with certain compilers (aocc, nvhpc)
    function jar_get_string_outof(jar_instance, string, position) result(success)
        implicit none
        class(jar),           intent(INOUT)        :: jar_instance  !> Data jar instance
        character(len=*),     intent(INOUT)        :: string        !> [out] The string we want to extract from the jar
        integer(JAR_ELEMENT), intent(IN), optional :: position  !> Insertion point (1 = start of jar), optional
        logical :: success                                      !> Whether the operation succeeded

        if (present(position)) then
            success = jar_instance % extract(string, int(storage_size(string), kind = 8), position)
        else
            success = jar_instance % extract(string, int(storage_size(string), kind = 8))
        end if
    end function jar_get_string_outof

#define IgnoreTypeKindRank object
#define ExtraAttributes , target
    !> Get data from jar at current (or specific position)
    function jar_get_outof(jar_instance, object, size_bits, position) result(success)
        implicit none
        class(jar), intent(INOUT) :: jar_instance               !> Data jar instance
#include <rmn/IgnoreTypeKindRank.hf>
        integer(C_INT64_T),   intent(IN), value :: size_bits    !> Size to insert in bits = storage_size(item) * nb_of_items
        integer(JAR_ELEMENT), intent(IN), optional :: position  !> Extraction point (1 = start of jar), optional
        logical :: success                                      !> Whether the operation succeeded

        integer(JAR_ELEMENT) :: size_elem_down, size_elem_up
        integer(JAR_ELEMENT) :: extraction_pos
        type(C_PTR) :: temp
        integer(JAR_ELEMENT), dimension(:), pointer :: object_as_array, jar_content

        success = .false.
        if (.not. C_ASSOCIATED(jar_instance%ptr)) return

        call C_F_POINTER(jar_instance%ptr, jar_content, [jar_instance%size_elem])   ! pointer to jar data
        if ( present(position) ) then
            extraction_pos = position - 1                                           ! insert at "position"
        else
            extraction_pos = jar_instance%bot                                       ! insert after data currently in jar
        endif
        if (extraction_pos < 0) return                                              ! invalid extraction position

        size_elem_down = size_bits / storage_size(jar_content(1))                   ! size of object in JAR_ELEMENTs (rounded down)
        size_elem_up   = (size_bits + storage_size(jar_content(1)) - 1) / storage_size(jar_content(1)) ! size of object in JAR_ELEMENTs (rounded up)
        if (extraction_pos + size_elem_up > jar_instance%top) return                ! insufficient data in jar

        temp = C_LOC(object)
        call C_F_POINTER(temp, object_as_array, [size_elem_down])                   ! object as an array of JAR_ELEMENT
        object_as_array(1 : size_elem_down) = jar_content(extraction_pos + 1 : extraction_pos + size_elem_down)        ! copy from data portion of jar

        if(size_elem_up > size_elem_down)then
            ! insert extra bytes. Need to cast everything as an array of bytes to get the exact size we want to copy
            block
                integer(C_INT64_T)   :: size_byte, size_byte_down
                integer(JAR_ELEMENT) :: byte_pos
                integer(C_INT8_T)    :: byte
                integer(C_INT8_T), dimension(:), pointer :: jar_bytes, object_bytes

                size_byte      = size_bits / storage_size(byte)                     ! size in bytes (rounded down, if number of bits doesn't fit)
                size_byte_down = size_elem_down * (storage_size(jar_content(1)) / storage_size(byte)) ! number of bytes already inserted
                byte_pos = extraction_pos * (storage_size(jar_content(1)) / storage_size(byte)) ! extraction_pos in bytes
                temp = C_LOC(object)
                call C_F_POINTER(temp, object_bytes, [size_byte])                   ! object as a byte array
                call C_F_POINTER(jar_instance%ptr, jar_bytes, [size_byte])          ! jar data as a byte array
                object_bytes(size_byte_down + 1 : size_byte) = jar_bytes(byte_pos + size_byte_down + 1 : byte_pos + size_byte)
            end block
        endif

        jar_instance%bot = extraction_pos + size_elem_up                            ! update jar bottom position
        success = .true.

    end function jar_get_outof

end module rmn_jar

!> \file
!> \brief circular buffer Fortran module (object oriented)
module rmn_circular_buffer_module
    use cb_common_module
    use rmn_common
    use rmn_libc
    implicit none
    include 'circular_buffer.inc'

    private

    public :: CB_KIND_CHAR, CB_KIND_INTEGER_4, CB_KIND_INTEGER_8, CB_KIND_REAL_4, CB_KIND_REAL_8, CB_DATA_ELEMENT, CB_DATA_ELEMENT_KIND
    public :: error_code_to_string
    public :: cb_stats

    !> \brief User-defined type for a one-producer, one-consumer circular queue. See #circular_buffer
    type, public :: rmn_circular_buffer
        !> \private
        private
        type(C_PTR) :: p = C_NULL_PTR     !< Pointer to internal struct of the #circular_buffer
        logical :: is_shared = .false.    !< Whether the circular_buffer is stored in shared memory
        logical :: is_owner  = .false.    !< Whether the circular_buffer owns the memory where it resides (i.e. whether it is responsible for freeing it)
    contains

        !> @{ \name Create/delete
        procedure :: create_local_bytes         !< \copydoc rmn_circular_buffer_module::create_local_bytes
        procedure :: create_shared_bytes        !< \copydoc rmn_circular_buffer_module::create_shared_bytes
        procedure :: create_from_pointer_bytes  !< \copydoc rmn_circular_buffer_module::create_from_pointer_bytes
        procedure :: create_from_other          !< \copydoc rmn_circular_buffer_module::create_from_other
        GENERIC   :: create_bytes => create_local_bytes, create_shared_bytes, create_from_pointer_bytes, create_from_other  !< generic create circular buffer
        procedure :: delete !< \copydoc rmn_circular_buffer_module::delete
        !> @}

        !> @{ \name Read/write data
        procedure :: peek             !< \copydoc rmn_circular_buffer_module::peek
        procedure :: get              !< \copydoc rmn_circular_buffer_module::get
        procedure :: put              !< \copydoc rmn_circular_buffer_module::put
        procedure :: cancel_get       !< \copydoc rmn_circular_buffer_module::cancel_get
        procedure :: cancel_put       !< \copydoc rmn_circular_buffer_module::cancel_put
        procedure :: wait_for_data    !< \copydoc rmn_circular_buffer_module::wait_for_data
        !> @}

        !> @{ \name Query metadata
        procedure :: get_num_spaces   !< \copydoc rmn_circular_buffer_module::get_num_spaces
        procedure :: get_num_elements !< \copydoc rmn_circular_buffer_module::get_num_elements
        procedure :: get_capacity     !< \copydoc rmn_circular_buffer_module::get_capacity
        !> @}

        !> @{ \name Check validity
        procedure :: is_valid             !< \copydoc rmn_circular_buffer_module::is_valid
        procedure :: get_integrity_status !< \copydoc rmn_circular_buffer_module::get_integrity_status
        !> @}

        !> @{ \name Debug info
        procedure :: print_header         !< \copydoc rmn_circular_buffer_module::print_header
        procedure :: print_stats          !< \copydoc rmn_circular_buffer_module::print_stats
        procedure, pass :: get_stats => circular_buffer_get_stats    !< \copydoc rmn_circular_buffer_module::circular_buffer_get_stats
        !> @}

        !> @{ \name Static functions
        procedure, nopass :: error_code_to_string !< \copydoc rmn_circular_buffer_module::error_code_to_string
        !> @}

    end type rmn_circular_buffer

contains

    !> Check integrity of the circular buffer: the pointer is valid and the integrity check on the underlying C struct passes.
    !> \sa CB_check_integrity
    pure function get_integrity_status(this) result(integrity_status)
        implicit none
        class(rmn_circular_buffer), intent(in) :: this    !< rmn_circular_buffer instance
        integer(C_INT) :: integrity_status !< A status code that indicate a specific error, if there is one
        integrity_status = CB_check_integrity(this % p)
    end function get_integrity_status

    !> Check integrity of the circular buffer: the pointer is valid and the integrity check on the underlying C struct passes.
    !> \sa CB_check_integrity
    !> \return Wether the circular buffer passes all checks
    pure function is_valid(this)
        implicit none
        class(rmn_circular_buffer), intent(IN) :: this !< rmn_circular_buffer instance
        logical :: is_valid
        is_valid = (this % get_integrity_status() == CB_SUCCESS)
    end function is_valid

    !> Print the C struct header of this circular buffer. See CB_print_header
    !> \sa CB_print_header
    subroutine print_header(this)
        implicit none
        class(rmn_circular_buffer), intent(INOUT) :: this
        call CB_print_header(this % p)
    end subroutine print_header

    !> Create a circular buffer in local memory
    !> ```
    !> type(rmn_circular_buffer) :: cb
    !> type(C_PTR) :: p
    !> 
    !> p = cb % create_local_bytes(num_bytes)
    !> p = cb % create(num_bytes)
    !> ```
    !> \sa CB_create_bytes
    function create_local_bytes(this, num_bytes) result(success)
        implicit none
        class(rmn_circular_buffer), intent(INOUT)     :: this      !< rmn_circular_buffer instance
        integer(C_SIZE_T),      intent(IN), value :: num_bytes !< size in bytes of the circular buffer
        logical :: success                                     !< Whether the created buffer is valid
        integer(C_INT) :: status

        this % p = CB_create_bytes(num_bytes)
        this % is_owner = .true.
        this % is_shared = .false.
        status = this % get_integrity_status()
        success = (status == CB_SUCCESS)
        if (.not. success) print '(A, A)', 'ERROR: local creation failed: ', error_code_to_string(status)
    end function create_local_bytes

    !> Create a circular buffer in shared memory
    !> ```
    !> type(rmn_circular_buffer) :: cb
    !> type(C_PTR) :: p
    !> 
    !> p = cb % create_shared_bytes(shmid, num_bytes)
    !> p = cb % create(shmid, num_bytes)
    !> ```
    !> \sa CB_create_shared_bytes()
    function create_shared_bytes(this, shmid, num_bytes) result(success)
        implicit none
        class(rmn_circular_buffer), intent(INOUT)     :: this   !< rmn_circular_buffer instance
        integer(C_INT),         intent(OUT)       :: shmid  !< identifier of shared memory area (see man shmget)
        integer(C_SIZE_T),      intent(IN), value :: num_bytes !< size in 32 bit elements of the circular buffer
        logical :: success                                  !< Whether the created buffer is valid
        integer(C_INT) :: status
        this % p = CB_create_shared_bytes(shmid, num_bytes)
        this % is_owner = .false.
        this % is_shared = .true.
        status = this % get_integrity_status()
        success = (status == CB_SUCCESS)
        if (.not. success) print '(A, A)', 'ERROR: creation in shared memory failed: ', error_code_to_string(status)
    end function create_shared_bytes

    !> Create a circular buffer from user supplied memory
    !> ```
    !> type(rmn_circular_buffer) :: cb
    !> type(C_PTR) :: p
    !> 
    !> p = cb % create_from_pointer_bytes(ptr, num_bytes)
    !> p = cb % create(ptr, num_bytes)
    !> ```
    !> \sa CB_from_pointer_bytes
    function create_from_pointer_bytes(this, ptr, num_bytes) result(success)
        implicit none
        class(rmn_circular_buffer), intent(INOUT) :: this      !< rmn_circular_buffer instance
        type(C_PTR), intent(IN), value        :: ptr       !< pointer to user supplied memory
        integer(C_SIZE_T), intent(IN), value  :: num_bytes !< size in 32 bit elements of the circular buffer
        logical :: success                                 !< Whether the created buffer is valid
        integer(C_INT) :: status

        this % p = CB_from_pointer_bytes(ptr, num_bytes)
        this % is_owner = .false.
        this % is_shared = .false.

        status = this % get_integrity_status()
        success = (status == CB_SUCCESS)
        if (.not. success) print '(A, A)', 'ERROR: creation from pointer failed: ', error_code_to_string(status)
    end function create_from_pointer_bytes

    !> Create a circular buffer from address of another circular buffer
    !> ```
    !> type(rmn_circular_buffer) :: cb
    !> type(C_PTR) :: p
    !> 
    !> p = cb % create_from_other(ptr)
    !> p = cb % create(ptr)
    !> ```
    function create_from_other(this, ptr) result(success)
        implicit none
        class(rmn_circular_buffer), intent(INOUT) :: this !< rmn_circular_buffer instance
        type(C_PTR), intent(IN), value        :: ptr  !< pointer to user supplied memory
        logical :: success                            !< Whether the resulting buffer is valid
        integer(C_INT) :: status

        this % p = ptr
        this % is_owner = .false.
        this % is_shared = .false.

        status = this % get_integrity_status()
        success = (status == CB_SUCCESS)
        if (.not. success) print '(A, A)', 'ERROR: creation from existing CB failed: ', error_code_to_string(status)
    end function create_from_other
  
    !> Get number of empty element slots available in the buffer
    !> ```
    !> num_integers = cb % get_num_spaces(CB_KIND_INTEGER_4)
    !> ```
    !> \sa CB_get_available_space_bytes
    pure function get_num_spaces(this, type_id) result(num_elements)
        implicit none
        class(rmn_circular_buffer), intent(IN) :: this     !< rmn_circular_buffer instance
        integer, intent(IN)                :: type_id  !< ID of the type of elements we want to fit
        integer(C_INT64_T) :: num_elements             !< Number of empty slots available, -1 if error

        integer            :: type_size
        integer(C_INT64_T) :: num_bytes

        type_size    = get_type_size(type_id)
        num_bytes    = CB_get_available_space_bytes(this % p)
        num_elements = num_bytes / type_size
        if (num_bytes < 0) num_elements = -1
    end function get_num_spaces

    !> Get current number of data elements from the given type stored in the buffer
    !> ```
    !> num_reals = cb % get_num_elements(CB_KIND_REAL_8)
    !> ```
    !> \sa CB_get_available_data_bytes
    pure function get_num_elements(this, type_id) result(num_elements)
        implicit none
        class(rmn_circular_buffer), intent(IN) :: this     !< rmn_circular_buffer instance
        integer, intent(IN)                :: type_id  !< ID of the type of elements we want to fit
        integer(C_INT64_T) :: num_elements             !< Number of (full) data elements stored, -1 if error

        integer :: type_size
        integer(C_INT64_T) :: num_bytes

        type_size = get_type_size(type_id)
        num_bytes = CB_get_available_data_bytes(this % p)
        num_elements = num_bytes / type_size
        if (num_bytes < 0) num_elements = -1
    end function get_num_elements

    !> Get max number of elements this buffer can hold
    !> \sa CB_get_capacity_bytes
    function get_capacity(this, type_id) result(num_elements)
        implicit none
        class(rmn_circular_buffer), intent(INOUT) :: this         !< The circular buffer instance
        integer,                intent(IN)    :: type_id      !< ID of the type of elements we want to fit
        integer(C_INT64_T)                    :: num_elements !< Max number of elements that can fit in the buffer

        num_elements = CB_get_capacity_bytes(this % p) / get_type_size(type_id)
    end function get_capacity

    !> \brief Look at the next elements in this buffer without extracting them
    !> \return .true. if peeking was successful, .false. otherwise
    !> \sa CB_get
#define IgnoreTypeKindRank dest
#define ExtraAttributes , target
    function peek(this, dest, num_elements, type_id, timeout_ms) result(success)
        implicit none
        class(rmn_circular_buffer), intent(INOUT)     :: this         !< The rmn_circular_buffer instance
#include <rmn/IgnoreTypeKindRank.hf>
        integer(C_SIZE_T),      intent(IN), value :: num_elements !< How many elements we want to look at
        integer,                intent(IN), value :: type_id      !< ID of the type of elements we are looking for
        integer, optional,      intent(IN)        :: timeout_ms   !< [optional] Number of milliseconds to wait before failing, (practically) infinity if absent
        logical :: success

        type(C_PTR)    :: temp
        integer        :: type_size
        integer(C_INT) :: status
        integer(C_INT) :: timeout_c

        success   = .false.
        temp      = C_LOC(dest)
        type_size = get_type_size(type_id)
        timeout_c = -1
        if (present(timeout_ms)) timeout_c = timeout_ms

        status = CB_get(this % p, temp, num_elements * type_size, CB_PEEK, timeout_c)
        if (status == 0) success = .true.
    end function peek

    !> Wait until num_elements (of type type_id) are available then extract them into dest
    !> ```
    !> success = cb % get(dest, num_elements, type_id, commit_transaction)
    !> success = cb % get(dest, num_elements, type_id, commit_transaction, timeout_ms = 10)
    !> ```
    !> \sa CB_get()
#define IgnoreTypeKindRank dest
#define ExtraAttributes , target
    function get(this, dest, num_elements, type_id, commit_transaction, timeout_ms) result(success)
        implicit none
        class(rmn_circular_buffer), intent(INOUT)     :: this               !< rmn_circular_buffer instance
#include <rmn/IgnoreTypeKindRank.hf>
        integer(C_SIZE_T),      intent(IN), value :: num_elements       !< number of elements to extract
        integer,                intent(IN), value :: type_id            !< ID of the type of elements we're looking for
        logical,                intent(IN), value :: commit_transaction !< Whether to update the buffer (ie _extract_ the data)
        integer, optional,      intent(IN)        :: timeout_ms         !< [optional] Number of milliseconds to wait before failing, (practically) infinity if absent
        logical :: success                                              !< Whether the operation was successful

        integer        :: type_size
        integer(C_INT) :: operation, status, timeout_c
        type(C_PTR)    :: temp

        success   = .false.
        temp      = C_LOC(dest)
        type_size = get_type_size(type_id)
        operation = CB_NO_COMMIT
        if (commit_transaction) operation = CB_COMMIT
        timeout_c = -1
        if (present(timeout_ms)) timeout_c = timeout_ms

        status = CB_get(this % p, temp, num_elements * type_size, operation, timeout_c)
        if (status == 0) success = .true.
    end function get

    !> Wait until num_elements of type type_id are available, then insert from src array
    !> ```
    !> success = cb % put(src, num_elements, type_id, commit_transaction)
    !> success = cb % put(src, num_elements, type_id, commit_transaction, timeout_ms = 1)
    !> success = cb % put(src, num_elements, type_id, commit_transaction, thread_safe = .true.)
    !> ```
    !> \sa CB_put()
#define IgnoreTypeKindRank src
#define ExtraAttributes , target
    function put(this, src, num_elements, type_id, commit_transaction, timeout_ms, thread_safe) result(success)
        implicit none
        class(rmn_circular_buffer), intent(INOUT)     :: this               !< rmn_circular_buffer instance
#include <rmn/IgnoreTypeKindRank.hf>
        integer(C_SIZE_T),      intent(IN), value :: num_elements       !< number of tokens to insert from src
        integer,                intent(IN), value :: type_id            !< ID of the type of elements we're looking for
        logical,                intent(IN), value :: commit_transaction !< Whether to make the inserted data immediately available
        integer, optional,      intent(IN)        :: timeout_ms         !< [optional] Number of milliseconds to wait before failing, (practically) infinity if absent
        logical, optional,      intent(IN)        :: thread_safe        !< [optional] Whether to perform the operation in a thread-safe way (.false. by default)
        logical :: success                                              !< Whether the operation was successful

        integer        :: type_size
        integer(C_INT) :: operation
        integer(C_INT) :: status
        type(C_PTR)    :: temp
        integer(C_INT) :: timeout_c, thread_safe_val

        success   = .false.
        temp      = C_LOC(src)
        type_size = get_type_size(type_id)
        operation = CB_NO_COMMIT
        if (commit_transaction) operation = CB_COMMIT

        timeout_c = -1
        if (present(timeout_ms)) timeout_c = timeout_ms

        thread_safe_val = 0
        if (present(thread_safe)) then
            if (thread_safe) thread_safe_val = 1
        end if

        status = CB_put(this % p, temp, num_elements * type_size, operation, timeout_c, thread_safe_val)
        if (status == 0) success = .true.
        if (.not. success) then
            print '(A, A, A, I8, A, I3, A, I8, A, I2, A, F12.2)', 'CB_put failed because ', error_code_to_string(status), ': ',       &
                num_elements, ' elements of type ', type_id, ', timeout of ', timeout_c, ' ms, thread_safe = ',             &
                thread_safe_val, ', buffer size (kB) ', real(this % get_capacity(CB_KIND_CHAR), kind=4) / 1024.0
        end if
    end function put

    !> \copydoc CB_cancel_get
    subroutine cancel_get(this)
        implicit none
        class(rmn_circular_buffer), intent(INOUT) :: this !< rmn_circular_buffer instance

        integer(C_INT) :: c_status
        c_status = CB_cancel_get(this % p)
    end subroutine cancel_get

    !> \copydoc CB_cancel_put
    subroutine cancel_put(this)
        implicit none
        class(rmn_circular_buffer), intent(INOUT) :: this !< rmn_circular_buffer instance

        integer(C_INT) :: c_status
        c_status = CB_cancel_put(this % p)
    end subroutine cancel_put

    !> Wait until a certain amount of data is available in the buffer
    function wait_for_data(this, num_bytes, timeout_ms) result(success)
        implicit none
        class(rmn_circular_buffer), intent(inout) :: this
        integer(C_SIZE_T),      intent(in)    :: num_bytes
        integer, optional,      intent(in)    :: timeout_ms  !< [optional] Number of milliseconds to wait before failing, (practically) infinity if absent
        logical :: success

        integer(C_INT32_T) :: timeout_c
        integer(C_INT64_T) :: status

        success = .false.

        timeout_c = -1
        if (present(timeout_ms)) timeout_c = timeout_ms

        status = CB_wait_data_available_bytes(this % p, num_bytes, timeout_c)

        if (status == 0) success = .true.
        if (.not. success) then
            print '(A, A, A, I8, A, I8, A, F12.2)', 'CB_wait_data_available failed because ',                              &
                error_code_to_string(int(status, kind = int32)), ': ',                                                     &
                num_bytes, ' bytes, timeout of ', timeout_c, ' ms, buffer size (kB) ',                                     &
                real(this % get_capacity(CB_KIND_CHAR), kind=4) / 1024.0
        end if
    end function wait_for_data

    !> Delete the buffer by disassociating from the underlying C pointer and, if applicable, releasing the memory
    !> \return .true. if there were no issues, .false. if detaching the shared memory failed
    !> \sa CB_detach_shared
    function delete(this) result(status)
        implicit none
        class(rmn_circular_buffer), intent(INOUT) :: this
        logical :: status

        status = .true.
        if (this % is_owner) then
            call libc_free(this % p)
        else if (this % is_shared) then
            status = CB_detach_shared(this % p) .ge. 0
        end if
        this % p = C_NULL_PTR
    end function delete

    !> Print usage statistics collected during the lifetime of the buffer
    !> \sa CB_print_stats
    subroutine print_stats(this, buffer_id, with_header)
        implicit none
        class(rmn_circular_buffer), intent(in) :: this        !< The CB we want to print
        integer(C_INT), intent(IN), value  :: buffer_id   !< ID of the buffer (will be printed at the start of the data line)
        logical,        intent(IN), value  :: with_header !< Whether to print a line describing the data

        integer(C_INT) :: with_header_c
        with_header_c = 0
        if (with_header) with_header_c = 1

        if (this % is_valid()) call CB_print_stats(this % p, buffer_id, with_header_c)
    end subroutine print_stats

    !> Retrieve a pointer to the stats struct stored with this rmn_circular_buffer instance.
    subroutine circular_buffer_get_stats(this, stats)
        implicit none
        class(rmn_circular_buffer),  intent(in)  :: this  !< CB instance
        type(cb_stats), pointer, intent(out) :: stats !< A pointer to the stats struct from this CB instance

        type(C_PTR) :: c_stats
        c_stats = CB_get_stats(this % p)
        call c_f_pointer(c_stats, stats)
    end subroutine circular_buffer_get_stats

    !> Get a human-readable string for a certain error code
    function error_code_to_string(error_code) result(error_string)
        implicit none
        integer(C_INT), intent(in) :: error_code !< The code we want to translate into a string
        character(len=:), allocatable :: error_string

        character(len=1), dimension(:), pointer :: tmp_string
        type(C_PTR) :: c_error_string
        
        integer :: i, num_char
        
        c_error_string = CB_error_code_to_string(error_code)
        call c_f_pointer(c_error_string, tmp_string, [8192])

        i = 1
        do 
            if (tmp_string(i) == c_null_char) exit
            i = i + 1
        end do
        num_char = i - 1

        allocate(character(len=num_char) :: error_string)
        error_string = transfer(tmp_string(1:num_char), error_string)
    end function error_code_to_string

end module rmn_circular_buffer_module

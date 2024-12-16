module test_circular_buffer_module
    implicit none
contains

subroutine shared_mem_test()
  use ISO_C_BINDING
  use iso_fortran_env
  use ioserver_mpi
  use app

  use circular_buffer_module
  implicit none

  integer(C_SIZE_T), parameter :: BUFFER_SIZE_BYTE = 128 * 4
  ! integer, parameter :: NUM_BUFFER_ELEMENTS = 128
  ! integer, parameter :: NUM_DATA_ELEMENTS = 10
  integer, parameter :: NPTEST = 200
  integer(C_SIZE_T), parameter :: STEP_SIZE = 5

  integer(MPI_ADDRESS_KIND), parameter :: WINDOW_SIZE = 1024 * 1024

  type(circular_buffer) :: buffer_a, buffer_b
  type(C_PTR)           :: shmem_ptr_a, shmem_ptr_b
  logical               :: success, success_b

  integer, dimension(NPTEST) :: local_data, received_data, source_data

  type(C_PTR) :: base_mem_ptr, target_mem_ptr
  integer(KIND=MPI_ADDRESS_KIND) :: target_size

  integer :: my_rank, num_procs
  integer :: i, errors, tmp_errors
  integer :: window
  integer :: target_disp_unit
  integer :: target_proc, source_proc
  integer(C_INT64_T) :: capacity

  type(App_Timer) :: timer

  integer :: ierr

  errors = 0

  ! Initialize MPI
  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, num_procs, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)

  if (num_procs < 2) then
    print *, 'This test needs at least 2 processes'
    errors = 1
    error stop 1
  end if

  call timer % create()

  target_proc = mod(my_rank + 1, num_procs)             ! next in the "ring"
  source_proc = mod(my_rank + num_procs - 1, num_procs) ! previous in the "ring"

!  print *, 'This is PE', my_rank + 1, ' of', num_procs

  ! Allocate MPI window in shared memory
  call MPI_Win_allocate_shared(WINDOW_SIZE, 4, MPI_INFO_NULL, MPI_COMM_WORLD, base_mem_ptr, window, ierr)
  call MPI_Win_shared_query(window, target_proc, target_size, target_disp_unit, target_mem_ptr, ierr)  ! get my victim's base address

  ! Initialize local data
  call init_array(local_data, my_rank)
  call init_array(source_data, source_proc)
  received_data(:) = -1

  shmem_ptr_a  = transfer(base_mem_ptr, C_NULL_PTR)   ! pointer to my circular buffer
  shmem_ptr_b  = transfer(target_mem_ptr, C_NULL_PTR) ! pointer to my target's circular buffer
  success = buffer_a % create_bytes(shmem_ptr_a, BUFFER_SIZE_BYTE) ! create my circular buffer

  capacity = buffer_a % get_capacity(CB_KIND_INTEGER_4)
  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------
  success_b = buffer_b % create_bytes(shmem_ptr_b)                      ! point to target's circular buffer

  if (.not. success .or. .not. buffer_a % is_valid() .or. .not. success_b .or. .not. buffer_b % is_valid()) then
    print *, 'Buffer initialisation failed ', error_code_to_string(buffer_a % get_integrity_status()), error_code_to_string(buffer_b % get_integrity_status())
    errors = errors + 1
    error stop 1
  end if

  if ((buffer_a % get_num_elements(CB_KIND_INTEGER_4) .ne. 0) .or. (buffer_b % get_num_elements(CB_KIND_INTEGER_4) .ne. 0)) then
    print *, 'GOT ERROR 0'
    errors = errors + 1
    error stop 1
  end if

  if (buffer_a % get_num_spaces(CB_KIND_INTEGER_4) .ne. buffer_b % get_num_spaces(CB_KIND_INTEGER_4)) then
    print *, 'GOT ERROR: buffer spaces are ', buffer_a % get_num_spaces(CB_KIND_INTEGER_4), buffer_b % get_num_spaces(CB_KIND_INTEGER_4)
    errors = errors + 1
    error stop 1
  end if


  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  print *, 'num elem before put (no commit) = ', buffer_b % get_num_elements(CB_KIND_CHAR)
  success = buffer_b % put(local_data, STEP_SIZE, CB_KIND_INTEGER_4, .false.) ! inject data into target's circular buffer
  if (.not. success) then
    print *, 'GOT ERROR: unable to put data in buffer (without committing)'
    error stop 1
  end if
  print *, 'num elem after put (no commit)= ', buffer_b % get_num_elements(CB_KIND_CHAR)

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  if (buffer_a % get_num_elements(CB_KIND_INTEGER_4) .ne. 0) then
    print *, 'GOT ERROR. We did not commit the transaction, but there is data in the buffer!'
    errors = errors + 1
    error stop 1
  end if

  print *, 'num elem before wait (1) = ', buffer_a % get_num_elements(CB_KIND_CHAR)
  success = buffer_a % wait_for_data(1_int64, timeout_ms = 50)
  if (success) then
    print *, 'GOT ERROR. We did not commit the transaction, but the timeout (1) ended successfully!'
    print *, 'num elem after wait (1) = ', buffer_a % get_num_elements(CB_KIND_CHAR)
    errors = errors + 1
    error stop 1
  end if

  print *, 'num elem before wait (2) = ', buffer_a % get_num_elements(CB_KIND_CHAR)

  success = buffer_a % wait_for_data(1_int64, timeout_ms = 0)
  if (success) then
    print *, 'GOT ERROR. We did not commit the transaction, but the timeout (2) ended successfully!'
    errors = errors + 1
    error stop 1
  end if
  print *, 'num elem after wait (2) = ', buffer_a % get_num_elements(CB_KIND_CHAR)

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  success = buffer_b % put(local_data, 0_8, CB_KIND_INTEGER_4, .true.)

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  if (buffer_a % get_num_elements(CB_KIND_INTEGER_4) < STEP_SIZE) then
    print *, 'GOT ERROR. We just committed the previous transaction, so there should at least that many elements: ', STEP_SIZE
    errors = errors + 1
    error stop 1
  end if

  success = buffer_a % peek(received_data, STEP_SIZE, CB_KIND_INTEGER_4)

  if ((buffer_a % get_num_elements(CB_KIND_INTEGER_4) < STEP_SIZE) .or. (.not. success)) then
    print *, 'GOT ERROR. We just peeked at the buffer, but the resulting number of elements is wrong!', &
             buffer_a % get_num_elements(CB_KIND_INTEGER_4), STEP_SIZE
    errors = errors + 1
    error stop 1
  end if

  success = buffer_a % get(received_data, STEP_SIZE, CB_KIND_INTEGER_4, .false.)
  if ((buffer_a % get_num_elements(CB_KIND_INTEGER_4) .ne. 0) .or. (.not. success)) then
    print *, 'GOT ERROR. We just read the data, but it looks like the buffer is *not* empty', buffer_a % get_num_elements(CB_KIND_INTEGER_4)
    errors = errors + 1
    error stop 1
  end if

  if (buffer_a % get_num_spaces(CB_KIND_INTEGER_4) > capacity - STEP_SIZE) then
    print *, 'GOT ERROR. We only read the data without extracting it. The space should not be available'
    errors = errors + 1
    error stop 1
  end if

  success = buffer_a % get(received_data, 0_8, CB_KIND_INTEGER_4, .true.)
  if ((buffer_a % get_num_elements(CB_KIND_INTEGER_4) .ne. 0) .or. (buffer_a % get_num_spaces(CB_KIND_INTEGER_4) .ne. capacity) .or. (.not. success)) then
    print *, 'GOT ERROR. Buffer should be completely empty'
    errors = errors + 1
  end if

  print *, my_rank, 'Got to this point'

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  success = buffer_b % put(local_data, STEP_SIZE, CB_KIND_INTEGER_4, .true.) ! inject data into target's circular buffer

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  success = buffer_a % get(received_data, STEP_SIZE - 1, CB_KIND_INTEGER_4, .true.) ! get from my own buffer (put there by source_proc)

  if (.not. all(received_data(1:STEP_SIZE - 2) == source_data(1:STEP_SIZE - 2))) then
    print *, 'GOT ERROR, data put directly in buffer by neighbor process (first call)'
    errors = errors + 1
  end if

  success = buffer_a % get(received_data(STEP_SIZE), 1_8, CB_KIND_INTEGER_4, .true.)  ! get from my own buffer (remainder of what was put)

  if (.not. all(received_data(1:STEP_SIZE - 2) == source_data(1:STEP_SIZE - 2))) then
    print *, 'GOT ERROR, data put directly in buffer by neighbor process (second call)'
    errors = errors + 1
  end if

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr) ! we want no interference from further down
  !--------------------------------------

  do i = 1, NPTEST, STEP_SIZE  ! ring test with wraparound , make sure NPTEST > size of circular buffer
    if(my_rank == 0) then
      success = buffer_b % put(local_data(i) , STEP_SIZE, CB_KIND_INTEGER_4, .true.) ! send to next in ring
      success = buffer_a % get(received_data(i), STEP_SIZE, CB_KIND_INTEGER_4, .true.) ! then get from previous in ring

      if (.not. all(local_data(i:i + STEP_SIZE - 1) == received_data(i:i + STEP_SIZE - 1))) then
        print *, 'GOT ERROR in ring data'
        errors = errors + 1
      end if

    else
      success = buffer_a % get(received_data(i) , STEP_SIZE, CB_KIND_INTEGER_4, .true.) ! get from previous in ring
      success = buffer_b % put(received_data(i) , STEP_SIZE, CB_KIND_INTEGER_4, .true.) ! pass to next in ring
    endif

  enddo
  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  if ((buffer_a % get_num_elements(CB_KIND_INTEGER_4) .ne. 0) .or. (buffer_b % get_num_elements(CB_KIND_INTEGER_4) .ne. 0)) then
    print *, 'GOT ERROR, there is some data left after the entire ring transmission is over'
    errors = errors + 1
  end if

  tmp_errors = errors
  call MPI_Reduce(tmp_errors, errors, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

  if(my_rank == 0) then  ! check that we got back what we sent
    if(errors > 0) then
      print *, 'RING ERRORS: ', errors
      print 3,received_data(1:NPTEST)
    else
      print *, 'Shared memory circular buffer test has succeeded'
    end if
3   format(25I6)
  endif

  success = buffer_a % delete()
  success = buffer_b % delete()

  call timer % delete()

  call MPI_Win_free(window, ierr)
  call MPI_Finalize(ierr)

  if (errors > 0) error stop 1

end subroutine shared_mem_test

subroutine init_array(array, rank)
  implicit none

  integer, dimension(:), intent(out) :: array
  integer, intent(in) :: rank

  integer :: i
  do i = 1, size(array)
    array(i) = (rank + 1) * 10000 + i
  end do
end subroutine init_array

end module test_circular_buffer_module

program test_circular_buffer
  use test_circular_buffer_module
  implicit none

  call shared_mem_test()

end program test_circular_buffer

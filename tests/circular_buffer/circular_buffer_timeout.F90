program circular_buffer_timeout
  use app
  use mpi
  use rmn_circular_buffer_module
  use rmn_common
  implicit none

  ! integer, parameter :: CONSUMER_RANK = 0
  integer, parameter :: PRODUCER_RANK = 1
  integer(MPI_ADDRESS_KIND), parameter :: CB_SIZE_BYTES = 1024
  integer(C_INT64_T), parameter :: TEST_VAL = 123454321

  integer :: rank, size, ierr
  type(rmn_circular_buffer) :: cb
  type(C_PTR) :: cb_memory
  logical :: success
  type(App_Timer) :: timer

  integer(C_INT64_T) :: i
  integer(C_INT64_T) :: result

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

  if (size .ne. 2) then
    print *, 'ERROR. This test must use exactly 2 processes'
    error stop 1
  end if

  call timer % create()

  cb_memory = app_allocate_shared(CB_SIZE_BYTES, MPI_COMM_WORLD)

  if (rank == 0) then
    success = cb % create_bytes(cb_memory, CB_SIZE_BYTES)
    if (.not. success) then
      print *, 'ERROR: Unable to create CB from root'
      error stop 1
    end if
  end if

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  if (rank .ne. 0) then
    success = cb % create_bytes(cb_memory)
    if (.not. success) then
      print *, 'ERROR: Unable to create CB from non-root PE'
      error stop 1
    end if
  end if

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  if (rank == PRODUCER_RANK) then
    call sleep_us(100000)
    success = cb % put(TEST_VAL, 1_8, CB_KIND_INTEGER_8, .true.)

  else
    success = cb % peek(result, 1_8, CB_KIND_INTEGER_8, timeout_ms = 0)
    success = cb % get(result, 1_8, CB_KIND_INTEGER_8, .true., timeout_ms = 4) .or. success
    if (success) then
      print *, 'ERROR: Get operation should have timed out!'
      error stop 1
    end if

    success = cb % get(result, 1_8, CB_KIND_INTEGER_8, .true.)
    if (.not. success .or. result .ne. TEST_VAL) then
      print *, 'ERROR: Unable to retrieve correct test value!'
    end if
  end if

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  ! Fill buffer
  if (rank == PRODUCER_RANK) then
    success = .true.
    do i = 1, cb % get_capacity(CB_KIND_INTEGER_8)
      success = cb % put(TEST_VAL, 1_8, CB_KIND_INTEGER_8, .true., timeout_ms=1) .and. success
    end do
    if (.not. success) then
      print *, 'ERROR: Unable to fill CB'
      error stop 1
    end if
  end if

  !--------------------------------------
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  !--------------------------------------

  if (rank == PRODUCER_RANK) then
    success = cb % put(TEST_VAL, 1_8, CB_KIND_INTEGER_8, .true., timeout_ms=1)
    success = cb % put(TEST_VAL, 1_8, CB_KIND_INTEGER_8, .true., timeout_ms=1) .or. success
    success = cb % put(TEST_VAL, 1_8, CB_KIND_INTEGER_8, .true., timeout_ms=1) .or. success
    if (success) then
      print *, 'ERROR: Should not be able to put anything in the buffer anymore'
      error stop 1
    end if

    success = cb % put(TEST_VAL, 1_8, CB_KIND_INTEGER_8, .true., timeout_ms = -1)
    if (.not. success) then
      print *, 'ERROR: Should have been able to put data by now!'
      error stop 1
    end if
  else
    call sleep_us(100000)
    success = cb % get(result, 1_8, CB_KIND_INTEGER_8, .true.)
  end if

  call timer % delete()
  call MPI_Finalize(ierr)

  if (rank == 0) print *, 'CB timeout test successful'

end program circular_buffer_timeout

module circular_buffer_concurrent_module
  use app
  use mpi
  use rmn_common
  use rmn_circular_buffer_module
  implicit none

  integer(MPI_ADDRESS_KIND), parameter :: CB_SIZE_BYTES = 1000
  integer, parameter :: NUM_ENTRIES = 100

end module circular_buffer_concurrent_module

program circular_buffer_concurrent
  use circular_buffer_concurrent_module
  implicit none

  integer :: rank, size, ierr
  integer :: i_entry, i_pe
  type(rmn_circular_buffer) :: cb
  type(C_PTR) :: cb_memory
  logical :: success
  integer :: val, sum, expected_sum

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)

  if (size < 3) then
    print *, 'ERROR. This test must use at the very least 3 processes'
    error stop 1
  end if

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

  if (rank .ne. 0) then
    do i_entry = 1, NUM_ENTRIES
      success = cb % put(rank, 1_8, CB_KIND_INTEGER_4, .true., thread_safe = .true.)
      if (.not. success) then
        print *, 'ERROR putting data in CB from rank ', rank
        error stop 1
      end if
    end do

    !--------------------------------------
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !--------------------------------------

    success = cb % put(rank, 1_8, CB_KIND_INTEGER_4, .false., thread_safe = .true.)
    if (success) then
      print *, 'ERROR: That operation was supposed to fail...', rank
      error stop 1
    end if

  else
    expected_sum = 0
    do i_pe = 1, size - 1
      expected_sum = expected_sum + i_pe
    end do
    expected_sum = expected_sum * NUM_ENTRIES

    sum = 0
    do i_entry = 1, NUM_ENTRIES
      do i_pe = 1, size - 1
        success = cb % get(val, 1_8, CB_KIND_INTEGER_4, .true.)
        if (.not. success) then
          print *, 'ERROR retrieving data from CB'
          error stop 1
        end if
        sum = sum + val
      end do
    end do

    !--------------------------------------
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    !--------------------------------------

    if (sum .ne. expected_sum) then
      print *,'ERROR: Got the wrong sum at the end', sum
      print *, 'expected ', expected_sum
      error stop 1
    end if

    print *, 'All tests successful'

  end if
  

  call MPI_Finalize(ierr)
end program circular_buffer_concurrent


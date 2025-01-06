module cb_single_test_procudures
  implicit none
contains

subroutine basic_test()
  use rmn_common
  use rmn_circular_buffer_module
  implicit none

  integer(C_SIZE_T), parameter :: BUFFER_SIZE_BYTES = 5001 * 4;

  integer, parameter :: NUM_ELEM = BUFFER_SIZE_BYTES / 4 * 3;
  integer(C_SIZE_T), parameter :: STEP_SIZE = 4;

  type(rmn_circular_buffer) :: circ_buffer
  integer, dimension(NUM_ELEM) :: local, local_out
  logical            :: success
  integer            :: i
  integer(C_INT64_T) :: num_elem_in, num_elem_free
  integer            :: num_error
  integer(C_INT64_T) :: buffer_limit

  num_error = 0
  do i = 1, size(local)
    local(i) = i
  enddo

  local_out(:) = -1
  success = circ_buffer % create_bytes(BUFFER_SIZE_BYTES)

  if (.not. success) then
    num_error = num_error + 1
    print *, 'AAAAHHHHHH Could not create buffer'
    return
  end if

  buffer_limit = circ_buffer % get_num_spaces(CB_KIND_INTEGER_4)

!  call circ_buffer % print_header()

  ! do i = 1, NUM_ELEM, STEP_SIZE
  do i = 1, 2, STEP_SIZE
    success = circ_buffer % put(local(i), STEP_SIZE, CB_KIND_INTEGER_4, .true.)
    num_elem_in = circ_buffer % get_num_elements(CB_KIND_INTEGER_4)

    if (num_elem_in .ne. STEP_SIZE) then
      num_error = num_error + 1
      print *, 'Wrong number of elements in buffer after adding data', num_elem_in, STEP_SIZE
    end if

    num_elem_in = circ_buffer % get_num_elements(CB_KIND_CHAR)
    if (num_elem_in .ne. STEP_SIZE * 4) then
      num_error = num_error + 1
      print *, 'Wrong number of elements (char) in buffer after adding data', num_elem_in, STEP_SIZE * 4
    end if

    num_elem_in = circ_buffer % get_num_elements(CB_KIND_INTEGER_8)
    if (num_elem_in .ne. STEP_SIZE / 2) then
      num_error = num_error + 1
      print *, 'Wrong number of elements (int8) in buffer after adding data', num_elem_in, STEP_SIZE / 2
    end if

    num_elem_free = circ_buffer % get_num_spaces(CB_KIND_INTEGER_4)
    if (num_elem_free .ne. buffer_limit - STEP_SIZE) then
      num_error = num_error + 1
      print *, 'Wrong space available in buffer after adding data', num_elem_free, buffer_limit - STEP_SIZE
    end if

    num_elem_free = circ_buffer % get_num_spaces(CB_KIND_CHAR)
    if (num_elem_free .ne. (buffer_limit - STEP_SIZE) * 4) then
      num_error = num_error + 1
      print *, 'Wrong space (char) available in buffer after adding data', num_elem_free, (buffer_limit - STEP_SIZE) * 4
    end if

    num_elem_free = circ_buffer % get_num_spaces(CB_KIND_REAL_8)
    if (num_elem_free .ne. (buffer_limit - STEP_SIZE) / 2) then
      num_error = num_error + 1
      print *, 'Wrong space (real8) available in buffer after adding data', num_elem_free, (buffer_limit - STEP_SIZE) / 2
    end if

    success = circ_buffer % get(local_out(i), STEP_SIZE, CB_KIND_INTEGER_4, .true.)
    num_elem_in = circ_buffer % get_num_elements(CB_KIND_INTEGER_4)
    num_elem_free = circ_buffer % get_num_spaces(CB_KIND_INTEGER_4)

    if (num_elem_in .ne. 0 .or. .not. success) then
      num_error = num_error + 1
      print *, 'We read everything, but there still seems to be some data left!', i
    end if

    if (num_elem_free .ne. buffer_limit) then
      num_error = num_error + 1
      print *, 'We read everything, but available space != buffer limit', i
    end if

    if (.not. all(local(i:i+STEP_SIZE -1) == local_out(i:i+STEP_SIZE -1))) then
      num_error = num_error + 1
      print *, 'Did not read the same data back!'
    end if
  end do

  success = circ_buffer % delete()
  if (.not. success) num_error = num_error + 1

  if (num_error > 0) then
    print *, 'AAAAAhhhhh we found errors when running the test!!', num_error
    error stop 1
  else
    print *, 'All good for basic test (create, put, get, data, space)'
  end if

end subroutine basic_test
end module cb_single_test_procudures

program test_circular_buffer_single_thread
  use cb_single_test_procudures
  implicit none

  call basic_test()

end program test_circular_buffer_single_thread

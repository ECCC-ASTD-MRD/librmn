
interface

function fst24_is_valid(file_name) result(is_valid) bind(C, name = 'fst24_is_valid')
  import C_CHAR, C_INT32_T
  implicit none
  character(C_CHAR), dimension(*), intent(in) :: file_name
  integer(C_INT32_T) :: is_valid
end function fst24_is_valid

function fst24_is_open(file) result(is_open) bind(C, name = 'fst24_is_open')
  import C_PTR, C_INT32_T
  implicit none
  type(C_PTR), intent(in), value :: file
  integer(C_INT32_T) :: is_open
end function fst24_is_open

function fst24_open(file_name, options) result(file_ptr) bind(C, name = 'fst24_open')
  import :: C_PTR, C_CHAR
  implicit none
  character(C_CHAR), dimension(*), intent(in) :: file_name
  character(C_CHAR), dimension(*), intent(in) :: options
  type(C_PTR) :: file_ptr
end function fst24_open

function fst24_close(file) result(could_close) bind(C, name = 'fst24_close')
  import :: C_PTR, C_INT32_T
  implicit none
  type(C_PTR), intent(in), value :: file
  integer(C_INT32_T) :: could_close
end function fst24_close

end interface

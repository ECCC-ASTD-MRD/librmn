
interface

function fst24_open(file_name, options) result(file_ptr) bind(C, name = 'fst24_open')
  import :: C_PTR, C_CHAR
  implicit none
  character(C_CHAR), dimension(*), intent(in) :: file_name
  character(C_CHAR), dimension(*), intent(in) :: options
  type(C_PTR) :: file_ptr
end function fst24_open

pure function fst24_is_open(file_ptr) result(is_open) bind(C, name = 'fst24_is_open')
  import :: C_PTR, C_INT32_T
  implicit none
  type(C_PTR), intent(in), value :: file_ptr
  integer(C_INT32_T) :: is_open
end function fst24_is_open

pure function fst24_get_iun(file) result (iun) bind(C, name = 'fst24_get_iun')
  import :: C_PTR, C_INT32_T
  implicit none
  type(C_PTR), intent(in), value :: file
  integer(C_INT32_T) :: iun
end function fst24_get_iun

function fst24_close(file) result(status) bind(C, name = 'fst24_close')
  import :: C_PTR, C_INT32_T
  implicit none
  type(C_PTR), intent(in), value :: file
  integer(C_INT32_T) :: status
end function fst24_close

end interface

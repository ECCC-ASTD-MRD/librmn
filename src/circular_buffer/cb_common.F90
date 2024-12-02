module cb_common_module
  use iso_c_binding
  implicit none

#include "cb_data.hf"

contains

  !> \brief Compute the size in bytes of a given type ID
  !> \return The size in bytes of the given type ID. -1 if the ID is invalid
  pure function get_type_size(type_id) result(type_size)
    implicit none
    integer, intent(IN) :: type_id
    integer :: type_size
    type_size = -1
    if (type_id < 0) type_size = -type_id
  end function get_type_size

 end module cb_common_module

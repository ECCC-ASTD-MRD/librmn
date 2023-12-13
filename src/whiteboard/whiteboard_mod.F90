module whiteboard_mod
  use ISO_C_BINDING
  use ISO_FORTRAN_ENV
  implicit none
#include <rmn/WhiteBoard_2.hf>
#undef WHITEBOARD_VERSION
contains
  function wb_is_ok(errcode) result(status)
    implicit none
    integer, intent(IN), value :: errcode
    logical :: status
    status = WB_IS_OK(errcode)
  end function
  function wb_is_error(errcode) result(status)
    implicit none
    integer, intent(IN), value :: errcode
    logical :: status
    status = WB_IS_ERROR(errcode)
  end function
  function wb_option_set(options, option) result (status)
    implicit none
    integer, intent(IN), value :: options, option
    logical :: status
    status = WB_OPTION_SET(options,option)
  end function
end module

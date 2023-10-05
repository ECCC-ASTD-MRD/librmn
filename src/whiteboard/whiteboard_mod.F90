module whiteboard_mod
  use ISO_C_BINDING
  use ISO_FORTRAN_ENV
  implicit none
#include <rmn/WhiteBoard_2.hf>
#undef WHITEBOARD_VERSION
end module

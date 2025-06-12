!> \file msg.inc
!> Interfaces for msg functions. This file should stay private, the preferred method to access these
!> functions is through the corresponding module

interface

subroutine msg_init()
   implicit none
end subroutine msg_init

subroutine msg_set_p0only(F_myPE)
   implicit none
   integer,intent(in) :: F_myPE
end subroutine msg_set_p0only

subroutine msg_set_can_write(F_canWrite_L)
   implicit none
   logical,intent(in) :: F_canWrite_L
end subroutine msg_set_can_write

subroutine msg_verbosity(F_msgLevel)
   implicit none
   integer,intent(in) :: F_msgLevel
end subroutine msg_verbosity

subroutine msg_verbosity_get(F_msgLevel)
   implicit none
   integer,intent(out) :: F_msgLevel
 end subroutine msg_verbosity_get

subroutine msg_set_minMessageLevel(F_msgLevel)
   implicit none
   integer,intent(in) :: F_msgLevel
end subroutine msg_set_minMessageLevel

subroutine msg_set_redirect2fileUnit(F_out_or_err,F_fileUnit)
   implicit none
   integer,intent(in) :: F_out_or_err
   integer,intent(in) :: F_fileUnit
end subroutine msg_set_redirect2fileUnit

subroutine msg_toall(F_msgLevel,F_Message)
   implicit none
   integer,intent(in) :: F_msgLevel
   character(len=*),intent(in) :: F_Message
end subroutine msg_toall

subroutine msg(F_msgLevel,F_Message)
   implicit none
   integer,intent(in) :: F_msgLevel
   character(len=*),intent(in) :: F_Message
end subroutine msg

function msg_getUnit(F_msgLevel) result(F_msgUnit)
   implicit none
   integer,intent(in) :: F_msgLevel
   integer :: F_msgUnit
end function msg_getUnit

subroutine msg_getInfo(F_canWrite_L,F_msgLevelMin,F_msgUnit,F_msgFormat_S)
   implicit none
   logical,intent(out) :: F_canWrite_L
   integer,intent(out) :: F_msgLevelMin,F_msgUnit
   character(len=*),intent(out) :: F_msgFormat_S
end subroutine msg_getInfo
 
subroutine msg_buffer_verbosity(F_msgLevel)
   implicit none
   integer,intent(in) :: F_msgLevel
end subroutine msg_buffer_verbosity

subroutine msg_buffer_verbosity_get(F_msgLevel)
   implicit none
   integer,intent(out) :: F_msgLevel
end subroutine msg_buffer_verbosity_get

subroutine msg_buffer_reset()
   implicit none
end subroutine msg_buffer_reset

subroutine msg_buffer_flush()
   implicit none
end subroutine msg_buffer_flush

end interface

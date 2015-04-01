!!!mod ViTypes - type definitions for the Vertical Interpolation package
!
!AUTHOR
!     J.W. Blezius NOV 2003
!
!REVISION
! v1_3    Blezius J.W.          - initial version
!
!OBJECT
!        Because the IBM insists on a structure argument being defined in a
!        a module, this file defines the necessary module.  However, this is only
!        as a development stepping stone; once the users code is running, he
!        should change to the new extended interface which uses an index key
!        instead of a structure.
!
!NOTES
!        It is the responsibility of the programmer to ensure that this interface
!        definition matches the actual interface in the modules.
!
!        For backwards compatibility, any change to the interface must be
!        accompanied with a new name (e.g. append a revision number) for that
!        interface.
!
!!

  module ViTypes
    implicit none
    save
#include "ViTypes_f90.h"
  end module ViTypes


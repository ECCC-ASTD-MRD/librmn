!!!interface definition for the Vertical Interpolation package
!
!AUTHOR
!     J.W. Blezius DEC 2002
!
!REVISION
! v1_0    Blezius J.W.          - initial version
! v1_3    Blezius J.W. OCT 2003 - add functions with the extended the interface
!    ?    Blezius J.W. FEB 2010 - remove functions without the extended interface
!
!OBJECT
!        To provide the user with the advantages of an explicit interface, this
!        interface definition is provided to be include'ed.
!
!NOTES
!        While, in principle, it should be possible to include this interface
!        definition in the functions themselves whose interfaces are herein
!        defined, the Pollux compiler has problems with this.  It finds
!        differences between the declared interfaces and the actual interfaces
!        (that would be, after all, the whole purpose of the exercise), but those
!        differences are not visible to the human eye.  Hmmm.  Therefore, it is
!        the responsibility of the programmer to ensure that this interface
!        definition matches the actual interface in the modules.
!
!        For backwards compatibility, any change to the interface must be
!        accompanied with a new name (e.g. append a revision number) for that
!        interface.
!
!!



interface
  integer function N_ViqkdefIfc_X(n_VertGridIndex, n_numVLevelsIn, n_gridTypeIn,&
                                r_vLevelIn, r_pTopAvgIn, r_pRefIn, r_rCoefIn)
    implicit none
    integer, intent(out) :: n_VertGridIndex
    integer, intent(in)::n_numVLevelsIn
    integer, intent(in) :: n_gridTypeIn

    real, dimension(n_numVLevelsIn), intent(in) :: r_vLevelIn

    real, intent(in)::r_pTopAvgIn
    real, intent(in)::r_pRefIn
    real, intent(in)::r_rCoefIn
  end function N_ViqkdefIfc_X




  integer function N_VidefsetIfc_X(n_niIn, n_njIn, n_vGridDestnIndex, &
                                   n_vGridSourceIndex, r_pSurf, r_pTopIn, &
                                   n_numExtArraysIn, n_numExtArraysOut, &
                                   r_ExtArraysIn, r_ExtArraysOut)
    implicit none
    integer, intent(in)::n_niIn, n_njIn

    integer, intent(in) :: n_vGridDestnIndex, &
                           n_vGridSourceIndex

    real, dimension(n_niIn, n_njIn), intent(in) :: r_pSurf
    real, dimension(n_niIn, n_njIn), intent(in) :: r_pTopIn

    integer, intent(in) :: n_numExtArraysIn
    integer, intent(in) :: n_numExtArraysOut
    real, dimension(n_niIn, n_njIn, n_numExtArraysIn), &
                                                    intent(in)  :: r_ExtArraysIn
    real, dimension(n_niIn, n_njIn, n_numExtArraysOut), &
                                                    intent(out) :: r_ExtArraysOut
  end function N_VidefsetIfc_X



  integer function N_VisetoptIfc(s_option, s_value)
    implicit none
    character(len=*), intent(in) :: s_option
    character(len=*), intent(in) :: s_value
  end function N_VisetoptIfc




  integer function N_VisintIfc(r_stateOut, r_stateIn, r_derivOut, r_derivIn, &
                            r_extrapGuideDown, r_extrapGuideUp)
    implicit none
    real, dimension(1,1,1), intent(out) :: r_stateOut
    real, dimension(1,1,1), intent(in)  :: r_stateIn
    real, dimension(1,1,1), intent(out) :: r_derivOut
    real, dimension(1,1,1), intent(in)  :: r_derivIn
    real, intent(in) :: r_extrapGuideDown, r_extrapGuideUp
  end function N_VisintIfc




  integer function N_VisintIfc_X(r_stateOut, r_stateIn, r_derivOut, r_derivIn, &
                                 r_extrapGuideDown, r_extrapGuideUp, &
                                 m_slStateValue, m_slFluxGradient, &
                                 n_numExtArraysIn, n_numExtArraysOut, &
                                 r_ExtArraysIn, r_ExtArraysOut)
    implicit none
    real, dimension(1,1,1), intent(out) :: r_stateOut
    real, dimension(1,1,1),  intent(in)  :: r_stateIn
    real, dimension(1,1,1), intent(out) :: r_derivOut
    real, dimension(1,1,1),  intent(in)  :: r_derivIn
    real, intent(in) :: r_extrapGuideDown, r_extrapGuideUp
    external m_slStateValue
    external m_slFluxGradient
    integer, intent(in) :: n_numExtArraysIn ! size of the array, r_ExtArraysIn
    integer, intent(in) :: n_numExtArraysOut! size of the array, r_ExtArraysOut
    real, dimension(1,1,1), intent(in)  :: r_ExtArraysIn
    real, dimension(1,1,1), intent(out) :: r_ExtArraysOut
  end function N_VisintIfc_X




  integer function N_VigdrlsIfc(n_indexIn)
    implicit none
    integer, intent(in) :: n_indexIn
  end function N_VigdrlsIfc
  
end interface

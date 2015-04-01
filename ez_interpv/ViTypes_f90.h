!!!type definitions for the Vertical Interpolation package
!
!AUTHOR
!     J.W. Blezius DEC 2002
!
!REVISION
! v1_0    Blezius J.W.          - initial version
!
!OBJECT
!        To avoid maintaining *.mod files for each architecture for each revision
!        of the vertical interpolation package, these definitions are
!        provided to be include'ed instead of use'ing their modules directly.
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

  type T_VerticalGridIfc
    ! information capable of specifying particular grid types is placed in
    !  variables here:
    integer N_gridType                  ! representation used for vertical levels
    integer N_numVLevels                ! number of vertical points in this grid

                                        ! vertical levels defined for this grid,
                                        ! in units that are implied by gridType
                                        ! (IP1 values, decoded from FSTD format)
    real, pointer, dimension(:) :: R_vLevel_p

    ! Other parameters that may be necessary, depending on gridType
    ! (surface pressure is not explicitly a part of the vertical grid)

    ! hybrid parameters
    real :: R_pTopAvg                  ! pressure (mb) at the model top(ceiling)
    real :: R_pRef                     ! reference pressure (mb)
    real :: R_rCoef                    ! known as 'expansion co-efficient'
  end type T_VerticalGridIfc


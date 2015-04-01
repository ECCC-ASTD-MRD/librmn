!!!constants for the Vertical Interpolation package
!
!AUTHOR
!     J.W. Blezius DEC 2002
!
!REVISION
! v1_0    Blezius J.W.          - initial version
!    ?    Blezius J.W.          - support UNnormalized hybrid grid
!         Blezius J.W. DEC 2010 - add support for the staggered grid type
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

  !!!!!
  !
  ! The set of possible errors that can be reported by the vertical interpolation
  ! package
  !
  !!!!!

  ! errors from the VerticalInterpolation class
  integer, parameter :: N_VI_VIERR_FAILED_ALLOCATION    =100, &
                        N_VI_VIERR_UNDEFINED_GRID_REQD  =101, &
                        N_VI_VIERR_GRIDS_NOT_SELECTED   =102, &
                        N_VI_VIERR_BAD_INTERPTYP_4_DATA =103, &
                        N_VI_VIERR_UNRECOGNIZED_OPTION  =104, &
                        N_VI_VIERR_UNRECOGNIZED_VALUE   =105, &
                        N_VI_VIERR_LN_PRESS_CONVERSION  =106, &
                        N_VI_VIERR_MISSING_SURFACE_DATA =108, &

  ! errors from VerticalGrid class
                        N_VI_VGERR_HYBRID_TO_PRES_CONVN =200, &
                        N_VI_VGERR_FAILED_ALLOCATION    =201, &
                        N_VI_VGERR_PTOP_MISSING         =202, &

  ! errors from the Interface routines
                        N_IFC_VGRID_REPOSITORY_OVERFLOW =300, &
                        N_IFC_REPOSITORY_INVALID_INDEX  =301, &
                        N_IFC_INVALID_DIMENSION         =302



  ! Possible values of the gridType.
  ! These values are the same as those used in CONVIP (except for eta) at the
  ! time of writing.  (However, future compatibility with CONVIP is not
  ! guaranteed.)
  integer, parameter :: N_GRID_TYPE_SIGMA    = 1, & ! P/Ps
                        N_GRID_TYPE_PRESSURE = 2, & ! in mb
                        N_GRID_TYPE_GENERIC  = 3, & ! units are user defined; 
                                                    ! (software won't convert it)
                        N_GRID_TYPE_HYBRID   = 5, & ! NORMALIZED hybrid (i.e. hybrid type 1)
                        N_GRID_TYPE_ETA      = 7, & !(Pt-P)/(Pt-Ps) -not in convip
                                                    ! UNnormalized hybrid (i.e. hybrid type 5, version 1)
                        N_GRID_TYPE_HYBRID_NOTNORM = 8, &
                        N_GRID_TYPE_STAGGERED = 9   ! Staggered (i.e. hybrid type 5, version 2)

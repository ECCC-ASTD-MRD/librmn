!!!s/r Interp1D_Linear Interfaces
!
!AUTHOR
!     J.W. Blezius MAY 2002 first library to replace duplicate interpolation
!                           routines
!
!REVISION
! v1_0    Blezius J.W.          - new code
! v1_3    Blezius J.W. OCT 2003 - add the extended interface
!
!OBJECT
!        To provide a means to easily compile the contained routine twice, once
!        with single-precision arguments, and once with double-precision
!        arguments.  The unextended interface is likewise compiled twice.
!
!NOTES
!
!!

!version that accepts real(single) arguments, with the extended interface
subroutine Interp1D_Linear_X  &
    (numInterpSets, srcNumLevels, destNumLevels, &
    src_ijDim, dst_ijDim, &

    vLevelSource, stateSource, stateDerivSource, &

    posnDestInSrc, vLevelDestn, stateDestn, stateDerivDestn, &

    extrapEnableDown, extrapEnableUp, &
    extrapGuideDown, extrapGuideUp, &

    flux, numExtArraysIn, numExtArraysOut, &
    ExtArraysIn, ExtArraysOut &
   )
#define real48 single
#include "Interp1D_Linear_Body.inc"
#undef real48
end subroutine

!version that accepts real(double) arguments, with the extended interface
subroutine Interp1D_Linear_X8  &
    (numInterpSets, srcNumLevels, destNumLevels, &
    src_ijDim, dst_ijDim, &

    vLevelSource, stateSource, stateDerivSource, &

    posnDestInSrc, vLevelDestn, stateDestn, stateDerivDestn, &

    extrapEnableDown, extrapEnableUp, &
    extrapGuideDown, extrapGuideUp, &

    flux, numExtArraysIn, numExtArraysOut, &
    ExtArraysIn, ExtArraysOut &
   )
#define real48 double
#include "Interp1D_Linear_Body.inc"
#undef real48
end subroutine




!version that accepts real(single) arguments
subroutine Interp1D_Linear  &
    (numInterpSets, srcNumLevels, destNumLevels, &
     src_ijDim, dst_ijDim, &

     vLevelSource, stateSource, stateDerivSource, &

     posnDestInSrc, vLevelDestn, stateDestn, stateDerivDestn, &

     extrapEnableDown, extrapEnableUp, &
     extrapGuideDown, extrapGuideUp &
    )
#define real48 single
#define DestnFunc Interp1D_Linear_X
#include "Interp1D_PreX_Shell.inc"
#undef DestnFunc
#undef real48
end subroutine

!version that accepts real(double) arguments
subroutine Interp1D_Linear8  &
    (numInterpSets, srcNumLevels, destNumLevels, &
     src_ijDim, dst_ijDim, &

     vLevelSource, stateSource, stateDerivSource, &

     posnDestInSrc, vLevelDestn, stateDestn, stateDerivDestn, &

     extrapEnableDown, extrapEnableUp, &
     extrapGuideDown, extrapGuideUp &
    )
#define real48 double
#define DestnFunc Interp1D_Linear_X8
#include "Interp1D_PreX_Shell.inc"
#undef DestnFunc
#undef real48
end subroutine

!!!s/r Extrap1D_Abort Interfaces
!
!AUTHOR
!     J.W. Blezius MAY 2014 First revision for this file
!
!REVISION
!         Blezius J.W. MAY 2014 - add abortion on extrapolation (this file)
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
subroutine Extrap1D_Abort_X  &
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
#include "Extrap1D_Abort_Body.inc"
#undef real48
end subroutine

!version that accepts real(double) arguments, with the extended interface
subroutine Extrap1D_Abort_X8  &
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
#include "Extrap1D_Abort_Body.inc"
#undef real48
end subroutine




!version that accepts real(single) arguments
subroutine Extrap1D_Abort  &
    (numInterpSets, srcNumLevels, destNumLevels, &
    src_ijDim, dst_ijDim, &

    vLevelSource, stateSource, stateDerivSource, &

    posnDestInSrc, vLevelDestn, stateDestn, stateDerivDestn, &

    extrapEnableDown, extrapEnableUp, &
    extrapGuideDown, extrapGuideUp &
   )
#define real48 single
#define DestnFunc Extrap1D_Abort_X
#include "Interp1D_PreX_Shell.inc"
#undef DestnFunc
#undef real48
end subroutine

!version that accepts real(double) arguments
subroutine Extrap1D_Abort8  &
    (numInterpSets, srcNumLevels, destNumLevels, &
     src_ijDim, dst_ijDim, &

     vLevelSource, stateSource, stateDerivSource, &

     posnDestInSrc, vLevelDestn, stateDestn, stateDerivDestn, &

     extrapEnableDown, extrapEnableUp, &
     extrapGuideDown, extrapGuideUp &
    )
#define real48 double
#define DestnFunc Extrap1D_Abort_X8
#include "Interp1D_PreX_Shell.inc"
#undef DestnFunc
#undef real48
end subroutine

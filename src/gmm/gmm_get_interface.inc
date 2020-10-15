interface gmm_get
    integer function gmm_getmeta2(iname, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        type(gmm_metadata), intent(out) :: m
    end function gmm_getmeta2

    integer function gmm_get184(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*8, pointer :: p(:,:,:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get184

    integer function gmm_get144(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*4, pointer :: p(:,:,:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get144

    integer function gmm_get284(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*8, pointer :: p(:,:,:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get284

    integer function gmm_get244(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*4, pointer :: p(:,:,:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get244

    integer function gmm_get384(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        complex*8, pointer :: p(:,:,:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get384

    integer function gmm_get183(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*8, pointer :: p(:,:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get183

    integer function gmm_get143(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*4, pointer :: p(:,:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get143

    integer function gmm_get283(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*8, pointer :: p(:,:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get283

    integer function gmm_get243(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*4, pointer :: p(:,:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get243

    integer function gmm_get383(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        complex*8, pointer :: p(:,:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get383

    integer function gmm_get182(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*8, pointer :: p(:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get182

    integer function gmm_get142(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*4, pointer :: p(:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get142

    integer function gmm_get282(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*8, pointer :: p(:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get282

    integer function gmm_get242(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*4, pointer :: p(:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get242

    integer function gmm_get382(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        complex*8, pointer :: p(:,:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get382

    integer function gmm_get181(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*8, pointer :: p(:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get181

    integer function gmm_get141(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*4, pointer :: p(:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get141

    integer function gmm_get281(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*8, pointer :: p(:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get281

    integer function gmm_get241(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*4, pointer :: p(:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get241

    integer function gmm_get381(iname, p, m)
#include  "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        complex*8, pointer :: p(:)
        type(gmm_metadata), optional, intent(out) :: m
    end function gmm_get381
end interface

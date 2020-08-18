interface gmm_create

    integer function gmm_create184(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*8, pointer :: p(:, :, :, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create184

    integer function gmm_create144(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*4, pointer :: p(:, :, :, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create144

    integer function gmm_create284(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*8, pointer :: p(:, :, :, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create284

    integer function gmm_create244(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*4, pointer :: p(:, :, :, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create244

    integer function gmm_create384(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        complex*8, pointer :: p(:, :, :, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create384

    integer function gmm_create183(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*8, pointer :: p(:, :, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create183

    integer function gmm_create143(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*4, pointer :: p(:, :, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create143

    integer function gmm_create283(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*8, pointer :: p(:, :, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create283

    integer function gmm_create243(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*4, pointer :: p(:, :, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create243

    integer function gmm_create383(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        complex*8, pointer :: p(:, :, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create383

    integer function gmm_create182(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*8, pointer :: p(:, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create182

    integer function gmm_create142(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*4, pointer :: p(:, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create142

    integer function gmm_create282(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*8, pointer :: p(:, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create282

    integer function gmm_create242(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*4, pointer :: p(:, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create242

    integer function gmm_create382(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        complex*8, pointer :: p(:, :)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create382

    integer function gmm_create181(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*8, pointer :: p(:)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create181

    integer function gmm_create141(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        integer*4, pointer :: p(:)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create141

    integer function gmm_create281(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*8, pointer :: p(:)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create281

    integer function gmm_create241(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        real*4, pointer :: p(:)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create241

    integer function gmm_create381(iname, p, field_meta, flags_arg)
        include "gmm_definitions.inc"
        character(len=*), intent(in) :: iname
        complex*8, pointer :: p(:)
        type(gmm_metadata), intent(inout) :: field_meta
        integer, intent(in), optional :: flags_arg
    end function gmm_create381

end interface

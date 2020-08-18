!> Encode/pack type(gmm_metadata) in a basic Fortran type
!> @author  Yves Chartier, 2008-04
function gmm_updatemeta(iname, F_meta) result(F_istat)
    use gmm_internals
    implicit none

    include 'gmm_directory_interface.inc'
    character(len=*), intent(in) :: iname
    type(gmm_metadata), intent(in) :: F_meta

    integer :: F_istat

    integer :: i
    integer*8 :: key

    key = 0
    call check_directory_entry(iname, key)
    if(cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! Quick check using key was not successful
        call find_directory_entry(iname, key)
    endif

    if (cur_page .eq. 0 .or. cur_entry .eq. 0) then
        ! Return null entry
        F_istat = GMM_VAR_NOT_FOUND
        return
    endif

    do i = 1, 4
        directory(cur_page)%entry(cur_entry)%l(i)%halo = F_meta%l(i)%halo
        directory(cur_page)%entry(cur_entry)%l(i)%halomax = F_meta%l(i)%halomax
        directory(cur_page)%entry(cur_entry)%l(i)%n = F_meta%l(i)%n
    enddo

   directory(cur_page)%entry(cur_entry)%a%uuid1 = F_meta%a%uuid1
   directory(cur_page)%entry(cur_entry)%a%uuid2 = F_meta%a%uuid2
   directory(cur_page)%entry(cur_entry)%a%flags = F_meta%a%flags

   F_istat = GMM_OK
end function gmm_updatemeta

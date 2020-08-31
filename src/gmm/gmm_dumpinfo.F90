subroutine gmm_dumpinfo(fldstat)
    use gmm_internals
    implicit none
    logical, intent(in), optional :: fldstat
    integer :: i, l_page, l_entry, nelm, crc
    type(gmm_layout), dimension(4) :: dims

    interface
        integer function f_calc_crc(obj, nbelem, seed, stride) bind(c, name = "calc_crc")
            use, intrinsic :: iso_c_binding, only: c_ptr
            type(c_ptr), value, intent(in) :: obj
            integer, intent(in) :: nbelem
            integer, intent(in) :: seed
            integer, value, intent(in) :: stride
        end function
    end interface

    integer(kind = 8) :: ptraddr

    l_page = 1
    l_entry = 1
    print *, 'GMM dumpinfo, number of variables in use is', used
    do i = 1, used
        dims = directory(l_page)%entry(l_entry)%l
        nelm = ( (dims(1)%high - dims(1)%low +1) * &
                 (dims(2)%high - dims(2)%low +1) * &
                 (dims(3)%high - dims(3)%low +1) * &
                 (dims(4)%high - dims(4)%low +1) )
        ptraddr = transfer(directory(l_page)%entry(l_entry)%array_addr, ptraddr)
        if (present(fldstat)) then
            print *, 'Appel a statfld a ecrire, fldstat=', fldstat
            print '(a,a,a,i10)', &
                'Name=', directory(l_page)%entry(l_entry)%name, &
                ' addr=', ptraddr
        else
            crc = f_calc_crc(directory(l_page)%entry(l_entry)%array_addr, nelm, 0, 1)
            print '(a,a,a,i10,a,i10)', &
                'Name=', directory(l_page)%entry(l_entry)%name, &
                ' addr=', ptraddr, &
                ' checksum=', crc
        endif
        l_entry = l_entry + 1
        if (l_entry .gt. PAGE_SIZE) then
            l_page = l_page + 1
            l_entry = 1
        endif
    enddo
end subroutine gmm_dumpinfo

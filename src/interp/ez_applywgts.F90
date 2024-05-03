   subroutine ez_applywgts(outfld, wts, idxs, infld, x, y, masque, ni_src, nj_src, ni_dst, nj_dst, n_wts)

   implicit none
   integer, intent(in) :: ni_src, nj_src, ni_dst, nj_dst, n_wts
   real, dimension(ni_src, nj_src), intent(in) :: x, y
   real, dimension(ni_src * nj_src), intent(in) :: infld
   real, dimension(ni_src * nj_src), intent(out) :: outfld
   real, dimension(ni_dst, nj_dst, n_wts), intent(in) :: wts
   integer, dimension(ni_dst, nj_dst, n_wts), intent(in) :: idxs
   integer, dimension(ni_dst * nj_dst), intent(in) :: masque

   real :: rmin, rmax
   integer :: i, j, k, n

   integer :: ezgetval, ezgetopt, ier

   character(len=32) :: interopt, xtrapopt
   real xtrapval

   ier = ezgetopt('INTERP_DEGREE', interopt)
   ier = ezgetopt('EXTRAP_DEGREE', xtrapopt)
   ier = ezgetval('EXTRAP_VALUE', xtrapval)
!   print *, interopt, xtrapopt, xtrapval

   if (xtrapopt(1:5) == 'value') then
      ier = ezgetval('EXTRAP_VALUE', xtrapval)
      outfld = xtrapval
   else
      rmin = minval(infld)
      rmax = maxval(infld)
      rmin  = rmin - 0.1 * (rmax - rmin)
      outfld = rmin
   endif


   do k = 1, ni_dst * nj_dst
     i = mod((k - 1), ni_dst) + 1
     j = 1 + k / ni_dst
     if (masque(k) == 1) then
        outfld(k) = 0.0
        do n = 1, n_wts
           if (idxs(i, j, n) < 1) exit
           outfld(k) = outfld(k) + wts(i,j,n) * infld(idxs(i,j,n))
        enddo
     endif
   enddo

   end subroutine ez_applywgts

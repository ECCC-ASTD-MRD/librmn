subroutine boxed_message(iun, msg, nl) ! print nl lines , centered in a * delimited box
  implicit none
  integer, intent(IN) :: iun
  integer, intent(IN) :: nl
  character(len=*), dimension(nl), intent(IN) :: msg

  integer :: i, left, right, nc
  character(len=78) :: line, lineb, linet

  write(line,'(78A1)') ('*',i=1,78)
  lineb = ' '
  write(iun,1) '*',line,'*'
  write(iun,1) '*',lineb,'*'
1 format(1X,A1,A78,A1)

  do i = 1, nl
    linet = msg(i)
    nc = min(78, len(trim(linet)))
    left = (78 - nc) / 2
    right = 78 - left - nc
    write(iun,1) '*', lineb(1:left)//linet(1:nc)//lineb(1:right), '*'
  enddo
  write(iun,1) '*',lineb,'*'
  write(iun,1) '*',line,'*'
end subroutine boxed_message

#if defined(SELF_TEST)
program test
  implicit none
  character(len=80), dimension(3) :: mesg
  mesg(1) = 'texte 1'
  mesg(2) = 'message 2'
  mesg(3) = 'avertissement 3'
  call boxed_message(6,mesg,1)
  call boxed_message(6,mesg,3)
end program
#endif

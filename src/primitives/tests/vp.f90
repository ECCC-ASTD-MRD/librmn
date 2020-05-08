subroutine vspownn(r,x,y,n)
integer n
real*4 x(*),y(*),r(*)
real (kind=8), dimension(n) :: t8,x8,y8,r8
do i = 1,n
  x8(i) = x(i)
  y8(i) = y(i)
enddo
call vlog(t8,x8,n)
do 10 j=1,n
 t8(j)=t8(j)*y8(j)
10 continue
call vexp(r8,t8,n)
do i = 1,n
  r(i) = r8(i)
enddo
return
end

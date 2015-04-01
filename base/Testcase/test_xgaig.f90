program xg_ig
real xg1,xg2,xg3,xg4,delta
integer ig1,ig2,ig3,ig4
delta=0.0
do irep=0,20
!print *,'===',i,'==='
ig1=0
ig2=0
ig3=0
ig4=0
xg1=-21.09375+irep*.005
xg2=47.81250
xg3=-21.09375+irep*.005
xg4=52.81250
!print *,xg1,xg2,xg3,xg4,ig1,ig2,ig3,ig4
!print 100,xg1,xg2,xg3,xg4
call cxgaig('E',ig1,ig2,ig3,ig4,xg1,xg2,xg3,xg4)
!print *,xg1,xg2,xg3,xg4,ig1,ig2,ig3,ig4
call cigaxg('E',xg1,xg2,xg3,xg4,ig1,ig2,ig3,ig4)
!print *,xg1,xg2,xg3,xg4,ig1,ig2,ig3,ig4
print 100,xg1,xg2,xg3,xg4,xg3-xg1,xg3-(-21.09375+irep*.005)
delta=max(delta,abs(xg3-(-21.09375+irep*.005)))
enddo
100 format(8F10.6)
print *,'max error=',delta,1.0/delta
stop
end
subroutine llfxy
stop
end
subroutine xyfll
stop
end
subroutine valide()
!if(3601.gt.3600) print *,'allo'
return
end


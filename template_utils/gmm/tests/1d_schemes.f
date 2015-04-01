      program CIP_1D


      PARAMETER (NX=400)
      IMPLICIT REAL(A-H,O-Z)




      DIMENSION F(NX),FX(NX),U(NX),FN(NX),FXN(NX),rho(NX),deltal(nx)
      real*4 errfil,wkid,conid,wtype,A(NX),dt4
C
C===== GRAPHIC INITIALIZE =========
C
      call wglscon('x')
      call wglpsz(800,400)
      winid = wglopw('cip_1D')
      call wglssp(0.,-0.1,1.25,0.35,0,0,0,0,0)
c      call wglias(.false.)
      call wglcol(BLANC)
      call wglclr
      call wgldbf
      call wglbbf
      call wglfsh


C
C==================================
C
C
      DT = 0.21
      DX = 1.0
      NT = 1000
      dt4=dt
C
	WRITE(*,*) 'Input an integer to choose a scheme'
	WRITE(*,*) '0: SPLINE; 1: CIP; 2: RCIP; 3: CSL2'
	WRITE(*,*) '4: CSL3_HYMAN; 5: CSL3_UNO; 6: CSL3_CW'
	READ (*,*) IC
	WRITE(*,*) 'Input an integer to choose a initial profile'
	WRITE(*,*) '1: triangle; 2: square'
	READ (*,*) ISHAPE
      CALL INIT(F,FX,rho,deltal,U,DX,DT,ISHAPE)
      DO 1000 ISTEP = 1 , NT
       IF (IC.EQ.0) THEN
       call SPLINE(F,FN,FX,FXN,U,DT,DX,NX)
       else if(ic.eq.1) then
       CALL  CIP1D(F,FX,U,DT,DX)
       else if(ic.eq.2) then
       CALL  RCIP1D(F,FX,U,DT,DX)
       else  if (ic.eq.3) then
       call CSL2   (F,rho,U,DT,DX,nx)
       else  if (ic.eq.4) then
       CALL CSL3_HYMAN(F,rho,U,DT,DX,NX)
       else  if (ic.eq.5) then
       CALL CSL3_UNO(F,rho,U,DT,DX,NX)
       else  if (ic.eq.6) then
       CALL CSL3_CW(F,rho,U,DT,DX,NX)
       endif
C
        PRINT *,'                     ',ISTEP
        	zmax=-1000.
       		zmin=1000.
                FSUM=0.0
                RSUM=0.0
 	     DO  i=1,NX
		if(zmax.le.f(i)) zmax=f(i)
		if(zmin.ge.f(i)) zmin=f(i)
                FSUM=FSUM+F(I)
                RSUM=RSUM+RHO(I)
 	    ENDDO
	  WRITE(*,*)'zmax=',zmax
       	  WRITE(*,*)'zmin=',zmin
       	  WRITE(*,*)'FSUM=',FSUM
       	  WRITE(*,*)'RSUM=',RSUM
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        do i=1,nx
           a(i)=f(i)
        enddo
      IF(MOD(ISTEP,10).EQ.0.or.istep.eq.1) THEN
         call wglswb
      CALL GRAPH(A,DT4,1.0,WKID,ISTEP,ISHAPE)
cx      CALL GRAPH(rho,DT,DX,WKID,ISTEP)
      ENDIF
 1000 CONTINUE
        CALL OUTPUT(F,rho,U,dx,IC)
C====== GRAPHIC CLOSE ======
C
        call wglfsh
	WRITE(*,*) 'SAFELY ENDED !!!'
        read(*,*)

C      CALL GCOPY
C
C===========================
C
      STOP
      END
        SUBROUTINE OUTPUT(F,FX,U,dx,IC)
        IMPLICIT REAL(A-H,O-Z)
        PARAMETER (NX=400)
        DIMENSION F(NX),FX(NX),U(NX)
	open(2,file='F_DAT')
 	write(2,1012) ((I),F(I), I=1,400)
 	close(2)
	open(3,file='U_DAT')
 	write(3,1012) ((I-0),U(I), I=1,400)
 	close(3)
	open(4,file='RHO_DAT')
 	write(4,1012) ((I),fx(I), I=1,400)
 	close(4)
 1012	format(I10,1x,F10.6)
	RETURN
	END
C
        SUBROUTINE INIT(F,FX,rho,deltal,U,DX,DT,ISHAPE)
        IMPLICIT REAL(A-H,O-Z)
        PARAMETER (NX=400)
        DIMENSION F(NX),FX(NX),U(NX),rho(NX),deltal(NX)
      DO 100 I = 1 , NX
        F(I) = 0.0
        FX(I)= 0.0
        u(i)=1.0
  100 CONTINUE
       IST=50
       IMD=65
       IEN=80
      IF(ISHAPE.EQ.1) THEN
      DO I=IST,IEN
       IF(I.LE.IMD) THEN
         F(I)=0.1*FLOAT(I-IST+1)/1.5
       ELSE
         F(I)=1.0-0.1*FLOAT(I-IMD)/1.5
       ENDIF
       ENDDO
      ELSE IF(ISHAPE.EQ.2) THEN
      DO  I = IST,IEN
        F(I) = 1.0
        ENDDO
      ENDIF
      DO 120 I=2,NX-1
        rho(I) = 0.5*(f(i+1)+f(i))
        deltal(I) = (f(i+1)-f(i))/dx
        fx(I) = (f(i+1)-f(i-1))/dx/2.0
  120 CONTINUE
      RETURN
      END
        SUBROUTINE CIP1D(F,FX,U,DT,DX)
        PARAMETER (NX=400)
        IMPLICIT REAL(A-H,O-Z)
        DIMENSION F(NX),FX(NX),FN(NX),FXN(NX),U(NX)
        DIMENSION IP(NX)
	DO 10 I=1,NX
	FN(I)=F(I)
	FXN(I)=FX(I)
10	CONTINUE
	DO 100 I1=3,NX-2
        UX=(U(I1+1)-U(I1-1))/DX/2.0
        UXX=(U(I1+1)+U(I1-1)-2.*U(I1))/DX/DX
        XDIS=-DT*U(I1)+DT*DT*U(I1)*UX/2.0
     1       -DT*DT*DT*U(I1)*(UX*UX+U(I1)*UXX)/6.0
           IP(I1)=I1+INT(XDIS/dx)
           I=MIN(NX-1,IP(I1))
           I=MAX(2   ,IP(I1))
	D=XDIS-DX*DBLE(IP(I1)-I1)
        IF (D.GE.0.0) THEN
        IS=1
        SDX=DX
        ELSE
        IS=-1
        SDX=-DX
        ENDIF
	S=(FN(I+IS)-FN(I))/SDX
	C=(FXN(I+IS)-2.*S+FXN(I))/(SDX*SDX)
	B=(3.*S-FXN(I+IS)-2.*FXN(I))/(SDX)
	F(I1)=FN(I)+FXN(I)*D+B*D*D+C*D*D*D
	FX(I1)=FXN(I)+2.*B*D+3.*C*D*D
100	CONTINUE
	DO 200 I=2,NX-1
	FX(I)=FX(I)*(1.-DT/DX*0.5*(U(I+1)-U(I-1)))
200 	CONTINUE
	F(2)=F(3)
	F(1)=F(2)
	F(II-1)=F(II-2)
	F(II)=F(II-1)
	FX(2)=FX(3)
	FX(1)=FX(2)
	FX(II-1)=FX(II-2)
cccmd	FX(II)=F(XII-1)
	RETURN
	END
C*****************************************************
        SUBROUTINE SPLINE(F,FN,T,TN,U,DT,DX,II)
       IMPLICIT REAL(A-H,O-Z)
       PARAMETER(JRED=400)
       DIMENSION AA(JRED),BB(JRED),CC(JRED),DD(JRED)
        DIMENSION F(II),T(II),U(II),FN(II),TN(II)
        intrinsic sign
        L=II-1
         DO 987 I=2,L
         AA(I)=1.
         BB(I)=4.
         CC(I)=1.
         DD(I)=3.*(F(I+1)-F(I-1))
 987     CONTINUE
cxxx         AA(L+1)=-1.
         AA(L+1)=0.0
         DD(L+1)=0.0
cxxx         CC(1)=-1.
         CC(1)=0.0
         DD(1)=0.0
         BB(1)=1.
         BB(L+1)=1.
         CALL THOMAS(1,II,AA,BB,CC,DD,TN)
	DO 10 I=1,II
	FN(I)=F(I)
10	CONTINUE
	DO 100 I=2,II-1
	IS=INT(-SIGN(1.,U(I)))
	SDX=-SIGN(1.,U(I))*DX
C	SDX=AMAX1(SDX,0.0001)
	S=(FN(I+IS)-FN(I))/SDX
	C=(TN(I+IS)-2.*S+TN(I))/(SDX*SDX)
	B=(3.*S-TN(I+IS)-2.*TN(I))/(SDX)
	D=-U(I)*DT
	F(I)=FN(I)+TN(I)*D+B*D*D+C*D*D*D
100	CONTINUE
	RETURN
	END
C	**********************************************************************
       SUBROUTINE THOMAS(IK,L,A,B,C,D,U)
       IMPLICIT REAL(A-H,O-Z)
       PARAMETER(JRED=400)
       DIMENSION A(JRED),B(JRED),C(JRED),D(JRED),U(JRED),
     $      BETA(JRED),GAMMA(JRED)
       BETA(IK)=C(IK)/B(IK)
       GAMMA(IK)=D(IK)/B(IK)
       IP=IK+1
       DO 1 I=IP,L
       BETA(I)=C(I)/(B(I)-A(I)*BETA(I-1))
 1     GAMMA(I)=(D(I)-A(I)*GAMMA(I-1))/(B(I)-A(I)*BETA(I-1))
       U(L)=GAMMA(L)
       LAST=L-IK
       DO 2 K=1,LAST
       I=L-K
       U(I)=GAMMA(I)-U(I+1)*BETA(I)
 2     CONTINUE
       RETURN
       END
C	**********************************************************************
        SUBROUTINE RCIP1D(F,FX,U,DT,DX)
        PARAMETER (NX=400)
        IMPLICIT REAL(A-H,O-Z)
        DIMENSION F(NX),FX(NX),FN(NX),FXN(NX),U(NX)
        DIMENSION IP(NX)
	DO 10 I=1,NX
	FN(I)=F(I)
	FXN(I)=FX(I)
10	CONTINUE
	DO 100 I1=3,NX-2
        UX=(U(I1+1)-U(I1-1))/DX/2.0
        UXX=(U(I1+1)+U(I1-1)-2.*U(I1))/DX/DX
        XDIS=-DT*U(I1)+DT*DT*U(I1)*UX/2.0
     1       -DT*DT*DT*U(I1)*(UX*UX+U(I1)*UXX)/6.0
           IP(I1)=I1+INT(XDIS/dx)
           I=MIN(NX-1,IP(I1))
           I=MAX(2   ,IP(I1))
	D=XDIS-DX*DBLE(IP(I1)-I1)
        IF (D.GE.0.0) THEN
        IS=1
        SDX=DX
        ELSE
        IS=-1
        SDX=-DX
        ENDIF
	S=(FN(I+IS)-FN(I))/SDX
        TS=FXN(IS+I)-S
        IF (ABS(TS).LE.1.0D-10) THEN
        TS=1.0D-10
        FXN(I)=S
        ENDIF
        DFF=FXN(I)*FXN(IS+I)
        IF (DFF.LE.0.0) THEN
	C=(ABS((S-FXN(I))/TS)-1.)/SDX
        ELSE
        C=0.0
        ENDIF
	E=(FXN(I)-S+(FXN(I+IS)-S)*(1.+C*SDX))/(SDX*SDX)
	B=S*C+(S-FXN(I))/SDX-E*SDX
	A=FXN(I)+FN(I)*C
	C1=1.+C*D
	IF(ABS(C1).LE.1.0D-7) C1=1.0D-7
	F(I1)=(FN(I)+A*D+B*D*D+E*D*D*D)/C1
	FX(I1)=(A+2.*B*D+3.*D*D*E)/C1-C*(FN(I)+A*D+B*D*D+E*D*D*D)/(C1*C1
     %)
100	CONTINUE
	DO 200 I=2,NX-1
	FX(I)=FX(I)*(1.-DT/DX*0.5*(U(I+1)-U(I-1)))
200 	CONTINUE
	F(2)=F(3)
	F(1)=F(2)
	F(II-1)=F(II-2)
	F(II)=F(II-1)
	FX(2)=FX(3)
	FX(1)=FX(2)
	FX(II-1)=FX(II-2)
cccmd	FX(II)=F(XII-1)
	RETURN
	END
        real  function xm(a,b)
        IMPLICIT REAL(A-H,O-Z)
        xm=b
        if(abs(a).le.abs(b)) xm=a
        return
        end
        real  function xmod(a,b)
        IMPLICIT REAL(A-H,O-Z)
        if (a*b.gt.0.0) then
        xmod=b
        if(abs(a).le.abs(b)) xmod=a
        else
        xmod=0.0
        endif
        return
        end
        SUBROUTINE CSL3_HYMAN(F,rho,U,DT,DX,II)
        IMPLICIT REAL(A-H,O-Z)
        parameter(im=400)
        DIMENSION F(II),U(II),FN(Im),rho(II)
        DIMENSION drho(IM),RHON(im),h(im)
        DIMENSION IP(IM),DELTAL(IM)
        intrinsic sign
	DO 10 I=1,II
	FN(I)=F(I)
	RHON(I)=RHO(I)
	h(I)=0.0
        IP(I)=I
        DELTAL(I)=0.0
10	CONTINUE
	DO 1 I=2,II-1
        h(i)=3./2.*rho(i)-0.25*(fn(i)+fn(i+1))
1	CONTINUE
	DO 101 I=3,II-2
CCCCCCCCCCCCCCCCC HYMAN APPROXIMATION CCCCCCCCCCCCCCCCCCCCC
        deltal(i)=(-h(i+2)+8.0*h(i+1)-8.*h(i-1)+h(i-2))/dx/12.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 101    CONTINUE
	DO 100 I1=3,II-2
        UX=(U(I1+1)-U(I1-1))/DX/2.0
        UXX=(U(I1+1)+U(I1-1)-2.*U(I1))/DX/DX
        XDIS=-DT*U(I1)+DT*DT*U(I1)*UX/2.0
     1       -DT*DT*DT*U(I1)*(UX*UX+U(I1)*UXX)/6.0
cx        XDIS=XDIS/DX
           IP(I1)=I1+INT(XDIS/dx)
           I=MIN(II-2,IP(I1))
           I=MAX(3   ,IP(I1))
	IS=INT(-SIGN(1.,U(I)))
        I12=min(I,I+IS)
        sgu=SIGN(1.,U(I))
	SDX=DX
        sdx2=dx*dx
        sdx3=dx*dx*dx
        c=-sgu*6.0/sdx*rho(i12)+sgu*6.0/sdx*fn(i)-2.*deltal(I12)
        a=-4.0*deltal(I12)/sdx2
     1   +sgu*4.0/sdx3*(fn(i)-fn(i+is))
        b=-6.0/sdx2*rho(i12)-sgu*6.0*deltal(I12)/sdx
     1   +3.0/sdx2*(3.0*fn(i)-fn(i+is))
	D=XDIS-DX*DBLE(IP(I1)-I1)
	F(I1)=FN(I)+c*D+b*D*D+a*D*D*D
        drho(i1)=-(FN(I)+c*D/2.0+b*D*D/3.0+a*D*D*D/4.)*D
100	CONTINUE
        drho(ii-1)=drho(ii-2)
        drho(2)   =drho(3)
	DO 200 I1=3,II-2
        I=MIN(II-2,IP(I1))
        I=MAX(3   ,IP(I1))
        rhosum=0.0
        do 202 i2=ip(i1),ip(i1+1)-1
           rhosum=rhon(i2)+rhosum
 202       continue
	rho(I1)=rhosum-drho(I1+1)/DX+drho(I1)/DX
200 	CONTINUE
        NSUB=1.
        DT=DT/FLOAT(NSUB)
        DO 105 N=1,NSUB
	DO 110 I1=3,II-2
        DT=DT*FLOAT(N)
        UX=(U(I1+1)-U(I1-1))/DX/2.0
        UXX=(U(I1+1)+U(I1-1)-2.*U(I1))/DX/DX
           XDIS=-DT*U(I1)+DT*DT*U(I1)*UX/2.0
     1                   -DT*DT*DT*U(I1)*(UX*UX+U(I1)*UXX)/6.0
           IP(I1)=I1+INT(XDIS/DX)
           I=MIN(II-2,IP(I1))
           I=MAX(3   ,IP(I1))
        DT=DT/FLOAT(N)
	F(I1)=F(I1)*(1.0-DT*((u(i+1)-u(i-1))/dx/4.0
     1  + (u(i+1)-u(i-1))/dx/4.0 ))
 110    CONTINUE
 105    CONTINUE
        DT =FLOAT(NSUB)*DT
	F(2)=F(3)
	F(1)=F(2)
	F(II-1)=F(II-2)
	F(II)=F(II-1)
	RHO(2)=RHO(3)
	RHO(1)=RHO(2)
	RHO(II-1)=RHO(II-2)
	RHO(II)=RHO(II-1)
	RETURN
	END
        SUBROUTINE CSL3_UNO(F,rho,U,DT,DX,II)
        IMPLICIT REAL(A-H,O-Z)
        parameter(im=400)
        DIMENSION F(II),U(II),FN(Im),rho(II)
        DIMENSION drho(IM),RHON(im),h(im)
        DIMENSION IP(IM),DELTAL(IM)
        intrinsic sign
	DO 10 I=1,II
	FN(I)=F(I)
	RHON(I)=RHO(I)
	h(I)=0.0
        IP(I)=I
        DELTAL(I)=0.0
10	CONTINUE
	DO 1 I=2,II-1
        h(i)=3./2.*rho(i)-0.25*(fn(i)+fn(i+1))
1	CONTINUE
	DO 101 I=3,II-2
CCCCCCCCCCCCCCCCC UNO reconstruction CCCCCCCCCCCCCCCCCCCCC
        cd0=(h(i)-2.0*h(i-1)+h(i-2))/dx/dx
        cd1=(h(i+1)-2.0*h(i)+h(i-1))/dx/dx
        cd2=(h(i+2)-2.0*h(i+1)+h(i))/dx/dx
        deltal(i)=xm((h(i+1)-h(i))/dx-0.5*dx*xm(cd1,cd2),
     1               (h(i)-h(i-1))/dx+0.5*dx*xm(cd1,cd0))
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 101    CONTINUE
	DO 100 I1=3,II-2
        UX=(U(I1+1)-U(I1-1))/DX/2.0
        UXX=(U(I1+1)+U(I1-1)-2.*U(I1))/DX/DX
        XDIS=-DT*U(I1)+DT*DT*U(I1)*UX/2.0
     1       -DT*DT*DT*U(I1)*(UX*UX+U(I1)*UXX)/6.0
cx        XDIS=XDIS/DX
           IP(I1)=I1+INT(XDIS/dx)
           I=MIN(II-2,IP(I1))
           I=MAX(3   ,IP(I1))
	IS=INT(-SIGN(1.,U(I)))
        I12=min(I,I+IS)
        sgu=SIGN(1.,U(I))
	SDX=DX
        sdx2=dx*dx
        sdx3=dx*dx*dx
        c=-sgu*6.0/sdx*rho(i12)+sgu*6.0/sdx*fn(i)-2.*deltal(I12)
        a=-4.0*deltal(I12)/sdx2
     1   +sgu*4.0/sdx3*(fn(i)-fn(i+is))
        b=-6.0/sdx2*rho(i12)-sgu*6.0*deltal(I12)/sdx
     1   +3.0/sdx2*(3.0*fn(i)-fn(i+is))
	D=XDIS-DX*DBLE(IP(I1)-I1)
	F(I1)=FN(I)+c*D+b*D*D+a*D*D*D
        drho(i1)=-(FN(I)+c*D/2.0+b*D*D/3.0+a*D*D*D/4.)*D
100	CONTINUE
        drho(ii-1)=drho(ii-2)
        drho(2)   =drho(3)
	DO 200 I1=3,II-2
        I=MIN(II-2,IP(I1))
        I=MAX(3   ,IP(I1))
        rhosum=0.0
        do 202 i2=ip(i1),ip(i1+1)-1
           rhosum=rhon(i2)+rhosum
 202       continue
	rho(I1)=rhosum-drho(I1+1)/DX+drho(I1)/DX
200 	CONTINUE
        NSUB=1.
        DT=DT/FLOAT(NSUB)
        DO 105 N=1,NSUB
	DO 110 I1=3,II-2
        DT=DT*FLOAT(N)
        UX=(U(I1+1)-U(I1-1))/DX/2.0
        UXX=(U(I1+1)+U(I1-1)-2.*U(I1))/DX/DX
           XDIS=-DT*U(I1)+DT*DT*U(I1)*UX/2.0
     1                   -DT*DT*DT*U(I1)*(UX*UX+U(I1)*UXX)/6.0
           IP(I1)=I1+INT(XDIS/DX)
           I=MIN(II-2,IP(I1))
           I=MAX(3   ,IP(I1))
        DT=DT/FLOAT(N)
	F(I1)=F(I1)*(1.0-DT*((u(i+1)-u(i-1))/dx/4.0
     1  + (u(i+1)-u(i-1))/dx/4.0 ))
 110    CONTINUE
 105    CONTINUE
        DT =FLOAT(NSUB)*DT
	F(2)=F(3)
	F(1)=F(2)
	F(II-1)=F(II-2)
	F(II)=F(II-1)
	RHO(2)=RHO(3)
	RHO(1)=RHO(2)
	RHO(II-1)=RHO(II-2)
	RHO(II)=RHO(II-1)
	RETURN
	END
        SUBROUTINE CSL3_CW(F,rho,U,DT,DX,II)
        IMPLICIT REAL(A-H,O-Z)
        parameter(im=400)
        DIMENSION F(II),U(II),FN(Im),rho(II)
        DIMENSION drho(IM),RHON(im),h(im)
        DIMENSION IP(IM),DELTAL(IM)
        intrinsic sign
	DO 10 I=1,II
	FN(I)=F(I)
	RHON(I)=RHO(I)
	h(I)=0.0
        IP(I)=I
        DELTAL(I)=0.0
10	CONTINUE
	DO 1 I=2,II-1
        h(i)=3./2.*rho(i)-0.25*(fn(i)+fn(i+1))
1	CONTINUE
CCCCCCCCCCCCCCCCC CW reconstruction CCCCCCCCCCCCCCCCCCCCC
	DO 101 I=2,II-1
	DFP12=h(I+1)-h(I)
	DFN12=h(I)-h(I-1)
	DFI=0.5*(h(I+1)-h(I-1))
	IF (DFI.GE.0.0) THEN
	SGN=1.0
	ELSE
	SGN=-1.0
	ENDIF
	DELTAL(I)=MIN1(ABS(DFI),2.0*ABS(DFP12),2.0*ABS(DFN12))*SGN
	DELTAL(I)=deltal(i)/dx
	IF (DFP12*DFN12.LE.0.0) DELTAL(I)=0.0
 101    CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
	DO 100 I1=3,II-2
        UX=(U(I1+1)-U(I1-1))/DX/2.0
        UXX=(U(I1+1)+U(I1-1)-2.*U(I1))/DX/DX
        XDIS=-DT*U(I1)+DT*DT*U(I1)*UX/2.0
     1       -DT*DT*DT*U(I1)*(UX*UX+U(I1)*UXX)/6.0
cx        XDIS=XDIS/DX
           IP(I1)=I1+INT(XDIS/dx)
           I=MIN(II-2,IP(I1))
           I=MAX(3   ,IP(I1))
	IS=INT(-SIGN(1.,U(I)))
        I12=min(I,I+IS)
        sgu=SIGN(1.,U(I))
	SDX=DX
        sdx2=dx*dx
        sdx3=dx*dx*dx
        c=-sgu*6.0/sdx*rho(i12)+sgu*6.0/sdx*fn(i)-2.*deltal(I12)
        a=-4.0*deltal(I12)/sdx2
     1   +sgu*4.0/sdx3*(fn(i)-fn(i+is))
        b=-6.0/sdx2*rho(i12)-sgu*6.0*deltal(I12)/sdx
     1   +3.0/sdx2*(3.0*fn(i)-fn(i+is))
	D=XDIS-DX*DBLE(IP(I1)-I1)
	F(I1)=FN(I)+c*D+b*D*D+a*D*D*D
        drho(i1)=-(FN(I)+c*D/2.0+b*D*D/3.0+a*D*D*D/4.)*D
100	CONTINUE
        drho(ii-1)=drho(ii-2)
        drho(2)   =drho(3)
	DO 200 I1=3,II-2
        I=MIN(II-2,IP(I1))
        I=MAX(3   ,IP(I1))
        rhosum=0.0
        do 202 i2=ip(i1),ip(i1+1)-1
           rhosum=rhon(i2)+rhosum
 202       continue
	rho(I1)=rhosum-drho(I1+1)/DX+drho(I1)/DX
200 	CONTINUE
        NSUB=1.
        DT=DT/FLOAT(NSUB)
        DO 105 N=1,NSUB
	DO 110 I1=3,II-2
        DT=DT*FLOAT(N)
        UX=(U(I1+1)-U(I1-1))/DX/2.0
        UXX=(U(I1+1)+U(I1-1)-2.*U(I1))/DX/DX
           XDIS=-DT*U(I1)+DT*DT*U(I1)*UX/2.0
     1                   -DT*DT*DT*U(I1)*(UX*UX+U(I1)*UXX)/6.0
           IP(I1)=I1+INT(XDIS/DX)
           I=MIN(II-2,IP(I1))
           I=MAX(3   ,IP(I1))
        DT=DT/FLOAT(N)
	F(I1)=F(I1)*(1.0-DT*((u(i+1)-u(i-1))/dx/4.0
     1  + (u(i+1)-u(i-1))/dx/4.0 ))
 110    CONTINUE
 105    CONTINUE
        DT =FLOAT(NSUB)*DT
	F(2)=F(3)
	F(1)=F(2)
	F(II-1)=F(II-2)
	F(II)=F(II-1)
	RHO(2)=RHO(3)
	RHO(1)=RHO(2)
	RHO(II-1)=RHO(II-2)
	RHO(II)=RHO(II-1)
	RETURN
	END
        SUBROUTINE CSL2(F,rho,U,DT,DX,II)
        IMPLICIT REAL(A-H,O-Z)
        parameter(im=400)
        DIMENSION F(II),U(II),FN(Im),rho(II)
        DIMENSION drho(IM),RHON(im)
        DIMENSION IP(IM)
        intrinsic sign
	DO 10 I=1,II
	FN(I)=F(I)
	RHON(I)=RHO(I)
        IP(I)=I
10	CONTINUE
	DO 100 I1=3,II-2
        UX=(U(I1+1)-U(I1-1))/DX/2.0
        UXX=(U(I1+1)+U(I1-1)-2.*U(I1))/DX/DX
        XDIS=-DT*U(I1)+DT*DT*U(I1)*UX/2.0
     1       -DT*DT*DT*U(I1)*(UX*UX+U(I1)*UXX)/6.0
           IP(I1)=I1+INT(XDIS/dx)
           I=MIN(II-2,IP(I1))
           I=MAX(3   ,IP(I1))
	IS=INT(-SIGN(1.,U(I)))
        I12=min(I,I+IS)
        sgu=SIGN(1.,U(I))
	SDX=DX
        sdx2=dx*dx
        sdx3=dx*dx*dx
        b=-sgu*6.0/sdx*rho(i12)+sgu*2.0/sdx*(fn(i+is)+2.*fn(I))
        a=-6.0/sdx2*rho(i12)+3.0/sdx2*(fn(i)+fn(i+is))
	D=XDIS-DX*DBLE(IP(I1)-I1)
	F(I1)=FN(I)+b*D+a*D*D
        drho(i1)=-(FN(I)+b*D/2.0+a*D*D/3.0)*D
100	CONTINUE
        drho(ii-1)=drho(ii-2)
        drho(2)   =drho(3)
	DO 200 I1=3,II-2
        I=MIN(II-2,IP(I1))
        I=MAX(3   ,IP(I1))
        rhosum=0.0
        do 202 i2=ip(i1),ip(i1+1)-1
           rhosum=rhon(i2)+rhosum
 202       continue
	rho(I1)=rhosum-drho(I1+1)/DX+drho(I1)/DX
200 	CONTINUE
        NSUB=1.
        DT=DT/FLOAT(NSUB)
        DO 105 N=1,NSUB
	DO 110 I1=3,II-2
        DT=DT*FLOAT(N)
        UX=(U(I1+1)-U(I1-1))/DX/2.0
        UXX=(U(I1+1)+U(I1-1)-2.*U(I1))/DX/DX
           XDIS=-DT*U(I1)+DT*DT*U(I1)*UX/2.0
     1                   -DT*DT*DT*U(I1)*(UX*UX+U(I1)*UXX)/6.0
           IP(I1)=I1+INT(XDIS/DX)
           I=MIN(II-2,IP(I1))
           I=MAX(3   ,IP(I1))
        DT=DT/FLOAT(N)
	F(I1)=F(I1)*(1.0-DT*((u(i+1)-u(i-1))/dx/4.0
     1  + (u(i+1)-u(i-1))/dx/4.0 ))
 110    CONTINUE
 105    CONTINUE
        DT =FLOAT(NSUB)*DT
	F(2)=F(3)
	F(1)=F(2)
	F(II-1)=F(II-2)
	F(II)=F(II-1)
	RHO(2)=RHO(3)
	RHO(1)=RHO(2)
	RHO(II-1)=RHO(II-2)
	RHO(II)=RHO(II-1)
	RETURN
	END
C   ************************
      SUBROUTINE GRAPH(F1,DT,DX,WKID,ISTEP,ISHAPE)
C   ************************
C
      PARAMETER (NX=400)
      DIMENSION F1(NX)
      DIMENSION XX(NX),YY(NX)
      DIMENSION XA(NX),YA(NX)

      character(len=14) :: nom_fichier
      character(len=4) :: cstep
C
      SCALX=0.8/FLOAT(NX)*2.0
      SCALY=0.25
c     CALL GSMKSC(0.03)

      call wglcol(0)
      call wglclr

      call wglcol(1)
C
        XX(1) = 0.0
        XX(2) = XX(1)
        YY(1) = 0.2*SCALY
        YY(2) = 1.4*SCALY
        call wglmvx(xx(1),yy(1))
        call wgldrx(xx(2),yy(2))

*        CALL GPL(2,XX,YY)
        XX(1) = 0.0
        XX(2) = FLOAT(NX-1)*SCALX
        YY(1) = 0.2*SCALY
        YY(2) = YY(1)
*        CALL GPL(2,XX,YY)

        call wglmvx(xx(1),yy(1))
        call wgldrx(xx(2),yy(2))
C
        ist=50
        imd=65
        ien=80
        XSIF=1.0*DT*FLOAT(ISTEP)
        IF(ISHAPE.EQ.1) THEN
        XX(1)=0.0
        XX(2)=(FLOAT(IST-1)+XSIF)*SCALX
        XX(3)=(FLOAT(IMD)+XSIF)*SCALX
        XX(4)=(FLOAT(IEN)+XSIF)*SCALX
        XX(5)=FLOAT(NX)*SCALX
        YY(1)=0.2*SCALY
        YY(2)=YY(1)
        YY(3)=1.2*SCALY
        YY(4)=YY(1)
        YY(5)=YY(1)

        call wglmvx(xx(1),yy(1))
        do i=2,5
           call wgldrx(xx(i),yy(i))
        enddo
*     CALL GPL(5,XX,YY)
C
       ELSE IF(ISHAPE.EQ.2) THEN
        XX(1)=0.0
        XX(2)=(FLOAT(IST)+XSIF)*SCALX
        XX(3)=XX(2)
        XX(4)=(FLOAT(IEN)+XSIF)*SCALX
        XX(5)=XX(4)
        XX(6)=FLOAT(NX)*SCALX
        YY(1)=0.2*SCALY
        YY(2)=YY(1)
        YY(3)=1.2*SCALY
        YY(4)=YY(3)
        YY(5)=YY(1)
        YY(6)=YY(1)
*        CALL GPL(5,XX,YY)
      ENDIF
      XX(1)=FLOAT(1)*SCALX
      YY(1)=(F1(1)+0.22)*SCALY
      call wglmvx(xx(1),yy(1))

      do 100 i=2,nx
         XX(I)=FLOAT(I-1)*SCALX
         YY(I)=(F1(I)+0.22)*SCALY
         call wgldrx(xx(i),yy(i))
 100  CONTINUE

!      if (istep == 1) then
!          write(cstep,'(i3.3)') istep
!          cstep = trim(cstep)
!          nom_fichier = 'test'//cstep//'.png'
         write(nom_fichier,'(a5,i4.4,a4') 'image',istep,'.png'
         print *, nom_fichier
         call create_image(nom_fichier)
!      endif
C
*      CALL GPM(NX,XX,YY)
C
      RETURN
      END

      subroutine gpl
      return
      end
      subroutine gpm
      return
      end

*/* RMNLIB - Library of useful routines for C and FORTRAN programming
* * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
* *                          Environnement Canada
* *
* * This library is free software; you can redistribute it and/or
* * modify it under the terms of the GNU Lesser General Public
* * License as published by the Free Software Foundation,
* * version 2.1 of the License.
* *
* * This library is distributed in the hope that it will be useful,
* * but WITHOUT ANY WARRANTY; without even the implied warranty of
* * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* * Lesser General Public License for more details.
* *
* * You should have received a copy of the GNU Lesser General Public
* * License along with this library; if not, write to the
* * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
* * Boston, MA 02111-1307, USA.
* */
      PROGRAM SLABTEST
*
      IMPLICIT NONE 
* 
      INTEGER I,J,K,N,NI,NJ,NK,INBR
      INTEGER MTL,MTLO
      INTEGER NIL,NJL
      INTEGER NXGRIDL,NYGRIDL
*     latlon grid for Wave Model
      PARAMETER (NIL=66,NJL=46)
      PARAMETER (NXGRIDL=NIL,NYGRIDL=NJL)
      PARAMETER (MTL=16,MTLO=16)

*
      REAL l(NIL,NJL)
      REAL xpl(MTL)
      REAL cubel(NIL,NJL,MTL)
      REAL slabl(NIL,MTL)
      REAL xlat0,xlon0,dlat,dlon
      integer xnio_cnt
      integer xniol(NIL,NJL,2)
      character *4 sf_end
      data sf_end/'SLB9'/
      integer mtasl(MTL)
      data mtasl/1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1/
      real mtmull(MTL)
      data mtmull/1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,
     +            1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0/
      real mtaddl(MTL)
      data mtaddl/0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     +            0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0/
      
      character*4 nomvarl(MTL)
      data nomvarl/'WH','UV','WD','MP','PD','SZ','DS','DE',
     +             'SQ','UU','VV','WU','WV','WX','WY','WZ'/
      integer ipp1l(MTL)
      data ipp1l/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      character*4 nvarl(MTLO)
      integer ip1l(MTLO),ip2l(MTLO),ip3l(MTLO),datypl(MTLO)
      integer ip3ll(MTLO)
      character*1 typvarl(MTLO)
      integer nbitl(MTLO)
      integer ifltl(MTLO)


      REAL xgridl(NXGRIDL),ygridl(NYGRIDL)
*
      integer dateo,datyp,deet,nbits,npas
      integer ippl
      integer s_dateo(2)
      data ippl/24/
      integer ip1,ip2,ip3,ig1,ig2,ig3,ig4,iprm
      integer n_ig1,n_ig2,n_ig3,n_ig4
      integer swa,lng,ubc,dltf,extra1,extra2,extra3
*
      integer xg1,xg2,xg3,xg4
*
      character grtyp*4,gxtyp*4,typvar*1,nom*2
      character s_etiket*8,etiket*8
*
*
      integer f_hand,ier
      INTEGER  FSTFRM, FSTOUV, FSTLIR, FSTVOI
      EXTERNAL FSTFRM, FSTOUV, FSTLIR ,FSTVOI,CIGAXG,CXGAIG
      INTEGER  FSTPRM, FSTECR, FNOM
      EXTERNAL FSTPRM, FSTECR, FNOM, FCLOS, NEWDATE
      INTEGER  SLABINI, SLABDSC, SLABXTR, SLABEND
      EXTERNAL SLABINI, SLABDSC, SLABXTR, SLABEND
 
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*
      xnio_cnt = 0
      INBR = FNOM ( 20 , '1998010700_024', 'STD+RND' , 0 ) 
      INBR = FNOM ( 30 , 'final.fst',  'STD+RND' , 0 ) 
* 
* 
      INBR = FSTOUV (20, 'RND') 
      INBR = FSTOUV (30, 'RND') 
*
      J=0
      DO 15 I=1,MTL
         if (mtasl(i).ne.0) then
             j = j+1
             nvarl(j)=nomvarl(i)
             ip1l(j)=ipp1l(i)
         endif
 15   CONTINUE
      
*     initialization of MTL variables
      DO 20 I=1,MTLO
         typvarl(i)='P'
         datypl(i)=1
         nbitl(i)=16
         ifltl(i)=4
         xpl(i)=0.0
         ip2l(i)=ippl
         ip3l(i)=0
         ip3ll(i)=1
 20   CONTINUE


*     Define XNIO's
*     xniol(i,j,1) extracts the whole LL grid(L) (NIL*NJL)
*     xniol(i,j,2) extracts every second point (NIL*NJL)from the L grid(L)
*                  to give L grid (S)
*
*
      DO 40 J=1,NJL
         DO 35 I=1,NIL
            xniol(i,j,1)= i + (j-1)*NIL
            xniol(i,j,2)=0
 35   CONTINUE
 40   CONTINUE



      gxtyp=grtyp
         xg1=ig1
         xg2=ig2
         xg3=ig3
         xg4=ig4
*
*    read the variable grid L records and fill in the cubel
      DO 115 I=1,MTL
       INBR = FSTLIR(cubel(1,1,I),20,NI,NJ,NK,-1,' ',ipp1l(i),
     +                 ippl,-1,' ',nomvarl(i))
 115   CONTINUE
*    get information from these records
         iprm = fstprm(inbr,DATEO,DEET,NPAS,NI,NJ,NK,NBITS,
     +                 DATYP,IP1,IP2,IP3,TYPVAR,NOM,ETIKET,
     +                 GRTYP,IG1,IG2,IG3,IG4,SWA,LNG,DLTF,UBC,
     +                 EXTRA1,EXTRA2,EXTRA3)
*    get the date,time
      print *,'before newdate, dateo=',dateo

      call newdate(dateo,s_dateo(1),s_dateo(2),-3)
      call newdate(dateo,s_dateo(1),s_dateo(2),3)
      print *,'after newdate, dateo=',dateo

*     initialization of the slab file
      s_etiket="STUFF"
      f_hand = slabini('slabfile1',s_dateo,npas,deet,s_etiket)
      print *,'f_hand=',f_hand
*     description of slab 2+3 (for variable grid L NIL*NJL)
      print *,'BEFORE slabdsc for slab1'
      print *,'gxtyp=',gxtyp,'grtyp=',grtyp
      print *,'xg1,xg2,xg3,xg4=',xg1,xg2,xg3,xg4
      print *,'nio,njo,NXGRIDL,NYGRIDL=',NIL,NJL,NXGRIDL,NYGRIDL
      print *,'ig1,ig2,ig3,ig4=',ig1,ig2,ig3,ig4
      print *,'mt_nrows=',MTLO
      print *,'ip1=',ip1l
      print *,'ip2=',ip2l
      print *,'ip3=',ip3l
      print *,'ip3ll=',ip3ll
      print *,'typvar=',typvarl
      print *,'nvar=',nvarl
      print *,'datyp=',datypl
      print *,'nbit=',nbitl
      print *,'iflt=',ifltl

      write(6,*)'IG1,IG2,IG3,IG4=',ig1,ig2,ig3,ig4
      CALL CIGAXG(grtyp,XLAT0,XLON0,DLAT,DLON,ig1,ig2,ig3,ig4)
      write(6,*)'XLAT0,XLON0,DLAT,DLON=',XLAT0,XLON0,DLAT,DLON
      CALL CXGAIG(grtyp,N_IG1,N_IG2,N_IG3,N_IG4,xlat0,
     + xlon0,dlat*2.0,dlon*2.0)
      write(6,*)'new IG1,IG2,IG3,IG4=',n_ig1,n_ig2,n_ig3,n_ig4

c   nobug
c     grtyp='K'
      ier=slabdsc(f_hand,1,gxtyp,xg1,xg2,xg3,xg4,NIL,NJL,
     + NXGRIDL,NYGRIDL,
     + xgridl,ygridl,grtyp,ig1,ig2,ig3,ig4,MTLO,0,
     + typvarl,nvarl,ip1l,ip2l,ip3l,datypl,nbitl,ifltl,xpl)

*     writeout NIL*NJL L records to RPN standard file
      n=1
      DO 130 K=1,MTL
         IF (mtasl(k).eq.1) THEN
      DO 125 J=1,NJL
         DO 120 I=1,NIL
            l(i,j)=cubel(i,j,k)*mtmull(k) + mtaddl(k)
 120  CONTINUE
 125  CONTINUE
      ier = fstecr(l,l,-1*nbitl(n),30,dateo,deet,npas,NIL,NJL,
     + 1,ip1l(n),ip2l(n),ip3l(n),typvarl(n),nvarl(n),s_etiket,
     + grtyp,ig1,ig2,ig3,ig4,datypl(n),.false.)

       n = n+1
       ENDIF
 130  CONTINUE
      

      print *,'slabxtr slabid 1'

      DO 250 J=1,NJL
      DO 240 K=1,MTL
      DO 230 I=1,NIL
      slabl(i,k)=cubel(i,j,k) 
 230  CONTINUE
 240  CONTINUE
      ier=slabxtr(f_hand,1,NIL,xniol(1,J,1),MTL,mtasl,
     + mtaddl,mtmull,slabl)
 250  CONTINUE

  
*
      ier=slabend(f_hand,sf_end)


*
      INBR = FSTFRM (20)
      INBR = FSTFRM (30)
      CALL FCLOS(20)
      CALL FCLOS(30)
* 
      STOP
      END 

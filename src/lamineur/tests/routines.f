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
c     ***************************************************************
c     *                     A S S E M B L E                         *
c     * Object :                                                    *
c     *         To assemble data field                              *
c     *                                                             *
c     * Arguments :                                                 *
c     *            IN     ni    : 1st dimension of field ZOUT       *
c     *            IN     nj    : 2nd dimension of field ZOUT       *
c     *            IN     nrows : 3rd dimension of field ZOUT       *
c     *            IN     slab  : data to assemble                  *
c     *            IN     nX    : dimension of hxpos                *
c     *            IN     hxpos : indicator of position in the grid *
c     *                                                             *
c     *            OUT    ZOUT  : field to return (assembled)       *
c     *                                                             *
c     ***************************************************************
      subroutine assemble(ZOUT,ni,nj,nrows,slab,nX,hxpos)
      implicit none
      
      integer nj, ni, nX, nrows
      real ZOUT(ni * nj, nrows)
      integer hxpos(nX)
      real slab(nX,nrows)
      integer I,k
      do k=1, nrows
         do I=1, nX
            ZOUT(hxpos(I), k) = slab(I,k)
         enddo
      enddo
      return
      end
c     ***************************************************************
c     *                     W R T S T D F                           *
c     * Object :                                                    *
c     *         To write standard file (FSTD)                       *
c     *                                                             *
c     * Arguments :                                                 *
c     *            IN    ZOUT   : data field to output              *
c     *            IN    iun    : unit number of the file           *
c     *            IN    dateo  : origin date of the field          *
c     *            IN    deet   : time step length in seconds       *
c     *            IN    npas   : time step number                  *
c     *            IN    ni     : X   dimension of field output     *
c     *            IN    nj     : Y   dimension of field output     *
c     *            IN    nxgrid : dimension of >> positional record *
c     *            IN    nygrid : dimension of ^^ positional record *
c     *            IN    nrows  : Z   dimension of field output     *
c     *            IN    ip1    : descriptor 1 (1 to 32767)         *
c     *            IN    ip2    : descriptor 2 (1 to 32767)         *
c     *            IN    ip3    : descriptor 3 (1 to 32767)         *
c     *            IN    typvar : type of field(P,C,A)              *
c     *            IN    nomvar : name of field                     *
c     *            IN    etiket : 12character stamp                 *
c     *            IN    grtyp  : grid type                         *
c     *            IN    ig1    : grid descriptor 1 (0 to 2047)     *
c     *            IN    ig2    : grid descriptor 2 (0 to 2047)     *
c     *            IN    ig3    : grid descriptor 3 (0 to 65535)    *
c     *            IN    ig4    : grid descriptor 4 (0 to 65535)    *
c     *            IN    datyp  : type of data field (real,integer..*
c     *            IN    Nextra : number of extra parameters        *
c     *                           (Nextra >= 0)                     *
c     *            IN    xtra   : field of optionnal variable       *
c     *                                (absent IF Nextra = 0)       *
c     *            IN    nbits  : number of bits for each data field*
c     *            IN    iflt   : number of filter passes for "     *
c     *            IN    list   : list of filter numbers            *
c     *            IN    L      : dimension of "list"               *
c     *            IN    S      : field record number               *
c     *                                                             *
c     ***************************************************************
      subroutine wrtstdf (ZOUT,iun, dateo, deet, npas, ni, nj,nxgrid,
     %           nygrid,  nrows, ip1, ip2, ip3, typvar,nomvar, etiket,
     %           grtyp, ig1, ig2, ig3, ig4, datyp,Nextra,xtra, nbits,
     %           iflt,list,L, S)
      implicit none
      integer fstecr
      integer ni, nj, nrows, k, Nextra, i, j
      integer nxgrid, nygrid
      real ZOUT(ni , nj, nrows), work(1), xtra(nrows, Nextra)
      integer ip1(nrows), ip2(nrows), ip3(nrows),npak, nbits(nrows)
      integer ig1, ig2, ig3, ig4
      integer iun, datyp(nrows)
      integer npas, deet, dateo
      character *4 nomvar(nrows)
      character *4 typvar(nrows)
      character *4 grtyp
      character *12 etiket
      integer ierr, ier, S ,iig3, iig4
      real ZWRK(nxgrid,nj)
      integer L, iflt(nrows)
      integer list(L)
      integer sum
      real FACT(L, (L + 1)/2)
      
C     print *,'L=',L
      iig3 = iand(ig3,65535)
      iig4 = iand(ig4,65535)
      do k=1, (L+1)/2
         do I=1, L
            fact(I,k) = 0
         enddo
      enddo
      do k=1,(L + 1)/2
         sum = 0
         do I=k, L - k + 1
            sum = sum + list(I)
         enddo
         do I=k,L - k + 1
            FACT(I,k) = float(list(I)) / float(sum)
         enddo
      enddo
C     print *,'grtyp,nxgrid,ni,nj=',grtyp(1:1),nxgrid,ni,nj

      do i=1,12
        if ( ichar(etiket(i:i)) .eq. 0 ) etiket(i:i)=' '
      enddo
      do 300 k=1, nrows
         do i=1,4
            if ( ichar(nomvar(k)(i:i)) .eq. 0 ) nomvar(k)(i:i)=' '
         enddo
         npak = -nbits(k)
c *** if for wraparound column needed for Z grid
         if (nxgrid .eq. (ni + 1) .and. grtyp(1:1).eq.'Z') then
            do j=1, nj
               do i=1, ni
                  ZWRK(i,j) = ZOUT(i,j,k)
               enddo
            enddo
c **** add extra column for wraparound
            do j=1,nj
               ZWRK(nxgrid,j) = ZWRK(1,j)
            enddo
            if ((iflt(k) .GT. 0) .and. (L .gt. 1)) then
               call filtre (ZWRK,nxgrid,nj,iflt(k),FACT,L)
            endif
            ierr = fstecr(ZWRK, work, npak, iun, dateo, deet,
     %           npas, nxgrid, nj, 1, ip1(k), ip2(k), ip3(k),
     %           typvar(k)(1:1), nomvar(k)(1:4), etiket(1:12),
     %           grtyp(1:1), ig1, ig2, ig3, ig4, datyp(k),
     %           .false.)

         else

c **** filter option for all grids but NOT Y grid
            if ((iflt(k) .GT. 0) .and. (L .gt. 1) .and.
     %          (grtyp(1:1) .ne. 'Y')) then
              call filtre (ZOUT(1,1,k),ni,nj,iflt(k),FACT,L)
            endif    
            ierr = fstecr(ZOUT(1,1,k), work, npak, iun, dateo, deet, 
     %           npas, ni, nj, 1, ip1(k), ip2(k), ip3(k), 
     %           typvar(k)(1:1), nomvar(k)(1:4), etiket(1:12),
     %           grtyp(1:1), ig1, ig2, iig3, iig4, datyp(k),
     %           .false.)
         endif
 300  continue
      
      if (Nextra .ne. 0) then
         ierr = fstecr(xtra ,WORK,npak, iun, 20002020,
     %        1, 1, nrows, Nextra,1, 0,0,S,'|',
     %        '||' ,'||||*||||','x',0,0,
     %        0,0,1, .false.) 
      endif
      
      return
      end
c     ***************************************************************
c     *                        F I L T R E                          *
c     * Object :                                                    *
c     *         To filter data.                                     *
c     *                                                             *
c     * Arguments :                                                 *
c     *    IN/OUT ZOUT  : data to filter                            *
c     *    IN     ni    : x dimension of data                       *
c     *    IN     nj    : y dimension of data                       *
c     *    IN     Npass : Number of passes for filtering            *
c     *    IN     facteur : Factors calculated from filter list     *
c     *    IN     L  : dimension of original list of filter numbers *
c     *                                                             *
c     ***************************************************************
      subroutine filtre (ZOUT, NI, NJ, Npass, facteur, L)
      implicit none
      
      integer NI, NJ, nrows
      integer L
      real ZOUT(NI ,NJ)
      real facteur(-L/2:L/2,(L+1)/2)
      real temp
      integer k,I,J
      integer nb_elm
      integer Npass, pass
    
      real result1(NI-2)
      real result2(NJ-2)
      
      do pass=1, Npass
         do J=1, NJ
            do I=2, NI-1
               temp = 0
               nb_elm = min(I-1,NI-I,L/2)
               do k = -nb_elm, nb_elm
                  temp = temp + ZOUT(I+k,J) *
     %                 facteur(k,(L/2+1)-nb_elm)
               enddo
               result1(I) = temp
            enddo
            do I=2, NI-1
               ZOUT(I,J) = result1(I)
            enddo
         enddo
         
         do I=1, NI
            do J=2, NJ-1
               temp=0
               nb_elm = min(J-1,NJ-J,L/2)
               do k = -nb_elm, nb_elm
                  temp = temp + ZOUT(I,J+k) * 
     %                 facteur(k,(L/2+1)-nb_elm)
               enddo
               result2(J) = temp
            enddo
            do J=2, NJ-1
               ZOUT(I,J) = result2(J)
            enddo
         enddo
      enddo
      
      return
      end
      
c     ***************************************************************
c     *                       W S T D F X Y                         *
c     * Object :                                                    *
c     *         To write record ('>>' and '^^') in standard file    *
c     *                                                             *
c     * Arguments :                                                 *
c     *            IN    xpos   : field to write (dim : nx)         *
c     *            IN    ypos   : field to write (dim : ny)         *
c     *            IN    iun    : unit number of the file           *
c     *            IN    dateo  : date of origin of the field       *
c     *            IN    deet   : time step length in seconds       *
c     *            IN    npas   : time step number                  *
c     *            IN    nx     : dimension of xpos                 *
c     *            IN    ny     : dimension of ypos                 *
c     *            IN    ip1    : descriptor 1                      *
c     *            IN    ip2    : descriptor 2                      *
c     *            IN    ip3    : descriptor 3                      *
c     *            IN    etiket : 12character stamp                 *
c     *            IN    grtyp_ : grid type for ">>" and "^^"       *
c     *            IN    ig1_   : grid descriptor 1 of ">>" and "^^"*
c     *            IN    ig2_   : grid descriptor 2 of ">>" and "^^"*
c     *            IN    ig3_   : grid descriptor 3 of ">>" and "^^"*
c     *            IN    ig4_   : grid descriptor 4 of ">>" and "^^"*
c     *            IN    ni     : X dimension of output grid        *
c     *            IN    nj     : Y dimension of output grid        *
c     *            IN    grtyp  : grid type of other records"       *
c     *                                                             *
c     ***************************************************************
      subroutine wstdfxy (xpos, ypos, iun, dateo, deet, npas, nx, ny,
     % 	                  ip1, ip2, iip3, etiket, grtyp_, ig1_,
     %                    ig2_, ig3_, ig4_, ni, nj, grtyp)
      implicit none
      integer fstecr
      integer ni, nj, nx, ny
      real xpos(nx), ypos(ny), work(1)
      integer ip1, ip2, iip3
      integer ig1_, ig2_, ig3_, ig4_
      integer datyp, npak, npas, deet, dateo
      
      integer i,j,ip3
      
      character *4 grtyp,grtyp_
      character *12 etiket
      integer ierr, iun

      ip3=iand(iip3,4095)
      if(grtyp(1:1) .eq. '#') ip3=0
c     Change compact to 32 bits, IEEE representn npak = -24 datyp=1
      npak = -32
      datyp = 5
      
      do i=1,8
        if ( ichar(etiket(i:i)) .eq. 0 ) etiket(i:i)=' '
      enddo
      if (grtyp(1:1) .eq. 'Z' .or. grtyp(1:1) .eq. '#') then
      ierr = fstecr(xpos, work, npak, iun, dateo, deet, 
     %                 npas, nx, 1, 1, ip1, ip2, ip3, 
     %                 'X', '>>', etiket,
     %                 grtyp_(1:1), ig1_, ig2_, ig3_, ig4_, datyp,
     %                 .false.)
      ierr = fstecr(ypos, work, npak, iun, dateo, deet, 
     %                 npas, 1, ny, 1, ip1, ip2, ip3, 
     %                 'X', '^^', etiket,
     %                 grtyp_(1:1), ig1_, ig2_, ig3_, ig4_, datyp,
     %                 .false.)

      else
      ierr = fstecr(xpos, work, npak, iun, dateo, deet, 
     %                 npas, ni, nj, 1, ip1, ip2, ip3, 
     %                 'X', '>>', etiket,
     %                 grtyp_(1:1), ig1_, ig2_, ig3_, ig4_, datyp,
     %                 .false.)
      ierr = fstecr(ypos, work, npak, iun, dateo, deet, 
     %                 npas, ni, nj, 1, ip1, ip2, ip3, 
     %                 'X', '^^', etiket,
     %                 grtyp_(1:1), ig1_, ig2_, ig3_, ig4_, datyp,
     %                 .false.)
      endif
      return
      end

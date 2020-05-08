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
      program tstvmm

      integer cle1,cle2,cle3,cle4,cle6,cle10
      integer clefs(20),junk(100)
      real array1,array2,array3,array4,array5,array6,hparray
      real*8 array10,hpcomp8(100),hparray8
      real compar5(25),hpcomp(100)

      integer lpiece, npiece
      character*60 attribu

      pointer (ptr1,array1(10))
      pointer (ptr2,array2(10))
      pointer (ptr3,array3(15))
      pointer (ptr4,array4(20))
      pointer (ptr5,array5(25))
      pointer (ptr6,array6(30))
      pointer (ptr10,array10(15))
      pointer (pthpa,hparray(100))
      pointer (pthpa8,hparray8(100))

      integer vmmallc, vmmpak, vmmcre, vmmlod, vmmget, vmmlck
      integer vmmulk, vmmuld, vmmuln, vmmrls, vmmlse,vmmcpk
      integer vmmsav,vmmatt,vmmrnm,vmmfgt,vmmdbg
      integer vmmpwd,vmmhpa,vmmhpd,vmmint,vmmdiag,vmmcks
      integer vmmckmx, vmmdmp
      external vmmallc, vmmpak, vmmcre, vmmlod, vmmget, vmmlck
      external vmmulk, vmmuld, vmmuln, vmmrls, vmmlse,vmmcpk
      external vmmsav,vmmatt,vmmrnm,vmmfgt,vmmdbg
      external vmmpwd,vmmhpa,vmmhpd,vmmint,vmmdiag,vmmcks
      external vmmckmx, vmmdmp

      external testit, rempli

      print *,' VMM test 1.0 vmmallc '
      ier = vmmallc(350)
*      ier = vmmallc(1000)
      call testit (ier)
      

      print *,' VMM test 1.1 vmmcre VAR #1 a VAR #4'
      cle1 = vmmcre('VAR #1',10,5,
     1         'MUSTEXIST,SAVE=Y,CL=1,W=9,INIT=R,SIZE=0')
      print*,' cle = ',cle1
      call testit (cle1)
      cle2 = vmmcre('VAR #2',15,4,'SAVE=N,CL=2,W=8,INIT=0')
      print*,' cle = ',cle2
      call testit (cle2)
      cle3 = vmmcre('VAR #3',20,6,'SAVE=Y,CL=2,W=7,INIT=0,SIZE=0')
      print*,' cle = ',cle3
      call testit (cle3)
      cle4 = vmmcre('VAR #4',25,8,'SAVE=Y,CL=6,W=6,INIT=0,SIZE=0')
      print*,' cle = ',cle4
      call testit (cle4)
      cle10 = vmmcre('VAR R8',15,1,'SAVE=Y,CL=9,W=6,INIT=R,SIZE=8')
      print *,' cle10 = ',cle10
      call testit (cle10)

      print*,' cle = ',cle1,cle2,cle3,cle4,cle10
      
      clefs(01) = cle1 +1
      clefs(02) = cle1 +3
      clefs(03) = cle1 +4

      clefs(04) = cle2 +1
      clefs(05) = cle2 +2
      clefs(06) = cle2 +3
      clefs(07) = cle2 +4

      clefs(08) = cle3 +2
      clefs(09) = cle3 +3
      clefs(10) = cle3 +4
      clefs(11) = cle3 +5
      clefs(12) = cle3 +6

      clefs(13) = cle4 +8
      clefs(14) = cle4 +7
      clefs(15) = cle4 +6
      clefs(16) = cle4 +5
      clefs(17) = cle4 +4
      clefs(18) = cle4 +3
      clefs(19) = cle4 +2
      clefs(20) = cle4 +1

      print *,' VMM test 1.2 vmmdbg TRACE '
      ier = vmmdbg('TRACE',clefs,20)
      call testit (ier)

      print *,' VMM test 1.3 vmmdbg CHECKSUM '
      ier = vmmdbg('CHECKSUM',clefs,20)
      call testit (ier)

      print *,' VMM test 2.1 vmmdmp dump des 3 structures '
      ier = vmmdmp(7)

      print *,' VMM test 2.0 vmmlod 190 mots / 200 '
      ier = vmmlod(clefs,12)
      call testit (ier)
CC      call flush(6)

      print *,' VMM test 2.1 vmmdmp dump des 3 structures '
      ier = vmmdmp(7)
      print *,'Debug apres vmmdmp ier=',ier
      call testit (ier)
      print *,' VMM test 2.2 vmmlse '
      ier = vmmlse()
      print *,' Debug vmmlse = ',ier
      call testit (ier)


      print *,' VMM test 3.0 vmmget clefs(01),cle1+3,cle2+2'
      ier = vmmget(clefs(01),ptr1,junk)
      call testit (ier)
      ier = vmmget(cle1+3,ptr2,junk)
      call testit (ier)
      ier = vmmget(cle2+2,ptr3,junk)
      print *,' ptr1 ptr2 ptr3 = ', ptr1,ptr2, ptr3
      call testit (ier)
CC      call flush(6)

      print *,' VMM test 3.1 vmmcks clefs(01) avant initialisation'
      ier = vmmcks(clefs(01),1)
      print*,' check sum = ',ier

      write(6,*) 'Debug array1 = ',array1
      write(6,*) 'Debug array2 = ',array2
      write(6,*) 'Debug array3 = ',array3
      do 100 i = 1,10
         array1(i) = float(i)
         array2(i) = float(i)
         array3(i) = 999.55
 100  continue


      print *,' VMM test 3.2 vmmcks clefs(01) apres initialisation'
      ichks1 = vmmcks(clefs(01),1)
      print*,' check sum = ',ichks1


      array1(4) = array1(2)

      print *,' VMM test 3.2 vmmcks clefs(01) apres modification'
      ichks2 = vmmcks(clefs(01),1)
      print*,' check sum = ',ichks2
      ier = 0

      if(ichks1 .eq. ichks2) ier = -1
      call testit(ier)

      print *,' VMM test 4.0 vmmdbg MEMDMP de 20 clefs '
      ier = vmmdbg('MEMDMP',clefs,20)
      call testit (ier)
      

      print *,' VMM test 5.0 vmmuld pour faire de la place au',
     %        ' prochain vmmlod et forcer l ejection de blocs'
      ier = vmmuld(clefs(07),1)
      ier = vmmuld(clefs(08),5)
      call testit (ier)
      
      print *,' VMM test 5.1 vmmdbg MEMDMP'
      ier = vmmdbg('MSG=Debug apres vmmuld',0,0)
      ier = vmmdbg('MEMDMP',clefs(7),6)

      print *,' VMM test 5.2 vmmlod((clefs(13),4)'
      print  *,' force ejection des blocs 6 a 11'
      ier = vmmlod(clefs(13),4)
      call testit (ier)

      print *,' VMM test 5.3 vmmget cle4+7'
      ier = vmmget(cle4+7,ptr5,junk)
      call testit (ier)

      print *,' VMM test 5.4 vmmget cle4+5'
      ier = vmmget(cle4+5,ptr5,junk)
      call testit (ier)

      print *,' VMM test 5.5 vmmuld cle4+5 et get du champ'
      ier = vmmuld(cle4+5,1)
      ier = vmmget(cle4+5,ptr5,junk)
      if(ier .eq. -102) ier = 0
      call testit(ier)

      print *,' VMM test 5.6 vmmlod cle4+5 et get du champ'
      ier = vmmlod(cle4+5,1)
      ier = vmmget(cle4+5,ptr5,junk)
      call testit(ier)


      do 200 i = 1,25
         array5(i) = float(i) +.1
         compar5(i) = array5(i)
 200  continue

      print *,' VMM test 6.0 vmmulk(cle4+5,1)'
      ier =  vmmulk(cle4+5,1)
      call testit (ier)

      print *,' VMM test 6.1 vmmcre de AAAAAA'
      cle6 = vmmcre('AAAAAA',30,1,'SAVE=Y,CL=3,W=5,INIT=0')
      call testit (ier)

      print *,' VMM test 6.2 vmmuld clefs(15)'
      ier = vmmuld(clefs(15),1)
      call testit (ier)

      print *,' VMM test 7.0 vmmlod test de deplacement de blocs'
      ier = vmmlod(cle6,1)
      call testit (ier)

      print *,' VMM test 7.1 vmmget de cle4+5 - check contenu'
      ier = vmmget(cle4+5,ptr5,junk)
      
      ier = 0
      do 250 i = 1, 25
          if(array5(i) .ne. compar5(i)) ier = ier -1
 250  continue
      call testit(ier)

CC      ier = vmmdmp(1)
      ier = vmmdbg('MEMDMP',clefs,20)
      print *,' VMM test 8.0 vmmrls + vmmlod du meme bloc'
      ier = vmmrls(cle2+2,1)
      ier = vmmlod(cle2+2,1)
      call testit(ier)
CC      ier = vmmdmp(1)

      print *,' VMM test  8.1 vmmrls de cle4+5'
      ier = vmmrls(cle4+5,1)
      if(ier .eq. -113) ier = 0
      call testit (ier)

      print *,' VMM test  9.0 vmmfgt de cle4+5'
      ier = vmmfgt(cle4+5,1)
      call testit (ier)

      print *,' Vmm test 9.1 vmmlod de cle4+5'
      ier = vmmlod(cle4+5,1)
      call testit(ier)

      print *,' VMM test 9.2 vmmget de cle4+5 -check contenu'
      ier = vmmget(cle4+5,ptr5,junk)
      do 300 i = 1,25
        if (array5(i) .ne. 0.0 ) ier = ier -1
 300  continue
      call testit (ier)

      do 350 i = 1,25
         array5(i) = compar5(i)
 350  continue

      print *,' VMM test 10.0 vmmsav de cle4+5'
      ier = vmmsav(cle4+5,1)
      call testit(ier)

      print *,' VMM test 10.1 vmmrls de cle4+5'
      ier = vmmrls(cle4+5,1)
      call testit(ier)

      print *,' VMM test 10.2 vmmlod de cle4+5' 
      ier = vmmlod(cle4+5,1)
      call testit(ier)

      print *,' VMM test  10.3 vmmget de cle4+5 - check contenu'
      ier = vmmget(cle4+5,ptr5,junk)
      
      ier = 0
      do 400 i = 1, 25
          if(array5(i) .ne. compar5(i)) ier = ier -1
 400  continue
      call testit(ier)

      
      print *,' VMM test 11.0 vmmrls de cle4+5'
      ier = vmmrls(cle4+5,1)
      call testit(ier)

      print *,' VMM test 11.1 vmmlck cle4+5 '
      print *,' champ pas en memoire - erreur'
      ier = vmmlck(cle4+5,1)
      print *, 'ier = ', ier
      if(ier .eq. -102) ier = 0
      call testit(ier)

      print *,' VMM test 11.2 vmmget cle4+5 '
      print *,' champ pas en memoire - erreur'
      ier = vmmget(cle4+5,ptr5,junk)
      if(ier .eq. -102) ier = 0
      call testit(ier)

      print *,' VMM test 11.3 vmmlod cle4+5 extrait sur disque'
      ier = vmmlod(cle4+5,1)
      call testit(ier)

      print *,' VMM test 11.4 deux vmmlck cle4+5 consecutifs'
      print *,' champ en memoire no lock '
      ier = vmmlck(cle4+5,1)
      print*,' Debug - ier = ', ier
      call testit(ier)

      print *,' VMM test 11.5 vmmlck cle4+5 '
      print *,' champ en memoire daja bloque'
      ier = vmmlck(cle4+5,1)
      if(ier .eq. -103) ier = 0
      call testit(ier)

      print *,' VMM test 11.6 vmmget cle4+5 '
      print *,' champ en memoire '
      ier = vmmget(cle4+5,ptr5,junk)
      ier = 0
      do 450 i = 1, 25
          if(array5(i) .ne. compar5(i)) ier = ier -1
 450  continue
      call testit(ier)

      do 500 i = 1, 25
        array5(i) = 9999.0
 500  continue

      print*,' VMM test 12.0 vmmuln de cle4+5'
      ier = vmmuln(cle4+5,1)
CC      ier = vmmdmp(7)
      call testit(ier)

      print *,' VMM test 12.1 vmmget cle4+5 '
      print *,' champ en memoire '
      ier = vmmget(cle4+5,ptr5,junk)

      print *,' VMM test 12.2 vmmget cle4+5 '
      print *,' champ en memoire '
      ier = vmmget(cle4+5,ptr5,junk)

      print*,' VMM test 12.3 vmmuln de cle4+5'
      ier = vmmuln(cle4+5,1)
CC      ier = vmmdmp(7)
      call testit(ier)

      print*,' VMM test 12.4 vmmrls de cle4+5'
      ier = vmmrls(cle4+5,1)
      call testit(ier)

      print*,' VMM test 12.5 vmmlod de cle4+5 -sur disque'
      ier = vmmlod(cle4+5,1)
      call testit(ier)

      print *,' VMM test 12.6 vmmget cle4+5 - check contenu'
      ier = vmmget(cle4+5,ptr5,junk)
      ier = 0
      do 550 i = 1, 25
          if(array5(i) .ne. compar5(i)) ier = ier -1
 550  continue
      call testit(ier)

      print *,' VMM test 13.0 vmmrls de 4 champs a partir de clef(03)'
      ier = vmmrls(clefs(3),4)
      call testit(ier)

      print*,' VMM test 13.1 vmmlse avant vmmpak'
      ier = vmmlse()
      print*,' Debug ier = ',ier
      call testit(ier)

      print*,' VMM test 13.2 vmmpak'
      ier = vmmpak()
      call testit(ier)

      print*,' VMM test 13.3 vmmlse apres vmmpak'
      ier = vmmlse()
      print*,' Debug ier = ',ier
      call testit(ier)

      print*,' VMM test 13.4 MEMDMP de 20 clefs'
CC      ier = vmmdmp(1)
  
      print*,' VMM test 14.0 VMMRNM de ''AAAAAA'' a ''BBBBBB'''
      ier = vmmrnm(cle6,'BBBBBB')
      call testit(ier)
     
      print*,' VMM test 14.1 VMMATT de ''BBBBBB'''
      ier = vmmatt('BBBBBB',lpiece, npiece,attribu)
      print*,' lpiece = ',lpiece, 'npiece=',npiece
      print*,' attributs = ',attribu
      print*, 'devrait etre: SAVE=Y,CL=3,W=5,INIT=0'
      print*,' cle = ',ier
      call testit(ier)

      do   600 i = 1, 100
         hpcomp(i) = float(i) + .01
 600  continue
      print *,' VMM test 15.0 VMMHPA de 100 mots'
      ier = vmmhpa(pthpa,100,0)
      print *,'DEBUG ier = ',ier
      if(ier .lt. 0) then
         call testit(ier)
      endif

      do   650 i = 1, 100
         hparray(i) = float(i) + .01
 650  continue

      ier = 0
      do 700 i = 1, 100
         if(abs(hpcomp(i)- hparray(i)) .gt .001) ier = ier -1
 700  continue

      call testit(ier)

      print *,' VMM test 15.1 VMMHPD de 100 mots'
      ier = vmmhpd(pthpa)
      call testit(ier)

      do   750 i = 1, 100
         hpcomp8(i) = float(i) + .01
 750  continue
      print *,' VMM test 15.2 VMMHPA de 100 mots real*8'
      ier = vmmhpa(pthpa8,100,8)

      if(ier .lt. 0) then
         call testit(ier)
      endif

      do   775 i = 1, 100
         hparray8(i) = float(i) + .01
 775  continue

      ier = 0
      do 776 i = 1, 100
         if(hpcomp8(i) .ne. hparray8(i)) ier = ier -1
 776  continue

      call testit(ier)

      print *,' VMM test 15.3 VMMHPD de 100 mots real*8'
      ier = vmmhpd(pthpa8)
      call testit(ier)

     
      print *,' VMM test 16.0 VMMPWD'
      ier = vmmpwd(36,0)
      call testit (ier)

      print *,' Quelques appels avec systemen barre'
      print *,' VMM test 16.1 VMMLOD '
      ier = vmmlod(cle6,1)
      if(ier .eq. -110) ier = 0
      call testit(ier)

      print *,' VMM test 16.2 VMMLCK '
      ier = vmmlck(cle6,1)
      if(ier .eq. -110) ier = 0
      call testit(ier)

      print *,' VMM test 16.3 VMMULN '
      ier = vmmuln(cle6,1)
      if(ier .eq. -110) ier = 0
      call testit(ier)

CC      ier = vmmdmp(1)
      print *,' VMM test 16.4 VMMGET '
      ier = vmmget(cle1+1,ptr1,junk)
      print *,' ier = ', ier
      call testit(ier)

      print *,' VMM test 16.5 VMMPWD avec bon password'
      ier = vmmpwd(36,1)
      call testit(ier)
      

      print*,' VMM test 17.0 vmmcre CCCCCC mustexist'
      cle6 = vmmcre('CCCCCC',30,1,'SAVE=Y,CL=4,W=5,INIT=0,MUSTEXIST')
      if (cle6 .eq. -111) then
         print*,' EN MODE REPRISE'
         call testit (0)
         ier = vmmuld(-1,0)
         go to 800
      endif
      call testit (ier)
      
      print*,' VMM test 17.1 vmmrnm CCCCCC pour DDD'
      ier = vmmrnm(cle6,'DDD')
      call testit(ier)


 800  continue

      print *,' VMM test 18.0 vmmlod VAR R8'
      ier = vmmlod(cle10+1,1)
      call testit(ier)
CC      call flush(6)

      print *,' VMM test 18.1 vmmget VAR R8'
      ier = vmmget(cle10+1,ptr10,junk)
      print *,' ptr10 = ', ptr10
      call testit ( ier)
CC      call flush(6)
      write(6,*) 'Debug array10 = ',array10

      print *,' VMM test 18.2 vmmsav VAR R8'
      ier = vmmsav(cle10+1,1)
      call testit(ier)

      print *,' VMM test 18.3 vmmrls VAR R8'
      ier = vmmrls(cle10+1,1)
      call testit(ier)

      print *,' VMM test 18.4 vmmlod du disque VAR R8'
      ier = vmmlod(cle10+1,1)
      call testit(ier)
CC      call flush(6)

      print *,' VMM test 18.5 vmmget VAR R8'
      ier = vmmget(cle10+1,ptr10,junk)
      print *,' ptr10 = ', ptr10
      call testit ( ier)
      write(6,*) 'Debug array10 = ',array10

      print *,' VMM test debordement VAR R8'
      call rempli(array10,36)
      ier = vmmuld(cle10+1,1) 
      if(ier  .eq. -117) ier = 0
      call testit(ier)

      print *,' VMM test 19.0 vmmint'
      ier = vmmint()
      call testit(ier)
      

      print *,' VMM test 20.0 vmmcpk sans vmmuld - erreur'
      ier = vmmcpk()
      if (ier .eq. -122) ier = 0
      call testit(ier)


      print *,' VMM test 21.0 vmmuld de tous les blocks'
      ier = vmmuld(-1,0)
      call testit(ier)
      ier = vmmdmp(7)

      print *,' VMM test 21.1 vmmcpk apres vmmuld'
      ier = vmmcpk()
      call testit(ier)

      print *,' VMM test 22.0 vmmlod 190 mots / 200 '
      ier = vmmlod(clefs,12)
      call testit (ier)


      print *,' VMM test 22.1 vmmuld de tous les blocks'
      ier = vmmuld(-1,0)
      call testit(ier)

      print *,' VMM test 22.2 vmmlod des memes 12 clefs'
      ier = vmmlod(clefs,12)
      call testit (ier)

      print *,' VMM test 22.3 vmmlod de 15 clefs ejecte tout'
      ier = vmmlod(clefs,15)
      call testit (ier)

      print *,' VMM test 22.4 vmmlod de 1001 clefs'
      ier = vmmlod(clefs,1001)
      if( ier .eq. -116) ier = 0

      call testit (ier)

      print *,' VMM test 23.0 chargement d''une clef negative'
      ier = vmmlod(-100,1)
      if (ier .eq. -101) ier = 0
      call testit(ier)

      print *,' VMM test 23.1 chargement d''une clef enorme'
      ier = vmmlod(10000000,1)
      if (ier .eq. -101) ier = 0
      call testit(ier)

      print *,' VMM test 24.0 vmmload-get clef1 mustexist '
      clefs(01) = cle1 +1 
      ier = vmmlod(clefs,1)
      ier = vmmget(clefs(01),ptr1,junk)

      ier = vmmuld(clefs,1)
      print *,' VMM test 25.0 vmmckmx sans vmmuld - erreur'
      ier = vmmckmx()
      if(ier .eq. -123) ier = 0
      call testit(ier)
      ier = vmmdmp(7)
      ier = vmmuld(-1,0)
      print *,' VMM test 25.1 vmmckmx apres vmmuld'
      ier = vmmckmx()
      call testit(ier)
      ier = vmmdmp(7)

      print *,' VMM test 25.0 vmmdiag'
      ier = vmmdiag()
      call testit (ier)



      print *,' ******* FIN DES TESTS ******* '
      stop
      end


      subroutine testit(valeur)

      integer valeur

      if(valeur .ge. 0) then
           print*,' *********  REUSSI ********'
           return
      endif

      print*,' ****  ERREUR **** ',valeur
      stop

      end

      subroutine rempli(tablo,n)
      integer n
      real tablo(n)

      integer i

      do 10 i = 1,n
         tablo(i) = 123456.7
 10   continue

      return
      end

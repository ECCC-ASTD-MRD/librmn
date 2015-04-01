
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

        integer function RPN_COMM_comm(com)
c	Luc Corbeil, 2000-11-21
c
c	lien entre chaine de caractere de communicateur
c	'GRID', 'EW' et 'NS' et leur numero assigne par
c	MPI.
c
        use rpn_comm
        implicit none
        include 'mpif.h'
!        include 'rpn_comm.h'
        character(len=*) com
        character(len=32) comm

        call rpn_comm_low2up(com,comm)

        if (comm(1:5).eq.'WORLD') then
           RPN_COMM_comm=MPI_COMM_WORLD
           return
        endif
        if (comm(1:4).eq.'GRID') then
           RPN_COMM_comm=pe_indomm
           return
        endif
	if (comm(1:4).eq.'DOMM') then
	   RPN_COMM_comm=pe_indomm
	   return
	endif
	if (comm(1:3).eq.'ALL') then
	   RPN_COMM_comm=pe_wcomm
	   return
	endif
	if(comm(1:4).eq.'DEFO') then
	   RPN_COMM_comm=pe_defcomm
           return
	endif
        if (comm(1:2).eq.'EW') then
           RPN_COMM_comm=pe_myrow
           return
        endif
        if (comm(1:2).eq.'NS') then
           RPN_COMM_comm=pe_mycol
           return
        endif
        if (comm(1:10).eq.'BLOCMASTER') then
           RPN_COMM_comm=pe_blocmaster
           return
        endif
        if (comm(1:4).eq.'BLOC') then
           RPN_COMM_comm=pe_bloc
           return
        endif
        if (comm(1:4).eq.'PEER') then
           RPN_COMM_comm=pe_mypeer
           return
        endif


        write(rpn_u,*) 'Unknown communicator ',comm,', aborting'
          stop
          
        return
        end

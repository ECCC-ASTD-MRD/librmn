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

        logical function RPN_COMM_grank(com)
c	Luc Corbeil, 2002-09-29
c
c	lien entre chaine de caractere de groupe
c	'GRID', 'EW' et 'NS' et leur numero assigne par
c	MPI.
c
        implicit none
        include 'mpif.h'

        integer group,rank,ierr
        integer  RPN_COMM_group
        external RPN_COMM_group
        character(len=*) com

        RPN_COMM_grank=.false.
 	group=rpn_comm_group(com)
        call MPI_Group_rank(group,rank,ierr)
	if(rank /= MPI_UNDEFINED) then
           RPN_COMM_grank=.true.
        endif
       
        return
        end

        logical function RPN_COMM_ngrank(group)
c	Luc Corbeil, 2002-09-29
c
c	lien entre chaine de caractere de groupe
c	'GRID', 'EW' et 'NS' et leur numero assigne par
c	MPI.
c
        implicit none
        include 'mpif.h'

        integer group,rank,ierr
        integer  RPN_COMM_group
        external RPN_COMM_group

        RPN_COMM_ngrank=.false.
c!!!!!!!if(group.lt.0) return
        call MPI_Group_rank(group,rank,ierr)
	if(rank /= MPI_UNDEFINED) then
           RPN_COMM_ngrank=.true.
        endif
       
        return
        end

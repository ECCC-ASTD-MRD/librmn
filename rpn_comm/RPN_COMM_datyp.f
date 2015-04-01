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

        integer function RPN_COMM_datyp(data_int)
c	Luc Corbeil, 2000-11-20
c	lien entre datatype et MPI_datatype
        use rpn_comm
        implicit none
        include 'mpif.h'
!        include 'rpn_comm.h'
        character(len=*) data_int
        character(len=32) datatype

        call rpn_comm_low2up(data_int,datatype)

        if (datatype(1:13).eq.'MPI_CHARACTER') then
           RPN_COMM_datyp=MPI_CHARACTER
           return
        endif
        if (datatype(1:12).eq.'MPI_INTEGER2') then
           RPN_COMM_datyp=MPI_INTEGER2
           return
        endif
        if (datatype(1:11).eq.'MPI_INTEGER') then
           RPN_COMM_datyp=MPI_INTEGER
           return
        endif
        if (datatype(1:18).eq.'MPI_DOUBLE_COMPLEX') then
           RPN_COMM_datyp=MPI_DOUBLE_COMPLEX
           return
        endif
        if (datatype(1:20).eq.'MPI_DOUBLE_PRECISION') then 
           RPN_COMM_datyp=MPI_DOUBLE_PRECISION
           return
        endif
        if (datatype(1:9).eq.'MPI_REAL4') then
           RPN_COMM_datyp=MPI_REAL4
           return
        endif
        if (datatype(1:9).eq.'MPI_REAL8') then
           RPN_COMM_datyp=MPI_REAL8
           return
        endif
        if (datatype(1:8).eq.'MPI_REAL') then
           RPN_COMM_datyp=MPI_REAL
           return
        endif
        if (datatype(1:11).eq.'MPI_COMPLEX') then
           RPN_COMM_datyp=MPI_COMPLEX
            return
        endif
        if (datatype(1:11).eq.'MPI_LOGICAL') then
           RPN_COMM_datyp=MPI_LOGICAL
           return
        endif

        write(rpn_u,*) 'Unknown datatype, aborting'
          stop
          
        return
        end

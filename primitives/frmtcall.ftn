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
#if defined (_FLOAT1)
      integer function rmtcall(sub,arg)
      integer sub,arg(41)
      integer base(0:1), ra, qqqcall
      external qqqcall


      ra = LOC(base)
      ra = -ra
*.X
      rmtcall = qqqcall(base(ra+sub),base(ra+arg(1)),
     %              base(ra+arg(2)),base(ra+arg(3)),base(ra+arg(4)),
     %              base(ra+arg(5)),base(ra+arg(6)),base(ra+arg(7)),
     %              base(ra+arg(8)),base(ra+arg(9)),base(ra+arg(10)),
     %              base(ra+arg(11)),base(ra+arg(12)),base(ra+arg(13)),
     %              base(ra+arg(14)),base(ra+arg(15)),base(ra+arg(16)),
     %              base(ra+arg(17)),base(ra+arg(18)),base(ra+arg(19)),
     %              base(ra+arg(20)),base(ra+arg(21)),base(ra+arg(22)),
     %              base(ra+arg(23)),base(ra+arg(24)),base(ra+arg(25)),
     %              base(ra+arg(26)),base(ra+arg(27)),base(ra+arg(28)),
     %              base(ra+arg(29)),base(ra+arg(30)),base(ra+arg(31)),
     %              base(ra+arg(32)),base(ra+arg(33)),base(ra+arg(34)),
     %              base(ra+arg(35)),base(ra+arg(36)),base(ra+arg(37)),
     %              base(ra+arg(38)),base(ra+arg(39)),base(ra+arg(40)),
     %              base(ra+arg(41)))
*.E
      return
      end
#endif

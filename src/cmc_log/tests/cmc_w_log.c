/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
 *                          Environnement Canada
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
main(argc,argv)
int argc;
char *argv[];
{
char *getenv();
int atoi();

if ( argc != 5 )
	{
	fprintf(stderr,"Usage:  cmc_w_log class message_no identifier text\n");
	exit(1);
	}
if ( NULL == getenv("CMC_LOGFILE") ) {
    if ( NULL == getenv("CMC_LOGFILE_PLUS") ) {
        fprintf(stderr,"CMC_LOGFILE environment variable not found\n");
        exit(2);
        }
    }
exit(c_cmcwlog(argv[1],atoi(argv[2]),argv[3],argv[4]));
}

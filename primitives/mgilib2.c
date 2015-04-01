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

/* MGILIB.C
   
   Functions written for programs that run in "parallel" and need 
   to exchange information simultaneously. MGI stands for Model Gossip
   Interface. Each program using these functions will have to compile 
   with this library. There are 6 functions:

   MGI_INIT
   to initialize a channel using the name given and open the socket.
   It will also allocate a writing buffer dynamically and if all is
   successful, it will return a channel number (socket descriptor).

   MGI_OPEN
   to open the channel in a certain mode:
   'R' for read: returns 0 to signal that data been written.
                 or returns nblks to be read
   'W' for write: returns 1 if open is ok.
   'S' for storing a restart file:returns 1 if open is ok.

   MGI_READ
   to read from a channel that is open for READ mode.
   It accepts the following type of data:
   'C' for CHARACTER
   'I' for INTEGER
   'R' for REAL
   'D' for REAL*8
   It returns the number of blocks left to read from channel.

   MGI_WRITE
   to write to a channel that is open for WRITE mode.
   It accepts the same type of data as MGI_READ.
   It returns the number of blocks written to channel.

   MGI_CLOS
   to close the mode of a channel and check to make sure all is
   transmitted as requested. It returns the status of the data
   file after it is closed.

   MGI_TERM
   to delete the PID file that was created in the beginning and 
   to release all the memory allocated dynamically. It closes all
   the filepipes therefore, breaking all the pipe connections with
   the other programs.

   ***NOTE: These functions are written to keep enough bits for the
   equivalent of an integer/float in C or integer/real*4 in FORTRAN.
   In other words, you will lose some precision with the 64-bit
   compilation unless real*8 is used.

*/

#include <stdio.h>
#include <stdlib.h>
#include <rpnmacros.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <string.h>
#include <ctype.h>
#include "mgi.h"
#include "gossip.h"

static channel chn[MAX_CHANNELS];

static int ichan = 0;
static int init = 0;
static int SIG_ACTIVE = 1;
static char PID_file[MAX_STR];
static char *mgidir;
static int *intBuffer /* , pos */;
/* static int *extBuffer; */
/* static float *fBuffer; */
/* static double *dBuffer; */

static void getmgidir();
static int makepidfile();
static void removepidfile();
static void strcopy(char *s, char *t, int charlen);
static int validchan(int chan);
static int bwrite(int chan, void *buffer, int nelem, char *dtype);
ftnword f77name (mgi_init) (char *channel_name, int lname);
ftnword f77name (mgi_open) (ftnword *f_chan, char *mode, int lmode);
ftnword f77name (mgi_read) (ftnword *f_chan, void *data, ftnword *f_nelm,char *dtype, int ltype);
ftnword f77name (mgi_write) (ftnword *f_chan, void *data, ftnword *f_nelm,char *dtype, int ltype);
ftnword f77name (mgi_clos) (ftnword *f_chan);
ftnword f77name (mgi_term) ();
/* void f77name (mgi_nosig) (); */

extern int connect_to_subchannel_by_name(char *channel, char *subchannel, char *mode);
extern int GET_ack_nack(int socket, char *message);
extern int write_record(int fclient, void *buf, int longueur, int tokensize);
extern void *read_record(int fclient, void *buf, int *longueur, int maxlongueur, int tokensize);
extern char *get_gossip_dir();
/* --------------------------------------------------------------------------- */
/* #define DEBUG */

/*********************************************************************************************/
/* to copy a string given by a fortran routine, 
   the space character is ignored, it is taken 
   here as the end of the string */
static void strcopy_(char *s1, char *s2, int lengths1, int lengths2)    
{
  int i = 0;

  while ( (*s1++ = *s2++) != ' ' && i++ < lengths2 );
  
}


/* to compare two strings given by a fortran routine, considering 
   the space character as the end of the string*/
static int f_strcmp(unsigned char *s1, unsigned char *s2, int s1length, int s2length)
{
  int i = 0;	 
  int length;
  
  if(s1length <= s2length)
    length = s1length;
  else
    length = s2length;

  while( i < length && *s1 != ' ' && *s2 != ' ' && *s1 == *s2 && *s1 != '\0' && *s2 != '\0')
    {
      s1++;
      s2++;
      i++;
    }

  
  if(*s1 == ' ')
    {
      fprintf(stderr, "mgilib2::f_strcmp(), before return if(*s1 == ' '), s1 => %s\n ", s1);
      return check_ends(s1, s2, s1length, s2length, i);
     
    }
  else if(*s2 == ' ')
    {
      fprintf(stderr, "mgilib2::f_strcmp(), before return if(*s2 == ' '), s2 => %s\n ", s2);
      return 2 + check_ends(s2, s1, s2length, s1length, i);
    }

  fprintf(stderr, "before return (*(s1) - *(s2)); s1length => %d\n ", s1length);
  fprintf(stderr, "before return (*(s1) - *(s2)); s2length => %d\n ", s2length);
  fprintf(stderr, "before return (*(s1) - *(s2)); i => %d\n ", i);
  fprintf(stderr, "before return (*(s1) - *(s2)); s1 => %s\n ", s1);
  fprintf(stderr, "before return (*(s1) - *(s2)); s2 => %s\n ", s2);
  
  return (*s1 - *s2);

}

int check_ends(char *s1, char *s2, int s1length, int s2length, int i)
{
  fprintf(stderr, "passage here, avant if(*s2 == ' '), (s1 - %d) = %s\n", i, s1 - i);
  fprintf(stderr, "passage here, avant if(*s2 == ' '), (s2 - %d) = %s\n", i, s2 - i);

  if(*s2 == ' ' )
    {
      fprintf(stderr, "pass here, if(*s2 == ' ' && *s2 == ' '), (s2 - 1) = %s\n", s2 - 1);
      return (*(s1 - 1) - *(s2 - 1));
    }
  else
    {
      fprintf(stderr, "passage here, if(*s1 == ' ' && *s2 != ' '),  (s1 - 1) = %s\n", s1 - 1);
      fprintf(stderr, "passage here, if(*s1 == ' ' && *s2 != ' '),  (s2 - 1) = %s\n", s2 - 1);
      if(i == s2length)
	    return (*(s1 - 1) - *(s2 - 1));
      else
	return -1;
    }
}
/***********************************************************************************************/


static void strcopy(char *s, char *t, int charlen)
     /* to copy a string given by a fortran routine and place the NULL 
	character at the end of the true (charlen) length of the string */
{
  int i;
  
  i = 0;
  while ( (*s++ = *t++) != ' ' && i++ < charlen);
  if (*s-- == ' ') *s = '\0';
  else *s++ = '\0';
}

static int validchan(int chan)
     /* to validate the channel number; it must be greater than
	zero and less than or equal to ICHAN*/
{
  if (chn[chan].buffer == NULL)
    return (-1);
  return(0);
}

static void getmgidir()
     /* to get the value of the environment variable "MGI_DIR" */
{
  if ( (mgidir = getenv("MGI_DIR")) == NULL)
    {
      printf("Environment variable \"MGI_DIR\" undefined --\n");
      exit(1);
    }
}

static int makepidfile()
     /* to make the PID file */
{
  char stuff[MAX_STR];
  sprintf(PID_file, "%s/%d", mgidir, getpid());
  sprintf(stuff, "%s/%s", mgidir, "PROCS");
  printf("linking :%s: to :%s:\n", PID_file, stuff);
  return(link(stuff, PID_file));
}

static void removepidfile()
     /* to remove the PID file */
{
  printf("removing %s\n", PID_file);
  unlink(PID_file);
}

static int bwrite (int chan, void *buffer, int nelem, char *dtype)
     /* To fill the write buffer of initialized channel "chan" on the server */
{
  int nb;

  send_command_to_server(chn[chan].gchannel, "WRITE");
#ifdef DEBUG
  printf("mgilib2::bwrite(), ==\n");
#endif

  if(*dtype == 'I' || *dtype == 'R')
    {
      nb = write_record(chn[chan].gchannel, (unsigned char *)buffer, nelem, sizeof(int));
      get_ack_nack(chn[chan].gchannel);
    }

  else if(*dtype == 'D')
    {
      nb = write_record(chn[chan].gchannel, (char *)buffer, nelem, sizeof(double));
      get_ack_nack(chn[chan].gchannel);

    }
  
  else if(*dtype == 'C')
    {
      nb = write_record(chn[chan].gchannel, buffer, nelem, 1);
      get_ack_nack(chn[chan].gchannel);
    }
  return nb;
}

ftnword f77name (mgi_term) ()
{
  /* */
  int chan, ier = -1;
  for (chan = 1; chan <= ichan; chan++)
    {
      if(chn[chan].name && strcmp((char *)chn[chan].name, "") && chn[chan].gchannel > 0)
	{
	  ier = send_command_to_server(chn[chan].gchannel, "END");
	  printf("MGI_TERM: subchannel \"%s\" has been closed!\n", chn[chan].name);
	  
	  if(chn[chan].buffer)
	    {
	      free(chn[chan].buffer);
	      chn[chan].buffer = NULL;
	    }
	}
    }
  return ier;
}

ftnword f77name (mgi_init) (char *channel_name, int lname)
     /* To initialize a channel given a channel_name.
	It will return a number to represent this channel (1 to MAX_CHANNELS-1 */
{
  int chan;
   
  if (init == 0)
    {
      init = 1;
    }

#ifdef DEBUG
  printf("MGI_INIT ** \n"); 
#endif
  ichan++;
  if (ichan >= MAX_CHANNELS)
    {
      printf("MGI_INIT: ERROR, Too many channels assigned; MAX = %d\n", MAX_CHANNELS);
      exit(1);
    }
  else
    {
      chan = ichan;
      if (lname < MAX_NAME)
      {
	strcopy(chn[chan].name, channel_name, lname);
      }
      else 
      {
	printf("MGI_INIT: ERROR, Length of channel name > %d chars.\n",
	       MAX_NAME-1);
	exit(1);
      }
      chn[chan].fd_data = -1;
      if (SIG_ACTIVE)
	{
	  printf("MGI_INIT: Opening channel: \"%s\" \n", chn[chan].name);
	}
    
      chn[chan].msgno_W = 0;
      chn[chan].msgno_R = 0;
      chn[chan].nblks = 0;
      chn[chan].mode = ' ';
      chn[chan].pos = 0;

    if ((intBuffer = (int *) malloc(BUFSIZE * sizeof(int))) == NULL)
      {
	printf("MGI_INIT: ERROR on channel %s: Cannot allocate memory for intBuffer\n",
	       chn[chan].name);
	exit(1);
      }
    chn[chan].buffer = intBuffer;
    }
  return(chan);
}

ftnword f77name (mgi_open) (ftnword *f_chan, char *mode, int lmode)
     /* to open a channel in mode "mode"; where mode can be:
	'R' for reading
	'W' for writing
	'S' for storing
     */
{
  int chan;
  chan = (int) *f_chan;

  /* fprintf(stderr, "default gossip dir = %s \n", get_gossip_dir()); */


  if (*mode == 'W') 
    {
      /* chn[chan].gchannel = connect_to_subchannel_by_name("mgi", chn[chan].name, "write"); */
      chn[chan].gchannel = connect_to_subchannel_by_name(get_gossip_dir(), chn[chan].name, "write");
    }
  else if (*mode == 'R') 
    {
      /* chn[chan].gchannel = connect_to_subchannel_by_name("mgi", chn[chan].name, "read"); */
      chn[chan].gchannel = connect_to_subchannel_by_name(get_gossip_dir(), chn[chan].name, "read");

    }
  else if (*mode == 'S') 
    { /* store mode (for restart files)*/
      chn[chan].mode = 'S';
      chn[chan].nblks = 0;
      chn[chan].msgno_W++;
      chn[chan].pos = 0;
    }
  if(chn[chan].gchannel < 0)
    {
      printf("MGI_OPEN, Connection Failed, the Server is may be down !!\n");
      exit(-1);
    }
  /* return chn[chan].gchannel; */
  return chan;
}

ftnword f77name (mgi_clos) (ftnword *f_chan)
     /* To close a channel and signal that it can be opened in another mode */
{
  int ier = 0, chan;
  chan = (int) *f_chan;
  
  if(chn[chan].gchannel != 0)
    {
      ier = send_command_to_server(chn[chan].gchannel, "END");
      printf("MGI_CLOS: subchannel \"%s\" is closed \n", chn[chan].name);
    }
  
  /*   if (validchan(chan) == -1) */
  /*     { */
  /*       printf("MGI_CLOS ERROR: %d is an invalid channel number\n", chan); */
  /*       return -1; */
  /*     } */
  
  if(chn[chan].buffer)
    {
      free(chn[chan].buffer);
      chn[chan].buffer = NULL;
    }  
  return ier;
  
}

ftnword f77name (mgi_write) (ftnword *f_chan, void *buffer, ftnword *f_nelem, char *dtype, int ltype)
     /* to write elements from "buffer" into the specified channel
	opened for WRITEMODE. It actually writes
	
	The following types of data (dtype) are accepted:
	'C': character
	'I': integer
	'R': real
	'D': real*8 ; note that only the precision of a real would be kept
     */
{
  int nb, chan, ier, nelem;
  int lnblnk_();

  chan = (int) *f_chan;
  nelem = (int) *f_nelem;
  ier = 1;

  if (*dtype == 'I' || *dtype == 'R' || *dtype == 'D' || *dtype == 'C') 
    {
      chn[chan].nblks++;
      printf("MGI_WRITE: dtype = %c, elts Nbr = %d, channel = %s\n", dtype[0], nelem, chn[chan].name);

#ifdef DEBUG
      printf("MGI_WRITE: dtype = %s\n", dtype);
      printf("MGI_WRITE: nelem = %d\n", nelem);
#endif

      if ((nb = bwrite(chan, (unsigned char *)buffer, nelem, dtype)) > 0)
	printf("MGI_WRITE: ERROR on %s: for msgno = %d, %d bytes written\n", chn[chan].name, chn[chan].msgno_W, nb);
      
    }

  else 
    {
      printf("MGI_WRITE: ERROR on channel %s: Unknown data type: %c\n", chn[chan].name, *dtype);
      exit(1);
    }
  ier = chn[chan].nblks;
  return(ier);
}


ftnword f77name (mgi_read) (ftnword *f_chan, void *buffer, ftnword *f_nelem, char *dtype, int ltype)

     /* to read elements directly from the data file related to the 
	specified channel into "buffer". The channel must be opened for 
	READMODE only.
	The following types of data (dtype) are accepted:
	'C': character
	'I': integer (int)
	'R': real    (float)
	'D': real*8  (double)
     */
{
  int i, ier, chan, nelem;
  int *ibuf;
  float *fbuf = (float *) buffer;
  double *dbuf = (double *) buffer;
  char * cbuf = (char *) buffer;;

  chan = (int) *f_chan;
  nelem = (int) *f_nelem;
  
  send_command_to_server(chn[chan].gchannel, "READ");
  

  if (*dtype == 'I')
    { /* integer */
      ibuf = (int *)malloc(nelem*sizeof(int));

      ibuf = (int *)read_record(chn[chan].gchannel, ibuf, &nelem, nelem, sizeof(int));
      ier = get_ack_nack(chn[chan].gchannel);
      fprintf(stderr, "MGI_READ, case: \"Integer\", elts Nbr = %d, channel = %s\n", nelem, chn[chan].name);
      
      if(ibuf != NULL)
	{
	  for(i = 0; i < nelem; i++) 
	    ((int *)buffer)[i] = ibuf[i];
	}
      else
	{
	  ier = signal_timeout();
	}
      return ier;
    }

  else if (*dtype == 'R')
    { /* float */
      fbuf = (float *)malloc(nelem*sizeof(int));
      fprintf(stderr, "MGI_READ, case 1: \"Real\", elts Nbr = %d, channel = %s\n", nelem, chn[chan].name);
      fbuf = (float *)read_record(chn[chan].gchannel, fbuf, &nelem, nelem, sizeof(int));
      ier = get_ack_nack(chn[chan].gchannel);
      
      if(fbuf != NULL)
	{
	  for(i = 0; i < nelem; i++) 
	    ((float *)buffer)[i] = fbuf[i];
	}
      else
	{
	  ier = signal_timeout();
	}
      return ier;
    }
  else if (*dtype == 'D')
    { /* double */
      dbuf = (double *)malloc(nelem*sizeof(double));
      dbuf = (double *)read_record(chn[chan].gchannel, dbuf, &nelem, nelem, sizeof(double));
      fprintf(stderr, "MGI_READ, case: \"Double, 1\", Element's Nbr = %d, channel = %s\n", nelem, chn[chan].name);
      ier = get_ack_nack(chn[chan].gchannel);
      
      if(dbuf != NULL)
	{
	  for(i = 0; i < nelem; i++) 
	    ((double *)buffer)[i] = dbuf[i];
	}
      else
	{
	  ier = signal_timeout();
	}
      return ier;
    }
  
  else if (*dtype == 'C')
    { /* character */
      cbuf = (char *)malloc(nelem*sizeof(char));
      cbuf = (char *)read_record(chn[chan].gchannel, cbuf, &nelem, nelem, sizeof(char));
      
      ier = get_ack_nack(chn[chan].gchannel);
      
      if(cbuf != NULL)
	{
	  for(i = 0; i < nelem; i++) 
	    ((char *)buffer)[i] = cbuf[i];
	  
	  fprintf(stderr, "MGI_READ, case: 'Character', elts Nbr = %d, channel = %s \n", nelem, chn[chan].name);   
	}
      else
	{
	  ier = signal_timeout();
	}
      return ier;
    }
  
  else
    {
      printf("MGI_READ: ERROR on channel %s: Unknown data type: %c\n", chn[chan].name, *dtype);
      /* exit(1); */
      return -1;
    }
  
  ier = chn[chan].nblks;
  return ier;
}

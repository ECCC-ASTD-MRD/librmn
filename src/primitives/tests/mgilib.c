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
   to exchange information at the same time. MGI stands for Model Gossip
   Interface. Each program using these functions will have to compile 
   with this library. There are 5 functions:

   MGI_INIT
   to initialize a channel using the name given and open the filepipe.
   It will also allocate a writing buffer dynamically and if all is
   successful, it will return a channel number. It will 
   also create a PID file on the first time that it is called to
   indicate that the calling program is active.

   MGI_OPEN
   to open the channel in a certain mode:
   'R' for read: returns 0 to signal that restart file has been written.
                 or returns nblks to be read
   'W' for write: returns 1 if open is ok.
   'S' for storing a restart file:returns 1 if open is ok.

   MGI_READ
   to read from a channel that is open for READ mode.
   It accepts the following type of data:
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

   MGI_NOSIG
   to disable the signals in the filepipes. This function is used
   when you want to create the initial restart file. Use this
   routine before any of the other routines and open the channel
   in STORE mode "MGI_OPEN(chan,'S')".

  ****NOTE: These functions are written to keep enough bits for the
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
#include "mgi_1.h"

static channel chn[MAX_CHANNELS];

static int ichan=0;
static int init= 0;
static int SIG_ACTIVE=1;
static char PID_file[MAX_STR];
static char *mgidir;
static int *intBuffer,pos;
static int *extBuffer;
static float *fBuffer;
static double *dBuffer;

static void getmgidir();
static int makepidfile();
static void removepidfile();
static void strcopy(char *s, char *t, int charlen);
static int validchan(int chan);
static int bwrite(int chan, void *buffer, int nelem, char *dtype);
ftnword f77name (mgi_init) (char *channel_name, int lname);
ftnword f77name (mgi_open) (ftnword *f_chan, char *mode, int lmode);
ftnword f77name (mgi_read) (ftnword *f_chan, void *data, ftnword *f_nelm,
                               char *dtype, int ltype);
ftnword f77name (mgi_write) (ftnword *f_chan, void *data, ftnword *f_nelm,
                               char *dtype, int ltype);
ftnword f77name (mgi_clos) (ftnword *f_chan);
void f77name (mgi_term) ();
void f77name (mgi_nosig) ();

static void strcopy(char *s, char *t, int charlen)
/* to copy a string given by a fortran routine and place the NULL 
character at the end of the true (charlen) length of the string */
{
   int i;

   i=0;
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
   else return(0);
}

static void getmgidir()
/* to get the value of the environment variable "MGI_DIR" */
{
 if ( (mgidir = getenv("MGI_DIR")) == NULL) {
      printf("Environment variable \"MGI_DIR\" undefined\n");
      exit(1);
      }
}

static int makepidfile()
/* to make the PID file */
{

char stuff[MAX_STR];

sprintf(PID_file,"%s/%d",mgidir,getpid());
sprintf(stuff,"%s/%s",mgidir,"PROCS");
printf("linking :%s: to :%s:\n",PID_file,stuff);
return(link(stuff,PID_file));
}

static void removepidfile()
/* to remove the PID file */
{
printf("removing %s\n",PID_file);
unlink(PID_file);
}

static int bwrite (int chan, void *buffer, int nelem, char *dtype)
/* To fill the write buffer of initialized channel "chan". When
the write buffer "chn[chan].buffer" is full, it will write the contents
out to the data file related to the channel */
{

   int nb,i,j;
   char *buf = (char *) buffer;
   ftnword  *ibuf = (ftnword *) buffer;
   ftnfloat *fbuf = (ftnfloat *) buffer;
   double *dbuf = (double *) buffer;

   intBuffer = chn[chan].buffer; /*set intBuffer to point to channel buffer*/
   pos = chn[chan].pos;
   nb = 0;
   if (*dtype == 'D'){
      if (nelem*2+pos > BUFSIZE && pos > 0) {
       nb = write(chn[chan].fd_data,intBuffer,pos*sizeof(int));
       pos = 0;
       }
      if (nelem*2 > BUFSIZE) {
         nb = write(chn[chan].fd_data,dbuf,nelem*sizeof(double)); 
         }
      else {
/* for (i=0; i < nelem; i++) printf("dbuf[%d]=%lf\n",i,dbuf[i]); */
           dBuffer = (double *) &intBuffer[pos];
           for (j=0; j < nelem; j++){
                dBuffer[j] = dbuf[j];
                pos = pos + 2;
                }
           }
   } 
   else { /* type I or R */
      if (nelem+pos > BUFSIZE && pos > 0) {
       nb = write(chn[chan].fd_data,intBuffer,pos*sizeof(int));
       pos = 0;
       }
      if (nelem > BUFSIZE) {
        if (sizeof(ftnword) > sizeof(int)){
           printf("MGI_WRITE WARNING on %s: Dyn alloc; nelem > %d(BUFSIZE)\n",
                              chn[chan].name,BUFSIZE);
           if ((extBuffer = (int *) malloc((nelem) * sizeof(int)))==NULL){
               printf("MGI_WRITE ERROR on channel %s: Cannot allocate memory for intBuffer\n",
                          chn[chan].name);
               exit(1);
               }
           intBuffer = extBuffer;
           }
        else {
           intBuffer = buffer;
           }
           }

      fBuffer = (float *) intBuffer;
      if (nelem <= BUFSIZE || sizeof(ftnword) > sizeof(int)){
         if (*dtype == 'I'){ /* integer */
/* for (i=0; i < nelem; i++) printf("ibuf[%d]=%d\n",i,ibuf[i]);*/
         for (j=0; j < nelem; j++){
          intBuffer[pos] = (int) ibuf[j];
          pos++;
          }
          }
      if (*dtype == 'R'){ /* real */
/* for (i=0; i < nelem; i++) printf("fbuf[%d]=%f\n",i,fbuf[i]); */
   for (j=0; j < nelem; j++){
          fBuffer[pos] = (float) fbuf[j];
          pos++;
          }
          }
          }
      if (nelem > BUFSIZE) { /* flush contents directly to channel */
          nb = write(chn[chan].fd_data,intBuffer,nelem*sizeof(int)); 
/* printf("MGI_WRITE_WARNING: write directly to file,bytes=%d\n",nb); */
          pos=0;
          }
          }
   if (extBuffer !=NULL){
      free(extBuffer); /* release newly allocated buffer */
      extBuffer=NULL;
      }
   chn[chan].pos = pos;
   return(nb);
}

void f77name (mgi_term) ()
/* to remove the PID file, de-allocate memory */
{
   int chan,ier;
   for (chan=1; chan < ichan; chan++){
   ier=close(chn[chan].fd_sig);
   printf("mgi_term %s is closed, status=%d\n",
                chn[chan].name,ier);
   free (chn[chan].buffer);
   chn[chan].buffer=NULL;
   }
   removepidfile();
}

void f77name (mgi_nosig) ()
/* to disable the signals between filepipes */
{
   SIG_ACTIVE=0;
   printf("MGI_NOSIG: SIGNALS are disabled\n");
}

ftnword f77name (mgi_init) (char *channel_name, int lname)
/* To initialize a channel given a channel_name.
   It will return a number to represent this channel (1 to MAX_CHANNELS-1 */
{
   int chan;
   char filename_S[MAX_STR];

   if (init == 0)
   {
   getmgidir();
   if (makepidfile()) {
      printf("unable to create PID file\n");
      exit(1);
      } 
    init = 1;
   }
       ichan++;
   if (ichan >= MAX_CHANNELS)
   {
       printf("MGI_INIT ERROR: Too many channels assigned; MAX=%d\n",
               MAX_CHANNELS);
       exit(1);
   }
   else
      {
       chan = ichan;
       if (lname < MAX_NAME)
       strcopy(chn[chan].name,channel_name,lname);
       else {
             printf("MGI_INIT ERROR: Length of channel name > %d chars.\n",
             MAX_NAME-1);
             exit(1);
            }
       sprintf(filename_S,"%s/S_%s",mgidir,chn[chan].name,chan);
       chn[chan].fd_data = -1;
       if (SIG_ACTIVE){
          chn[chan].fd_sig = open(filename_S,O_RDWR,0755);
          printf("mgi_init: opening channel %s, fd_sig=%d\n",
                         filename_S,chn[chan].fd_sig);
          }
       chn[chan].msgno_W = 0;
       chn[chan].msgno_R = 0;
       chn[chan].nblks = 0;
       chn[chan].mode = ' ';
       chn[chan].pos = 0;
       if ((intBuffer = (int *) malloc(BUFSIZE * sizeof(int)))==NULL)
       {
        printf("MGI_INIT ERROR on channel %s: Cannot allocate memory for intBuffer\n",
                chn[chan].name);
        exit(1);
        }
       chn[chan].buffer = intBuffer;
      }
   return(chan);
}

ftnword f77name (mgi_open) (ftnword *f_chan, char *mode, int lmode)
/* to open a channel in mode "mode" where mode can be:
   'R' for reading
   'W' for writing
   'S' for storing
*/
{
   int i,ier,nb,chan,fd_rst;
   int sigdata[2];
   char filename_D[MAX_STR],filename_R[MAX_STR];

   chan = (int) *f_chan;
   if (validchan(chan) == -1){
      printf("MGI_OPEN ERROR: %d is an invalid channel number\n",
              chan);
      exit(1);
      }
   ier=1;
   if (chn[chan].mode != ' ') {
       printf("MGI_OPEN ERROR: Channel %s is already open for MODE %c\n",
                 chn[chan].name,chn[chan].mode);
       exit(1);
   }
   sprintf(filename_D,"%s/D_%s",mgidir,chn[chan].name,chan);
   sprintf(filename_R,"%s/R_%s",mgidir,chn[chan].name,chan);
   if (*mode == 'W') { /* write mode */
   chn[chan].mode = 'W';
   chn[chan].nblks = 0;
   chn[chan].msgno_W++;
/* Before writing to data file, make sure it doesn't exist. This
   insures that we don't overwrite the previous data */
   while((chn[chan].fd_data=open(filename_D,O_CREAT+O_WRONLY+O_EXCL,0755))==-1)
          sleep(2);
   chn[chan].pos = 0;
   }
   else if (*mode == 'S') { /* store mode (for restart files)*/
   chn[chan].mode = 'S';
   chn[chan].nblks = 0;
   chn[chan].msgno_W++;
   while((chn[chan].fd_data=open(filename_D,O_CREAT+O_WRONLY+O_EXCL,0755))==-1)
          sleep(2);
   chn[chan].pos = 0;
   }
   else if (*mode == 'R') { /* read mode */
   chn[chan].mode = 'R';
   if (chn[chan].msgno_R == 0 && 
      (fd_rst=open(filename_R,O_RDONLY,0755)) != -1){
     printf("mgi_open restart: msgno_R=%d msgno_W=%d\n",
              chn[chan].msgno_R,chn[chan].msgno_W);
     printf("mgi_open on %s, restart file found, fd_rst=%d\n",
               filename_R, fd_rst);
       nb = read(fd_rst,sigdata,2*sizeof(int));
       chn[chan].msgno_W = sigdata[0];
       chn[chan].nblks = sigdata[1];
       close(fd_rst);
       unlink(filename_R);
       }
   else{
       chn[chan].msgno_R++;
       nb = read(chn[chan].fd_sig,sigdata,2*sizeof(int));
       chn[chan].msgno_W = sigdata[0];
       chn[chan].nblks = sigdata[1];
   /*  printf("mgi_open on %s for MODE %c:msgno_W=%d,nblks=%d\n",
         chn[chan].name, *mode, chn[chan].msgno_W,chn[chan].nblks);*/ 
       }
       ier = sigdata[1];
   if ((chn[chan].fd_data = open(filename_D,O_RDONLY,0555))==-1){
     printf("MGI_OPEN ERROR on opening %s\n", filename_D);
     exit(1);
     }
   if (chn[chan].msgno_W != chn[chan].msgno_R){
     printf("MGI_OPEN ERROR on channel %s: MISMATCH MESSAGE NO in read\n",
                 chn[chan].name);
     printf("msgno_W=%d, msgno_R=%d\n",chn[chan].msgno_W,chn[chan].msgno_R);
     exit(1);
     }
   }
   else { /* incorrect mode */
   printf("MGI_OPEN ERROR on channel %s: Incorrect Mode=%c\n",
                  chn[chan].name,*mode);
   exit(1);
   }
   return(ier);
}

ftnword f77name (mgi_clos) (ftnword *f_chan)
/* To close a channel and signal that it can be opened in another mode */
{
   int i,ier,nb,fd_rst,chan;
   int sigdata[2];
   char filename_D[MAX_STR],filename_R[MAX_STR];

   chan = (int) *f_chan;
   if (validchan(chan) == -1){
      printf("MGI_CLOS ERROR: %d is an invalid channel number\n",
              chan);
      exit(1);
      }
   sprintf(filename_D,"%s/D_%s",mgidir,chn[chan].name,chan);
   sprintf(filename_R,"%s/R_%s",mgidir,chn[chan].name,chan);

   if (chn[chan].mode == 'W'){ /* write mode */
      intBuffer=chn[chan].buffer;
      pos=chn[chan].pos;
      /* it will flush the remaining contents from the write buffer 
         into the output data file before closing the channel */
      if (pos>0){
         nb = write(chn[chan].fd_data,intBuffer,pos*sizeof(int));
         if (nb <=0)
         printf("MGI_CLOS WARNING on channel %s: Write error on %s\n",
                 chn[chan].name,filename_D);
         
         }
      sigdata[0] = chn[chan].msgno_W;
      sigdata[1] = chn[chan].nblks;
      ier=close(chn[chan].fd_data);
      nb = write(chn[chan].fd_sig,sigdata,2*sizeof(int));
/* printf("mgi_clos WRITEMODE: %s closed,status=%d,msgno=%d,nblks written=%d\n",
      filename_D,ier,chn[chan].msgno_W,chn[chan].nblks);
   printf("mgi_clos WRITEMODE on %s: msgno=%d,nblks=%d\n",
                  chn[chan].name,sigdata[0],sigdata[1]);*/
      }

   else if (chn[chan].mode == 'S'){ /* store mode */
      intBuffer=chn[chan].buffer;
      pos=chn[chan].pos;
      /* it will flush the remaining contents from the write buffer 
         into the restart file before closing the channel */
      if (pos>0){
          nb = write(chn[chan].fd_data,intBuffer,pos*sizeof(int));
          if (nb <=0)
          printf("MGI_CLOS WARNING on channel %s: Write error on %s\n",
                 chn[chan].name,filename_D);
          }
      ier=close(chn[chan].fd_data);
/* printf("mgi_clos STOREMODE: %s closed,status=%d,msgno=%d,nblks written=%d\n",
      filename_D,ier,chn[chan].msgno_W,chn[chan].nblks);*/
      sigdata[0] = 0;
      sigdata[1] = chn[chan].nblks;
      if((fd_rst=open(filename_R,O_CREAT+O_WRONLY+O_EXCL,0755))==-1)
          printf("MGI_CLOS ERROR: Cannot create restart file: %s\n",filename_R);
      nb = write(fd_rst,sigdata,2*sizeof(int));
      close(fd_rst);
      sigdata[0] = chn[chan].msgno_W;
      sigdata[1] = 0;
      if (SIG_ACTIVE) nb = write(chn[chan].fd_sig,sigdata,2*sizeof(int));
   /* printf("mgi_clos STOREMODE on %s: msgno=%d,nblks=%d\n",
                  chn[chan].name,sigdata[0],sigdata[1]);*/
      }

   else if (chn[chan].mode == 'R'){ /* read mode */
   /* check to see if everything that was read before closing
      and also delete the data file */
      if (chn[chan].nblks != 0) {
         printf ("MGI_CLOS ERROR on channel %s: blocks left to read=%d\n",
                                      chn[chan].name,chn[chan].nblks);
         exit(1);
         }
      ier=close(chn[chan].fd_data);
      unlink(filename_D);
   /* printf("mgi_clos: %s is closed, msgno=%d,status=%d\n",
               filename_D,chn[chan].msgno_R,ier);*/
      }
   else {
        printf("MGI_CLOS WARNING: Channel not open:[%s]\n",chn[chan].name);
        ier=-1;
        }
   /* reset channel to NULL mode */
   chn[chan].mode = ' ';
   chn[chan].pos = 0;
   chn[chan].nblks = 0;
   return(ier);
}
 

ftnword f77name (mgi_write) (ftnword *f_chan, void *buffer, 
                    ftnword *f_nelem, char *dtype, int ltype)
/* to write elements from "buffer" into the specified channel
opened for WRITEMODE or STOREMODE only. It actually writes
(bwrite) to a write buffer before outputting it to the data file. 
If the buffer is too small, it will allocate a bigger buffer
for this channel.
The following types of data (dtype) are accepted:
'I': integer
'R': real
'D': real*8 ; note that only the precision of a real would be kept
*/
{
   int i,ier,nb,sigdata[2],chan,nelem;

   chan = (int) *f_chan;
   if (validchan(chan) == -1){
      printf("MGI_WRITE ERROR: %d is an invalid channel number\n",
              chan);
      exit(1);
      }
   nelem = (int) *f_nelem;
   nb=0;
   if (chn[chan].mode != 'W' && chn[chan].mode != 'S'){
   printf ("MGI_WRITE ERROR on channel %s: Channel not open for WRITE\n",
                                      chn[chan].name);
        exit(1);
   }

   if (*dtype == 'I' || *dtype == 'R' || *dtype == 'D'){
     chn[chan].nblks++;
     if ((nb = bwrite(chan,buffer,nelem,dtype)) < 0) 
     printf("MGI_WRITE WARNING on %s: for msgno=%d,%d bytes written, blocks written=%d\n",
     chn[chan].name,chn[chan].msgno_W,nb,chn[chan].nblks);
     }
   else {
     printf("MGI_WRITE ERROR on channel %s: Unknown data type: %c\n",
                chn[chan].name,*dtype);
         exit(1);
        }
   ier = chn[chan].nblks;
   return(ier);
}

ftnword f77name (mgi_read) (ftnword *f_chan, void *buffer, 
                       ftnword *f_nelem, char *dtype, int ltype)
/* to read elements directly from the data file related to the 
specified channel into "buffer". The channel must be opened for 
READMODE only.
The following types of data (dtype) are accepted:
'I': integer (int)
'R': real    (float)
'D': real*8  (double)
*/
{
   int i,ier,nb,chan,nelem;
   int *ibuf = (int *) buffer;
   float *fbuf = (float *) buffer;
   ftnword *lbuf = (ftnword *) buffer;
   double *dbuf = (double *) buffer;

   chan = (int) *f_chan;
   if (validchan(chan) == -1){
      printf("MGI_READ ERROR: %d is an invalid channel number\n",
              chan);
      exit(1);
      }
   nelem = (int) *f_nelem;
   nb = 0;
   if (chn[chan].mode != 'R'){
   printf ("MGI_READ ERROR on channel %s: Not open for READ\n",
                                               chn[chan].name);
        exit(1);
   }

   if (*dtype == 'I'){ /* integer */
   if (sizeof(ftnword) == sizeof(int))
   nb = read(chn[chan].fd_data,ibuf,nelem*sizeof(int));
   else{
   nb = read(chn[chan].fd_data,&ibuf[nelem],nelem*sizeof(int));
   for (i=0; i < nelem; i++) lbuf[i]= (ftnword) ibuf[i+nelem];
   }
/* for (i=0; i < nelem; i++) printf("ibuf[%d]=%d\n",i,ibuf[i]); */
   }
   else if (*dtype == 'R'){ /* real */
   if (sizeof(ftnfloat) == sizeof(float))
   nb = read(chn[chan].fd_data,fbuf,nelem*sizeof(float));
   else{
   nb = read(chn[chan].fd_data,&fbuf[nelem],nelem*sizeof(float));
   for (i=0; i < nelem; i++) dbuf[i]= (ftnfloat) fbuf[i+nelem];
   }
/* for (i=0; i < nelem; i++) printf("fbuf[%d]=%f\n",i,fbuf[i]); */
   }
   else if (*dtype == 'D'){ /* real*8 */
   nb = read(chn[chan].fd_data,dbuf,nelem*sizeof(double));
/* for (i=0; i < nelem; i++) printf("dbuf[%d]=%lf\n",i,dbuf[i]); */
   }
   else {
        printf("MGI_READ ERROR on channel %s: Unknown data type: %c\n",
                chn[chan].name,*dtype);
        exit(1);
        }
   chn[chan].nblks--;
/* printf("mgi_read on %s: fd=%d, msgno=%d, bytes read=%d, blocks left=%d\n", 
   chn[chan].name, chn[chan].fd_data,chn[chan].msgno_R,nb, chn[chan].nblks);*/
   if (nb <=0) {
     printf("MGI_READ WARNING on %s: msgno=%d, %d bytes read, blocks left=%d\n",
             chn[chan].name,chn[chan].msgno_R,nb,chn[chan].nblks);
      }
   ier = chn[chan].nblks;
   return(ier);
}

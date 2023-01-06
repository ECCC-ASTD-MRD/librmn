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

/*! \file mgilib2.c

    Functions written for programs that run in "parallel" and need
    to exchange information simultaneously. MGI stands for Model Gossip
    Interface. Each program using these functions will have to compile
    with this library.

   These functions are written to keep enough bits for the
   equivalent of an integer/float in C or integer/real*4 in FORTRAN.
   In other words, you will lose some precision with the 64-bit
   compilation unless real*8 is used.
*/

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <string.h>
#include <strings.h>
#include <sys/types.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/resource.h>

#include <App.h>
#include <rmn/rpnmacros.h>
#include <rmn/mgi.h>
#include <rmn/gossip.h>
#include <rmn/gossip_constants.h>

#define CLOSE -5

static channel chn[MAX_CHANNELS];

static int ichan = 0;
static int init = 0;
static int SIG_ACTIVE = 1;
static char PID_file[MAX_STR];
static char *mgidir;
static int *intBuffer;
//! Number of connexion retries
int USER_TRY_CONNECT = 10;

static void getmgidir();
static int makepidfile();
static void removepidfile();
static void strcopy(char *s, char *t, int charlen);
static int validchan(int chan);
static int bwrite(int chan, void *buffer, int nelem, char *dtype);
int32_t f77name (mgi_init) (const char * const channel_name, F2Cl lname);
int32_t f77name (mgi_open) (const int32_t * const f_chan, const char * const mode, F2Cl lmode);
int32_t f77name (mgi_read) (const int32_t * const f_chan, void *data, const int32_t * const f_nelm, const char * const dtype, F2Cl ltype);
int32_t f77name (mgi_write) (int32_t *f_chan, void *data, int32_t *f_nelm, char *dtype, F2Cl ltype);
int32_t f77name (mgi_clos) (int32_t *f_chan);
int32_t f77name (mgi_term) ();
void f77name (mgi_set_timeout) (int32_t *chan, int32_t *timeout);

int32_t f77name (mgi_read_oob) ();
int32_t f77name (mgi_write_oob) ();


//! Disable the signals between filepipes
//! \deprecated
void f77name (mgi_nosig)() {
    /* SIG_ACTIVE = 0; */
    Lib_Log(APP_LIBRMN,APP_WARNING,"%s: deprecated call\n",__func__);
}


//! Copy a string given by a fortran routine
//! The space character is ignored, it is taken here as the end of the string
static void strcopy_(
    //! Destination string
    char *srcStr,
    //! Source string
    char *dstStr,
    //! Unsused
    int lengthsrcStr,
    //! Length of the source string
    int lengthdstStr
) {
    int i = 0;

    while ( (*srcStr++ = *dstStr++) != ' ' && i++ < lengthdstStr );
}


//! Utility function for f_strcmp
static int check_ends(
    char *s1,
    char *s2,
    int s2Len,
    int equalLen
) {
    if (*s2 == ' ' ) {
        return *(s1 - 1) - *(s2 - 1);
    } else {
        if (equalLen == s2Len) {
            return *(s1 - 1) - *(s2 - 1);
        } else {
            return -1;
        }
    }
}


//! Compare two strings given by a fortran routine, considering the space character as the end of the string
static int f_strcmp(
    unsigned char *s1,
    unsigned char *s2,
    int s1Len,
    int s2Len
) {
    int equalLen = 0;
    int length;

    if (s1Len <= s2Len) {
        length = s1Len;
    } else {
        length = s2Len;
    }

    while( equalLen < length && *s1 != ' ' && *s2 != ' ' && *s1 == *s2 && *s1 != '\0' && *s2 != '\0') {
        s1++;
        s2++;
        equalLen++;
    }


    if (*s1 == ' ') {
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: before return if (*s1 == ' '), s1 => %s\n",__func__,s1);
        return check_ends(s1, s2, s2Len, equalLen);
    } else if (*s2 == ' ') {
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: before return if (*s2 == ' '), s2 => %s\n",__func__,s2);
        return 2 + check_ends(s2, s1, s1Len, equalLen);
    }

    return *s1 - *s2;
}



//! Copy a string given by a fortran routine and place the NULL character at the end of the true (charlen) length of the string
static void strcopy(
    char *s,
    char *t,
    int charlen
) {
    int i = 0;

    while ( (*s++ = *t++) != ' ' && i++ < charlen);
    if (*s-- == ' ') {
        *s = '\0';
    } else {
        *s++ = '\0';
    }
}


//! Validate the channel number; it must be greater than zero and less than or equal to ICHAN
static int validchan(
    int chan
) {
    if ( chn[chan].buffer == NULL ) return -1;
    return 0;
}


//! Get the value of the environment variable "MGI_DIR"
static void getmgidir() {
    if ( (mgidir = getenv("MGI_DIR")) == NULL) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Environment variable \"MGI_DIR\" undefined\n",__func__);
    }
}

//! Make the PID file
static int makepidfile() {
    char stuff[MAX_STR];
    sprintf( PID_file, "%s/%d", mgidir, getpid() );
    sprintf( stuff, "%s/%s", mgidir, "PROCS" );
    Lib_Log(APP_LIBRMN,APP_INFO,"%s: linking :%s: to :%s:\n",__func__,PID_file,stuff);
    return link(stuff, PID_file);
}


//! Remove the PID file
static void removepidfile() {
    Lib_Log(APP_LIBRMN,APP_INFO,"%s: removing %s\n",__func__,PID_file);
    unlink(PID_file);
}


//! Fill the write buffer of initialized channel "chan" on the server
static int bwrite ( int chan, void *buffer, int nelem, char *dtype ) {
    int ier;

    Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: COMM1-W send_command_to_server() channel: %d\n",__func__,chn[chan].gchannel);

    ier = send_command_to_server(chn[chan].gchannel, "WRITE");

    if (ier < 0) {
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: Unable to send write command, send_command_to_server returns ier= %d with error on channel: %d\n",__func__,ier,chn[chan].gchannel);
        return -1;
    }

    if (*dtype == 'I' || *dtype == 'R') {
        ier = write_record(chn[chan].gchannel, (unsigned char *)buffer, nelem, sizeof(int));
    } else if (*dtype == 'D') {
        ier = write_record(chn[chan].gchannel, (char *)buffer, nelem, sizeof(double));
    } else if (*dtype == 'C') {
        ier = write_record(chn[chan].gchannel, buffer, nelem, 1);
    }

    Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: COMM8-R get_ack_nack()\n",__func__);

    if (get_ack_nack(chn[chan].gchannel)) {
        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: COMM8-R get_ack_nack() error on channel: %d\n",__func__,chn[chan].gchannel);
        return -1;
    } else {
        return 0;
    }

    return ier;
}


//! Close a channel and signal that it can be opened in another mode
//! \return Status of the data file after it is closed
int32_t f77name (mgi_clos) (
    int32_t *f_chan
) {
    int ier = 0;
    char buf[1024];
    int chan = (int) *f_chan;

    if (chn[chan].gchannel != 0) {
        snprintf(buf, 1023, "%s %s", "END", chn[chan].name);
        ier = send_command(buf);
        Lib_Log(APP_LIBRMN,APP_INFO,"%s: subchannel \"%s\" is closed\n",__func__,chn[chan].name);
    }

    if (chn[chan].buffer) {
        free(chn[chan].buffer);
        chn[chan].buffer = NULL;
    }
    return ier;
}


//! Close all channels
int32_t f77name (mgi_term) () {
    int ier = -1;

    for (int chan = 0; chan <= ichan; chan++) {
        if (chn[chan].name && strcmp((char *)chn[chan].name, "") && chn[chan].gchannel > 0) {
            ier = send_command("END");
            Lib_Log(APP_LIBRMN,APP_INFO,"%s: bchannel \"%s\" has been closed\n",__func__,chn[chan].name);

            if (chn[chan].buffer) {
                free(chn[chan].buffer);
                chn[chan].buffer = NULL;
            }
        }
    }

    return ier;
}


//! Initialize a channel using the name given and open the socket
//! Allocate a writing buffer dynamically and if all is successful, it will return a channel number (socket descriptor).
//! \return Number representing this channel (1 to MAX_CHANNELS - 1)
int32_t f77name (mgi_init) (
    const char * const channel_name,
    F2Cl lname
) {
    if (init == 0) {
        init = 1;
    }

    ichan++;
    if (ichan >= MAX_CHANNELS) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Too many channels assigned; MAX = %d\n",__func__,MAX_CHANNELS);
        return INIT_ERROR;
    }

    int chan = ichan;
    if (lname < MAX_NAME) {
        strcopy(chn[chan].name, channel_name, lname);
    } else {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Length of channel name > %d chars\n",__func__,MAX_NAME-1);
        return INIT_ERROR;
    }
    chn[chan].fd_data = -1;
    if (SIG_ACTIVE) {
        Lib_Log(APP_LIBRMN,APP_INFO,"%s: Opening channel: \"%s\"\n",__func__,chn[chan].name);
    }

    // initialize channel
    chn[chan].msgno_W = 0;
    chn[chan].msgno_R = 0;
    chn[chan].nblks = 0;
    chn[chan].mode = ' ';
    chn[chan].pos = 0;
    chn[chan].gchannel = 0;

    if ((intBuffer = (int *) malloc(BUFSIZE * sizeof(int))) == NULL) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Cannot allocate memory for intBuffer on channel %s\n",__func__,chn[chan].name);
        return INIT_ERROR;
    }

    chn[chan].buffer = intBuffer;

    return chan;
}


//! Open a channel
//! \return Channel number
int32_t f77name (mgi_open) (
    //! Channel number
    const int32_t * const f_chan,
    //! Channel mode
    //! 'R' for reading
    //! 'W' for writing
    //! 'S' for storing a restart
    const char * const mode,
    F2Cl lmode
) {
    int chan = (int) *f_chan;

    if (*mode == 'W') {
        chn[chan].gchannel = connect_to_subchannel_by_name( get_gossip_dir(0), chn[chan].name, "write" );

        if ( chn[chan].gchannel < 0 ) {
            chn[chan].gchannel = retry_connect( chan );
        }

    } else if (*mode == 'R') {
        chn[chan].gchannel = connect_to_subchannel_by_name( get_gossip_dir(0), chn[chan].name, "read" );

        if ( chn[chan].gchannel < 0 ) {
            chn[chan].gchannel = retry_connect( chan );
        }
    } else if (*mode == 'S') {
        /* store mode (for restart files)*/
        chn[chan].mode = 'S';
        chn[chan].nblks = 0;
        chn[chan].msgno_W++;
        chn[chan].pos = 0;
    }

    if (chn[chan].gchannel < 0) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Connection Failed, the Server may be down\n",__func__);
        return CONNECTION_ERROR;
    }

    /* initialize timeout table */
    init_client_table( chn[chan].gchannel );

    return chan;
}


//! Set the number of connexion retries
void f77name (mgi_set_retry_connect) (
    int32_t *nbRetries
) {
    Lib_Log(APP_LIBRMN,APP_INFO,"%s: Setting try to connect USER_TRY_CONNECT: \"%d\" times\n",__func__,(int)*nbRetries);
    if ((int) *nbRetries > 0 && (int) *nbRetries < 10) {
        USER_TRY_CONNECT = (int) *nbRetries;
    }
}

//! Get the number of connexion retries
int mgi_get_retry_connect(
    int chan
) {
    return USER_TRY_CONNECT;
}


//! Retry to establish the connexion.  Wait 10 seconds between attempts.
int retry_connect(
    //! [in] Channel number
    int chan
) {
    int PING_INTERVAL = 10;
    int maxAttempts = mgi_get_retry_connect(chan);
    int attemptsRemaining = maxAttempts;

    while ( chn[chan].gchannel < 0 && attemptsRemaining > 0 ) {
        sleep( PING_INTERVAL );
        Lib_Log(APP_LIBRMN,APP_INFO,"%s: Connection to Server Failed,  retry to connect: \"%d/%d\n",__func__,maxAttempts-attemptsRemaining+1,maxAttempts);
        chn[chan].gchannel = connect_to_subchannel_by_name( get_gossip_dir(0), chn[chan].name, "write" );
        attemptsRemaining--;
    }
    return chn[chan].gchannel;
}


//! Write elements to the specified channel
//! \return Number of blocks written to channel
int32_t f77name (mgi_write) (
    //! Channel number
    int32_t *f_chan,
    //! Source buffer
    void *buffer,
    //! Number of elements to write
    int32_t *f_nelem,
    //! Datatype
    //! 'C': character
    //! 'I': integer
    //! 'R': real
    //! 'D': real*8 ; note that only the precision of a real would be kept
    char *dtype,
    F2Cl ltype
) {
    int ier, chan, nelem;
    int lnblnk_();

    chan = (int) *f_chan;
    nelem = (int) *f_nelem;
    char *tmpstr;

    Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: data type = %c, elts Nbr = %d, subchannel = %s\n",__func__,dtype[0],nelem,chn[chan].name);

    if ( nelem <= 0 ) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Cannot write data with length = %d\n",__func__,nelem);
        return WRITE_ERROR;
    }

    if ( chn[chan].gchannel < 0 ) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Cannot connect to server using descriptor: \"%d\"\n",__func__,chn[chan].gchannel);
        return WRITE_ERROR;
    }

    if ( *dtype == 'C' ) {
        nelem = ( *f_nelem < ltype ) ? (int) *f_nelem : ltype;
        tmpstr = (char *)malloc(nelem + 1);

        strncpy( tmpstr, (char *)buffer, nelem);
        tmpstr[nelem] = '\0';

        Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: data type = %c, elts Nbr = %d, strlen = %d,  subchannel = %s\n",__func__,dtype[0],nelem,ltype,chn[chan].name);

        if ((ier = bwrite(chan, (unsigned char *)tmpstr, nelem, dtype)) < 0) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Write on %s\n",__func__,chn[chan].name);
            free(tmpstr);
            return WRITE_ERROR;
        }
        free(tmpstr);
    } else if (*dtype == 'I' || *dtype == 'R' || *dtype == 'D' ) {
        chn[chan].nblks++;

        if ((ier = bwrite(chan, (unsigned char *)buffer, nelem, dtype)) < 0) {
            Lib_Log(APP_LIBRMN,APP_ERROR,"%s: (I || R || D) error %s\n",__func__,chn[chan].name);
            return WRITE_ERROR;
        }
    } else {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Unknown data type: %c on channel %\n",__func__,*dtype,chn[chan].name);
        return WRITE_TYPE_ERROR;
    }

    if (ier < 0) {
        if (get_timeout_signal(chn[chan].gchannel)) {
            if (*dtype == 'C') {
                Lib_Log(APP_LIBRMN,APP_WARNING,"%s: Timeout for write %d of Character data\n",__func__,nelem);
            } else if (*dtype == 'I') {
                Lib_Log(APP_LIBRMN,APP_WARNING,"%s: Timeout for write %d of Integer data\n",__func__,nelem);
            } else if (*dtype == 'R') {
                Lib_Log(APP_LIBRMN,APP_WARNING,"%s: Timeout for write %d of Real data\n",__func__,nelem);
            } else if (*dtype == 'D') {
                Lib_Log(APP_LIBRMN,APP_WARNING,"%s: Timeout for write %d of Double data\n",__func__,nelem);
            }

            return signal_timeout(chn[chan].gchannel);
        }
    }

    return ier;
}


//! Set channel timeout
void f77name (mgi_set_timeout) (
    //! [in] Channel number
    int32_t *chan,
    //! [in] Timeout ins seconds
    int32_t *timeout
) {
    set_client_timeout(chn[(int) *chan].gchannel, (int) *timeout);
}


//! Read elements directly from the data file related to the specified channel
int32_t f77name (mgi_read) (
    //! [in] Channel number
    const int32_t * const f_chan,
    //! Buffer where to place the data read
    void *buffer,
    //! [in] Number of elements to read
    const int32_t * const f_nelem,
    //! [in] Data type of elements
    //! 'C': character
    //! 'I': integer (int)
    //! 'R': real    (float)
    //! 'D': real*8  (double)
    const char * const dtype,
    F2Cl ltype
) {
    int chan = (int) *f_chan;
    int nelem = (int) *f_nelem;

    Lib_Log(APP_LIBRMN,APP_DEBUG,"%s: data type = %c, elts Nbr = %d, strlen = %d,  subchannel = %s\n",__func__,dtype[0],nelem,ltype,chn[chan].name);

    if (nelem <= 0) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Cannot read data with length = %d\n",__func__,nelem);
        return DATA_LENGTH_ERROR;
    }

    bzero(buffer, nelem);

    int ier = send_command_to_server(chn[chan].gchannel, "READ");

    if (ier < 0) {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Unable to send write command for channel: \"%s\"\n",__func__,chn[chan].name);
        return SEND_COMMAND_ERROR;
    }

    if (*dtype == 'I') {
        buffer = (int *)read_record( chn[chan].gchannel, (int *)buffer, &nelem, nelem, sizeof(int) );

        if (buffer != NULL) {
            get_ack_nack( chn[chan].gchannel );
            return ier = nelem;
        } else {
            if ( get_timeout_signal(chn[chan].gchannel) ) {
                Lib_Log(APP_LIBRMN,APP_WARNING,"%s: Timeout for read \"Integer\"\n",__func__);
                ier = READ_TIMEOUT;
            } else {
                Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Problem read \"Integer\"\n",__func__);
                return READ_ERROR;
            }
        }
    } else if (*dtype == 'R') {
        struct rusage mydata;

        buffer = (float *)read_record(chn[chan].gchannel, (float *)buffer, &nelem, nelem, sizeof(int));

        if (buffer != NULL) {
            get_ack_nack(chn[chan].gchannel);
            return ier = nelem;
        } else {
            if ( get_timeout_signal( chn[chan].gchannel ) ) {
                Lib_Log(APP_LIBRMN,APP_WARNING,"%s: Timeout for read \"Real\"\n",__func__);
                ier = READ_TIMEOUT;
            } else {
                Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Problem read \"Real\"\n",__func__);
                return READ_ERROR;
            }
        }
    } else if (*dtype == 'D') {
        buffer = (double *)read_record(chn[chan].gchannel, (double *)buffer, &nelem, nelem, sizeof(double));

        if (buffer != NULL) {
            get_ack_nack(chn[chan].gchannel);
            return ier = nelem;
        } else {
            if ( get_timeout_signal( chn[chan].gchannel ) ) {
                Lib_Log(APP_LIBRMN,APP_WARNING,"%s: Timeout for read \"Double\"\n",__func__);
                ier = READ_TIMEOUT;
            } else {
                Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Problem read \"Double\"\n",__func__);
                return READ_ERROR;
            }
        }
    } else if (*dtype == 'C') {
        char *temp = (char *)buffer;

        for (int i = 0; i < ltype ; i++ ) {
            temp[i] = ' ';
        }

        buffer = (char *)read_record(chn[chan].gchannel, (char *)buffer, &nelem, nelem, sizeof(char));

        for (int i = nelem+1 ; i < ltype ; i++ ) {
            temp[i] = ' ';
        }

        if (buffer != NULL) {
            get_ack_nack(chn[chan].gchannel);
            return ier = nelem;
        } else {
            if ( get_timeout_signal( chn[chan].gchannel ) ) {
                Lib_Log(APP_LIBRMN,APP_WARNING,"%s: Timeout for read \"Character\"\n",__func__);
                ier = READ_TIMEOUT;
            } else {
                Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Problem read \"Charactere\"\n",__func__);
                return READ_ERROR;
            }
        }
    } else {
        Lib_Log(APP_LIBRMN,APP_ERROR,"%s: Unknown data type: %c on channel %s",__func__,*dtype,chn[chan].name);
        return READ_TYPE_ERROR;
    }

    if (ier == CLOSE) {
        close_channel(chn[chan].gchannel, chn[chan].name);
    }

    return ier;
}

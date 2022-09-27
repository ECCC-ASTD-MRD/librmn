/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2000  Division de Recherche en Prevision Numerique
 *                          Environnement Canada
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>

#include <netdb.h>
#include <time.h>
#include <signal.h>
#include <errno.h>

#include <rmn/gossip.h>

#include <dirent.h>

#include <fcntl.h>
#include <sys/param.h>

#ifdef __linux__
#include <linux/limits.h>
#else
#error "Unknown OS!  Don't known how to get PATH_MAX!"
#endif

#include "md5.h"

/* used by inet_addr, not defined on all architectures!? */
#ifndef INADDR_NONE
#define INADDR_NONE ((unsigned long) -1)
#endif

//! Size of socket buffers in KiloBytes
#define SOCK_BUF_SIZE 1024

#define  IS_OK    0
#define  NOT_OK   1
#define  LOAD    -2
#define  TIMEOUT -5


static int endian_flag = 1;
static char *little_endian = (char *)&endian_flag;
static int maxsize = 1024;
static int must_init_signal = 1;
static int timeout = false;
static char gossip_default_dir[] = "mgi";
//! Server socket
static struct sockaddr_in server;
static socklen_t sizeserver = sizeof server;


int signal_timeout( int channel );
void set_timeout_signal( int channel, int option );
int get_timeout_signal( int channel );

int connect_with_timeout(const char * const ipaddress, const int portno, const int timeout);

void check_data(char *record, int size);

char *get_link_address(char *path, const char *filename);


//! \bug When buiding with -DDEBUG, the time_base() function is called, but it's not defined
// extern long long time_base();
// Nasty workaround:
#ifdef DEBUG
long long time_base() {
    return 0;
}
#endif


//! Get the value of the GSSIPDIR environment variable
//! \return Char pointer to the gossip directory path
char *get_gossip_dir(
    //! [in] Verbose.  If > 0, print messages
    const int verbose
) {
    if ( !(getenv("GOSSIPSERVER")) ) {
        if (verbose) {
            fprintf(stderr,"Environment variable \"GOSSIPSERVER\" undefined, default channel \"mgi\" will be used\n");
        }
        return gossip_default_dir;
    }

    if ( strlen(getenv("GOSSIPSERVER") ) == 0 ) {
        if (verbose) {
            fprintf(stderr,"Environment variable \"GOSSIPSERVER\" empty, default channel \"mgi\" will be used\n");
        }
        // default_dir = "mgi";
        return gossip_default_dir;
    }

    /* return gossip_dir; */
    return getenv("GOSSIPSERVER");
}


//! Get the IP address corresponding to a channel
//! \return The IP address in the form of a decimal string or NULL in case of failure
char *get_server_host(
    const char * const channel
) {
    struct hostent *host_str;
    struct sockaddr_in addr;

    char * host_and_port = get_host_and_port(channel);

    // Obtain pointer to host name structure, using inet_addr() function call to pass IP address in dotted decimal notation.
    char * ipStr = strtok(host_and_port, ":");
    if (ipStr != NULL) {
        addr.sin_addr.s_addr = inet_addr(ipStr);
        //! \obsolete Should use getnameinfo instead
        host_str = gethostbyaddr( (char *)&addr.sin_addr.s_addr, sizeof(addr.sin_addr.s_addr), AF_INET );

        // If a host name structure is obtained, print the host name
        if (host_str) {
            return host_str->h_name;
        } else {
            fprintf(stderr,"Sorry, unable to determine host name\n");
            return NULL;
        }
    } else {
        fprintf(stderr,"gossip_sock::get_server_host(), ipStr is null\n");
        return NULL;
    }
}


//! Get the host name corresponding to an IP address
//! \return String pointer to the host name or NULL in case of failure
char *get_server_name(
    char *host_ip
) {
    struct hostent *host_str;
    struct sockaddr_in addr;
    char *host_IP, *delimiter = ":";

    // Obtain pointer to host name structure, using inet_addr() function call to pass IP address in dotted decimal notation.
    if ((host_IP = strtok(host_ip, delimiter)) != NULL) {
        if ( strcmp(host_IP, "host_IP") == 0 ) {
            fprintf(stderr,"Sorry, unable to determine \"%s\" host name\n", host_ip);
            return NULL;
        }

        addr.sin_addr.s_addr = inet_addr(host_IP);
        host_str = gethostbyaddr((char *)&addr.sin_addr.s_addr, sizeof(addr.sin_addr.s_addr), AF_INET);

        if ( host_str != NULL ) {
            return host_str->h_name;
        } else {
            fprintf(stderr,"Sorry, unable to determine \"%s\" host name\n", host_ip);
            return NULL;
        }
    } else {
        fprintf(stderr,"gossip_sock::get_server_name(), host_IP is null\n");
        return NULL;
    }
}


//! Special gethostname with IBM p690 name substitution
//! \return 0 on success, -1 otherwise
int GetHostName(
    char *name,
    size_t len
) {
    int res = gethostname(name, len);
#ifdef DEBUG
    fprintf(stderr, "gossip_sock::GetHostName(), Host Name: %s\n", name);
#endif

    //! \todo Remove this hack or even this function because without the hack, it's just a wrapper to gethostname()
    if ( name[0] == 'c' && name[2] == 'f' && name[5] == 'p' && name[7] == 'm' && name[8] == '\0' ) {
        name[7] = 's';  /* name = cxfyypzm, return cxfyypzs instead */
    }

    return res;
}


//! Write hostname:port_number into channel description file
//! \return 0 on success, -1 otherwise
int set_host_and_port(
    //! [in] Name of the channel file
    const char * const channel_file,
    //! [in] Hostname and port. The host should normally be in IPV4 decimal notation (xx.yy.zz.tt)
    const char * const host_and_port
) {
    if ( strncmp(channel_file, "Anonym", 6) == 0 ) return 0;

    fprintf(stderr, "Channel Description file: %s\n", channel_file);

    char buf[PATH_MAX];
    // $HOME/.broker/channel_name is the file path and name for channel descriptor
    snprintf(buf, PATH_MAX - 1, "%s/.gossip/%s", getenv("HOME"), channel_file);

    int fserver = open(buf, O_WRONLY + O_CREAT, 0700);
    if ( fserver == -1 ) {
        fprintf(stderr, "Can't open or create Channel Description file (%s)\n", buf);
        return -1;
    };

    // copy host IP and port to buf
    int nc = snprintf(buf, PATH_MAX - 1, "%s\n", host_and_port);

    // write buf to file: $HOME/.broker/channel_name
    if ( write(fserver, buf, nc) <= 0 ) {
        fprintf(stderr, "Can't write into Channel Description file\n");
        close(fserver);
        return -1;
    }
    close(fserver);
    return 0;
}


//! Get the hostname and port number from channel description file
//! \return Pointer to character string upon success, NULL pointer in case of failure. The caller must free the object designated by the returned pointer after use.
char *get_host_and_port(
    //! [in] Name of the channel file
    const char * const channelName
) {
#ifdef DEBUG
    fprintf(stderr, "gossip_sock::get_host_and_port(\"%s\")\n", channelName);
#endif

    char channelFilePath[PATH_MAX];
    snprintf(channelFilePath, PATH_MAX - 1, "%s/.gossip/%s", getenv("HOME"), channelName);
#ifdef DEBUG
    fprintf(stderr, "gossip_sock::get_host_and_port(), channelFilePath = \"%s\"\n", channelFilePath);
#endif

    int fd = open(channelFilePath, O_RDONLY);
    if (fd == -1) {
       fprintf(stderr, "Can't open Channel Description file\n");
       return NULL;
    }

    char * hostPortStr = malloc(PATH_MAX);
    if (read(fd, hostPortStr, PATH_MAX) <= 0) {
        fprintf(stderr, "Can't Read Channel Description file \"%s\" \n", channelName);
        close(fd);
        free(hostPortStr);
        return NULL;
    }
    close(fd);

    if (index(hostPortStr, '\n') ) {
        *index(hostPortStr,'\n') = '\0' ;
    } else {
        fprintf(stderr, "Invalid Channel Description file\n");
        free(hostPortStr);
        return NULL;
    }

#ifdef DEBUG
    fprintf(stderr, "gossip_sock::get_host_and_port(), get_host_and_port() = \"%s\"\n", hostPortStr);
#endif

    return hostPortStr;
}


//! Get the authorization token from the $HOME/.broker/.Bauth file
//! \return Pointer to character string upon success, NULL pointer in case of failure. The caller must free the object designated by the returned pointer after use.
char *get_broker_Authorization()
{
    char buf[PATH_MAX];
    snprintf(buf, PATH_MAX - 1, "%s/.gossip", getenv("HOME"));

    if ( chmod(buf, 0711) ) {
        fprintf(stderr, "Improper permissions for broker directory %s\n", buf);
        return NULL;
    }

    snprintf(buf, PATH_MAX - 1, "%s/.gossip/.Bauth", getenv("HOME"));
    printf("Authorization file path is %s\n", buf);

    if ( chmod(buf, 0600) ) {
        fprintf(stderr, "Improper permissions for Authorization file\n");
        return NULL;
    }

    int fd = open(buf, O_RDONLY);
    if ( fd == -1 ) {
        fprintf(stderr, "Can't open Authorization file\n");
        return NULL;
    };

    char * auth_buf = malloc(PATH_MAX);

    if ( read(fd, auth_buf, PATH_MAX) <= 0 ) {
        fprintf(stderr, "Can't read Authorization file\n");
        close( fd );
        if ( auth_buf ) {
            free(auth_buf);
        }
        return NULL;
    }

    close(fd);

    if ( index(auth_buf, '\n') ) {
        *index(auth_buf, '\n') = '\0' ;
    } else {
        fprintf(stderr, "Invalid Authorization file\n");
        if ( auth_buf ) {
            free(auth_buf);
        }
        return NULL;
    }

    return auth_buf;
}


//! Write the authorization token into the $HOME/.broker/.Bauth file
void set_broker_Authorization(int auth_token) {
    int fd;
    char buf[PATH_MAX];
    int nc;

    snprintf(buf, PATH_MAX - 1, "%s/.gossip/.Bauth", getenv("HOME"));

    if ( (fd = open(buf, O_WRONLY)) == -1 ) {
        fprintf(stderr,"Can't open Authorization file\n");
        exit(1);
    };
    nc = snprintf(buf, PATH_MAX - 1, "%d\n", auth_token);
    write(fd, buf, nc + 1);
    close(fd);
}


//! Accept connections on the bound server socket
//! \return Socket for incoming connection or -1 on error
//! bind_sock_to_port must have been called before connection can be accepted
int accept_from_sock(
    int fserver
) {
    int fclient = accept(fserver, (struct  sockaddr *)&server, &sizeserver);

    if (fclient < 0) {
        fprintf(stderr, "Accept failed!\n");
        return -1;
    }

   return fclient;
}


//! Bind an existing socket to a free (automatic) port
//! \return Port number bound to the socke or -1 on error
int bind_sock_to_port(
    int sockfd
) {
    struct sockaddr_in server_eff;
    socklen_t sizeserver_eff = sizeof server_eff;

    server.sin_family = AF_INET;
    server.sin_port = htons(0);
    server.sin_addr.s_addr = INADDR_ANY;

    if ( bind(sockfd, (struct  sockaddr *)&server, sizeserver) < 0 ) {
        fprintf(stderr, "Bind failed! \n");
        return -1;
    }
    getsockname(sockfd, (struct  sockaddr *)&server_eff, &sizeserver_eff);
    return ntohs(server_eff.sin_port);
}


//! Create a network socket
//! \return Socket's file descriptor
int get_sock_net()
{
    /* ignore SIGPIPE signal (i.e. do no abort but return error) */
    // DO THIS ONLY ONCE
    if (must_init_signal) {
        signal(SIGPIPE, SIG_IGN);
        must_init_signal = 0;
    }

   return socket(AF_INET, SOCK_STREAM, 0);
}


//! Disable the Nagle (TCP No Delay) algorithm
void disable_nagle(
    int socket
) {
    int flag = 1;

    int ret = setsockopt( socket, IPPROTO_TCP, TCP_NODELAY, (char *)&flag, sizeof(flag) );

    if (ret == -1) {
       printf("Couldn't setsockopt(TCP_NODELAY)\n");
       exit(EXIT_FAILURE);
    }
}


//! Set buffer sizes (recv and send) for a newly created socket
//! \return Always 0
int set_sock_opt(
    int sockfd
) {
    socklen_t optsize;
    int b0 = 0;
    socklen_t optval = SOCK_BUF_SIZE * 1024;

    b0 = setsockopt(sockfd, SOL_SOCKET, SO_SNDBUF,(char *)&optval, sizeof(optval));

    if (b0 != 0) {
        fprintf(stderr, "Error setting SO_SNDBUF size \n");
    }

    optval = 0;
    optsize = 4;
    getsockopt(sockfd, SOL_SOCKET, SO_SNDBUF, (char *)&optval, &optsize);
    fprintf(stderr,"SO_SNDBUF=%d, optsize = %d\n", optval, optsize);

    if ( sockfd > 0) {
       disable_nagle( sockfd );
    }

#ifdef DEBUG
    fprintf(stderr,"SO_SNDBUF=%d, optsize = %d\n", optval, optsize);
#endif

    optval = SOCK_BUF_SIZE*1024;
    b0 = setsockopt(sockfd, SOL_SOCKET, SO_RCVBUF, (char *)&optval, sizeof(optval));

    if (b0 != 0) {
        fprintf(stderr, "Error setting SO_RCVBUF size \n");
    }

    optval = 0;
    optsize = 4;
    getsockopt(sockfd, SOL_SOCKET, SO_RCVBUF, (char *)&optval, &optsize);
    fprintf(stderr, "SO_RCVBUF = %d, optsize = %d\n", optval, optsize);
#ifdef DEBUG
    fprintf(stderr, "SO_RCVBUF = %d, optsize = %d\n", optval, optsize);
#endif

    return 0;
}


//! Get the hostname corresponding to an IP address
void ip_to_host_name(
    //! [out] Hostname corresponding to the provided IP address.  Space must be allocated before calling this function!
    char *hostname
    //! \bug There is no size parameter to be able to prevent hostname buffer overruns!
) {
    struct hostent *host_str;
    struct sockaddr_in addr;

    // Obtain pointer to host name structure, using inet_addr() function call to pass IP address in dotted decimal notation.
    if ( (int)( addr.sin_addr.s_addr = inet_addr(hostname) ) == -1) {
        /* fprintf(stderr,"IP address must be of the form a.b.c.d\n"); */
        fprintf(stderr,"Server host: %s\n", hostname);
    } else {
        host_str = gethostbyaddr( (char *)&addr.sin_addr.s_addr, sizeof addr.sin_addr.s_addr, AF_INET );

        strncpy( hostname, host_str->h_name, strlen( host_str->h_name ));
        hostname[strlen( host_str->h_name )] = '\0';
    }
}


//! Obtain the IPV4 adress of a host specified by name
int get_ip_address(
    //! [in] Hostname
    const char * const hostname
) {
#ifdef DEBUG
    fprintf(stderr, "get_ip_address(\"%s\")\n", hostname);
#endif
    //! \deprecated gethostbyname() is deprecated.  It should be replaced with getaddrinfo()
    struct hostent * answer = gethostbyname(hostname);
    if (NULL == answer) {
        fprintf(stderr, "Cannot get address for host = %s\n", hostname);
        return -1;
    }

    int ** addr_list = (int **)answer->h_addr_list;
    // ipaddr is in host byte order
    int ipaddr = ntohl(**addr_list);

#ifdef DEBUG
    char ipStr[INET_ADDRSTRLEN];
    struct in_addr addr;
    addr.s_addr = htonl(ipaddr);
    inet_ntop(AF_INET, &addr, ipStr, INET_ADDRSTRLEN);
    fprintf(stderr, "get_ip_address(), IP address of %s:\"%s\"\n", hostname, ipStr);
#endif

    return ipaddr;
}


//! Get host's own IPV4 address
int get_own_ip_address()
{
    char buf[PATH_MAX];
    if (GetHostName(buf, sizeof(buf) - 1 )) {
        fprintf(stderr, "Can't find hostname\n");
        return -1;
    }
    return get_ip_address(buf);
}


/* given a [host:]port specification, connect to it */
/* if host: is not specified, use localhost         */

//! \return value is the connected socket
int connect_to_hostport(
    const char * const target
) {
#ifdef DEBUG
    fprintf(stderr, "gossip_sock::connect_to_hostport(\"%s\")\n", target);
#endif

    struct sockaddr_in server;
    // Why does this update the file global static def?  It's not even used in this function!
    sizeserver = sizeof server;


    char hostName[PATH_MAX];
    int ipaddr;
    int portno;
    if (NULL == strstr(target, ":")) {
        // No host specified, use local host
        portno = atoi(target);
        if (GetHostName(hostName, sizeof hostName)) {
            fprintf(stderr, "Can't find hostname\n");
            return -1;
        }

        ipaddr = get_ip_address(hostName);
    } else {
        // use specified host, find address
        // This assumes characters only use 1 byte.  This may not always be true!
        char *colonChar = strstr(target, ":");
        portno = atoi(colonChar + 1);
        int hostNameLen = colonChar - target;
        strncpy(hostName, target, hostNameLen);
        hostName[hostNameLen] = '\0';
        ipaddr = get_ip_address(hostName);
    }

    char ipStr[INET_ADDRSTRLEN];
    struct in_addr addr;
    addr.s_addr = htonl(ipaddr);
    inet_ntop(AF_INET, &addr, ipStr, INET_ADDRSTRLEN);
#ifdef DEBUG
    fprintf(stderr, "gossip_sock::connect_to_hostport() - Connecting to %s:%d ...\n", ipStr, portno);
#endif

    // 1 second timeout for connection
    char buf2[PATH_MAX];
    int fserver;
    while ((fserver = connect_with_timeout(hostName, portno, 1)) < 0) {
        fprintf(stderr, "IP = %s not working, will check using alias", ipStr);
        fserver = get_server_alias(buf2, hostName, PATH_MAX);
        fprintf(stderr, " %s\n", buf2);
        if ( fserver < 0 ) {
            // no alias found
            return fserver;
        }
        strncpy(hostName, buf2, PATH_MAX - 1);
    }

    return fserver;
}


//! Connect to given port on the specified host within the specified time
//! \return The opened socket on success, -1 otherwise
int connect_with_timeout(
    //! [in] Target host's IP adress
    const char * const ipaddress,
    //! [in] Port to which to connect
    const int portno,
    //! [in] Timeout for connexion in seconds
    const int timeout
) {

    // Create socket
    int soc = socket(AF_INET, SOCK_STREAM, 0);
    if (soc < 0) {
        fprintf(stderr, "Error creating socket (%d %s)\n", errno, strerror(errno));
        return -1;
    }

    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(portno);
    addr.sin_addr.s_addr = inet_addr(ipaddress);

    // Set non-blocking
    long arg = fcntl(soc, F_GETFL, NULL);
    if ( arg < 0 ) {
        fprintf(stderr, "Error fcntl(..., F_GETFL) (%s)\n", strerror(errno));
        close(soc);
        return -1;
    }
    arg |= O_NONBLOCK;

    if ( fcntl(soc, F_SETFL, arg) < 0) {
        fprintf(stderr, "Error fcntl(..., F_SETFL) (%s)\n", strerror(errno));
        close(soc);
        return -1;
    }

    int res = connect(soc, (struct sockaddr *)&addr, sizeof(addr));
#ifdef DEBUG
    fprintf(stderr, "gossip_sock::connect_with_timeout() - connect(...) = %d\n", res);
#endif

    if (res < 0) {
        if (errno == EINPROGRESS) {
            fprintf(stderr, "EINPROGRESS in connect() - selecting\n");
            socklen_t lon;
            int valopt;
            struct timeval tv;
            fd_set myset;
            do {
                tv.tv_sec = timeout;
                tv.tv_usec = 0;
                FD_ZERO(&myset);
                FD_SET(soc, &myset);
                // monitor fd socket for write operation during timeout
                res = select(soc+1, NULL, &myset, NULL, &tv);

                if (res < 0 && errno != EINTR) {
                    fprintf(stderr, "Error connecting %d - %s\n", errno, strerror(errno));
                    close(soc);
                    return -1;
                } else if (res > 0) {
                    /* Socket selected for write  */
                    lon = sizeof(int);
                    if (getsockopt(soc, SOL_SOCKET, SO_ERROR, (void*)&valopt, &lon) < 0) {
                        fprintf(stderr, "Error in getsockopt() %d - %s\n", errno, strerror(errno));
                        close(soc);
                        return -1;
                    }
                    /* Check the value returned...  */
                    if (valopt) {
                        fprintf(stderr, "Error in delayed connection() %d - %s\n", valopt, strerror(valopt));
                        close(soc);
                        return -1;
                    }
                    break;
                } else {
                    fprintf(stderr, "Timeout in select() - Cancelling!\n");
                    close(soc);
                    return -1;
                }
            } while (1);
        } else {
            // not EINPROGRESS
            fprintf(stderr, "Error connecting %d - %s\n", errno, strerror(errno));
            close(soc);
            return -1;
        }
    }

    // Set to blocking mode again...
    if ( (arg = fcntl(soc, F_GETFL, NULL)) < 0) {
        fprintf(stderr, "Error fcntl(..., F_GETFL) (%s)\n", strerror(errno));
        close(soc);
        return -1;
    }
    arg &= (~O_NONBLOCK);

    if ( fcntl(soc, F_SETFL, arg) < 0) {
        fprintf(stderr, "Error fcntl(..., F_SETFL) (%s)\n", strerror(errno));
        close(soc);
        return -1;
    }
    // I hope that is all
    if (soc > 0) {
        // Set buffer sizes for socket and disable Nagle algorithm
        set_sock_opt(soc);
    }
    return soc;
}


//! Connect to local socket within the specified time
//! \return The opened socket on success, -1 otherwise
int connect_with_timeout_localport(
    //! [in] Target host's IP adress.  NOT USED!
    const char * const ipaddress,
    //! [in] Port to which to connect
    const int portno,
    //! [in] Timeout for connexion in seconds
    const int timeout
) {
    int res;
    struct sockaddr_in addr;
    long arg;
    fd_set myset;
    struct timeval tv;
    int valopt;
    socklen_t lon;
    int soc;

    soc = socket(AF_INET, SOCK_STREAM, 0);
    if (soc < 0) {
       fprintf(stderr, "Error creating socket (%d %s)\n", errno, strerror(errno));
       return -1;
    }

    addr.sin_family = AF_INET;
    addr.sin_port = htons(portno);
    addr.sin_addr.s_addr = INADDR_ANY;

    /* Set non-blocking  */
    if ( (arg = fcntl(soc, F_GETFL, NULL)) < 0) {
        fprintf(stderr, "Error fcntl(..., F_GETFL) (%s)\n", strerror(errno));
        close(soc);
        return -1;
    }
    arg |= O_NONBLOCK;

    if ( fcntl(soc, F_SETFL, arg) < 0) {
        fprintf(stderr, "Error fcntl(..., F_SETFL) (%s)\n", strerror(errno));
        close(soc);
        return -1;
    }

    res = connect(soc, (struct sockaddr *)&addr, sizeof(addr));

    time_t current_time = time(NULL);
    struct tm *tm = localtime(&current_time);
    //        printf("\nCurrent Date and Time:\n");
    //        printf("%s\n", asctime(tm));
    fprintf(stderr, "connect_with_timeout_localport: PINGER connect port=<%d> ,res=<%d>  ! Current Date and Time: %s \n", portno, res, asctime(tm));

    if (res < 0) {
        if (errno == EINPROGRESS) {
            fprintf(stderr, "EINPROGRESS in connect() - selecting\n");
            do {
                tv.tv_sec = timeout;
                tv.tv_usec = 0;
                FD_ZERO(&myset);
                FD_SET(soc, &myset);
                /* monitor fd socket for write operation during timeout */
                res = select(soc+1, NULL, &myset, NULL, &tv);

                if (res < 0 && errno != EINTR) {
                    fprintf(stderr, "Error connecting %d - %s\n", errno, strerror(errno));
                    close(soc);
                    return -1;
                } else if (res > 0) {
                    /* Socket selected for write  */
                    lon = sizeof(int);
                    if (getsockopt(soc, SOL_SOCKET, SO_ERROR, (void*)(&valopt), &lon) < 0) {
                        fprintf(stderr, "Error in getsockopt() %d - %s\n", errno, strerror(errno));
                        close(soc);
                        return -1;
                    }
                    /* Check the value returned...  */
                    if (valopt) {
                        fprintf(stderr, "Error in delayed connection() %d - %s\n", valopt, strerror(valopt));
                        close(soc);
                        return -1;
                    }
                    break;
                } else {
                    fprintf(stderr, "Timeout in select() - Cancelling!\n");
                    close(soc);
                    return -1;
                }
            } while (1);
        } else {
            /* not EINPROGRESS */
            fprintf(stderr, "Error connecting %d - %s\n", errno, strerror(errno));
            close(soc);
            return -1;
        }
    }
    /* Set to blocking mode again...  */
    if ( (arg = fcntl(soc, F_GETFL, NULL)) < 0) {
        fprintf(stderr, "Error fcntl(..., F_GETFL) (%s)\n", strerror(errno));
        close(soc);
        return -1;
    }
    arg &= (~O_NONBLOCK);

    if ( fcntl(soc, F_SETFL, arg) < 0) {
        fprintf(stderr, "Error fcntl(..., F_SETFL) (%s)\n", strerror(errno));
        close(soc);
        return -1;
    }
    /* I hope that is all  */
    if(soc > 0) {
        /*JMB   set buffer sizes for socket and disable Nagle algorithm */
        set_sock_opt(soc);
    }
    return soc;
}


//! Get working IP address of link found in aliases
int get_server_alias(
    char *path,
    const char *filename,
    int maxlen
) {
    char fpath[PATH_MAX + 1];
    int nchars;
    char *temp;

    path[0] = '\0';
    temp = getenv("GOSSIP_ALIASES");
    if (temp) {
        snprintf(fpath, PATH_MAX - 1, "%s/%s", temp, filename);
        nchars = readlink(fpath, path, maxlen - 1);
        if (nchars > 0) {
            path[nchars] = '\0';
            return 0;
        }
    }

    temp = getenv("HOME");
    if (temp) {
        snprintf(fpath, PATH_MAX - 1, "%s/GossipAliases/%s", temp, filename);
        nchars = readlink(fpath, path, maxlen - 1);
        if (nchars > 0) {
            path[nchars] = '\0';
            return 0;
        }
    }

    temp = getenv("ARMNLIB");
    if (temp) {
        snprintf(fpath, PATH_MAX - 1, "%s/data/GossipAliases/%s", temp, filename);
        nchars = readlink(fpath, path, maxlen - 1);

        if (nchars > 0) {
            path[nchars] = '\0';
            return 0;
        }
    }

    /* everything failed */
    return -1;
}



//! Connect to a port on local host
//! \return Socket descriptor
int connect_to_localport(
    int port
) {
    struct sockaddr_in server;
    int sizeserver = sizeof server;
    int fserver;

#ifdef DEBUG
    fprintf(stderr, "\n gossip_sock::connect_to_localport(int port), server port = %d\n", port);
#endif

    fserver = socket(AF_INET, SOCK_STREAM, 0);
    server.sin_family = AF_INET;
    server.sin_port = htons(port);
    server.sin_addr.s_addr = INADDR_ANY;


#ifdef DEBUG
    fprintf(stderr, "\n gossip_sock::connect_to_localport(int port), fserver = %d\n", fserver);
#endif

    if (connect(fserver, (struct  sockaddr *)&server, sizeserver) < 0) {
        fprintf(stderr, "Connection to local port <%d> failed! \n", port);
        fprintf(stderr, "The Server exited abnormally !!\n");
        return -1;
    }

    return fserver;
}

 /* bind a server port to a local port, return         */
 /* hostname:port string as well as socket descriptor. */
 /* bind to any free port                              */
 int bind_to_localport(int *port, char *buf, int maxbuf)  /*   %ENTRY%   */
 {
      int fserver, ipaddr, b0, b1, b2, b3, server_port;

      /*   get a socket */
      fserver = get_sock_net();

      /*   set buffer sizes for socket */
      set_sock_opt(fserver);

      /*   bind to a free port, get port number */
      server_port = bind_sock_to_port(fserver);
      *port = server_port;

      /*   write host:port into buffer and return the socket file descriptor */
      ipaddr = get_own_ip_address();     /* get own IPV4 address as 32 bit integer */
 /*
      fprintf(stderr, "bind_to_localport(): ipaddr = %d\n", ipaddr);
 */

      b0 = ipaddr >> 24; b1 = ipaddr >> 16 ; b2 = ipaddr >> 8 ; b3 = ipaddr;
      b0 &= 255; b1 &= 255; b2 &= 255; b3 &= 255;     /* split IPV4 address */
      snprintf(buf, maxbuf, "%d.%d.%d.%d:%d", b0, b1, b2, b3, server_port);

      fprintf(stderr, "bind_to_localport(): fserver = %d, addr = %s, server_port = %d\n", fserver, buf, server_port);

      return fserver;
 }

//! Send reply to command, ACK if status=0, NACK if status nonzero
void send_ack_nack(
    int fclient,
    int status
) {
#ifdef DEBUG
    fprintf(stderr, "\n gossip_sock::send_ack_nack(): STATUS = %d\n", status);
    fflush(stderr);
#endif
    int err;

    if (status) {
        err = write_ft_nonblocking_socket(fclient, "NACK\0", 5);
    } else {
        err = write_ft_nonblocking_socket(fclient, "ACK\0\0", 5);
    }

    if (err < 0) {
        fprintf(stderr, "gossip_sock:: send_ack_nack(): write errno= %d \n",errno);
        fflush(stderr);
    }
 }

 
//! Get reply to command from server 0=ACK, nonzero=NACK
int get_ack_nack(
    int fserver
) {
    char reply[5];
    int ier;

#ifdef DEBUG
   unsigned long long tt1, tt2;
   tt1 = time_base();
#endif


   if ( (ier = read_ft_nonblocking_socket(fserver, reply, sizeof(reply))) < 0 ) {
#ifdef DEBUG
   tt2 = time_base() - tt1;
   fprintf(stderr, "\n get_ack_nack: read_ft_nonblocking_socket error, tid= %lu, errno= %d, Wall Clock = %llu bigticks \n", pthread_self(), errno, tt2);
   fflush(stderr);
#endif
        return -1;
    }

    if (strncmp(reply, "ACK", 3) == 0) {
#ifdef INFOLEVEL1
        fprintf(stderr, "\n get_ack_nack: GOOD received ACK = %s\n", reply);
        fflush(stderr);
#endif
        return 0;
    } else {
        if (strncmp(reply, "NACK", 4) == 0) {
            fprintf(stderr, "\n get_ack_nack: Error: received NACK = %s\n",reply);
            fflush(stderr);
            return -1;
         }

        reply[4] = '\0';
        fprintf(stderr, "\n get_ack_nack: read_ft Error: = %s\n", reply);
        fflush(stderr);
        return -1;
    }
}


//! Send command
//! \return 0 or -1 upon ACK or NACK
int send_command_to_server(
    const int fserver,
    const char * const buf
) {
   int ier1, ier2;

#ifdef INFOLEVEL1
    fprintf(stderr, "\n gossip_sock::send_command_to_server: COMM1-W send_command_to_server on channel: %d", fserver);
    fprintf(stderr, "\n gossip_sock::send_command_to_server, command sent: \"%s\"\n", buf);
    fflush(stderr);
#endif

    ier1 = write_ft_nonblocking_socket(fserver, buf, strlen(buf));

    if (ier1 < 0) {
        fprintf(stderr, "\n gossip_sock::(): send_command_to_server returns ier1= %d\n", ier1);
        fprintf(stderr, "\n gossip_sock::(): send_command_to_server write(), ier1= %d, errno= (%d,%s) \n", ier1, errno, strerror(errno));
        fflush(stderr);
    }

#ifdef INFOLEVEL1
    fprintf(stderr, "\n gossip_sock::send_command_to_server: COMM2-R get_ack_nack() on channel: %d", fserver);
    fflush(stderr);
#endif

    ier2 = get_ack_nack(fserver);

    if (ier2 < 0) {
        fprintf(stderr, "\n gossip_sock::send_command_to_server:  get_ack_nack returns ier2= %d\n", ier2);
        fprintf(stderr, "\n Command rejected !!! < %s >, using channel: %d\n", buf, fserver);
        fflush(stderr);
        return -1;
    }

    return 0;
}

//! Get a 32 bit integer through a network socket
//! (BIG-ENDIAN) order can also be used for a float as long as sizeof(float) is equal to sizeof(int) and both are equal to 4
int32_t get_int32_from_channel(
    int channel
) {
    int32_t to_receive;
    int ier;
    char *tagptr;

#ifdef DEBUG
   unsigned long long tt1, tt2;
   fprintf(stderr,"\n get_int_from_channel: channel= %d \n",channel);
   tt1 = time_base();
#endif

    tagptr = (char *)&to_receive;

    ier = read_ft_nonblocking_socket(channel, tagptr, sizeof(to_receive));

    if (ier < 0) {
#ifdef DEBUG
        fprintf(stderr, "\n get_int_from_channel: read_ft_nonblocking_socket returns ier=%d \n", ier);
        fprintf(stderr, "\n get_int_from_channel: ERROR errno= %d, %s Wall Clock = %llu bigticks \n", errno, strerror(errno), tt2);
        fprintf(stderr, "\n get_int_from_channel: sizeof(to_receive)=%d \n", sizeof(to_receive));
        fflush(stderr);

        tt2 = time_base() - tt1;
#endif
    }

#ifdef INFOLEVEL1
    fprintf(stderr, "\n get_int_from_channel: sizeof(to_receive)=%d \n", sizeof(to_receive));
    fprintf(stderr, "\n get_int_from_channel: to_receive=%d \n", to_receive);
#endif

    if (*little_endian) {
        swap_4(to_receive);
    }

    // This value is necessary for read_record since it tells the size of the data block to read
    return to_receive;
}

//! Write a 32 bit integer through a network socket
//! (BIG-ENDIAN) order can also be used for a float as long as sizeof(float) is equal to sizeof(int) and both are equal to 4
void put_int32_to_channel(
    int channel,
    int32_t to_send
) {
   int32_t to_be_sent = to_send;
   int ier;
   char * char_to_send;

    if (*little_endian) {
        swap_4(to_be_sent);
    }

    char_to_send = (char*)&to_be_sent;

   ier = write_ft_nonblocking_socket(channel, char_to_send, sizeof(to_be_sent));

    if (ier < 0) {
        fprintf(stderr, "\n put_int32_to_channel: ERROR from write_ft: ier=%d",ier);
        fflush(stderr);
    }
}


//! Send special message through a network stream socket
static send_request(
    int channel,
    char *request
) {
    int ier;

#ifdef DEBUG
    fprintf(stderr, "\ngossip_sock::send_request on channel %d, request is: %s\n", channel, request);
    fflush(stderr);
#endif

   if ( (ier = write_ft_nonblocking_socket(channel, request, strlen(request))) < 0) {
#ifdef DEBUG
        fprintf(stderr, "\n gossip_sock::send_request(), ERROR on channel %d for request= %s \n", channel, request );
#endif
        return -1;
    }

    return 0;
}


//! Get special message from a network stream socket
static get_request(
    int channel,
    char *request
) {
    char reply[128];
    int ier;
    int len = strlen(request);

    len = len > sizeof(reply) - 1 ? sizeof(reply) - 1 : len ;

    if ( (ier = read_ft_nonblocking_socket(channel, reply, len)) < 0) {
#ifdef DEBUG
        fprintf(stderr, "\n gossip_sock::get_request(), ERROR reply= %s \n", reply);
        fflush(stderr);
#endif
        return -1;
    }

    return 0;
}


//! Cconnect to subchannel by name
//! \return Socket descriptor on success or -1 on error
int connect_to_subchannel_by_name(
    char *channel,
    char *subchannel,
    char *mode
) {
    int fserver;
    char command[PATH_MAX];

    /* login to server, get socket descriptor */
    fserver = connect_to_channel_by_name(channel);

#ifdef DEBUG
    fprintf(stderr, "fserver: %d\n" , fserver);
#endif

    if (fserver < 0) {
        return fserver;
    }

#ifdef DEBUG
    fprintf(stderr, "gossip_sock::connect_to_subchannel_by_name(), mode = %s, subchannel = %s \n", mode, subchannel);
#endif

    /* send EXEC, mode, and subchannel command to server */
    snprintf(command, sizeof(command) - 1, "EXEC %s %s", mode, subchannel);

    if ( send_command_to_server(fserver, command) != 0 ) {
        return -1;
    }

   return fserver;
}


 /* login sequence has the following syntax         */
 /* LOGIN uid pid Auth_token hostname               */
 //! Connect to channel and send LOGIN sequence
int connect_to_channel_by_name_2(
    //! [in] Channel name (name or @hostname:port)
    const char * const name,
    const char * const msg
) {
#ifdef DEBUG
    fprintf(stderr, "%s::connect_to_channel_by_name_2(\"%s\", \"%s\")\n", __FILE__, name, msg);
#endif
    if (!get_broker_Authorization()) {
        fprintf(stderr, "Authorizartion token failure \n");
        return -1;
    }

    char host_name[PATH_MAX];
    if (GetHostName(host_name, sizeof host_name)) {
        fprintf(stderr, "Can't get local hostname\n");
        return -1;
    }

    // connect to data server by channel_name or @host:port
    int fserver;
    const char * channelName = name;
    if ( *channelName == '@' ) {
        channelName++;
        fprintf(stderr, "Connecting to: \"%s\"\n", channelName);
        fserver = connect_to_hostport(channelName);
        if (fserver < 0) return -1;
    } else {
        char * host_and_port = get_host_and_port(channelName);
        if (host_and_port == NULL) return -1;

        fprintf(stderr, "Opening channel \"%s\" to name: \"%s\" and port: \"%s\"\n", channelName, channelName, host_and_port);
        fserver = connect_to_hostport(host_and_port);

        if (msg && strlen(msg) > 0) {
            fprintf(stderr, "Opening channel: \"%s\" with ip and port: \"%s\" using socket: %d\n", channelName, host_and_port, fserver);
        }

        if (host_and_port != NULL) {
            free(host_and_port);
        }

#ifdef DEBUG
        fprintf(stderr, "gossip_sock::connect_to_channel_by_name_2(), fserver = %d\n", fserver);
#endif
        if (fserver < 0) {
            fprintf(stderr, "gossip_sock::connect_to_channel_by_name_2(), fserver = %d\n", fserver);
            return -1;
        }
    }

    // send LOGIN command to server
    int Bauth_token = 0xFFFFFFFF;
    char * temp = get_broker_Authorization();
    if (temp) {
        sscanf(temp, "%u", &Bauth_token);
        free(temp);
    }

    // Authentify user befor sending to the Server
    unsigned char buffer[16];
    if (md5_ssh(buffer)) {
        fprintf(stderr, "md5_ssh FAILED\n");
        return -1;
    }
    fprintf(stderr, "SSH Digest: %x\n", buffer);

    char buf[PATH_MAX];
    snprintf(buf, PATH_MAX - 1, "%s %d %d %u:%s:%s", "LOGIN", getuid(), getpid(), Bauth_token, host_name, msg);

    if (send_command_to_server(fserver, buf)) {
        fprintf(stderr, "LOGIN rejected\n");
        return -1;
    } else {
        fprintf(stderr, "LOGIN accepted\n");
        return fserver;
    }
}


//! Connect to channel and send LOGIN sequence
int connect_to_channel_by_name(
    const char * const name
) {
    if (name && strlen(name) > 0 ) {
        return connect_to_channel_by_name_2(name, "");
    }
    return connect_to_channel_by_name_2(get_gossip_dir(1), "");
}


//! Set read/wrtie tiemout
void set_stream_timeout(
    const int channel,
    const int timeout
) {
    set_client_timeout(channel, timeout);
    fprintf(stderr, "timeout = %d\n", timeout);
}


//! Get read/wrtie tiemout
//! \return read/write timeout
int get_stream_timeout(int channel) {
    return get_client_timeout(channel);
}


// JMB - TEST  write_ft_nonblocking_socket as a replacement for write_stream
// use in put_int32_to_channel and send_ack_nack as a safe IO compared to
// a simple write()

int write_ft_nonblocking_socket(int fd, char *ptr, int n)  /*   %ENTRY%   */
{
  int written;
  fd_set wfds;
  struct timeval tv;
  int ierw, iers;
  int iter, total;

 // write_ft_nonblocking_socket returns a value 0 (success) or -1 (error)
 // to match expectation of routines in mgilib2 and gossip_sock

#ifdef DEBUG
  /****************end timing******************/
  unsigned long long tt1,tt2,clk1,clk2;
  tt1 = time_base();
  /****************end timing******************/
#endif

#ifdef DEBUG
  fprintf(stderr, "\n gossip_sock:: write_ft_nonblocking_socket()   bytes to write = %d\n", n);
  fflush(stderr);
#endif

  iter=0;
  total=0;
  // ERROR return code on write
  // ERROR return code on select (>0
while (n > 0)
{
    iter++;
    FD_ZERO(&wfds);
    FD_SET(fd, &wfds);
    tv.tv_sec = get_stream_timeout(fd);
    tv.tv_usec = 0;

    written = write (fd, ptr, n);

    if (written < 0) {
        if (errno == EINTR) {
               fprintf(stderr, "\n gossip_sock::write_ft_nonblocking_socket()  iter=%d, error EINTR errno= %d, %s %d bytes written\n",iter,errno,strerror(errno),total);
               fflush(stderr);
            continue; /* perfectly normal; try again */
        } else if (errno == EAGAIN) {
            FD_ZERO(&wfds);
            FD_SET(fd, &wfds);
            tv.tv_sec = get_stream_timeout(fd);
            tv.tv_usec = 0;

            iers = select(fd+1, NULL, &wfds, NULL, &tv);

            if (iers < 0) {
                /* error; log/die/whatever and close() socket */
               fprintf(stderr, "\n gossip_sock::write_ft_nonblocking_socket()  iter=%d,  iers= %d FATAL error on select EAGAIN errno= (%d, %s) %d bytes written\n",iter,iers, errno,strerror(errno),total);
               fflush(stderr);
               return -1;
            } else if (iers == 0) {
                /* timed out without receiving any data; log/die/whatever and close() */
               fprintf(stderr, "\n gossip_sock::write_ft_nonblocking_socket()  iter=%d,  iers= %d timeout on select EAGAIN errno= (%d,%s) %d bytes written\n",iter,iers, errno,strerror(errno),total);
               fflush(stderr);
               return -1;
            }
            /* else, socket is now writeable, so loop back up and do the write() again */

#ifdef INFOLEVEL1
               fprintf(stderr, "\n gossip_sock::write_ft_nonblocking_socket()  iter=%d, iers= %d select WRITEABLE W errno= (%d,%s) %d bytes written\n",iter,iers, errno,strerror(errno),total);
               fflush(stderr);
#endif
               continue;
        } else {
            /* some real error; log/die/whatever and close() socket */
           fprintf(stderr, "\n gossip_sock::write_ft_nonblocking_socket() iter=%d, FATAL write error errno= (%d,%s) %d bytes written\n",iter,errno,strerror(errno),total);
           fflush(stderr);
           return -1;
        }
    } else if (written == 0) {
        /* the connection has been closed by your peer; clean-up and close() */
           fprintf(stderr, "\n gossip_sock::write_ft_nonblocking_socket() iter=%d, FATAL write error CONNECTION CLOSED errno= (%d,%s) %d bytes written\n",iter,errno,strerror(errno),total);
           fflush(stderr);
           return -1;
    } else {
        /* you got some data; do whatever with it... */

      n -= written;
      ptr += written;
      total += written;

#ifdef INFOLEVEL1
      fprintf(stderr, "\n gossip_sock::write_ft_nonblocking_socket()  iter=%d, %d bytes written OK \n",iter,total);
      fflush(stderr);
#endif
    }

}

#ifdef DEBUG
  /****************end timing******************/
  tt2 = time_base() - tt1;
  fprintf(stderr,"\n gossip_sock::write_ft_nonblocking_socket():  END wrote %d bytes, bytes remaining= %d, Wall Clock = %llu bigticks", total, n, tt2);
  fflush(stderr);
  /****************end timing******************/
#endif

  return n;
}


//! Write bytes to a stream socket
//! \return 0 on success, negative number of byte not written
int write_stream(
    //! [in] Socket into which to write
    const int fd,
    //! [in] Pointer to the data to write
    const char * const data,
    //! [in] Number of bytes to write
    const int nBytes
) {
    fd_set wfds;
    struct timeval tv;

#ifdef DEBUG
    unsigned long long tt1, tt2, clk1, clk2;
    tt1 = time_base();

    fprintf(stderr, "gossip_sock::write_stream(), nombre de bytes a envoyer = %d\n", nBytes);
    fflush(stderr);
#endif

    FD_ZERO(&wfds);
    FD_SET(fd, &wfds);

    tv.tv_sec = get_stream_timeout(fd);
    tv.tv_usec = 0;

    int res;
    const char * ptr = data;
    int bytesRemaining = nBytes;
    while (bytesRemaining > 0) {
        if (select(fd + 1, NULL, &wfds, NULL, &tv)) {
            res = write(fd, ptr, bytesRemaining);
#ifdef DEBUG
            printf("\nwrite_stream: sent \"%d\" bytes", res);
#endif
        } else {
            return -bytesRemaining;
        }

        if (res <= 0) {
            return -bytesRemaining;
        }
        bytesRemaining -= res;
        ptr += res;
    }

#ifdef DEBUG
    tt2 = time_base() - tt1;
    printf("\nwrite_stream: res = %d Wall Clock = %llu bigticks,", res, tt2);

    fprintf(stderr, "gossip_sock::write_stream(), nombre de bytes envoyes = %d\n", res);
    fflush(stderr);
#endif

    return bytesRemaining;
}


//! \return 0 (success) or -1 (error) to match expectation of routines in mgilib2 and gossip_sock
int read_ft_nonblocking_socket(
    int fd,
    char *ptr,
    int n
) {
#ifdef DEBUG
    unsigned long long tt1, tt2, clk1, clk2;
    tt1 = time_base();

    fprintf(stderr, "\n gossip_sock:: read_ft_nonblocking_socket()   bytes to read = %d\n", n);
    fflush(stderr);
#endif
    int remaining = n;
    int iter = 0;
    int total = 0;
    // ERROR return code on read
    // ERROR return code on select (>0
    while (remaining > 0) {
        iter++;

        fd_set rfds;
        FD_ZERO(&rfds);
        FD_SET(fd, &rfds);
        struct timeval tv;
        tv.tv_sec = get_stream_timeout(fd);
        tv.tv_usec = 0;
        int iers = select(fd+1, &rfds, NULL, NULL, &tv);

#ifdef INFOLEVEL1
        fprintf(stderr, "\n read_ft_nonblocking_socket()  iter=%d, select returns iers=%d \n",iter,iers);
        fflush(stderr);
#endif
        int bytesread = read (fd, ptr, remaining);
#ifdef INFOLEVEL1
        fprintf(stderr, "\n read_ft_nonblocking_socket()  iter=%d, read returns bytesread=%d \n",iter,bytesread);
        fprintf(stderr, "\n read_ft_nonblocking_socket()  read returns bytesread=%d \n",bytesread);
        fflush(stderr);
#endif

        if (bytesread < 0) {
            if (errno == EINTR) {
                fprintf(stderr, "\n gossip_sock::read_ft_nonblocking_socket()  iter=%d, error EINTR errno= (%d,%s) %d bytes bytesread\n", iter, errno, strerror(errno), total);
                fflush(stderr);
                // perfectly normal; try again
                continue;
            } else if (errno == EAGAIN) {
                FD_ZERO(&rfds);
                FD_SET(fd, &rfds);
                tv.tv_sec = get_stream_timeout(fd);
                tv.tv_usec = 0;

                iers = select(fd + 1, &rfds, NULL, NULL, &tv);
                fprintf(stderr, "\n read_ft_nonblocking_socket()  select returns iers=%d \n",iers);

                if (iers < 0) {
                    // error; log/die/whatever and close() socket
                    fprintf(stderr, "\n gossip_sock::read_ft_nonblocking_socket()  iter=%d,  iers= %d FATAL error on select EAGAIN errno= (%d,%s) %d bytes bytesread\n", iter, iers, errno, strerror(errno), total);
                    fflush(stderr);
                    return -1;
                } else if (iers == 0) {
                    // timed out without receiving any data; log/die/whatever and close()
                    fprintf(stderr, "\n gossip_sock::read_ft_nonblocking_socket()  iter=%d,  iers= %d timeout on select EAGAIN errno= (%d,%s) %d bytes bytesread\n", iter, iers, errno, strerror(errno), total);
                    fflush(stderr);
                    return -1;
                }
#ifdef INFOLEVEL1
                fprintf(stderr, "\n gossip_sock::read_ft_nonblocking_socket()  iter=%d, iers= %d select READABLE W errno= (%d,%s) %d bytes bytesread\n", iter, iers, errno, strerror(errno), total);
                fflush(stderr);
#endif
                // Socket is now readable, so loop back up and do the read() again
                continue;
            } else {
                // some real error; log/die/whatever and close() socket
                fprintf(stderr, "\n gossip_sock::read_ft_nonblocking_socket() iter=%d, FATAL read error errno= (%d,%s) %d bytes bytesread\n", iter, errno, strerror(errno), total);
                fflush(stderr);
                return -1;
            }
        } else if (bytesread == 0) {
            // the connection has been closed by your peer; clean-up and close()
            fprintf(stderr, "\n gossip_sock::read_ft_nonblocking_socket() iter=%d, FATAL read error CONNECTION CLOSED errno= (%d,%s) %d bytes bytesread\n", iter, errno, strerror(errno), total);
            fflush(stderr);
            return -1;
        } else {
            // you got some data; do whatever with it...
            remaining -= bytesread;
            ptr += bytesread;
            total += bytesread;

#ifdef INFOLEVEL1
            fprintf(stderr, "\n gossip_sock::read_ft_nonblocking_socket()  iter=%d, %d bytes bytesread OK \n",iter,total);
            fflush(stderr);
#endif
        }
    } // (remaining > 0)

#ifdef DEBUG
    tt2 = time_base() - tt1;
    fprintf(stderr,"\n gossip_sock::read_ft_nonblocking_socket():  END read %d bytes, bytes remaining= %d, Wall Clock = %llu bigticks", total, remaining, tt2);
    fflush(stderr);
#endif

    return remaining;
}


int read_ft_nonblocking_socket_count(int fd, char *ptr, int n)  /*   %ENTRY%   */
{
  int bytesread;
  fd_set rfds;
  struct timeval tv;
  int iers;
  int iter, total;
  int remaining;

 // read_ft_nonblocking_socket_count returns the number of bytes read (success) or -1 (error)
 // to match expectation of routines in mgilib2 and gossip_sock

#ifdef DEBUG
  /****************end timing******************/
  unsigned long long tt1,tt2,clk1,clk2;
  tt1 = time_base();
  /****************end timing******************/
#endif

#ifdef DEBUG
  fprintf(stderr, "gossip_sock:: read_ft_nonblocking_socket()   bytes to read = %d\n", n);
  fflush(stderr);
#endif
  remaining=n;
  iter=0;
  total=0;
  // ERROR return code on read
  // ERROR return code on select (>0
while (remaining > 0)
{
    iter++;

    FD_ZERO(&rfds);
    FD_SET(fd, &rfds);
    tv.tv_sec = get_stream_timeout(fd);
    tv.tv_usec = 0;
    iers = select(fd+1, &rfds, NULL, NULL, &tv);

    bytesread = read (fd, ptr, remaining);

    if (bytesread < 0) {
        if (errno == EINTR) {
               fprintf(stderr, "gossip_sock::read_ft_nonblocking_socket_count()  iter=%d, error EINTR errno= (%d,%s) %d bytes bytesread\n",iter,errno,strerror(errno),total);
               fflush(stderr);
            continue; /* perfectly normal; try again */
        } else if (errno == EAGAIN) {
            FD_ZERO(&rfds);
            FD_SET(fd, &rfds);
            tv.tv_sec = get_stream_timeout(fd);
            tv.tv_usec = 0;

            iers = select(fd+1, &rfds, NULL, NULL, &tv);

            if (iers < 0) {
                /* error; log/die/whatever and close() socket */
               fprintf(stderr, "gossip_sock::read_ft_nonblocking_socket_count()  iter=%d,  iers= %d FATAL error on select EAGAIN errno= (%d,%s) %d bytes bytesread\n",iter,iers, errno,strerror(errno),total);
               fflush(stderr);
               return -1;
            } else if (iers == 0) {
                /* timed out without receiving any data; log/die/whatever and close() */
               fprintf(stderr, "gossip_sock::read_ft_nonblocking_socket_count()  iter=%d,  iers= %d timeout on select EAGAIN errno= (%d,%s) %d bytes bytesread\n",iter,iers, errno,strerror(errno),total);
               fflush(stderr);
               return -1;
            }
            /* else, socket is now readable, so loop back up and do the read() again */
#ifdef INFOLEVEL1
               fprintf(stderr, "gossip_sock::read_ft_nonblocking_socket_count()  iter=%d, iers= %d select READABLE W errno= (%d,%s) %d bytes bytesread\n",iter,iers, errno,strerror(errno),total);
               fflush(stderr);
#endif
               continue;
        } else {
            /* some real error; log/die/whatever and close() socket */
           fprintf(stderr, "gossip_sock::read_ft_nonblocking_socket_count() iter=%d, FATAL read error errno= (%d,%s) %d bytes bytesread\n",iter,errno,strerror(errno),total);
           fflush(stderr);
           return -1;
        }
    } else if (bytesread == 0) {
        /* the connection has been closed by your peer; clean-up and close() */
           fprintf(stderr, "gossip_sock::read_ft_nonblocking_socket_count() iter=%d, FATAL read error CONNECTION CLOSED errno= (%d,%s) %d bytes bytesread\n",iter,errno,strerror(errno),total);
           fflush(stderr);
           return -1;
    } else {
        /* you got some data; do whatever with it... */

      remaining -= bytesread;
      ptr += bytesread;
      total += bytesread;

#ifdef INFOLEVEL1
      fprintf(stderr, "gossip_sock::read_ft_nonblocking_socket_count()  iter=%d, %d bytes bytesread OK \n",iter,total);
      fflush(stderr);
#endif
    }

}

#ifdef DEBUG
  /****************end timing******************/
  tt2 = time_base() - tt1;
  fprintf(stderr,"\ngossip_sock::read_ft_nonblocking_socket_count():  END read %d bytes, bytes remaining=%d Wall Clock = %llu bigticks", total, remaining, tt2);
  fflush(stderr);
  /****************end timing******************/
#endif

  return total;
}


//! Read data from a stream socket
//! \return Number of bytes read
int read_stream(
    //! [in] Socket from which to read the data
    const int fd,
    //! [in] Pointer to where the data read will be stored
    char * const data,
    //! [in] Number of bytes to read
    const int nbytes
) {
    fd_set rfds;
    struct timeval tv;

#ifdef DEBUG
    unsigned long long clk1, clk2;

    fprintf(stderr, "gossip_sock::read_stream(), bytes to be read = %d\n", nbytes);
    fprintf(stderr, "gossip_sock::read_stream(), fd = %d\n", fd);
#endif

    FD_ZERO(&rfds);
    FD_SET(fd, &rfds);

    tv.tv_sec = get_stream_timeout(fd);
    tv.tv_usec = 0;
#ifdef DEBUG
    clk1 = time_base();
#endif

    int bytesRemaining = nbytes;
    int bytes_read = 0;
    int res;
    char * ptr = data;
    while (bytesRemaining > 0) {
        if (select(fd + 1, &rfds, NULL, NULL, &tv)) {
            res = read(fd, ptr, bytesRemaining);
#ifdef DEBUG
            fprintf(stderr,"\n read_stream(): bytes read = %d of nbytes = %d", res, nbytes);
#endif
        } else {
#ifdef DEBUG
        fprintf(stderr,"\n read_stream(): select problem, errno= %d", errno);
#endif
            return 0;
        }
        if (res <= 0) {
          return res;
        }

        bytesRemaining -= res;
        ptr += res;
        bytes_read += res;
    }

#ifdef DEBUG
    clk2 = time_base() - clk1;
    fprintf(stderr,"\n read_stream: bytes read = %d, Total Wall Clock = %llu bigticks,", nbytes, clk2);

    fprintf(stderr, "gossip_sock::read_stream(), after while(), bytes read = %d\n", res);
#endif

  return bytes_read;
}




//! Swap elements if little endian
void check_swap_records(
    //! [inout] Buffer in which to swap the endianess
    void * const record,
    //! [in] Number of elements to swap
    const int size,
    //! [in] Size of the elements to swap
    const int tokensize
) {
    if (!*little_endian || tokensize == ONE_BYTE) return;

    if (tokensize == TWO_BYTES) {
        int16_t *element = (int16_t *)record;

#ifdef DEBUG
        fprintf(stderr, "gossip_sock::check_swap_records(),  TWO_BYTES\n");
#endif

        for(int i = 0; i < size; i++) {
            swap_2(*element);
            element++;
        }
    }

  if (tokensize == FOUR_BYTES) {
      int32_t *element = (int32_t *)record;

#ifdef DEBUG
      fprintf(stderr, "gossip_sock::check_swap_records(),  FOUR_BYTES\n");
#endif

        for(int i = 0; i < size; i++){
            swap_4(*element);
            element++;
        }
    } else if (tokensize == EIGHT_BYTES) {
        int64_t *element = (int64_t *)record;

        for(int i = 0; i < size; i++){
            swap_8(*element);
            element++;
        }

#ifdef DEBUG
        fprintf(stderr, "gossip_sock::check_swap_records(),  EIGHT_BYTES\n");
#endif
    }
#ifdef DEBUG
    fprintf(stderr, "gossip_sock::check_swap_records(),  end\n");
#endif
}


/*   Write a record to socket in the format: lentgth + record + length */
int write_record(int fclient, void *record, int size, int tokensize)  /*   %ENTRY%   */
{
  //JMB  int nbytes;
  int ier;

#ifdef DEBUG
  /****************start timing******************/
  unsigned long long tt1,tt2,clk1,clk2;
  tt1 = time_base();
  /**************** timing******************/
#endif

#ifdef INFOLEVEL1
  fprintf(stderr, "\n write_record: COMM3-R get_request() SEND on channel: %d",fclient);
  fflush(stderr);
#endif

  /* wait for send request */
  //JMB  if(!get_request(fclient, "SEND"))
  if(get_request(fclient, "SEND"))
    {
      fprintf(stderr, "gossip_sock::write_record(), problem getting SEND request\n");
      return -1;
    }

#ifdef DEBUG
  /****************end timing******************/
  tt2 = time_base() - tt1;
  fprintf(stderr,"\n write_record(): tid= %lu, get SEND request Wall Clock = %llu bigticks,", tt2,pthread_self());
  /****************end timing******************/
#endif

#ifdef DEBUG
  /****************start timing******************/
  tt1 = time_base();
  /**************** timing******************/
#endif

  set_timeout_signal(fclient, false);

#ifdef DEBUG
  /****************end timing******************/
  tt2 = time_base() - tt1;
  fprintf(stderr,"\nwrite_record(): set_timeout_signal Wall Clock = %llu bigticks,", tt2);
  /****************end timing******************/
#endif
  /* data delivery protocol: nbytes | data | nbytes */

  /* send data length */

#ifdef DEBUG
  /****************start timing******************/
  tt1 = time_base();
  /****************start timing******************/
#endif

#ifdef INFOLEVEL1
  fprintf(stderr, "\n write_record: COMM4-W put_int32_to_channel() TAG1 on channel: %d",fclient);
  fflush(stderr);
#endif

  put_int32_to_channel(fclient, size * tokensize); /* send the 1st length tag = size */

#ifdef DEBUG
  /****************end timing******************/
  tt2 = time_base() - tt1;
  fprintf(stderr,"\nwrite_record(): put 1st int into channel Wall Clock = %llu bigticks,", tt2);
  /****************end timing******************/
#endif

  /* send data */
#ifdef DEBUG
  /****************start timing******************/
  tt1 = time_base();
  /****************start timing******************/
#endif

  check_swap_records(record, size, tokensize); /* check for data swaping */

#ifdef DEBUG
  /****************end timing******************/
  tt2 = time_base() - tt1;
  fprintf(stderr,"\nwrite_record(): check_swap_records Wall Clock = %llu bigticks,", tt2);
  /****************end timing******************/
#endif

  /* send data length */

#ifdef DEBUG
  /****************start timing******************/
  tt1 = time_base();
  /****************start timing******************/
#endif

#ifdef INFOLEVEL1
  fprintf(stderr, "\n write_record: COMM5-W write_ft() DATA on channel: %d",fclient);
  fflush(stderr);
#endif

  //JMB  nbytes = write_stream(fclient, record, size * tokensize);
  ier = write_ft_nonblocking_socket(fclient, record, size * tokensize);

#ifdef DEBUG
  /****************end timing******************/
  tt2 = time_base() - tt1;
  fprintf(stderr,"\nwrite_record(): write_stream data to socket, size = %d, Wall Clock = %llu bigticks,", size, tt2);
  /****************end timing******************/
#endif

#ifdef DEBUG
  /****************start timing******************/
  tt1 = time_base();
  /****************start timing******************/
#endif

  if(ier != 0)
    {
      send_ack_nack(fclient, NOT_OK);
      set_timeout_signal(fclient, true);
      return ier;
    }

#ifdef DEBUG
  /****************end timing******************/
  tt2 = time_base() - tt1;
  fprintf(stderr,"\nwrite_record(): set_timeout_signal, Wall Clock = %llu bigticks,", tt2);
  /****************end timing******************/
#endif

#ifdef DEBUG
  /****************start timing******************/
  tt1 = time_base();
  /****************start timing******************/
#endif

  check_swap_records(record, size, tokensize); /* swap back if necessary */

#ifdef DEBUG
  /****************end timing******************/
  tt2 = time_base() - tt1;
  fprintf(stderr,"\nwrite_record(): check data swapping, Wall Clock = %llu bigticks,", tt2);
  /****************end timing******************/
#endif

#ifdef DEBUG
  /****************start timing******************/
  tt1 = time_base();
  /****************start timing******************/
#endif

#ifdef INFOLEVEL1
  fprintf(stderr, "\n write_record: COMM6-W put_int32_to_channel() TAG2 on channel: %d",fclient);
  fflush(stderr);
#endif

  put_int32_to_channel(fclient, size * tokensize); /* send the 2nd length tag = size */

#ifdef DEBUG
  /****************end timing******************/
  tt2 = time_base() - tt1;
  fprintf(stderr,"\nwrite_record(): put 2nd int into channel Wall Clock = %llu bigticks,", tt2);
  /****************end timing******************/
#endif

#ifdef DEBUG
  /****************start timing******************/
  tt1 = time_base();
  /****************start timing******************/
#endif

#ifdef INFOLEVEL1
  fprintf(stderr, "\n write_record: COMM7-W get_ack_nack() on channel: %d",fclient);
  fflush(stderr);
#endif

  /****************write: get ACK_NACK******************/
  get_ack_nack(fclient);
  /****************write: get ACK_NACK******************/

#ifdef DEBUG
  /****************end timing******************/
  tt2 = time_base() - tt1;
  fprintf(stderr,"\nwrite_record(): get ack_nak Wall Clock = %llu bigticks,", tt2);
  /****************end timing******************/
#endif

  return ier;
}

/* blind data sink, swallow nbytes bytes from descriptor fd. stop if error of EOF
   return 0 if success, -number of bytes not swallowed otherwise                  */
static int swallow_data(int fd, int nbytes)
{
  int bytes_read;
  char buffer[4096];

  while (nbytes > 0)
    {
      bytes_read = read(fd, buffer, sizeof(buffer) < nbytes ? sizeof(buffer) : nbytes);
      if(bytes_read <= 0) return(-nbytes);
      nbytes -= bytes_read;
    }

  return 0;
}


//! Read a record from socket in the format lentgth + record + length
//! \return Pointer to buffer where data was placed
void *read_record(
    //! [in] Client id
    const int fclient,
    //! [in,out] If NULL, allocate space for data, otherwise use the provided address
    void * records,
    //! [in,out] Read length (in tokensize units).  Set to 0 allow an unspecified length.  Will be updated at the end of the function
    int *length,
    //! [in] Maximum size of records (in tokensize units). Set to 0 allow an unspecified length.
    const int maxlength,
    //! [in] Token size in bytes
    int tokensize
) {
    set_timeout_signal(fclient, false);

    tokensize = ( tokensize > 1 ) ? tokensize : 1;

#ifdef DEBUG
    fprintf(stderr, "\n gossip_sock::read_record(), before send_request(), fclient = %d\n", fclient);
#endif

    // send SEND request
    send_request( fclient, "SEND" );

    // data delivery protocol: | length | data | length |

    // read 1st length
#ifdef DEBUG
    // Variables for timing
    unsigned long long tt1, tt2, clk1, clk2;
    tt1 = time_base();
#endif
    int length1 = get_int32_from_channel(fclient);
#ifdef DEBUG
    tt2 = time_base() - tt1;
    fprintf(stderr, "\n read_record(): get 1st int = %d from socket Wall Clock = %llu bigticks,", length1, tt2);
    fflush(stderr);

    fprintf(stderr, "\n gossip_sock::read_record(), 1st length tag = %d \n", length1);
    fflush(stderr);
#endif

    if ( length1 == 0 ) {
        swallow_data(fclient, length1);
        send_ack_nack(fclient, NOT_OK);
        set_timeout_signal(fclient, true);
        fprintf(stderr, "\n gossip_sock::read_record: Problem reading TAG1 length1= %d", length1);
        fflush(stderr);
        return NULL;
    }

    if ( length1 > maxlength * tokensize && maxlength > 0 ) {
        fprintf(stderr, "\n gossip_sock::read_record: Problem reading TAG1 length: \"%d\" is greater than max requested: \"%d\" \n", length1, maxlength);
        fflush(stderr);
        if (swallow_data(fclient, length1) != 0) {
            fprintf(stderr, "\n gossip_sock::read_record() : cannot get enough data \n");
            fflush(stderr);
        }
        send_ack_nack(fclient, NOT_OK);
        return NULL;
    }

    if ( length1 > maxsize ) {
        maxsize = length1;
    }

    char *records2 = NULL;
    records2 = (records == NULL) ? malloc(maxsize + 2 * sizeof(int)) : records;
    if (records2 == NULL) {
        fprintf(stderr, "\n gossip_sock::read_record: cannot allocate memory for data with size = %d\n", length1);
        fflush(stderr);
        swallow_data(fclient, length1);
        send_ack_nack(fclient, NOT_OK);
        return NULL;
    }


    // read data, and get received stream length
#ifdef DEBUG
    tt1 = time_base();
#endif
    int length2 = read_ft_nonblocking_socket_count(fclient, records2, length1);
#ifdef DEBUG
    tt2 = time_base() - tt1;
    fprintf(stderr, "\n read_record(): read_stream data from socket, size = %d, Wall Clock = %llu bigticks,", length1, tt2);
    fflush(stderr);
#endif

    // If length2 < 0 => there was an error reading data
    if (length2 < 0) {
        swallow_data(fclient, length1);
        send_ack_nack(fclient, NOT_OK);
        set_timeout_signal(fclient, true);
        fprintf(stderr, "\n gossip_sock::read_record: error reading DATA block length2= %d\n", length2);
        fflush(stderr);

        if (records == NULL && records2 != NULL) free(records2);
        return NULL;
    }

    // read 2nd length

#ifdef DEBUG
    tt1 = time_base();
#endif
  int length3 = get_int32_from_channel(fclient);
#ifdef DEBUG
    tt2 = time_base() - tt1;
    fprintf(stderr,"\n read_record(): get 2nd length TAG = %d from socket Wall Clock = %llu bigticks,", length3, tt2);
    fflush(stderr);
#endif

    if ( length1 != length2 ) {
        fprintf(stderr, "\n read_record: Problem DATA bytes read  %d NOT EQUAL to TAG1= %d \n", length2, length1);
        fflush(stderr);
        send_ack_nack(fclient, NOT_OK);
        set_timeout_signal(fclient, false);

        if (records == NULL) free(records2);
        return NULL;
    }

    if (*length > 0 && *length * tokensize != length2) {
        fprintf(stderr, "\n read_record: Problem requested DATA length %d != TAG2 = %d\n", *length * tokensize , length2);
        fflush(stderr);
        send_ack_nack(fclient, NOT_OK);
        set_timeout_signal(fclient, true);

        if (records == NULL && records2 != NULL) free(records2);
        return NULL;
    }


    // check length values
    if (length1 != length3) {
        fprintf(stderr, "\n read_record: Problem TAGS read length1 = %d NOT EQUAL to length3 = %d \n", length1, length3);
        fflush(stderr);
        send_ack_nack(fclient, NOT_OK);
        /* set_timeout_signal(fclient, false); */
        set_timeout_signal(fclient, true);

        if (records != NULL && records2 != NULL) free(records2);
        return NULL;
    }

    check_swap_records(records2, length1 / tokensize, tokensize);

    send_ack_nack(fclient, IS_OK);
    // return total number of bytes read
    *length = length2 / tokensize;

#ifdef DEBUG
    fprintf(stderr, "\n gossip_sock::read_record(), *length = %d\n", *length);
#endif

    return records2;
}


//! signal read timeout return special code TIMEOUT = -5
int signal_timeout(int channel) {
    return TIMEOUT;
}

//! Set timeout option to true if read timeout expires
void set_timeout_signal(int channel, int option) {
    timeout = option;
}

//! Get timeout option (true or false), used in case to indicate the reason of read problem
int get_timeout_signal(int channel)
{
    return timeout;
}

/* before server exit after timeout store all server */
/* pending data in file channel_subchannel_gsave.    */
/* data will be loaded at restart time               */
int store_channel_data(char *buffer, int nbytes, char *file_name)  /*   %ENTRY%   */
{
  int fd;
  char buf[PATH_MAX];

  /* Current_Working_dir/channel_subchannel_gsave is the data file path to be stored */
  snprintf(buf, sizeof(buf) - 1, "%s_%s_gsave", get_gossip_dir(0), file_name);


#ifdef DEBUG
  fprintf(stderr, "gossip_sock::store_channel_data(), buf = %s\n", buf);
#endif

    if((fd = open(buf, O_WRONLY + O_CREAT, 0700)) == -1)
    {
      fprintf(stderr, "Can't Open or Create Channel Data file\n");
      return -1;
    }

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::store_channel_data():  nbytes = %d\n", nbytes);
#endif


  if(write(fd, (char *)buffer, nbytes) != nbytes)
    {
      fprintf(stderr, "store_channel_data: Error writing into data file\n");
      close(fd);
      return -1;
    }
  close(fd);
  return 0;
}

/* get file size using file descriptor */
long fsize(FILE* fd)
{
  long savepos, size;

  savepos = ftell(fd);          /* save position    */
  fseek(fd, 0, SEEK_END);       /* go to the end    */
  size = ftell(fd);             /* read size        */
  fseek(fd, savepos, SEEK_SET); /* restore position */

  return size;
}

//! Get file size
//! \return File size in bytes
int get_file_size(const char * const file_name) {
#ifdef DEBUG
    fprintf(stderr, "gossip_sock::get_file_size: file_name = %s\n", file_name);
#endif
    char buf[PATH_MAX];
    snprintf(buf, PATH_MAX - 1, "./%s", file_name);
#ifdef DEBUG
    fprintf(stderr, "gossip_sock::get_file_size(): buf = %s\n", buf);
#endif

    FILE * ifp = fopen(buf, "r");
    if (ifp  == NULL) {
        fprintf(stderr, "data file: %s, doesn't exist!\n", buf);
        return 0;
    }

#ifdef DEBUG
    fprintf(stderr, "gossip_sock::get_file_size(): ifp = %d\n", ifp);
#endif
    int the_size = fsize(ifp);
    fclose(ifp);

#ifdef DEBUG
    fprintf(stderr, "gossip_sock::get_file_size(), file size = %d\n",the_size );
#endif
    return the_size;
}

/* read data in file channel_subchannel_gsave,              */
/* rename data file to channel_subchannel_gback             */
/* return number of bytes read from file (= data file size) */
int read_data_file(char *file_name, char *buffer, int size)   /*   %ENTRY%   */
{
  int i, fd;
  char buf[PATH_MAX];
  char nbuf[PATH_MAX] = "";
  char *delimiter = "_", *token;

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::read_data_file(): file_name = %s\n", file_name);
#endif

  snprintf(buf, sizeof(buf) - 1, "./%s", file_name);

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::read_data_file(): buf = %s\n", buf);
#endif

  i = 0;
  if((fd = open(buf, O_RDONLY)) == -1)
    {
      fprintf(stderr, "data file: %s doesn't exist\n", buf);
      return -1;

     }

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::read_data_file(): fd = %d \n", fd);
#endif

  i = read(fd, buffer, size);

  if( i > size || i <= 0)
     {
      fprintf(stderr, "Can't read data file, i = %d, size = %d\n", i, size);
      close(fd);
      return -1;

    }

  close(fd);

#ifdef DEBUG
  fprintf(stderr, " Will Try to rename data file \"%s\"\n", buf);
#endif

  token = strtok(file_name, delimiter);

  if(token == NULL)
    return -1;

  strncpy (nbuf, token, strlen(token));
  strncpy (nbuf + strlen(nbuf), "_", strlen("_"));

  while((token = strtok(NULL, delimiter)) != NULL && (strcmp(token, "gsave") != 0))
    {
      strncpy (nbuf + strlen (nbuf), token, strlen(token));
      strncpy (nbuf + strlen(nbuf), "_", strlen("_"));
    }

  strncpy (nbuf + strlen (nbuf), "gback", strlen("_gback"));
  nbuf[strlen(nbuf)] = '\0';


#ifdef DEBUG
  fprintf(stderr, " Will Try to rename data file < %s > ***\n", nbuf);
#endif

  if((rename(buf, nbuf)) < 0)
    fprintf(stderr, "Can't rename data file\n");

#ifdef DEBUG
  fprintf(stderr, "Data file < %s > renamed succefully \n", nbuf);
  fprintf(stderr, "gossip_sock::read_data_file(), number of bytes read i = %d\n", i);
#endif

  return i;
}

/* connect to default server, default channel will be */
/* $GOSSIPSERVER env. var. or default one "mgi"       */
/*   return socket descriptor                         */
int connect_to_server()
{
  int fserver = connect_to_channel_by_name("");

  return fserver;
}


//! Send flag status to server to check active channels on running server with channel $GOSSIPDIR (default: "mgi")
//! \return 0 if command accepted, -1 otherwise
int get_status(char * const reply) {
    char buf[128];
    char reply0[1025];

    char * reply1 = (reply == NULL) ? &reply0[0] : reply;
    reply1[0] = '\0';

    int fserver = connect_to_server();
    if (fserver > 0) {
        sprintf(buf, "STATUS");
        int status = send_command_to_server(fserver, buf);

        if (status != 0) {
            fprintf(stderr, "command \"%s\" rejected \n", buf);
        } else {
            // fprintf(stderr, "command \"%s\" accepted\n", buf);
            while (read(fserver, reply1, PATH_MAX) > 0);
        }

        close(fserver);
        return status;
    } else {
        fprintf(stderr, "No server running on channel \"%s\"!!\n", get_gossip_dir(0));
        return -1;
    }
}

//! Send command to server running on channel $GOSSIPDIR
//! \return 0 on success, -1 otherwise
int send_command(const char * const command) {
    int status;
    int fserver = connect_to_server();
    if (fserver > 0) {
        status = send_command_to_server(fserver, command);
        close(fserver);
    } else {
        fprintf(stderr, "No server running on channel \"%s\" !!\n", get_gossip_dir(0));
        status = -1;
    }
    return status;
}

/* close channel blocked on server with read request     */
/* identified by its name and socket descriptor fclient. */
/* send infos with "END" flag to server, return 0 upon   */
/* success, -1 else                                      */
int close_channel(
    int fclient,
    char *channel
) {
    if (fclient != 0) {
    char buf[PATH_MAX];
        snprintf(buf, PATH_MAX - 1, "%s %s", "END", channel);
        return send_command(buf);
    }

    return 0;
}

// ******************** command server functions ***********************

//! Add the message size to the stream bytes
void pack_cmd(const char * const buffer, char * const tmpbuf) {
    int nbytes = strlen(buffer);
    bzero(tmpbuf, strlen(buffer) + sizeof(int));

    if ( !*little_endian ) {
        swap_4(nbytes);
    }

    memcpy(tmpbuf, (char *)&nbytes, sizeof(int));
    memcpy(tmpbuf + sizeof(int), buffer, strlen(buffer) );

    fprintf(stderr, "sending command: %s\n", tmpbuf + sizeof(int));
}


//! Send command to server return bytes sent
//! \return 0 all command bytes sent, >0 failure
int send_command_to_server2(int fclient, const char * const buffer) {
    char * tmpbuf = (char *)malloc(strlen(buffer) + sizeof(int));
    if (!tmpbuf) {
        fprintf(stderr, "Error: cannot allocate memory for buffer command !!!\n");
        exit(1);
    }

    pack_cmd(buffer, tmpbuf);
    int nbytes = write(fclient, tmpbuf, strlen(buffer) + sizeof(int));

    int reply = get_ack_nack(fclient);

    if (reply < 0) {
        fprintf(stderr, "Problem getting ACK from server !!!\n");
    }

    if (tmpbuf) {
        free(tmpbuf);
    }

    return nbytes;
}


//! Open client socket to command server
int cmd_open() {
    int fserver = connect_to_channel_by_name("cmd");
    fprintf(stderr, "fserver = %d\n", fserver);

    if (fserver < 0) {
        fprintf(stderr, "Error: cannot connect to server\n");
        exit(1);
    }

    return fserver;
}


//! Close client socket to command server
void cmd_close(int fclient) {
    char tmpbuf[128];
    pack_cmd("quit", tmpbuf);
    int nbytes = write_stream(fclient, tmpbuf, strlen("quit") + sizeof(int));
    fprintf(stderr, "nbytes sent for quit:  %d\n", nbytes);

    if (nbytes > 0) {
        fprintf(stderr, "command \"%s\" has been rejected \n", "quit");
    }
    close(fclient);
}


void check_data(char *record, int size) {
    fprintf(stderr, "check_data( ):  size = %d\n", size);

    if (record && size > 1000) {
        float element;
        for(int i = 0; i < (size / 4); i++) {
            memcpy(&element, record, sizeof(int));
            fprintf(stderr, "check_data( ):  element[%d] = %f\n", i, element);
            record += sizeof( int );
        }
        record -= (size / 4 - 1) * sizeof(int);
    }
}

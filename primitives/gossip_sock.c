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
#include <string.h>
#include <strings.h> 
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <time.h>
#include <signal.h>

#include <gossip.h>

#include <fcntl.h>
#include <sys/param.h>

static int endian_flag = 1;
static char *little_endian = (char *)&endian_flag;
static int must_init_signal = 1;

/* size of socket buffers in KiloBytes */
#define SOCK_BUF_SIZE 256
#define BPATH         1024


#define IS_OK       0
#define NOT_OK      1
#define LOAD        -2

#define FALSE            0
#define TRUE        !FALSE

static int timeout = FALSE;

int signal_timeout(int channel);
void set_timeout_signal(int channel, int option);
int get_timeout_signal(int channel);


char *get_gossip_dir(int display)
     /* to get the value of the environment variable "GSSIPDIR" */
{
  if ( !(getenv("GOSSIPSERVER")) )
    {
      if(display)
	printf("Environment variable \"GOSSIPSERVER\" undefined, default channel \"mgi\" will be used\n");
      return "mgi";
    }


  if( strlen(getenv("GOSSIPSERVER") ) == 0 )
    {
      if( display )
	printf("Environment variable \"GOSSIPSERVER\" empty, default channel \"mgi\" will be used\n");
      /* return default_dir = "mgi"; */
      return "mgi";
    }
  /* return gossip_dir; */
  return getenv("GOSSIPSERVER");
}

/* getservername using description file, */
/* read IP address, return NULL in case of failure */
char *get_server_host(char *channel)
{
  struct hostent *host_str;
  struct sockaddr_in addr;
  char *host_and_port, *host_IP, *delimiter = ":";
  
  host_and_port = get_host_and_port(channel);
  
  /* Obtain pointer to host name structure, using */
  /* inet_addr() function call to pass IP address */
  /* in dotted decimal notation.                  */

  if((host_IP = strtok(host_and_port, delimiter)) != NULL)
    {
      addr.sin_addr.s_addr = inet_addr(host_IP);
      host_str = gethostbyaddr((char *)&addr.sin_addr.s_addr, sizeof(addr.sin_addr.s_addr), AF_INET); 
      
      /* If a host name structure is obtained, */
      /* print the host name */
      
      if (host_str)
	{
	  return host_str->h_name;
	}
      
      else
	{
	  printf("Sorry, unable to determine host name\n");
	  return NULL;
	}
    }
  else
    {
      printf("gossip_sock::get_server_host(), host_IP is null\n");

      return NULL;
    }
}

/* getservername by IP address */
/* return NULL in case of failure */
char *get_server_name(char *host_ip)
{
  struct hostent *host_str;
  struct sockaddr_in addr;
  char *host_IP, *delimiter = ":";
  
  /* Obtain pointer to host name structure, using */
  /* inet_addr() function call to pass IP address */
  /* in dotted decimal notation.                  */

  if((host_IP = strtok(host_ip, delimiter)) != NULL)
    {
      addr.sin_addr.s_addr = inet_addr(host_IP);
      host_str = gethostbyaddr((char *)&addr.sin_addr.s_addr, sizeof(addr.sin_addr.s_addr), AF_INET); 
      
      /* If a host name structure is obtained, */
      /* print the host name */
      
      if ( host_str )
	{
	  if( strcmp( host_str->h_name, "c4f09p1s" ) == 0 )
	    return "maia";

	  return host_str->h_name;
	}
      
      else
	{
	  printf("Sorry, unable to determine host name\n");
	  return NULL;
	}
    }
  else
    {
      printf("gossip_sock::get_server_name(), host_IP is null\n");

      return NULL;
    }
}


/* GetHostName special gethostname with IBM p690 name substitution */
int GetHostName(char *name, size_t len)  /*   %ENTRY%   */
{
  int junk;

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::GetHostName(), Host Name: %s\n", name);
#endif

  junk = gethostname(name, len);
  if( name[0] == 'c' && name[2] == 'f' && name[5] == 'p' && name[7] == 'm' && name[8] == '\0' )
    name[7] = 's';  /* name = cxfyypzm, return cxfyypzs instead */
  return(junk);
}

/* write hostname:port_number into channel description file */
/* hostname normally in IPV4 notation xx.yy.zz.tt           */
int set_host_and_port(char *channel_file, char *host_and_port)  /*   %ENTRY%   */
{
     int fserver;
     char buf[1024];
     int nc;

     if(strncmp(channel_file, "Anonym", 6) == 0) 
       return(0) ; /* anonymous channel created */

     fprintf(stderr, "Channel Description file: %s\n", channel_file);

     /* $HOME/.broker/channel_name is the file path and name for channel descriptor */
     snprintf(buf, 1023,"%s/.broker/%s", getenv("HOME"), channel_file);

     if((fserver = open(buf, O_WRONLY + O_CREAT, 0700)) == -1) 
       { 
         fprintf(stderr, "Can't open or create Channel Description file\n");
         return(-1);
       };

     /* copy host IP and port to buf */
     nc = snprintf(buf, 1023, "%s\n", host_and_port);

     /* write buf to file: $HOME/.broker/channel_name */
     if(write(fserver, buf, nc) <= 0)
       {
         fprintf(stderr, "Can't write into Channel Description file\n");
	 close(fserver);
         return(-1);
       }
     close(fserver);
     return(0);
}

/* read hostname:port_number from channel description file 
   return pointer to character string upon success
   return NULL pointer in case of failure
*/
char *get_host_and_port(char *channel_file)  /*   %ENTRY%   */
{
  /* char *chan_buf = malloc(1024); */
  char *chan_buf;
  int fd;
  char buf[1024];

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::get_host_and_port(), channel_file = %s\n", channel_file);
#endif

  snprintf(buf, 1023, "%s/.broker/%s", getenv("HOME"), channel_file);

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::get_host_and_port(), buf contient = %s\n", buf);
#endif

  if((fd = open(buf, O_RDONLY)) == -1) 
    {
      fprintf(stderr, "Can't open Channel Description file\n");
      return(NULL);
    }
  chan_buf = malloc(1024);
  if(read(fd, chan_buf, 1024) <= 0)
    {
      fprintf(stderr, "Can't Read Channel Description file \"%s\" \n", channel_file);
      close(fd);
      free(chan_buf);
      return(NULL);
    }

  close(fd);

  if(index(chan_buf, '\n'))
    {
      *index(chan_buf,'\n') = '\0' ;
    } 
  else
    {
      fprintf(stderr, "Invalid Channel Description file\n");
      free(chan_buf);
      return(NULL);
    }
  return(chan_buf);
}

/* get Authorization token from file     */
/* $HOME/.broker/.Bauth,  return pointer */
/* to character string upon success,     */
/* return NULL pointer in case of error  */
char *get_broker_Authorization()  /*   %ENTRY%   */
{
  char *auth_buf;
  int fd;
  char buf[1024];
  char *homedir = getenv("HOME");
  
  snprintf(buf, 1023, "%s/.broker", homedir);
  
  if(chmod(buf, 0711))
    {
      fprintf(stderr, "Improper permissions for broker directory %s\n", buf);
      return(NULL); 
    }

  snprintf(buf, 1023, "%s/.broker/.Bauth", getenv("HOME"));

  if(chmod(buf, 0600))
    {
      fprintf(stderr, "Improper permissions for Authorization file\n");
      return(NULL);
    }
  if((fd = open(buf, O_RDONLY)) == -1) 
    {
      fprintf(stderr, "Can't open Authorization file\n");
      return(NULL); 
    };

  auth_buf = malloc(1024);

  if(read(fd, auth_buf, 1024) <= 0)
    {
      fprintf(stderr, "Can't read Authorization file\n");
      close( fd );
      if( auth_buf )
	free( auth_buf );

      return (NULL);
    }

  close(fd);
  
  if(index(auth_buf, '\n'))
    {
      *index(auth_buf, '\n') = '\0' ;
    }
  else 
    {
      fprintf(stderr, "Invalid Authorization file\n");
      if( auth_buf )
	free( auth_buf );

      return ( NULL );
    }

  return ( auth_buf );
}

/* write Authorization token into file */
/* $HOME/.broker/.Bauth                */
void set_broker_Authorization(int auth_token)  /*   %ENTRY%   */
{
     int fd;
     char buf[1024];
     int nc;

     snprintf(buf, 1023, "%s/.broker/.Bauth", getenv("HOME"));

     if((fd = open(buf, O_WRONLY)) == -1)
       {
         fprintf(stderr,"Can't open Authorization file\n");
         exit(1);
       };
     nc = snprintf(buf, 1023, "%d\n", auth_token);
     write(fd, buf, nc + 1);
     close(fd);
}

static struct  sockaddr_in server;                /* server socket */
static socklen_t sizeserver = sizeof server;

/* accept connections on the bound server socket, */
/* return socket for incoming connection          */
/* bind_sock_to_port must have been called        */
/* before connection can be accepted              */
int accept_from_sock(int fserver)  /*   %ENTRY%   */
{
    int fclient =  accept(fserver, (struct  sockaddr *)&server, &sizeserver);
    if(fclient < 0) 
      {
        fprintf(stderr, "Accept failed!\n");
        return(-1);
      }
    return fclient;
}

/* bind an existing socket to a free (automatic) port, */
/* return port number existing socket usually created  */
/* by get_sock_net                                     */
int bind_sock_to_port(int s)  /*   %ENTRY%   */
{
     struct sockaddr_in server_eff;
     socklen_t sizeserver_eff = sizeof server_eff ;

     server.sin_family = AF_INET;
     server.sin_port = htons(0);
     server.sin_addr.s_addr = INADDR_ANY;

     if(bind(s, (struct  sockaddr *)&server, sizeserver) < 0)
          {
            fprintf(stderr, "Bind failed! \n");
            return(-1);
          }
     getsockname(s, (struct  sockaddr *)&server_eff, &sizeserver_eff);
     return ntohs(server_eff.sin_port);
}

/* create a network socket ; return socket descriptor */
int get_sock_net()  /*   %ENTRY%   */
{
  /* ignore SIGPIPE signal (i.e. do no abort but return error) */
  
  if(must_init_signal)
    {  /* DO THIS ONLY ONCE */
      
      signal(SIGPIPE, SIG_IGN);
      must_init_signal = 0;
    }
  
  return socket(AF_INET, SOCK_STREAM, 0);
}

/* set buffer sizes (recv and send) for a newly */
/* created socket (always returns 0)            */
int set_sock_opt(int s)  /*   %ENTRY%   */
{
  socklen_t optval, optsize;
  int b0 = 0;
  
  optval = SOCK_BUF_SIZE*1024;
  
  b0 = setsockopt(s, SOL_SOCKET, SO_SNDBUF,(char *)&optval, sizeof(optval));

  if(b0 != 0)
    { 
      fprintf(stderr, "Error setting SO_SNDBUF size \n"); 
    }

  optval = 0;
  optsize = 4;
  getsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *)&optval, &optsize);

#ifdef DEBUG
  fprintf(stderr,"SO_SNDBUF=%d, optsize = %d\n", optval, optsize);
#endif

  optval = SOCK_BUF_SIZE*1024;
  b0 = setsockopt(s, SOL_SOCKET, SO_RCVBUF, (char *)&optval, sizeof(optval));

  if(b0 != 0)
    { 
      fprintf(stderr, "Error setting SO_RCVBUF size \n");
    }

  optval = 0;
  optsize = 4;
  getsockopt(s, SOL_SOCKET, SO_RCVBUF, (char *)&optval, &optsize);

#ifdef DEBUG
  fprintf(stderr, "SO_RCVBUF = %d, optsize = %d\n", optval, optsize);
#endif
  
  return(0);
}

/* obtain the IPV4 adress of a host specified by name */
int get_ip_address(char *hostname)  /*   %ENTRY%   */
{
     int **addr_list;
     struct hostent *answer;
     int ipaddr = 0;
     int b0, b1, b2, b3;

     if(NULL == (answer = gethostbyname(hostname)))
       {
         fprintf(stderr, "Cannot get address for host = %s\n", hostname);
         return(-1);
       }

     addr_list = (int **)answer->h_addr_list;
     ipaddr = ntohl(**addr_list);

     b0 = ipaddr >> 24; b1 = ipaddr >> 16 ; b2 = ipaddr >> 8 ; b3 = ipaddr;
     b0 &= 255;
     b1 &= 255;
     b2 &= 255;
     b3 &= 255;
     
#ifdef DEBUG
     fprintf(stderr, "get_ip_address(), IP address of %s = %d.%d.%d.%d \n", hostname, b0, b1, b2, b3);
#endif
     return(ipaddr);
}

/* obtain own host's IPV4 address */
int get_own_ip_address()  /*   %ENTRY%   */
{
     char buf[1024];

     if(GetHostName(buf, sizeof buf - 1 ))
       {
         fprintf(stderr, "Can't find hostname\n");
         return(-1);
       }

     return get_ip_address(buf);
}

/* given a [host:]port specification, connect to it */
/* if host: is not specified, use localhost         */
/* the return value is the connected socket         */
int connect_to_hostport(char *target2)  /*   %ENTRY%   */
{
     char buf[1024];
     int fserver;
     int b0, b1, b2, b3, sizeserver;
     int ipaddr;
     char *portno;
     char *target;

     struct sockaddr_in server;

     sizeserver = sizeof server;
     target = target2;

     if(NULL == strstr(target, ":"))
       {   /* no host specified, use local host */
         portno = target;
	 if(GetHostName(buf, sizeof buf))
	   {
	     fprintf(stderr, "Can't find hostname\n");
	     return(-1);
	   } 
	 ipaddr = get_ip_address(buf);
       } 
     else 
       {  /* use specified host, find address */
	 portno = strstr(target, ":"); 
	 *portno = '\0';
	 portno++;
	 ipaddr = get_ip_address(target);
       }

#ifdef DEBUG
     fprintf(stderr, "gossip_sock::connect_to_hostport() with ip addr: %d\n", ipaddr);
#endif

     b0 = ipaddr >> 24; b1 = ipaddr >> 16 ; b2 = ipaddr >> 8 ; b3 = ipaddr;
     b0 &= 255;
     b1 &= 255;
     b2 &= 255;
     b3 &= 255;
     snprintf(buf, sizeof buf - 1, "%d.%d.%d.%d", b0, b1, b2, b3);
     
#ifdef DEBUG
     fprintf(stderr, "Connecting to %d.%d.%d.%d:%s\n", b0, b1, b2, b3, portno);
#endif

     fserver = socket(AF_INET, SOCK_STREAM, 0);

     set_sock_opt(fserver);
     server.sin_family = AF_INET;
     server.sin_port = htons(atoi(portno));
     server.sin_addr.s_addr = inet_addr(buf);

     if(connect(fserver, (struct  sockaddr *)&server, sizeserver) < 0) 
       {
	 fprintf(stderr,"Connection Failed, no gossip Server running!\n");
	 return(-1);
       }
     
     return(fserver);
}

/* connect to a port on local host return socket descriptor */
int connect_to_localport(int port)  /*   %ENTRY%   */
{
     struct sockaddr_in server;
     int sizeserver = sizeof server;
     int fserver;
     
#ifdef DEBUG
     fprintf(stderr, "gossip_sock::connect_to_localport(int port), server port = %d\n", port);
#endif
     
     fserver = socket(AF_INET, SOCK_STREAM, 0);
     server.sin_family = AF_INET;
     server.sin_port = htons(port);
     server.sin_addr.s_addr = INADDR_ANY;

     
#ifdef DEBUG
     fprintf(stderr, "gossip_sock::connect_to_localport(int port), fserver = %d\n", fserver);
#endif
      
      if(connect(fserver, (struct  sockaddr *)&server, sizeserver) < 0)
        {
          fprintf(stderr, "Connection to local port <%d> failed! \n", port);
	  fprintf(stderr, "The Server exited abnormally !!\n");
          return(-1);
        }

     return(fserver);
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
     ipaddr = get_own_ip_address();                    /* get own IPV4 address as 32 bit integer */
     b0 = ipaddr >> 24; b1 = ipaddr >> 16 ; b2 = ipaddr >> 8 ; b3 = ipaddr;
     b0 &= 255; b1 &= 255; b2 &= 255; b3 &= 255;     /* split IPV4 address */
     snprintf(buf, maxbuf, "%d.%d.%d.%d:%d", b0, b1, b2, b3, server_port);

     return(fserver);
}

/* send reply to command, ACK if status=0, NACK if status nonzero */
void send_ack_nack(int fclient, int status)  /*   %ENTRY%   */
{
  
  if(status)
     {
         write(fclient, "NACK\0", 5);
     }
   else
     {
       write(fclient, "ACK\0\0", 5);
     }
}

/* get reply to command from server 0=ACK, nonzero=NACK*/
int get_ack_nack(int fserver)  /*   %ENTRY%   */
{
   char reply[5];
   int n;
   
   n = read(fserver, reply, sizeof(reply));

#ifdef DEBUG
   fprintf(stderr, "gossip_sock::get_ack_nack(): n = read() = %d\n", n);
#endif
 
   if (n < 3) 
     {
       fprintf(stderr, "Error: Bad reply\n");
       return(1) ; /* NACK */
     }

   return(strncmp(reply, "ACK", 3));
}

/* send command and return 0 or -1 upon ACK or NACK */
int send_command_to_server(int fserver, char *buf)  /*   %ENTRY%   */
{
  int retour;
  /* send command to server */
  retour = write(fserver, buf, strlen(buf));
  
#ifdef DEBUG
   fprintf(stderr, "gossip_sock::send_command_to_server(fserver), command sent: \"%s\"\n", buf); 
#endif
   /* and check reply */
   if(get_ack_nack(fserver)) 
     { 
       fprintf(stderr, "Command rejected !!! < %s >, using channel: %d, retour = %d, longueur = %d\n", buf, fserver, retour, strlen(buf)); 
       return(-1);
     }
   else 
     { 
       return(0);
     }

}

/* get a 32 bit integer through a socket in network */
/* (BIG-ENDIAN) order can also be used for a float as   */ 
/* long as sizeof(float) is equal to sizeof(int) and    */
/* both are equal to 4                                  */
INT_32 get_int32_from_channel(int channel)  /*   %ENTRY%   */
{
  INT_32 to_receive;

  fd_set rfds;
  struct timeval tv;
  int res;
  
  FD_ZERO(&rfds);
  FD_SET(channel, &rfds);

  tv.tv_sec = get_stream_timeout(channel);
  tv.tv_usec = 0;
  
  if (select(channel+1, &rfds, NULL, NULL, &tv))
    {
      res = read(channel, &to_receive, sizeof(to_receive));
    }
  else
    {
      return(0);
    }
  if (res <= 0)
    {
      return (res);
    }
    
  if(*little_endian) 
    {
      swap_4(to_receive);
    }
  
  return(to_receive);
}

/* put a 32 bit integer through a socket in network */
/* (BIG-ENDIAN) order can also be used for a float as   */ 
/* long as sizeof(float) is equal to sizeof(int) and    */
/* both are equal to 4                                  */
void put_int32_to_channel(int channel, INT_32 to_send)  /*   %ENTRY%   */
{
  INT_32 to_be_sent = to_send;

  if(*little_endian)
    {
      swap_4(to_be_sent);
    }

  write(channel, &to_be_sent, sizeof(to_be_sent)); /*   %ENTRY%   */
}

/* send special message through a stream socket in network */
static void send_request(int channel, char *request)
{
#ifdef DEBUG
  fprintf(stderr,"gossip_sock::send_request(int channel, char *request), request is: %s\n", request);
#endif
  write_stream(channel, request, strlen(request));
   
}
/* get special message from a stream socket in network  */
static get_request(int channel, char *request)
{
  char reply[128];
  int n;
  int len = strlen(request);
  
  len = len > sizeof(reply)-1 ? sizeof(reply) - 1 : len ;
  n = read_stream(channel, reply, len);
  reply[n > 0 ? n : 0] = '\0';
  
#ifdef DEBUG
  fprintf(stderr, "gossip_sock::get_send_request(), nbre of bytes read =>: %d\n", n);
  fprintf(stderr, "gossip_sock::get_send_request(), reply =>: %s\n", reply);
#endif
  
  if(strncmp(reply, request, strlen(request)) == 0)
    return (1);
  else
    return (0);
  
}

/* connect to subchannel by name using server channel name */
/* subchannel name, and read/write mode, return socket     */
/* descriptor, should have positive value on success, else */
/* connot be used to communicate                           */
int connect_to_subchannel_by_name(char *channel, char *subchannel, char *mode) /*   %ENTRY%   */
{
  int fserver;
  char command[1024];

   /* login to server, get socket descriptor */
  fserver = connect_to_channel_by_name(channel);

#ifdef DEBUG
  fprintf(stderr, "fserver: %d\n" , fserver);
#endif

  if (fserver < 0) 
    return fserver;
  

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::connect_to_subchannel_by_name(), mode = %s, subchannel = %s \n", mode, subchannel);
#endif

  /* send EXEC, mode, and subchannel command to server */
  snprintf(command, sizeof(command) - 1, "EXEC %s %s", mode, subchannel);
  
  if ( send_command_to_server(fserver, command) != 0 ) return(-1);
  
  return fserver;
}

#ifdef DEBUG
int GET_ack_nack(int socket, char *message) /*   %ENTRY%   */
{
  static int compteur = 0;
  int msg;
  compteur++;

  if (message != NULL)
    {
      fprintf(stderr, "connect_to_subchannel_by_name::GET_ack_nack(): compteur: %d, socket: %d, message: %s, de longueur = %d\n", compteur, socket, message, strlen(message));
    }

  msg = get_ack_nack(socket);

  if (msg != 0)
    {
      fprintf(stderr, "connect_to_subchannel_by_name::Probleme avec le socket: %d , message: %s, reponse = %d != 0\n", socket, message, msg);
    }
  return msg;
}
#endif

/* connect to channel_name and send LOGIN sequence */
/*         channel name has 2 forms :              */
/*                               1- name           */
/*                               2- @hostname:port */
/* login sequence has the following syntax         */
/* LOGIN uid pid Auth_token hostname               */
int connect_to_channel_by_name_2(char *name, char * msg)  /*   %ENTRY%   */
{
     /* char *Auth_token = get_broker_Authorization(); */
     int Bauth_token = 0xFFFFFFFF;
     char buf[1024];
     char host_name[1024];
     int fserver;
     
     if(!get_broker_Authorization())
       {
         fprintf(stderr, "Authorizartion token failure \n");
         return(-1);
       }
     if(GetHostName(host_name, sizeof host_name))
       {
         fprintf(stderr, "Can't get local hostname\n");
         return(-1);
       }

     
    /* connect to data server by channel_name or @host:port */
     if ( *name == '@' ) 
       {
         name++;
         fprintf(stderr, "Connecting to: \"%s\"\n", name);
         fserver = connect_to_hostport(name);
         if(fserver < 0)
           return(-1);
       } 
     else 
       {
         char *host_and_port = get_host_and_port(name);
         
         if(host_and_port == NULL) 
           return(-1);

	 

#ifdef DEBUG         
         fprintf(stderr, "Opening channel \"%s\" to name: \"%s\" and port: \"%s\"\n", name, name, host_and_port);
#endif
	 

         fserver = connect_to_hostport(host_and_port);

	 if ( msg && strlen( msg ) > 0 )
	 fprintf( stderr, "Opening channel: \"%s\" with ip and port: \"%s\" using socket: %d\n", name, host_and_port, fserver );

	 if(host_and_port != NULL)
	   {
	     free(host_and_port);
	   }
         
#ifdef DEBUG
         fprintf(stderr, "gossip_sock::connect_to_channel_by_name_2(), fserver = %d\n", fserver);
#endif
         if(fserver < 0) 
           {
#ifdef DEBUG
             fprintf(stderr, "gossip_sock::connect_to_channel_by_name_2(), fserver = %d\n", fserver);
#endif
             return(-1);
           }
       }

     /* send LOGIN command to server */
     sscanf(get_broker_Authorization(), "%u", &Bauth_token);

     if(get_broker_Authorization())
       free(get_broker_Authorization());

     /* add the new info msg to the buffer content before sendind it to the Server */
          
     snprintf(buf, 1023, "%s %d %d %u:%s:%s", "LOGIN", getuid(), getpid(), Bauth_token, host_name, msg);

     if(send_command_to_server(fserver, buf)) 
       {
         fprintf(stderr, "LOGIN rejected\n");
         return(-1);
       } 
     else 
       {
         fprintf(stderr, "LOGIN accepted\n");
         return(fserver);
       }
}

/* connect to channel_by_name and send LOGIN sequence */
/*         channel name has 2 forms :              */
/*                               1- name           */
/*                               2- @hostname:port */
/* login sequence has the following syntax         */
/* LOGIN uid pid Auth_token hostname               */
int connect_to_channel_by_name(char *name)  /*   %ENTRY%   */
{
  /* return connect_to_channel_by_name_2(name, ""); */

  return connect_to_channel_by_name_2(get_gossip_dir(1), "");

}
/* set read/wrtie tiemout */
void set_stream_timeout(int channel, int new_time)
{
  set_client_timeout(channel, new_time);
  fprintf(stderr, "timeout = %d\n", new_time);
}

/* get read/wrtie tiemout*/
int get_stream_timeout(int channel)
{
  return get_client_timeout(channel);
}


/* Write "n" bytes to a stream socket return 0 if OK  */
/* (all bytes have been sent), return -number of bytes not */
/* written if not OK (number of bytes not sent)       */
int write_stream(int fd, char *ptr, int n)  /*   %ENTRY%   */
{
  int  res;
  fd_set wfds;
  struct timeval tv;

#ifdef DEBUG    
  fprintf(stderr, "gossip_sock::write_stream(), nombre de bytes a envoyer = %d\n", n);
  fflush(stderr);
#endif

  FD_ZERO(&wfds);
  FD_SET(fd, &wfds);
 
  while (n > 0) 
    {
      tv.tv_sec = get_stream_timeout(fd);
      tv.tv_usec = 0;
      
      if (select(fd+1, NULL, &wfds, NULL, &tv))
      	{
	  res = write(fd, ptr, n);
	}
      else
        return(-n);
       
      if (res <= 0)
        return (-n);          
      n -= res;
      ptr += res;
    }
  
#ifdef DEBUG
  fprintf(stderr, "gossip_sock::write_stream(), nombre de bytes envoyes = %d\n", res);
  fflush(stderr);
#endif

  return n;
} 


/* Read "n" bytes from a stream socket,     */
/* return bytes_read (number of bytes read) */
int read_stream(int fd, char *ptr, int nbytes)  /*   %ENTRY%   */
{
  int  n, res, bytes_read; 
  fd_set rfds;
  struct timeval tv;
  
  n = nbytes;
  bytes_read = 0;


#ifdef DEBUG 
  fprintf(stderr, "gossip_sock::read_stream(), bytes to be read = %d\n", n);
  fprintf(stderr, "gossip_sock::read_stream(), fd = %d\n", fd);
#endif

  FD_ZERO(&rfds);
  FD_SET(fd, &rfds);

  while (n > 0)
    {  
      tv.tv_sec = get_stream_timeout(fd);
      tv.tv_usec = 0;

      if (select(fd+1, &rfds, NULL, NULL, &tv))
	{
	  res = read(fd, ptr, n);
	}
      else
	{
	  return(0);
	}
      if (res <= 0)
        {
          return (res);
        }
      
      n -= res;
      ptr += res;
      bytes_read += res;
    }

#ifdef DEBUG 
  fprintf(stderr, "gossip_sock::read_stream(), after while(), bytes read = %d\n", res);
#endif   
  
  return bytes_read;
} 

/* swap elements of size tokensize bytes if little endian */
void check_swap_records(void *record, int size, int tokensize) /*   %ENTRY%   */
{
  
  if(!*little_endian || tokensize == ONE_BYTE)
    return;
   
  if(tokensize == TWO_BYTES)
    {
      int i;
      
      INT_16 *element = (INT_16 *)record;
      
#ifdef DEBUG
      fprintf(stderr, "gossip_sock::check_swap_records(),  TWO_BYTES\n");
#endif
      
      for(i = 0; i<size; i++)
        {
	  swap_2(*element);
          element++;
        }
    }


  if(tokensize == FOUR_BYTES)
    {
      int i;
      
      INT_32 *element = (INT_32 *)record;

#ifdef DEBUG
      fprintf(stderr, "gossip_sock::check_swap_records(),  FOUR_BYTES\n");
#endif

      for(i = 0; i<size; i++)
        {
	  swap_4(*element);
          element++;
        }
    }
  else if(tokensize == EIGHT_BYTES)
    {
      int i;
      INT_64 *element = (INT_64 *)record;
      
      for(i = 0; i<size; i++)
        {
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
  int nbytes;
  
  /* wait for send request */ 
  if(!get_request(fclient, "SEND"))
    {
      fprintf(stderr, "gossip_sock::write_record(), problem getting SEND request\n");
      return -1;
    }

  set_timeout_signal(fclient, FALSE);
  /* data delivery protocol: nbytes | data | nbytes */

  /* send data length */
  put_int32_to_channel(fclient, size * tokensize); /* send the 1st length tag = size */
  
  /* send data */
  check_swap_records(record, size, tokensize); /* check for data swaping */

  /* send data length */
  nbytes = write_stream(fclient, record, size * tokensize);

  if(nbytes != 0)
    {
      send_ack_nack(fclient, NOT_OK);
      set_timeout_signal(fclient, TRUE);
      return nbytes;
    }
  
#ifdef DEBUG 
  fprintf(stderr, "gossip_sock::write_record(),  nombre de bytes non envoyes = %d\n", nbytes);
#endif
  
  check_swap_records(record, size, tokensize); /* swap back if necessary */
 
  put_int32_to_channel(fclient, size * tokensize); /* send the 2nd length tag = size */
  
  get_ack_nack(fclient);
 
  return nbytes;
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
  
  return(0);
}

/* Read a record from socket in the format lentgth + record + length */
/* if records==NULL allocate space for data                          */
/* if maxlength==0 no maximum length is specified                    */
/* if length !=0, record length must be: (length) * tokensize        */
void *read_record(int fclient, void *records, int *length, int maxlength, int tokensize)  /*   %ENTRY%   */
{
  char *records2 = NULL;
  
  int length1, length2, length3;
  
  set_timeout_signal(fclient, FALSE);
  
  tokensize = (tokensize > 1)?tokensize:1;
   
#ifdef DEBUG 
  fprintf(stderr, "gossip_sock::read_record(), before send_request(), fclient = %d\n", fclient);
#endif
  
  /* send SEND request */
  send_request(fclient, "SEND");
    
  /**** data delivery protocol: | length | data | length | ****/

  /* read 1st length */
  length1 = get_int32_from_channel(fclient);
  
#ifdef DEBUG 
  fprintf(stderr, "gossip_sock::read_record(), 1st length tag = %d \n", length1);
#endif
  
  if(length1 == 0)
    {
      swallow_data(fclient, length1);
      send_ack_nack(fclient, NOT_OK);
      set_timeout_signal(fclient, TRUE);
      return NULL;
    }

  if(length1 > maxlength * tokensize && maxlength > 0)
    {
      fprintf(stderr, "Problem reading records, length: \"%d\" is greater than max requested: \"%d\" \n", length1, maxlength);
      if (swallow_data(fclient, length1) != 0) 
       {
         fprintf(stderr, "gossip_sock::read_record() : cannot get enough data \n");
       }
      send_ack_nack(fclient, NOT_OK);
      return NULL;
    }
 
  

  records2 = (records == NULL)? malloc(length1):records;

  if(records2 == NULL) 
    {
      fprintf(stderr, "read_record: cannot allocate memory for data with size = %d\n", length1);
      swallow_data(fclient, length1);
      send_ack_nack(fclient, NOT_OK);
      return NULL;
    }
   
  /* read data, and get received stream length */
  length2 = read_stream(fclient, records2, length1);
   
  /* If length2 == 0 => there was a timeout for reading data */
  if(length2 == 0)
    {
      swallow_data(fclient, length1);
      send_ack_nack(fclient, NOT_OK);
      set_timeout_signal(fclient, TRUE);

      if(records == NULL)
	free(records2);
      return NULL; 
    }

  /* read 2nd length  */
  length3 = get_int32_from_channel(fclient);
 
#ifdef DEBUG
  fprintf(stderr, "gossip_sock::read_record(), 2nd length tag = %d \n", length3);
#endif
  
  if(length1 != length2)
    {
      fprintf(stderr, "Problem when reading data: record length = %d != 1st data tag = %d \n", length2, length1);
      send_ack_nack(fclient, NOT_OK);
      set_timeout_signal(fclient, FALSE);

      if(records == NULL)
	free(records2);
      return NULL;
    }
  
  if(*length > 0 && *length * tokensize != length2)
    {
      fprintf(stderr, "Problem reading requested data length %d != length2 = %d\n", *length * tokensize , length2);

      send_ack_nack(fclient, NOT_OK);
      set_timeout_signal(fclient, TRUE);

      if(records == NULL)
	free(records2);
      return NULL;
    }
  
  
  /* check length values */
  if(length1 != length3)
    {
      fprintf(stderr, "Problem when reading data length tags: length1 = %d != length3 = %d \n", length1, length3);
      send_ack_nack(fclient, NOT_OK);
      /* set_timeout_signal(fclient, FALSE); */
      set_timeout_signal(fclient, TRUE);

      if(records == NULL)
	free(records2);
      return NULL;
    } 
  
  /* check swap records */
  check_swap_records(records2, length1/tokensize, tokensize);
  
  /********************************/
  
  send_ack_nack(fclient, IS_OK);
  
  /* return total number of bytes read */
  
  *length = length2/tokensize;

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::read_record(), *length = %d\n", *length);
#endif
  
  return records2;
}

/* signal read timeout return special code -5 */
int signal_timeout(int channel)
{
  return -5;
}

/* set timeout option to TRUE if read timeout expires*/
void set_timeout_signal(int channel, int option)
{
  timeout = option;
}

/* return timeout option (TRUE or FALSE), used in case */
/* to indicate the reason of read problem              */
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
  char buf[BPATH];
  
  /* Current_Working_dir/channel_subchannel_gsave is the data file path to be stored */
  snprintf(buf, sizeof(buf)-1, "%s_%s_gsave", get_gossip_dir(0), file_name);
  

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::store_channel_data(), buf = %s\n", buf);
#endif

    if((fd = open(buf, O_WRONLY + O_CREAT, 0700)) == -1) 
    { 
      fprintf(stderr, "Can't Open or Create Channel Data file\n");
      return(-1);
    }

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::store_channel_data():  nbytes = %d\n", nbytes);
#endif

  
  if(write(fd, (char *)buffer, nbytes) != nbytes) 
    {
      fprintf(stderr, "store_channel_data: Error writing into data file\n");
      close(fd);
      return(-1);
    }
  close(fd);
  return(0);
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

/* get file size using file name */
int get_file_size(char *file_name)   /*   %ENTRY%   */
{
  FILE *ifp;
  char buf[BPATH];
  int the_size;
  
  
#ifdef DEBUG
  fprintf(stderr, "gossip_sock::get_file_size: file_name = %s\n", file_name);
#endif

  snprintf(buf, 1023, "./%s", file_name);
#ifdef DEBUG  
  fprintf(stderr, "gossip_sock::get_file_size(): buf = %s\n", buf);
#endif
 
  if ((ifp = fopen(buf, "r")) == NULL)
    {
      fprintf(stderr, "data file: %s, doesn't exist!\n", buf);
      return 0;
    }
  
#ifdef DEBUG
  fprintf(stderr, "gossip_sock::get_file_size(): ifp = %d\n", ifp);
#endif
  the_size = fsize(ifp);
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
  char buf[BPATH];
  char nbuf[BPATH] = "";
  char *delimiter = "_", *token;
      
#ifdef DEBUG 
  fprintf(stderr, "gossip_sock::read_data_file(): file_name = %s\n", file_name);
#endif

  /* snprintf(buf, sizeof(buf) - 1, "./%s_gsave", file_name); */
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
  fprintf(stderr, "Data file < %s > renamed succefully, return result = %d\n", nbuf ,c);
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
 
/* send flag status to server to check active channels      */
/* on running server with channel $GOSSIPDIR                */
/* default = "mgi", return 0 if command accepted, -1 if not */
int get_status(char *reply)
{
  int fserver = 0;
  char *buf, *reply1;
  int status;

  buf = (char *)malloc(128);
  reply1 = (reply == NULL) ? (char *)malloc(1024) : reply;
  fserver = connect_to_server(); 
  
  if(fserver > 0)
    {
      sprintf(buf, "STATUS");
      status = send_command_to_server(fserver, buf);

      if(status != 0) 
	{
	  fprintf(stderr, "command \"%s\" rejected \n", buf);
	  close(fserver);
	  return status;
	}
      else
	{
	  /* fprintf(stderr, "command \"%s\" accepted\n", buf); */
	  while(read(fserver, reply1, 1024) > 0);
	  close(fserver);
	  return status;
	}
    }

  else
    {
      fprintf(stderr, "No server running on channel \"%s\"!!\n", get_gossip_dir(0));
      return status = -1;
    }

  if(buf)
    free(buf);

  if(reply == NULL)
    free(reply1);
}

/* send command to server running on channel $GOSSIPDIR */
/* return 0 upon success, -1 else                       */
int send_command(char *command)
{
  int fserver = 0;
  int status;
  
  fserver = connect_to_server(); 
  
  if(fserver > 0)
    {
      status = send_command_to_server(fserver, command);

      close(fserver);
     
    }
  else
    {
      fprintf(stderr, "No server running on channel \"%s\" !!\n", get_gossip_dir(0));
      status = -1;
    }
  
  return status;
}

/* close channel blocked on server with read request     */
/* identified by its name and socket descriptor fclient. */
/* send infos with "END" flag to server, return 0 upon   */
/* success, -1 else                                      */
int close_channel(int fclient, char *channel)
{
  int ier = 0; 
  char buf[1024];

  if(fclient != 0)
    {
      snprintf(buf, 1023, "%s %s", "END", channel);
      ier = send_command(buf);
    }

  return ier;

}

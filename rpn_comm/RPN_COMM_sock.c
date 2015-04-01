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
#include <netinet/tcp.h>
#include <netdb.h>
#include <time.h>
#include <signal.h>

#include <mpi.h>

#include <rpnmacros.h>

static int must_init_signal=1;

/* ------------------ START OF CODE BORROWED AND/OR ADAPTED FROM gossip_sock  ---------------- */
/* routines are declared STATIC to avoid potential name / calling sequence conflicts */

/* size of socket buffers in KiloBytes */
#define SOCK_BUF_SIZE 4

/* GetHostName special gethostname with IBM p690 name substitution */
static int GetHostName(char *name, size_t len)
{
  int junk=gethostname(name,len);
  if( name[0]=='c' && name[2]=='f' && name[5]=='p' && name[7]=='m' && name[8]=='\0' )
    name[7]='s';  /* name = cxfyypzm, return cxfyypzs instead */
  return(junk);
}

/* accept connections on the bound server socket return socket for incoming connection */
/* bind_sock_to_port must have been called before connection can be accpeted           */
static int accept_from_sock(int fserver)
{
    struct  sockaddr_in server;                /* server socket */
    socklen_t sizeserver = sizeof server;

    int fclient =  accept(fserver,(struct  sockaddr *)&server, &sizeserver);
    if(fclient < 0) {
      fprintf(stderr,"Accept failed!\n");
      return(-1);
      }
#ifdef DEBUG
    printf("Connection from addr %x, port %d\n",ntohl(server.sin_addr.s_addr),ntohs(server.sin_port));
#endif
    return fclient;
}

/* bind an existing socket to a free (automatic) port, return port number */
/* existing socket usually created by get_sock_net                        */
static int bind_sock_to_port(int s)  /*   %ENTRY%   */
{

     struct sockaddr_in server;
     socklen_t sizeserver = sizeof server;

     server.sin_family= AF_INET;
     server.sin_port= htons(0);
     server.sin_addr.s_addr= INADDR_ANY;

     if(bind(s,(struct  sockaddr *)&server,sizeserver)<0)
          {
          fprintf(stderr,"Bind failed! \n");
          return(-1);
          }
     getsockname(s,(struct  sockaddr *)&server,&sizeserver);
     return ntohs(server.sin_port);
}

/* create a network socket ; return socket descriptor */
static int get_sock_net()  /*   %ENTRY%   */
{
/*   ignore SIGPIPE signal (i.e. do no abort but return error) */

     if(must_init_signal) {  /* DO THIS ONLY ONCE */

       signal(SIGPIPE,SIG_IGN);
       must_init_signal=0;

     }
     return socket(AF_INET, SOCK_STREAM, 0);
}

/* set buffer sizes (recv and send) for a newly created socket (always returns 0) */
static int set_sock_opt(int s)  /*   %ENTRY%   */
{
     socklen_t optval, optsize;
     int b0=0;

     optval=SOCK_BUF_SIZE*1024;
     b0=setsockopt(s,SOL_SOCKET,SO_SNDBUF,(char *)&optval,sizeof(optval));
     if(b0!=0) { fprintf(stderr,"Error setting SO_SNDBUF size \n"); }
     optval=0;optsize=4;
     getsockopt(s,SOL_SOCKET,SO_SNDBUF,(char *)&optval,&optsize);
#ifdef DEBUG
     fprintf(stderr,"SO_SNDBUF=%d, optsize=%d\n",optval,optsize);
#endif

     optval=SOCK_BUF_SIZE*1024;
     b0=setsockopt(s,SOL_SOCKET,SO_RCVBUF,(char *)&optval,sizeof(optval));
     if(b0!=0) { fprintf(stderr,"Error setting SO_RCVBUF size \n");}
     optval=0;optsize=4;
     getsockopt(s,SOL_SOCKET,SO_RCVBUF,(char *)&optval,&optsize);
#ifdef DEBUG
     fprintf(stderr,"SO_RCVBUF=%d, optsize=%d\n",optval,optsize);
#endif
     optval=1;
     b0 = setsockopt( s, IPPROTO_TCP, TCP_NODELAY, (char *)&optval, sizeof(optval) );
     if(b0!=0) { fprintf(stderr,"Error setting TCP_NODELAY \n");}
     return(0);
}

/* obtain the IPV4 adress of a host specified by name */
static int get_ip_address(char *hostname)
{
     int **addr_list;
     struct hostent *answer;
     int ipaddr=0;

     if(NULL==(answer=gethostbyname(hostname))) {
       fprintf(stderr,"Cannot get address for host=%s\n",hostname);
       return(-1);
       }

     addr_list=(int **)answer->h_addr_list;
     ipaddr=ntohl(**addr_list);
     return(ipaddr);
}

/* obtain own host's IPV4 address 
*/
static int get_own_ip_address()  /*   %ENTRY%   */
{
     char buf[1024];
     if(GetHostName(buf,sizeof buf)){
       fprintf(stderr,"Can't find hostname\n");
       return(-1);
       }
     return get_ip_address(buf);

}

/* given a [host:]port specification, connect to it
   if host: is not specified, use localhost
   the return value is the connected socket
*/
static int connect_to_port(int ipaddr, int port)
{
     int fserver, err;

     struct sockaddr_in server;
     int sizeserver = sizeof server;

     fserver = socket(AF_INET, SOCK_STREAM, 0);

     set_sock_opt(fserver);

     server.sin_family= AF_INET;
     server.sin_port= htons(port);
     server.sin_addr.s_addr= htonl(ipaddr);
/*
printf("Trying to connect to port %d at %x\n",port,ipaddr);
*/
     if((err=connect(fserver,(struct  sockaddr *)&server,sizeserver))<0) 
          {
          perror("connect_to_port");
          fprintf(stderr,"Connect failed! with error=%d\n",err);
          return(-1);
          }

     return(fserver);
}

/* bind a server port to a local port, return socket descriptor. bind to any free port */
/*                                     return port and ip address                      */
static int bind_to_port(int *port, int *ipaddress)
{
     int fserver,ipaddr,server_port;

/*   get a socket */
     fserver = get_sock_net();

/*   set buffer sizes for socket */
     set_sock_opt(fserver);

/*   bind to a free port, get port number */
     server_port = bind_sock_to_port(fserver);
     *port = server_port;

/*   get ip address and return the socket file descriptor */
     ipaddr=get_own_ip_address();                    /* get own IPV4 address as 32 bit integer */
     *ipaddress=ipaddr;

     return(fserver);
}

/* ------------------- END OF CODE BORROWED AND/OR ADAPTED FROM gossip_sock  ----------------- */

int f77name(f_omp_get_max_threads)();
int f77name(f_omp_set_num_threads)();
static int omp_max_threads=1;

static void f77name(save_openmp_state)()
{
  int ONE=1;

  omp_max_threads=f77name(f_omp_get_max_threads)();
  f77name(f_omp_set_num_threads)(&ONE);
/*
  printf("Saved state : %d CPUs\n",omp_max_threads);
  printf("Number of threads set to %d\n",ONE);
*/
}
void f77name(restore_openmp_state)()
{
  f77name(f_omp_set_num_threads)(&omp_max_threads);
/*
  printf("Number of threads set to %d\n",omp_max_threads);
*/
}

struct set_of_ports {
	int my_server;              /* file descriptor of bound socket */
	int my_ip;                  /* own ip address */
	int my_port;                /* own port number for bound socket */
	int *list_server;           /* pointer to list of other's ip addresses */
	int *list_port;             /* pointer to list of other's port numbers */
	int pe_me;                  /* own pe number in communicator space */
	int nprocs;                 /* number of PEs in communicator space */
	int comm;                   /* communicator */
	struct set_of_ports *next;  /* pointer to next set_of_ports in chain */
       };
static struct set_of_ports *chain = NULL;  /* chain of set_of_ports */
       
static struct set_of_ports *init_set_of_ports()
{
	struct set_of_ports *p=(struct set_of_ports *)malloc(sizeof(struct set_of_ports));
	
	p->my_server=-1;
	p->my_ip=-1;
	p->my_port=-1;
	p->list_server=NULL;
	p->list_port=NULL;
	p->pe_me=-1;
	p->nprocs=-1;
	p->comm=-1;
	p->next=NULL;
	
	return(p);
}

ftnword f77name(rpn_comm_softbarrier_init)(ftnword *comm)  /* bind to port, return pointer to structure */
{
	struct set_of_ports *p=(struct set_of_ports *)malloc(sizeof(struct set_of_ports));
	
	if(p == NULL) return(-1);
	
	p->my_server=bind_to_port(&(p->my_port),&(p->my_ip));
	MPI_Comm_rank(*comm,&(p->pe_me));
	MPI_Comm_size(*comm,&(p->nprocs));
	p->list_server= malloc(p->nprocs*sizeof(int));
	p->list_port  = malloc(p->nprocs*sizeof(int));
	p->comm       = *comm;
	p->next       = chain;
	
	chain         = p;
	
	listen(p->my_server,2);
	MPI_Allgather(&(p->my_ip)  ,1,MPI_INTEGER,p->list_server,1,MPI_INTEGER,*comm);
	MPI_Allgather(&(p->my_port),1,MPI_INTEGER,p->list_port  ,1,MPI_INTEGER,*comm);
#ifdef DEBUG
	if(p->pe_me==0){
          int pe;
	  for(pe=0;pe<p->nprocs;pe++) printf("PE=%d, IP=%x, port=%d\n",pe,p->list_server[pe],p->list_port[pe]);
	}
#endif
	return(*comm);
}
ftnword f77name(rpn_comm_softbarrier_init_all)(){
	ftnword world=MPI_COMM_WORLD;
	return(f77name(rpn_comm_softbarrier_init)(&world));
}

/* sequence of operations

   PE 0                      PE 1            .......     PE last

   accept                    accept                      NO-OP
   NO-OP                     connect(PE 0)               connect(PE last-1)
   NO-OP                     read(PE 0)                  read(PE last-1)
   write(PE 1)               write(PE 2)                 NO-OP
   read(PE 1)                read(PE 2)                  NO-OP
   NO-OP                     write(PE 0)                 write(PE last-1)
   NO-OP                     close(PE 0)                 close(PE last-1)
   close(PE 1)               close(PE 2)                 NO-OP

*/
int f77name(rpn_comm_softbarrier)(ftnword *comm)   /* perform a soft sync */
{
  char buf[1024];
  int fdesc_up=-1, fdesc_down=-1;
  int status=-1;
  struct set_of_ports *p=chain;
  
  while ( (p!=NULL) && (p->comm != *comm) ) p = p->next ;
  if ( p == NULL ) return(-1);
  
  f77name(save_openmp_state)();
#ifdef DEBUG
  printf("Entering rpn_comm_soft_sync, PE=%d\n",p->pe_me);
  fflush(stdout);
  if(p->pe_me == 0) sleep(1);
#endif
  if(p->pe_me != p->nprocs-1) fdesc_down=accept_from_sock(p->my_server);
#ifdef DEBUG
  if(p->pe_me != p->nprocs-1) printf("PE=%d, accept_from_sock,fdesc_down=%d\n",p->pe_me,fdesc_down);
  fflush(stdout);
#endif
  if(p->pe_me != 0){
#ifdef DEBUG
    printf("PE=%d,connect_to_port %d@%x\n",p->pe_me,p->list_port[p->pe_me-1],p->list_server[p->pe_me-1]);
#endif
    fdesc_up=connect_to_port(p->list_server[p->pe_me-1],p->list_port[p->pe_me-1]);
#ifdef DEBUG
    printf("PE=%d,connected_to_port %d@%x, desc=%d\n",p->pe_me,p->list_port[p->pe_me-1],p->list_server[p->pe_me-1],fdesc_up);
#endif
    read(fdesc_up,buf,4);
    }
  if(p->pe_me != p->nprocs-1) write(fdesc_down,buf,4);

  if(p->pe_me != p->nprocs-1) read(fdesc_down,buf,4);
  if(p->pe_me != 0) write(fdesc_up,buf,4);

  if(fdesc_up != -1) close(fdesc_up);
  if(fdesc_down != -1) close(fdesc_down);
  f77name(restore_openmp_state)();
  return(MPI_SUCCESS);
}

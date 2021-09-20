#include <stdint.h>
#include <rpnmacros.h>

#define swap_2(mot) { register uint16_t tmp =(uint16_t)mot; \
   mot = (tmp>>8) | (tmp<<8) ; }

#define swap_4(mot) { register uint32_t tmp =(uint32_t)mot; \
   mot = (tmp>>24) | (tmp<<24) | ((tmp>>8)&0xFF00) | ((tmp&0xFF00)<<8); }

#define swap_8(mot) { register uint64_t tmp1; register uint64_t tmp2; register uint32_t mot1;\
   tmp1 = ((uint64_t) mot << 32) >> 32 ; \
   tmp2 = (uint64_t) mot >> 32 ; \
   mot  = ( (tmp1>>24) | (tmp1<<24) | ((tmp1>>8)&0xFF00) | ((tmp1&0xFF00)<<8) ) << 32 ; \
   mot1 = ( (tmp2>>24) | (tmp2<<24) | ((tmp2>>8)&0xFF00) | ((tmp2&0xFF00)<<8) ); \
   mot |= mot1; }


#define swap_4_4(mot1,mot2) { register uint32_t tmp1 = (uint32_t)mot1; \
                              register uint32_t tmp2 = (uint32_t)mot2; \
     mot2 = (tmp1>>24) | (tmp1<<24) | ((tmp1>>8)&0xFF00) | ((tmp1&0xFF00)<<8); \
     mot1 = (tmp2>>24) | (tmp2<<24) | ((tmp2>>8)&0xFF00) | ((tmp2&0xFF00)<<8); }

/* macro to swap the bytes in a 32-bit variable */
#define swapbytes32(x) \
{ \
    unsigned int data = *(unsigned int*)&(x); \
    data = ((data & 0xff000000) >> 24) |    \
           ((data & 0x00ff0000) >>  8) |    \
           ((data & 0x0000ff00) <<  8) |    \
           ((data & 0x000000ff) << 24);     \
    *(unsigned int*)&(x) = data;            \
}

/* macro to swap the bytes in a 64-bit variable */
#define swapbytes64(x) \
{ \
    unsigned int *words = (unsigned int *)&(x); \
    unsigned int temp0;  \
    unsigned int temp1;  \
    temp0 = words[0];    \
    swapbytes32(temp0);  \
    temp1 = words[1];    \
    swapbytes32(temp1);  \
    words[1] = temp0;    \
    words[0] = temp1;    \
}

#define ONE_BYTE    1
#define TWO_BYTES   2
#define FOUR_BYTES  4
#define EIGHT_BYTES 8

#define MAX_CLIENTS 128
#define MAX_EXTENDED_CLIENTS 128


typedef struct {
  int uid;
  int pid;
  int socket;
  int client_id;
  char * command;
} CLIENT_SLOT;


typedef struct {
  int uid;
  int pid;
  int socket;
  int client_id;
  char * command;
  void *data;
  void (*user_function)(void *);
} extendedClientSlot;


typedef struct {
  char *name;
  void (*function)(CLIENT_SLOT *);
} TABLE_SLOT;


typedef struct {
  char *name;
  void (*function)(extendedClientSlot *);
} EXTENDED_TABLE_SLOT;


typedef struct {
  int fd;
  unsigned char *buffer;
  unsigned char *in;
  unsigned char *out;
  unsigned char *limit;
  unsigned int BufferSize;
  unsigned int RecLen;
  unsigned int Log2Siz;
  unsigned char flags[4];
} gossip_stream;


int set_host_and_port(char *channel_file, char *host_and_port);
char *get_host_and_port(char *channel_file);
char *get_broker_Authorization();
void set_broker_Authorization(int auth_token);
int accept_from_sock(int fserver);
int bind_sock_to_port(int s);
int get_sock_net();
int set_sock_opt(int s);
int get_ip_address(char *hostname);
int get_own_ip_address();
int connect_to_hostport(char *target);
int connect_to_localport(int port);
int bind_to_localport(int *port, char *buf, int maxbuf);
void send_ack_nack(int fclient,int status);
int get_ack_nack(int fserver);
int send_command_to_server(int fserver, char *buf);
int32_t get_int32_from_channel(int channel);
void put_int32_to_channel(int channel, int32_t to_send);
int connect_to_channel_by_name(char *name);
void set_exit_requested();
void exit_from_client_thread(extendedClientSlot *client);
void increment_client_count();
void decrement_client_count();
int get_client_count();
void reset_timeout_counter();
void increment_timeout_counter();
void decrement_timeout_counter();
int set_timeout_counter(int timeout_value);
int get_timeout_counter();

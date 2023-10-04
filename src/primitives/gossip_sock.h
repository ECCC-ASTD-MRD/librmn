#ifndef RMN_PRIMITIVES_GOSSIP_SOCK_H__
#define RMN_PRIMITIVES_GOSSIP_SOCK_H__

int close_channel(int fclient, char *channel);
int connect_to_subchannel_by_name(char *channel, char *subchannel, char *mode);
int get_timeout_signal( int channel );
int signal_timeout( int channel );

#endif // RMN_PRIMITIVES_GOSSIP_SOCK_H__

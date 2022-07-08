#ifndef GOSSIP_CONSTANTS_H
#define GOSSIP_CONSTANTS_H

/*! \file gossip_constant.h Gossip constants meant to be used by C and Fortran applications */

#ifdef __c
extern "C" {
#endif

#define INIT_ERROR (-1)
#define SERVER_ERROR (-2)
#define CONNECTION_ERROR (-3)
#define READ_ERROR (-4)
#define WRITE_ERROR (-5)
#define READ_TIMEOUT (-6)
#define WRITE_TIMEOUT (-7)
#define READ_TYPE_ERROR (-8)
#define WRITE_TYPE_ERROR (-9)
#define DATA_LENGTH_ERROR (-10)
#define SEND_COMMAND_ERROR (-11)

#ifdef __c
}
#endif

#endif /* GOSSIP_CONSTANTS_H */

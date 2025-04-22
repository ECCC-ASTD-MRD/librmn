#ifndef RMN_PRIMITIVES_H__
#define RMN_PRIMITIVES_H__

#include "rmn/primitives.h"

int get_appl_var(char* varname, char *value, int ln, int lng);
int get_client_timeout(const int fclient);
void init_client_table(const int channel);
int md5_ssh(unsigned char output[16]);
void set_client_timeout(const int fclient, const int timeout);

void c_env_var_cracker(const char * const fstoption, void (*user_function)(), const char * const lang);

void f77name(ieeepak)(int32_t * IFLD, int32_t * IPK, const int32_t * NI, const int32_t * NJ, const int32_t * NPAK, const int32_t * serpas,
                      const int32_t * mode);

#endif // RMN_PRIMITIVES_H__

#ifndef BURP_H
#define BURP_H

#include <stdint.h>
#include <rpnmacros.h>

int c_mrbprml(int buf[], int bkno, int tblprm[], int nprm, int inblocs);

int32_t f77name(mrbdel)(uint32_t *buf, int32_t *f_number);
int c_mrbdel(void *buffer, int number);

#endif

#ifndef BURP98_H
#define BURP98_H

#include "qstdir.h"

void build_burp_prim_keys(burp_record *brpk, uint32_t *keys, burp_record *mask, uint32_t *mskkeys, int index, int mode);
void build_burp_info_keys(uint32_t * const buf, uint32_t * const keys, const int index, const int mode);

#endif

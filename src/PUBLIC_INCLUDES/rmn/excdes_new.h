#ifndef EXCDES_NEW_H
#define EXCDES_NEW_H

#ifndef MAX_Nlist
#define MAX_Nlist 50
#endif

#ifndef MAX_requetes
#define MAX_requetes 50
#endif

#define READLX_DELTA -3
#define READLX_RANGE -2

#ifdef __cplusplus
extern "C" {
#endif

void RequetesInit(void);
int C_fst_match_req(const int handle);

int Xc_Select_ip1(const int set_nb, const int des_exc, const void * const iplist, const int nelm);
int Xc_Select_ip2(const int set_nb, const int des_exc, const void *iplist, int nelm);
int Xc_Select_ip3(const int set_nb, const int des_exc, const void *iplist, int nelm);

int XC_get_MAX_Nlist();
int XC_get_MAX_requetes();

//! \todo Déterminer si cette fonction est interne ou publique
void C_requetes_init(char *requetes_filename, const char * const debug_filename);

#ifdef __cplusplus
}
#endif

#endif

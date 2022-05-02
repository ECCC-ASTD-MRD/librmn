#ifndef EXCDES_NEW_H
#define EXCDES_NEW_H

#ifndef MAX_Nlist
#define MAX_Nlist 50
#endif

#ifndef MAX_requetes
#define MAX_requetes 50
#endif

int XC_get_MAX_Nlist();
int XC_get_MAX_requetes();

//! \todo Déterminer si cette fonction est interne ou publique
void C_requetes_init(char *requetes_filename, const char * const debug_filename);

#endif

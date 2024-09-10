//! \author M. Valin,   Recherche en Prevision Numerique
//! \date 2024

#include <stddef.h>

size_t CopyFileData(const int fdi, const int fdo, const size_t nbytes);
size_t SparseConcatFd(const int fdi, const int fdo, const int diag);
size_t SparseConcatFile(const char *name1, const char *name2, const int diag) ;
int SparseConcat_main(int argc, char **argv) ;

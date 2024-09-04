//! \author  M. Valin,   Recherche en Prevision Numerique, 2024

#include <stdio.h>
#include <string.h>

#include <App.h>
#include <rmn/sparse_concat.h>

static void usage(char *argv0) {
    App_Log(APP_VERBATIM,
        "Append a set of potentially sparse files to another potentially sparse file\n\n"
        "usage :\n"
        "  %s file_0 \n"
        "     print a hole/data map of sparse file\n"
        "  %s file_0 file_1 [file_2] ... [file_n] \n"
        "     append sparse files file_1 ... file_n to file_0 as a sparse file\n",
        argv0, argv0);
}

//! If only one argument, print data/hole map
//! If more than one argument append given files to the first in the list
int main(int argc, char **argv){
  char *name_in = "/dev/null";
  char *argv0 = argv[0] ;

  if (argc < 2) {
    usage(argv0);
    return 1;
  }

  if(argc > 2) {
    name_in  = argv[2] ;
  }
  size_t szo = SparseConcatFile(argv[1], name_in, argc == 2);            // argc ==2 : dump file map
  for (int i = 3 ; i < argc ; i++) {                                     // more than 2 arguments ?
    szo = SparseConcatFile(argv[1], argv[i], 0);
  }
  if(argc > 2) App_Log(APP_INFO, "Final size of %s = %ld\n", argv[1], szo) ;

  return 0 ;
}

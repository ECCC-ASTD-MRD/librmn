#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/file.h>
#include <sys/stat.h>

main(int argc,char* argv[]) {

   struct stat st;

   if (stat(argv[1],&st)==-1) {
      fprintf(stderr,"stat failed\n");
   } else{
      fprintf(stderr,"stat ok\n");
   }


   if (access(argv[1],R_OK)==-1) {
      fprintf(stderr,"access failed\n");
   } else{
      fprintf(stderr,"access ok\n");
   }
}

#include <stdio.h>

int main(int argc, char **argv){
  while((*argv) != NULL){
    fprintf(stderr, "%s ", *argv) ;
    argv++ ;
  }
}

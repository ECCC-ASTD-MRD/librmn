#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main(){
  int status1, status2 ;
  return system("diff -q ./test_simd.ref ./test_simd.txt") ? 1 : 0 ;
}

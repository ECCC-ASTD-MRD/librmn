#include <stdio.h>
#include <stdlib.h>
#include <rpnmacros.h>

extern unsigned int calc_crc(unsigned char *p, ftnword *flen, ftnword *fseed, ftnword stride);

typedef struct 
  {
  char grtyp[8], grref[8];
  int ni, nj, ig1, ig2, ig3, ig4, ig1ref, ig2ref, ig3ref, ig4ref;
  } _grinfo;

main()
  {
  _grinfo gridinfo;
  int fseed, un, gridinfosize;
  unsigned int grid_crc, grid_index, grid_subindex;
  
  fseed = 0;
  un = 1;
  
  memset(&gridinfo, (int)NULL, sizeof(gridinfo));
  
  gridinfo.grtyp[0] = 'Z';
  gridinfo.grref[0] = 'E';
  gridinfo.ni = 801;
  gridinfo.nj = 601;
  gridinfo.ig1 = 1;
  gridinfo.ig2 = 2;
  gridinfo.ig3 = 3;
  gridinfo.ig4 = 4;
  gridinfo.ig1ref = 0;
  gridinfo.ig2ref = 0; 
  gridinfo.ig3ref = 9000;
  gridinfo.ig4ref = 100;
  
  gridinfosize = sizeof(_grinfo);
  grid_crc = calc_crc(&gridinfo, &gridinfosize, &fseed, un);
  grid_index = grid_crc % 262111;
  grid_subindex = grid_index % 463;
 /* printf("crc: %u %x, index: %d\ subindex: %d\n", grid_crc, grid_crc, grid_index, grid_subindex); */
  
  memset(&gridinfo, (int)NULL, sizeof(gridinfo));
  
  gridinfo.grtyp[0] = 'G';
  gridinfo.ni = 400;
  gridinfo.nj = 200;
  gridinfo.ig1 = 0;
  gridinfo.ig2 = 0;
  gridinfo.ig3 = 0;
  gridinfo.ig4 = 0;
  
  gridinfosize = sizeof(_grinfo);
  grid_crc = calc_crc(&gridinfo, &gridinfosize, &fseed, un);
  grid_index = grid_crc % 262111;
  grid_subindex = grid_index % 463;
/*  printf("crc: %u %x, index: %d\ subindex: %d\n", grid_crc, grid_crc, grid_index, grid_subindex); */

  }

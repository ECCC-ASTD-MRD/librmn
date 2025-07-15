/* Hopefully useful functions
 * Copyright (C) 2023  Recherche en Prevision Numerique
 *
 * This is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 */
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <rmn/c_record_io.h>

// read a 32 bit (integer or float) data record (native endianness)
// layout :  number_of_dimensions dimensions[number_of_dimensions] data[] number_of_data
// data array is in Fortran order (first index varying first)
// filename    [IN] : path/to/file (only valid if fdi == 0)
// fdi      [INOUT] : ABS(fdi) = file descriptor. if fdi < 0, close file after reading record
// dims[]   [INOUT] : array dimensions (max 10 dimensions) (OUT if ndim == 0, IN for check if ndim > 0)
// ndim     [INOUT] : number of dimensions (0 means unknown and get dimensions and number of dimensions)
// ndata      [OUT] : number of data items read, -1 if error, 0 if End Of File
// name       [OUT] : 4 character array that will receive the variable name
// return : pointer to data array from file
// if *fdi != 0, filename will be ignored
void *read_32bit_data_record(char *filename, int *fdi, int *dims, int *ndim, int *ndata){
  char name[5] ;
  return read_32bit_data_record_named(filename, fdi, dims, ndim, ndata, name) ;
}
void *read_32bit_data_record_named(char *filename, int *fdi, int *dims, int *ndim, int *ndata, char name[4]){
  ssize_t nc = 4, nd ;
  ssize_t nr ;
  void *buf = NULL ;
  uint32_t ntot ;
  int i ;
  int fd = *fdi ;
  int diml[7] ;

  name[0] = name[1] = name[2] = name[3] = ' ' ;                // set name to all spaces
  *ndata = -1 ;                                                // precondition for error
  fd = (fd < 0) ? -fd : fd ;                                   // ABS(fd), negative fdi means close after reading record
  if(fd == 0 && filename[0] == '\0') return NULL ;             // filename should be valid if fd == 0
  if(fd == 0) fd = open(filename, O_RDONLY) ;                  // open file using filename only if fd == 0
//   if(fd != 0 && filename[0] != '\0') return NULL ;             // invalid fd / filename combination
//   fd = (filename[0] != '\0') ? open(filename, O_RDONLY) : fd ; // open file if filename supplied
  if(fd < 0) return NULL ;                                     // open failed
  if(*fdi == 0) *fdi = fd ;                                    // new fd stored in fdi

  if( (nr = read(fd, &ntot, nc)) == 0){                        // read number of dimensions, check for EOF
    fprintf(stderr,"INFO : EOF\n") ;
    *ndata = 0 ;                                               // EOF, no data read, not an error
    return NULL ;
  }
  if(nr != nc) {
    fprintf(stderr,"ERROR, short read\n");
    return NULL ;
  }
  name[0] = ntot >> 25 ; ntot <<= 7 ;                   // extract the 4 character variable name
  name[1] = ntot >> 25 ; ntot <<= 7 ;
  name[2] = ntot >> 25 ; ntot <<= 7 ;
  name[3] = ntot >> 25 ; ntot <<= 7 ;
  for(i=0 ; i<4 ; i++) name[i] = (name[i] == 0) ? ' ' : name[i] ;
  ntot >>= 28 ;                                         // get the number of dimensions (lower 4 bits)

  if(ntot > 7){
    fprintf(stderr,"ERROR, more than 7 dimensions (%d)\n", ntot) ;
    return NULL ;
  }
  if(*ndim == 0) {
    *ndim = (int32_t)ntot ;                             // return number of dimensions to caller
    for(i=0 ; i<(int32_t)ntot ; i++) dims[i] = 0 ;               // set dimensions to 0 (they will be returned to the caller)
  }else{
    if(ntot != (uint32_t)(*ndim)) {
      fprintf(stderr,"ERROR, read ndim = %d, should be %d\n", ntot, *ndim) ;
      goto error ;
    }
  }
  nd = 1 ;
  nr = read(fd, diml, nc * ntot) ;                      // read the dimensions
  if(nr != nc * ntot) goto error ;                      // OOPS short read
  for(i=0 ; i<(int32_t)ntot ; i++) {                    // check the dimensions
    if(dims[i] == 0) dims[i] = diml[i] ;                // return dimension to caller if dims[i] == 0
    if(dims[i] != diml[i]){                             // check dimension against request
      fprintf(stderr,"ERROR, dimension %d mismatch\n", i+1);
      goto error ;
    }
    nd *= diml[i] ;                                     // update size of array to be read
  }
  if(nd <= 0) {
    fprintf(stderr,"ERROR, one or more dimension is <= 0\n") ;
    goto error ;
  }
  buf = malloc(nd * 4) ;                                // allocate space to read record data[]
  if(buf == NULL) goto error ;                          // alloc failed
  if( (nr = read(fd, buf, nd*4)) < nd*4) {              // OOPS short read for data
    fprintf(stderr,"ERROR, attempted to read %ld bytes, got %ld\n", nd*4, nr) ;
    *ndata = nr/4 ;                                     // number of data read
    goto error ;
  }
  if( (nr = read(fd, &ntot, nc)) != nc) {               // check number of data from record
    fprintf(stderr,"ERROR, short read (number of data)\n");
    goto error ;       // OOPS short read
  }
  if(ntot != nd) {
    fprintf(stderr,"ERROR, inconsistent number of data ntot = %d. nd = %lu\n", ntot, nd);
    goto error ;  // inconsistent number of data
  }
  *ndata = ntot ;

end:
  if(*fdi < 0) close(fd) ;

  return buf ;

error:
//   fprintf(stderr,"DEBUG : ERROR in read_32bit_data_record\n");
  if(buf != NULL) free(buf) ;
  buf = NULL ;
  goto end ;
}

// write a 32 bit (integer or float) data record (native endianness)
// layout :  number_of_dimensions dimensions[number_of_dimensions] data[] number_of_data
// data array is in Fortran order (first index varying first)
// filename    [IN] : path/to/file (only valid if fdi == 0)
// fdi      [INOUT] : ABS(fdi) = file descriptor. if fdi < 0, close file after reading record
// dims[]      [IN] : array dimensions (max 10 dimensions) (OUT if ndim == 0, IN for check if ndim > 0)
// ndim        [IN] : number of dimensions (0 means unknown and get dimensions and number of dimensions)
// buf         [IN] : data to be written
// name        [IN] : 4 character array containing the variable name
// return : number of data items written (-1 in case of error)
// if *fdi != 0, filename will be ignored
int write_32bit_data_record(char *filename, int *fdi, int *dims, int ndim, void *buf){
  return write_32bit_data_record_named(filename, fdi, dims, ndim, buf, NULL) ;
}
int write_32bit_data_record_named(char *filename, int *fdi, int *dims, int ndim, void *buf, char name[4]){
  int fd = *fdi ;
  ssize_t nc = 4 ;
  ssize_t nr ;
  int ndims, ntot = 0, i, clos = (*fdi < 0) ;

  fd = (fd < 0) ? -fd : fd ;                                   // ABS(fd), negative fdi means close after reading record
  if(fd == 0 && filename[0] == '\0') return -1 ;               // filename MUST be valid if fd == 0
  if(fd == 0) fd = open(filename, O_WRONLY | O_CREAT, 0777) ;  // open file using filename only if fd == 0
//   if(fd != 0 && filename[0] != '\0') return -1 ;               // invalid fd / filename combination
//   fd = (filename[0] != '\0') ? open(filename, O_WRONLY | O_CREAT, 0777) : fd ;    // open file if filename supplied
  if(fd < 0) return -1 ;                                       // open failed
  *fdi = fd ;                                                  // new fd stored in fdi
  if(buf == NULL){                                             // just open or close the file if buf is NULL
    ntot = 0 ;
fprintf(stderr,"INFO: just closing fd = %d\n", fd);
    goto end ;
  }

  ndims = 0 ;                                                  // encode number of dimensions and 4 character name
  for(i=0 ; i<4 ; i++) { ndims <<= 7 ; ndims |= (name[i] & 0x7F) ; }
  ndims <<= 4 ; ndims |= ndim ;

  if( (nr = write(fd, &ndims, nc)) != nc) goto error ;         // number of dimensions
  if( (nr = write(fd, dims, ndim*nc)) != ndim*nc) goto error ; // dimensions
  ntot = 1 ;
  for(i=0 ; i<ndim ; i++) ntot *= dims[i] ;
  if( (nr = write(fd, buf, nc*ntot)) != nc*ntot) goto error ;  // data
  nr = write(fd, &ntot, nc) ;                                  // number of data
  if(nr != nc) goto error ;                                    // done this way to suppress warnings
end:
  if(clos) close(fd) ;
  return ntot ;
error:
  ntot = -1 ;
  goto end ;
}

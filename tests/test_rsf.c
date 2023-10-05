
#if defined(RSF_OPEN)

#include "rsf/rsf_int.h"
#define META_SIZE 6
int64_t RSF_Scan_vdir(RSF_File *fp, int64_t key0, uint32_t *criteria, uint32_t *mask, uint32_t lcrit, uint64_t *wa, uint64_t *rl);
int32_t RSF_Get_vdir_entry(RSF_File *fp, int64_t key, uint64_t *wa, uint64_t *rl, uint32_t **meta);

void usage(char **argv){
  fprintf(stderr,"usage : %s rsf_file [verbose]\n",argv[0]);
}
int the_test(int argc, char **argv){
  int64_t verbose = 0 ;
  char command[1024] ;
  int32_t meta_dim = META_SIZE ;
  RSF_handle h1 ;
  int64_t key = 0 ;
  uint32_t criteria[1024] ;
  uint32_t mask[1024] ;
  int i ;
  uint64_t wa, rl ;
  uint32_t *meta ;
  int32_t ml ;

  if(argc < 2){
    usage(argv) ;
    exit(1) ;
  }
  if(argc > 2) verbose = atoi(argv[2]) ;
  snprintf(command, sizeof(command), "ls -l %s", argv[1]) ;
  if(system(command) == 0) fprintf(stderr,"command '%s' O.K.\n", command) ;
  h1 = RSF_Open_file(argv[1], RSF_RO, &meta_dim, "DeMo", NULL);  // open file
  for(i=0 ; i<1024 ; i++) {mask[i] = 0 ; criteria[i] = 0 ; } ;
  for(i=0 ; i<33 ; i++){
    if(i == 29) { mask[1] = 0xFFFFFFFF ; criteria[1] = 0xFFFFFFFF ; }
    key = RSF_Scan_vdir(h1.p, key, criteria, mask, (2+i <= 5) ? (2+i) : 5 , &wa, &rl);
    if( i < 30) {
      ml = RSF_Get_vdir_entry(h1.p, key, &wa, &rl, &meta);
    }else{
      ml = -1 ; wa = 1 ; rl = 1 ;
    }
    fprintf(stderr,"i = %2d, key = %16.16lx, wa = %lx, rl = %ld, ml = %d\n", i, key, wa, rl, ml) ;
  }
  fprintf(stderr,"-------------- RSF_Dump_dir --------------\n") ;
  RSF_Dump_dir(h1) ;
  fprintf(stderr,"-------------- RSF_Dump_vdir --------------\n") ;
  RSF_Dump_vdir(h1) ;
  return(0) ;
}

#endif // RSF_OPEN

#if defined(RSF_DUMP)

#include <rmn/rsf.h>
void usage(char **argv){
  fprintf(stderr,"usage : %s rsf_file [verbose]\n",argv[0]);
}
int the_test(int argc, char **argv){
  int64_t verbose = 0 ;
  char command[1024] ;
  RSF_handle h1 ;
  int32_t meta_dim = 0 ;
  int i ;
  uint64_t key ;
  void *p ;

  if(argc < 2){
    usage(argv) ;
    exit(1) ;
  }
  if(argc > 2) verbose = atoi(argv[2]) ;
  snprintf(command, sizeof(command), "ls -l %s", argv[1]) ;
  system(command) ;
  if(verbose >= 0) {
    RSF_Dump(argv[1], verbose) ;
  }else{
    h1 = RSF_Open_file(argv[1], RSF_RO, meta_dim, "demo", NULL);  // open file
    fprintf(stderr,"file '%s', meta_dim = %d\n",argv[1],meta_dim) ;
    for(i = 0 ; i < 5 ; i++) {
      key = i + 1 ;
      key += 0x100000000ul ;         // simulate file slot 0 for this file
      p = RSF_Get_record(h1, key) ;
      if(p) free(p) ;
    }
    RSF_Dump_vdir(h1) ;                                             // dump memory directoey
  }
  return(0) ;
}

#endif // RSF_DUMP

#if defined(TEST7S)
// test7s.Abs filename ntests
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <rsf.h>
#include <mpi.h>

#define REC_META 6
#define DIR_META 4
#define MAX_DATA 3000
#define MAX_REC 128
#define FILE_RECORD 2

// generate data values
static inline int32_t data_value(int32_t index, int32_t recno, int32_t rank){
  return index + recno + rank ;
}

// generate metadata values
static inline int32_t meta_value(int32_t index, int32_t recno, int32_t rank){
  return index + recno + rank ;
}

// fill data[0] -> data[ndata - 1]
void fill_data(int32_t *data, int64_t ndata, int32_t recno, int32_t rank){
  int32_t i;
  for(i=0 ; i<ndata ; i++) {
    data[i] = data_value(i, recno, rank) ;
  }
}

// fill meta[1] -> meta[nmeta - 1] (meta[0] is left untouched)
void fill_meta(uint32_t *meta, int32_t nmeta, int32_t recno, int32_t rank){
  int32_t i;
  for(i=1 ; i<nmeta ; i++) {
    meta[i] = meta_value(i, recno, rank) ;
  }
}

int32_t check_data(int32_t *data, int32_t ndata, int32_t recno, int32_t rank){
  int32_t i;
  int32_t errors = 0 ;
  for(i=0 ; i<ndata ; i++) {
    if( data[i] != data_value(i, recno, rank) ) errors++ ;
  }
  return errors ;
}

int32_t check_meta(uint32_t *meta, int32_t nmeta, int32_t recno, int32_t rank){
  int32_t i;
  int32_t errors = 0 ;
  for(i=1 ; i<nmeta ; i++) {
    if( meta[i] != meta_value(i, recno, rank) ) errors++ ;
  }
  return errors ;
}

int the_test(int argc, char **argv){
  int my_rank, nprocs ;
  int ntests = 999 ;
  int creator = 0 ;
  int32_t meta_dim = DIR_META ;
  int32_t meta_rdim = 0 ;
  int32_t data[MAX_DATA] ;
//   int32_t data_in[MAX_DATA] ;
  uint32_t meta[REC_META] ;
  uint32_t criteria[REC_META] ;
  uint32_t mask[DIR_META] ;
  int64_t put_slot[MAX_REC] ;
  int32_t slot_meta[MAX_REC] ;
  int32_t meta0[MAX_REC] ;
  int64_t segsize = 0 ;
  int64_t ndata, max_bytes ;
  int64_t key ;
//   uint64_t wa, rl ;
  RSF_handle h1 ;
  int32_t recno = 0 ;
  int32_t i, j ;
  RSF_record *record = NULL ;
  RSF_record_info ri ;
  useconds_t usleep_delay = 10000 ;
  int32_t status ;

  MPI_Init(&argc, &argv) ;
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank) ;
  MPI_Comm_size(MPI_COMM_WORLD, &nprocs) ;
  if(argc < 2) goto ERROR ;
  if(argc >= 3) {
    ntests = atoi(argv[2]) ;
  }
  if(argc >= 4) {
    creator = atoi(argv[3]) ;
  }
  fprintf(stderr,"PE %d of %d, pid = %d, ntests = %d\n", my_rank+1, nprocs, getpid(), ntests) ;
  if(creator >= nprocs) {
    if(my_rank == 0) fprintf(stderr,"ERROR : invalid creator process %d (should be between 0 and %d)\n", creator, nprocs-1);
    goto ERROR ;
  }
  if(my_rank == creator){
    fprintf(stderr,"===== verbosity level table =====\n") ;
    for(i = RSF_DIAG_NONE-1 ; i <= RSF_DIAG_DEBUG2 + 1 ; i++) fprintf(stderr, "level %2d = '%s'\n", i, RSF_diag_level_text(i)) ;
    fprintf(stderr,"verbosity level is now %s, was %s\n", 
            RSF_diag_level_text(RSF_DIAG_DEBUG0), RSF_diag_level_text(RSF_set_diag_level(RSF_DIAG_DEBUG0))) ;
  }

  fprintf(stderr,"=============== phase 1, creator process = %d ===============\n", creator) ;

  MPI_Barrier(MPI_COMM_WORLD) ;

  int errors = 0 ;
  if(my_rank == creator){
//     segsize = 1024 * 1024 ;
    segsize = 0 ;
    h1 = RSF_Open_file(argv[1], RSF_RW, &meta_dim, "DeMo", &segsize);  // open segmented file in exclusive mode
    for(i=0 ; i<5 ; i++){
      ndata = 100 + i ;
      if(record != NULL) RSF_Free_record(record) ;
      max_bytes = (i & 0x3) + ndata * sizeof(int32_t) ;
      record = RSF_New_record(h1, REC_META, DIR_META, max_bytes, NULL, max_bytes) ;
      fprintf(stderr,"record allocated at %p \n", record);
      meta[0] = (8 + i) | (1 << 8) ;                 // class 1 records, record type = 8 + i
      fill_meta(meta, REC_META, recno, my_rank) ;
      fill_data(data, ndata, recno, my_rank) ;
      if(i != FILE_RECORD){                          // regular data
        slot_meta[i] = DIR_META ;
        put_slot[i] = RSF_Put_bytes(h1, NULL, meta, REC_META, DIR_META, data, (i & 0x3) + ndata * sizeof(int32_t), DT_32) ;
//         fprintf(stderr,"record data size = %d, ndata = %d\n", (i & 0x3) + ndata * sizeof(int32_t), ndata) ;
//         meta0[i] = meta[0] ;
      }else{                                         // file container
        put_slot[i] = RSF_Put_file(h1, "icc.txt", meta, 2) ;
        slot_meta[i] = 2 ;
//         meta0[i] = RT_FILE + (RT_FILE_CLASS << 8) ;
      }
      meta0[i] = meta[0] ;
      fprintf(stderr," recno = %d, slot = %ld [", recno, put_slot[i]) ;
      for(j=0 ; j<REC_META ; j++) fprintf(stderr," %8.8x", meta[j]) ;
      fprintf(stderr,"]\n") ;
      recno++ ;
    }
    if(record != NULL) RSF_Free_record(record) ;
    fprintf(stderr,"===      get record information      ===\n") ;
    for(i=0 ; i<recno ; i++){
      ri = RSF_Get_record_info(h1, put_slot[i]) ;
      fprintf(stderr," record %d : wa = %8.8ld, rl = %8ld, dl = %8ld(%dB), rec_meta = %d, dir_meta = %d(%d) %s\n",
              i, ri.wa, ri.rl, ri.data_size, ri.elem_size, ri.rec_meta, ri.dir_meta, ri.dir_meta0, ri.fname ? ri.fname : "" ) ;
    }
    fprintf(stderr,"===      test record locator      ===\n") ;
    for(i=1 ; i<DIR_META ; i++) mask[i] = 0xFFFFFFFF ;
    mask[0] = 0 ; // record class and type will be ignored

    fprintf(stderr," - from beginning of file\n");
    errors = 0 ;
    for(i=0 ; i<recno ; i++){ // find loop starting at beginning of file
      fill_meta(criteria, REC_META, i, my_rank) ;
      key = 0 ;
      key = RSF_Lookup(h1, key, criteria, mask, slot_meta[i]) ;
      fprintf(stderr,"record = %d, key = %16.16lx, expected = %16.16lx, ML = %d\n", 
                     i, key, put_slot[i], slot_meta[i]) ;
      if(key != put_slot[i]) errors++ ;
    }
    fprintf(stderr,"%s\n", errors == 0 ? "SUCCESS" : "FAILED") ;
    if(errors > 0) goto END1 ;

    fprintf(stderr," - from previous position in file\n");
    key = 0 ;
    errors = 0 ;
    for(i=0 ; i<recno ; i++){ // find loop starting at previously found key
      fill_meta(criteria, REC_META, i, my_rank) ;
      key = RSF_Lookup(h1, key, criteria, mask, slot_meta[i]) ;
      fprintf(stderr,"record = %d, key = %16.16lx, expected = %16.16lx, ML = %d\n", 
                     i, key, put_slot[i], slot_meta[i]) ;
      if(key != put_slot[i]) errors++ ;
    }
    fprintf(stderr,"%s\n", errors == 0 ? "SUCCESS" : "FAILED") ;
    if(errors > 0) goto END1 ;

    fprintf(stderr," - from beginning of file, with class and record type\n");
    errors = 0 ;
    mask[0] = 0xFFFFFFFFu ;
    for(i=0 ; i<recno ; i++){ // find loop starting at beginning of file
      criteria[0] = meta0[i] ; // account for file container records
      fill_meta(criteria, REC_META, i, my_rank) ;
      key = 0 ;   // from start of file
      key = RSF_Lookup(h1, key, criteria, mask, slot_meta[i]) ;
      fprintf(stderr,"record = %d, key = %16.16lx, expected = %16.16lx, ML = %d\n", i, key, put_slot[i], slot_meta[i]) ;
      if(key != put_slot[i]) errors++ ;
    }
    fprintf(stderr,"%s\n", errors == 0 ? "SUCCESS" : "FAILED") ;
    if(errors > 0) goto END1 ;

    fprintf(stderr," - from previous position in file, with class and record type\n");
    errors = 0 ;
    mask[0] = 0xFFFFFFFFu ;
    key = 0 ;   // from start of file
    for(i=0 ; i<recno ; i++){ // find loop starting at previous position in file
      criteria[0] = meta0[i] ; // account for file container records
      fill_meta(criteria, REC_META, i, my_rank) ;
      key = RSF_Lookup(h1, key, criteria, mask, slot_meta[i]) ;
      fprintf(stderr,"record = %d, key = %16.16lx, expected = %16.16lx, ML = %d\n", i, key, put_slot[i], slot_meta[i]) ;
      if(key != put_slot[i]) errors++ ;
    }
    fprintf(stderr,"%s\n", errors == 0 ? "SUCCESS" : "FAILED") ;
    if(errors > 0) goto END1 ;
    RSF_Close_file(h1) ;
  }   // if(my_rank == creator)

END1 :
  MPI_Bcast(&errors, 1, MPI_INTEGER, creator, MPI_COMM_WORLD) ;
  if(errors != 0) goto FAILED ;
  MPI_Barrier(MPI_COMM_WORLD) ;

  if(ntests <= 1) goto END ;
  MPI_Bcast(&recno,    1,     MPI_INTEGER, creator, MPI_COMM_WORLD) ;
  MPI_Bcast(meta0,     recno, MPI_INTEGER, creator, MPI_COMM_WORLD) ;
  MPI_Bcast(slot_meta, recno, MPI_INTEGER, creator, MPI_COMM_WORLD) ;

  fprintf(stderr,"=============== phase 2 (all but process %d) ===============\n", creator) ;
  int reload = 0 ;
  if(my_rank != creator){
    segsize = 111111 ;
    meta_rdim = 0 ;
    h1 = RSF_Open_file(argv[1], RSF_RO, &meta_rdim, "DeMo", &segsize);
    fprintf(stderr," segsize = %ld, meta_dim = %d\n", segsize, meta_rdim) ;

    fprintf(stderr," - blind next key lookup + record read\n") ;
    key = 0 ;
    i = 0 ;
    errors = 0 ;
    while( (key = RSF_Lookup(h1, key, NULL, NULL, 0)) >= 0) {
      RSF_record_info rec_info = RSF_Get_record_info(h1, key) ;
//       uint32_t *dirmeta;
      ndata = 100 + i ;
      if((rec_info.meta[0] & 0xFF) != RT_FILE) {      // not a file container
        record = RSF_Get_record(h1, key) ;
        errors += check_data(record->data, ndata, i, creator) ;
        fprintf(stderr," key = %16.16lx, status = %d, data = %p, ndata = %ld(%ld)\n", 
                      key, status, record->data, ndata * rec_info.elem_size, record->data_size) ;
      }else{                                          // this is a file container
        uint32_t *metaf, fmeta_size ;
        int64_t key2 = RSF_Get_file(h1, key, "tagada.txt", &metaf, &fmeta_size) ;
        fprintf(stderr," key = %16.16lx,  %16.16lx, file size = %ld\n", key, key2, rec_info.file_size) ;
        if(key2 == key) reload++ ;
        fprintf(stderr,"reload %s as tagada.txt  : %s\n", rec_info.fname,(key == key2) ? "SUCCESS" : "FAILED" ) ;
      }
      i++ ;
      if(record != NULL) {
        RSF_Free_record(record) ;
        record = NULL ;
      }
    }
    fprintf(stderr," end key = %16.16lx, read errors = %d, ", key, errors) ;  // ends on invalid key
    fprintf(stderr,"%s\n", errors == 0 ? "SUCCESS" : "FAILED") ;

    fprintf(stderr," - targeted key lookup from beginning of file\n") ;
    key = 0 ;
    for(i=1 ; i<DIR_META ; i++) mask[i] = 0xFFFFFFFF ;
    mask[0] = 0xFFFFFFFFu ;
    errors = 0 ;
    for(i=0 ; i<recno ; i++){
      key = 0 ;
      criteria[0] = meta0[i] ; // account for file container records
      fill_meta(criteria, REC_META, i, creator) ;  // use creator rank for criteria
      key = RSF_Lookup(h1, key, criteria, mask, slot_meta[i]) ;
      fprintf(stderr," key = %16.16lx, ML = %d\n", key, slot_meta[i]) ;
      if(key <= 0) {
        fprintf(stderr,"ERROR: key = %16.16lx, at record #%d\n", key, i) ;
        errors++ ;
      }
    }
    RSF_Close_file(h1) ;
  }
  int total_errors = 0 ;
  int total_reload = 0 ;
  MPI_Allreduce(&errors, &total_errors, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD) ;
  MPI_Allreduce(&reload, &total_reload, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD) ;
  if(total_reload != 1) {
    fprintf(stderr,"ERROR: expecting 1 successful reload, got %d FAILED\n", total_reload) ;
    total_errors++ ;
  }else{
    fprintf(stderr,"Phase 2 successful reloads = %d, SUCCESS\n", total_reload) ;
  }
  fprintf(stderr,"Phase 2 TOTAL ERRORS = %d\n", total_errors) ;
  if(ntests <= 2) goto END ;

  fprintf(stderr,"=============== phase 3 (%d existing records) ===============\n", recno) ;

  segsize = 1024 * 1024 ;
  h1 = RSF_Open_file(argv[1], RSF_RW, &meta_dim, "DeMo", &segsize);  // open segmented file in parallel mode
  usleep(usleep_delay = 100000) ;
  for(i=0 ; i<2 ; i++){
      meta[0] = RT_DATA | (1 << (8 + my_rank)) ;                 // class 2**my_rank records
      fill_meta(meta, REC_META, recno+i, my_rank) ;
      fill_data(data, 100+i, recno, my_rank) ;
      put_slot[i] = RSF_Put_bytes(h1, NULL, meta, REC_META, DIR_META, data, 100L+i, DT_32) ;
      fprintf(stderr," recno = %d, slot = %16.16lx [", recno+i, put_slot[i]) ;
      for(j=0 ; j<REC_META ; j++) fprintf(stderr," %8.8x", meta[j]) ;
      fprintf(stderr,"]\n") ;
  }
  RSF_Close_file(h1) ;

  MPI_Barrier(MPI_COMM_WORLD) ;
  if(ntests <= 3) goto END ;
  fprintf(stderr,"=============== phase 4 (%d existing records) ===============\n", recno + 2*nprocs) ;
  segsize = 0 ;
  h1 = RSF_Open_file(argv[1], RSF_RO, &meta_dim, "DeMo", &segsize);

  fprintf(stderr," - blind next key lookup\n") ;
  key = 0 ;
  int count = 0 ;
  while( (key = RSF_Lookup(h1, key, NULL, NULL, 0)) >= 0) {
    fprintf(stderr," key = %16.16lx\n", key) ;
    count++ ;
  }
//   fprintf(stderr," end key = %16.16lx\n", key) ;  // ends on invalid key
  fprintf(stderr,"# of records found = %d, %s\n", count, count == recno + 2*nprocs ? "SUCCESS" : "FAILED") ;

  // look up all new records 
  fprintf(stderr," - targeted key lookup from beginning of file (new records)\n") ;
  mask[0] = 0xFFFFFFFFu ;
  errors = 0 ;
  for(j = 0 ; j < nprocs ; j++){                             // loop over PE ranks
    criteria[0] = RT_DATA | (1 << (8 + j)) ;                 // class 2**j records
    for(i=0 ; i<2 ; i++){                                    // loop over records per rank
      key = 0 ;
      fill_meta(criteria, DIR_META, recno+i, j) ;
      key = RSF_Lookup(h1, key, criteria, mask, DIR_META) ;
      if(key <= 0) errors++ ;
      fprintf(stderr," key = %16.16lx, ML = %d\n", key, DIR_META) ;
    }
  }
  fprintf(stderr,"%s\n", errors == 0 ? "SUCCESS" : "FAILED") ;
  RSF_Close_file(h1) ;
  total_errors = 0 ;
  MPI_Allreduce(&errors, &total_errors, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD) ;
  fprintf(stderr,"Phase 4 TOTAL ERRORS = %d\n", total_errors) ;

END :
  MPI_Finalize() ;
  return(0) ;
FAILED :
  fprintf(stderr,"ERROR(S) in test\n") ;
  goto END ;
ERROR :
  if(my_rank == 0) fprintf(stderr,"usage : %s rsf_file_name number_of_tests creator_pe\n", argv[0]) ;
  goto END ;
}
#endif // TEST7S

#if defined(TEST7)

#include <rsf_int.h>
#include <mpi.h>
#define NDATA 3000
#define NREC 10
#define META_SIZE 6
#define META_DIM 4
#define NFILES 1

int the_test(int argc, char **argv){
  int my_rank, nprocs ;
  RSF_handle h1 ;
  int32_t meta_dim = META_DIM ;
  int64_t segsize = 1024*1024 ;
  uint32_t meta[META_SIZE] ;
//   uint32_t mask[META_SIZE] ;
  int32_t data[NDATA+NREC] ;
  size_t data_size ;
  int i, j, ndata ;
  int64_t slot2, free_space1, free_space2 ;
  int32_t meta0 ;
  uint32_t *metaf, fmeta_size ;
  int64_t put_slot[NREC] ;
  int64_t file_slot[NREC] ;
  char command[1024] ;
  int status ;

  MPI_Init(&argc, &argv) ;
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank) ;
  MPI_Comm_size(MPI_COMM_WORLD, &nprocs) ;
  fprintf(stderr,"PE %d of %d, pid = %d\n", my_rank+1, nprocs, getpid()) ;
  if(argc < 2) goto ERROR ;
  MPI_Barrier(MPI_COMM_WORLD) ;
  h1 = RSF_Open_file(argv[1], RSF_RW, &meta_dim, "DeMo", &segsize);  // open file
//   meta[0] = RT_XDAT ;
//   meta[0] = 127 | (1 << 16) ;
  for(i = 1 ; i < META_SIZE ; i++) meta[i] = 0xFFFFFF0 + i ;
  for(i = 0 ; i < NREC ; i++){
    if(my_rank == 0){           // first PE
      if(i == NREC/2){
        free_space1 = RSF_Available_space(h1) ;
        RSF_Switch_sparse_segment(h1, 0L) ;
        free_space2 = RSF_Available_space(h1) ;
        fprintf(stderr,"DEBUG: switching to a new segment, free space increased from %ld to %ld\n", free_space1, free_space2) ;
      }
    }
    if(my_rank == nprocs -1){    // last PE
      if(i == NREC/2){
        free_space1 = RSF_Available_space(h1) - 32 ;
        status = RSF_Put_null_record(h1, free_space1) ;
        fprintf(stderr,"DEBUG: %s null record of size %ld\n", status ? "wrote" : "failed to write", free_space1) ;
      }
    }
    meta[0] = (( (i & 0x3) + 8) & 0xFF) | (1 << ((i & 0x3) + 8)) ;
    if(i >= 8) meta[0] = RT_XDAT | (1 << (RT_XDAT + 8)) ;
    meta[1] = my_rank ;
    for(j=2 ; j < META_SIZE-1 ; j++) {
      meta[j] = (j << 16) + i + (0xF << 8) ;
    }
    meta[META_SIZE-1] = ( (my_rank +1 ) << 16 ) + nprocs ;
    ndata = NDATA + i ;
    data_size = ndata * sizeof(int32_t) ;
    if(ndata > NDATA+NREC){
      fprintf(stderr,"ERROR: data overflow ndata = %d, max allowed = %d\n", ndata, NDATA+NREC*NFILES);
      exit(1) ;
    }
    for(j=0 ; j < ndata    ; j++) {
      data[j] = j+i ;
    }
    // metadata in vdir shorter by 1 than metadata in record
    put_slot[i] = RSF_Put_bytes(h1, NULL, meta, META_SIZE, (META_SIZE-1), data, data_size, DT_32) ;
    // fprintf(stderr,"PUT %p\n",h1.p);
    if(i == 3) {
      meta0 = meta[0] ;
      meta[0] = RT_FILE ;
      if(argc > 2) {
        file_slot[i] = RSF_Put_file(h1, argv[2], meta, 2) ;
        fprintf(stderr,"DEBUG: adding file '%s', slot = %lx\n", argv[2], file_slot[i]) ;
      }
      meta[0] = meta0 ;
      status = RSF_Put_null_record(h1, 1000l) ;
      fprintf(stderr,"DEBUG: %s null record of size %ld\n", status ? "wrote" : "failed to write", 1000l) ;
    }
    if(i == 5) {
      if(argc > 2) {
        fprintf(stderr,"DEBUG: retrieving file '%s', slot = %lx\n", argv[2], file_slot[3]) ;
        slot2 = RSF_Get_file(h1, file_slot[3], "tagada.txt", &metaf, &fmeta_size) ;
        fprintf(stderr,"DEBUG: slot read/written = %lx/%lx,  %s\n",file_slot[3], slot2, (file_slot[3] == slot2) ? "SUCCESS" : "ERROR") ;
        status = RSF_Put_null_record(h1, 2000l) ;
        fprintf(stderr,"DEBUG: %s null record of size %ld\n", status ? "wrote" : "failed to write", 2000l) ;
      }
    }
  }
  MPI_Barrier(MPI_COMM_WORLD) ;
  RSF_Close_file(h1) ;
  MPI_Barrier(MPI_COMM_WORLD) ;
// goto END ;
  if(my_rank == nprocs -1){
    snprintf(command, sizeof(command), "cp %s %s.bak", argv[1], argv[1]) ;
    system(command) ;
    fprintf(stderr,"====== Fusing segments (last process) ======\n") ;
    free_space1 = RSF_Available_space(h1) ;
    fprintf(stderr,"free space before open = %16.16lx\n", free_space1);
    h1 = RSF_Open_file(argv[1], RSF_RW + RSF_FUSE, &meta_dim, "DeMo", NULL);
//     h1 = RSF_Open_file(argv[1], RSF_RO , &meta_dim, "DeMo", NULL);
    free_space1 = RSF_Available_space(h1) ;
    fprintf(stderr,"free space = %16.16lx, meta_dim = %d\n", free_space1, meta_dim);
    RSF_Dump_vdir(h1) ;
    RSF_Close_file(h1) ;
  }
END :
  MPI_Finalize() ;
  return(0) ;
ERROR :
  if(my_rank == 0) fprintf(stderr,"usage : %s rsf_file\n", argv[0]) ;
  goto END ;
}

#endif // TEST7

#if defined(TEST6)

#include <rsf.h>
#define META_SIZE 6

int the_test(int argc, char **argv){
  RSF_handle h1, h2;
  int32_t meta_dim = META_SIZE ;
  int64_t segsize = 0 ;
  char command[1024] ;

  fprintf(stderr,"Opening h1 as R/O\n");
  h1 = RSF_Open_file(argv[1], RSF_RO, &meta_dim, "DeMo", NULL);
  fprintf(stderr,"Opening h1 as RW\n");
  h1 = RSF_Open_file(argv[1], RSF_RW, &meta_dim, "DeMo", NULL);
  RSF_Dump(argv[1], 0) ;
  RSF_Close_file(h1) ;
  snprintf(command, sizeof(command), "ls -l %s", argv[1]) ;
  system(command) ;
  RSF_Dump(argv[1], 0) ;
  fprintf(stderr,"Reopening h1 as RW\n");
  segsize = 4096 ;
  h1 = RSF_Open_file(argv[1], RSF_RW, &meta_dim, "DeMo", &segsize);
  system(command) ;
  RSF_Dump(argv[1], 0) ;
//   fprintf(stderr,"Opening h2 as RW (same file as h1)\n");
//   h2 = RSF_Open_file(argv[1], RSF_RW, &meta_dim, "DeMo", NULL);
//   RSF_Dump(argv[1], 0) ;
  RSF_Close_file(h1) ;
  fprintf(stderr,"Closed h1 \n");
  system(command) ;
  RSF_Dump(argv[1], 1) ;
  fprintf(stderr,"Fusing segments of h1\n");
  h1 = RSF_Open_file(argv[1], RSF_RW | RSF_FUSE, &meta_dim, "DeMo", NULL);
  RSF_Close_file(h1) ;
  RSF_Dump(argv[1], 1) ;
}

#endif

#if defined(TEST1)

#include <rsf.h>

#define NDATA 10
#define NREC 3
#define META_SIZE 6
#define NFILES 4
int the_test(int argc, char **argv){
//   start_of_record sor = {0, 1, 4, 32767} ;
  int fd ;
  RSF_handle h ;
  int32_t meta_dim = META_SIZE ;
  uint32_t meta[META_SIZE] ;
  uint32_t mask[META_SIZE] ;
  uint32_t criteria[META_SIZE] ;
  int32_t data[NDATA+NREC*NFILES] ;
  size_t data_size ;
  int32_t i, j, k, ndata ;
  char *names[] = { "bad.rsf", "demo1.rsf", "demo2.rsf", "demo3.rsf", NULL };
  int32_t zero = 0 ;
  int64_t key0 ;
  int i0 = 0;
  int j0 ;
  int64_t keys[4096] ;
  uint32_t *dataptr, *metaptr ;
  int32_t meta_size ;
  uint64_t segsize = 0;

  bzero(mask, META_SIZE*sizeof(uint32_t)) ;  // search mask (ignore everything for match purposes)
  for(i=0 ; i<5 ; i++) fprintf(stderr,"%d %p",i, names[i]) ; fprintf(stderr,"\n");

  meta[0]           = 0xCAFEFADEu ;
  meta[META_SIZE-1] = 0xDEABDEEFu ;

  h = RSF_Open_file("tag-ada.rsf", RSF_RW, &zero, "DeMo", NULL);
  if( ! RSF_Valid_handle(h) ) {
    fprintf(stderr,"create with metadata length == 0 failed as expected\n") ;
  }else{
    fprintf(stderr,"ERROR: create with metadata length == 0 did not fail as expected\n") ;
  }

  fprintf(stderr,"=========== RSF version %s test ===========\n",RSF_VERSION_STRING) ;
  fprintf(stderr,"=========== file creation test 1 ===========\n") ;
  for(k=0 ; k<4 ; k++){
//     fprintf(stderr,"before RSF_Open_file\n");
    fprintf(stderr,"before RSF_Open_file, k = %d, p = %s\n", k, names[k]) ;
    h = RSF_Open_file(names[k], 0, &meta_dim, "DeMo", NULL);
    fprintf(stderr,"after RSF_Open_file\n");
    if( ! RSF_Valid_handle(h) ) {
      // open is expected to fail for file bad.rsf (wrong permissions on file)
      fprintf(stderr,"ERROR: open failed for file '%s'\n", names[k]) ;
      continue ;
    }
//     fprintf(stderr,"before put\n");
    for(i = 0 ; i < NREC ; i++){
      fprintf(stderr,"writing record\n");
//       fprintf(stderr,"writing record %d\n",i) ;
      for(j=1 ; j < meta_dim-1 ; j++) {
        meta[j] = (j << 16) + i + (k << 8) ;
      }
      ndata = NDATA + i0 ;
      if(ndata > NDATA+NREC*NFILES){
        fprintf(stderr,"ERROR: data overflow ndata = %d, max allowed = %d\n", ndata, NDATA+NREC*NFILES);
        exit(1) ;
      }
      data_size = ndata * sizeof(data[0]) ;
      for(j=0 ; j < ndata    ; j++) {
        data[j] = j+i0 ;
      }
//       fprintf(stderr," : %9x %9x %9x\n",data[0], data[ndata/2], data[ndata-1]);
      RSF_Put_bytes(h, meta, data, data_size) ; i0++ ;
    }
    fprintf(stderr,"%s created\n",names[k]) ;
    RSF_Dump_dir(h) ;
    RSF_Close_file(h) ;
    RSF_Dump(names[k], 0) ;
  }
  system("cat demo[1-3].rsf >demo0.rsf") ;
  fprintf(stderr,"=========== concatenation test ===========\n") ;
  RSF_Dump("demo0.rsf", 0) ;
// exit(0) ;
  fprintf(stderr,"=========== add records test ===========\n") ;
  meta_dim = 0 ;
  segsize = 65536 ;
//   segsize = 0 ;
  h = RSF_Open_file("demo0.rsf", RSF_RW + RSF_FUSE, &meta_dim, "DeMo", &segsize);
//   h = RSF_Open_file("demo0.rsf", RSF_RW, &meta_dim, "DeMo", &segsize);
  fprintf(stderr,"meta_dim = %d, h.p = %p\n", meta_dim, h.p) ;
  RSF_Dump("demo0.rsf", 0) ;
  fprintf(stderr,"=========== after open ===========\n") ;
  if(h.p == NULL) exit(0) ;

  for(i = 0 ; i < NREC ; i++){
    for(j=1 ; j < meta_dim-1 ; j++) {
      meta[j] = (j << 16) + i + (0xF << 8) ;
    }
    ndata = NDATA + i0 ;
    data_size = ndata * sizeof(int32_t) ;
    if(ndata > NDATA+NREC*NFILES){
      fprintf(stderr,"ERROR: data overflow ndata = %d, max allowed = %d\n", ndata, NDATA+NREC*NFILES);
      exit(1) ;
    }
    for(j=0 ; j < ndata    ; j++) {
      data[j] = j+i0 ;
    }
    RSF_Put_bytes(h, meta, data, data_size) ; fprintf(stderr,"PUT %p\n",h.p);
    i0++ ;
  }
RSF_Dump("demo0.rsf", 0) ;
  fprintf(stderr,"=========== dump memory directory test ===========\n") ;
  RSF_Dump_dir(h) ;
  RSF_Close_file(h) ;
RSF_Dump("demo0.rsf", 0) ;
// exit(0) ;
  fprintf(stderr,"=========== scan test ===========\n") ;
  h = RSF_Open_file("demo0.rsf", RSF_RO, &meta_dim, "DeMo", NULL);
  key0 = 0 ;
  bzero(keys, sizeof(keys)) ;
  i0 = 0 ;
  bzero(mask,sizeof(mask)) ;
  for(j0 = 0 ; j0 < META_SIZE ; j0++) fprintf(stderr,"|%8.8x|",mask[j0]); fprintf(stderr,"\n");
// fprintf(stderr,"calling RSF_Lookup, crit = %8.8x @%p, mask = %8.8x @%p\n", criteria[0], criteria, mask[0], mask);
  key0 = RSF_Lookup(h, key0, &criteria[0], &mask[0]) ;
  while(key0 > 0) {
    keys[i0++] = key0 ;
    key0 = RSF_Lookup(h, key0, &criteria[0], &mask[0]) ;
  }
  for(j0 = 0 ; j0 < i0 ; j0++) fprintf(stderr,"%12.12lx ",keys[j0]) ;
//   for(j0 = 0 ; keys[j0] != 0 ; j0++) fprintf(stderr,"%12.12lx ",keys[j0]) ;
  fprintf(stderr,"\n") ;
  fprintf(stderr,"h = %p\n",h);
// exit(0) ;
  metaptr = NULL ;
  metaptr = (int32_t *) RSF_Get_record_meta(h, keys[0], &meta_size, &data_size) ;
  for(j0 = 0 ; keys[j0] != 0 ; j0++){
//     metaptr = (uint32_t *) RSF_Get_meta(h, keys[j0], &meta_size, &data_size) ;
    metaptr = (uint32_t *) RSF_Get_record_meta(h, keys[j0], &meta_size, &data_size) ;
    fprintf(stderr,"%p ",metaptr);
//     dataptr = RSF_Get(h, keys[j0], NULL, &data_size) ;
  }
  fprintf(stderr,"\n");
  RSF_Close_file(h) ;
// exit(0) ;

  fprintf(stderr,"=========== dump file test ===========\n") ;
  segsize = 32768 ;
  h = RSF_Open_file("demo0.rsf", RSF_RW, &meta_dim, "DeMo", &segsize);
  RSF_Close_file(h) ;
  segsize = 65536 ;
  h = RSF_Open_file("demo0.rsf", RSF_RW, &meta_dim, "DeMo", &segsize);
  RSF_Close_file(h) ;
  segsize = 32768 ;
  h = RSF_Open_file("demo0.rsf", RSF_RW, &meta_dim, "DeMo", &segsize);
  RSF_Close_file(h) ;
  system("ls -l demo0.rsf") ;
  RSF_Dump("demo0.rsf", 0) ;
}
#endif

#if defined(TEST3)

#include <rsf.h>

// basic function test
#define NDATA 10
#define NREC 3
#define META_SIZE 6
int the_test(int argc, char **argv){
//   start_of_record sor = {0, 1, 4, 32767} ;
  int fd ;
  RSF_handle h ;
  int32_t meta_dim = META_SIZE ;
  uint32_t meta[META_SIZE] ;
  uint32_t mask[META_SIZE] ;
  uint32_t criteria[META_SIZE] ;
//   int64_t s ;
  int32_t data[NDATA+NREC] ;
  size_t data_size ;
  int32_t i, j, k, ndata ;
  char *names[] = { "bad.rsf", "demo1.rsf", "demo2.rsf", "demo3.rsf", NULL };
  int32_t zero = 0 ;
  int64_t key0 ;
  int i0 = 0;
  int j0 ;
  int64_t keys[NREC * 16] ;
  uint32_t *dataptr, *metaptr ;
  int32_t meta_size ;

  bzero(mask, META_SIZE*sizeof(uint32_t)) ;  // search mask (ignore everything for match purposes)
  for(i=0 ; i<5 ; i++) fprintf(stderr,"%d %p",i, names[i]) ; fprintf(stderr,"\n");

  meta[0]           = 0xCAFEFADEu ;
  meta[META_SIZE-1] = 0xDEABDEEFu ;

  h = RSF_Open_file("tag-ada.rsf", RSF_RW, &zero, "DeMo", NULL);
  if( ! RSF_Valid_handle(h) ) {
    fprintf(stderr,"create with metadata length == 0 failed as expected\n") ;
  }else{
    fprintf(stderr,"ERROR: create with metadata length == 0 did not fail as expected\n") ;
  }

  fprintf(stderr,"=========== file creation test ===========\n") ;
  for(k=0 ; k<4 ; k++){
    fprintf(stderr,"before RSF_Open_file, k = %d, p = %s\n", k, names[k]) ;
    h = RSF_Open_file(names[k], 0, &meta_dim, "DeMo", NULL);
    if( ! RSF_Valid_handle(h) ) {
      fprintf(stderr,"ERROR: open failed for file '%s'\n", names[k]) ;
      continue ;
    }
  //   fprintf(stderr,"handle = %16p\n", h.p);
    for(i = 0 ; i < NREC ; i++){
      for(j=1 ; j < meta_dim-1 ; j++) {
        meta[j] = (j << 16) + i + (k << 8) ;
      }
      ndata = NDATA + i0 ;
      data_size = ndata * sizeof(data[0]) ;
      for(j=0 ; j < ndata    ; j++) {
        data[j] = j+i0 ;
      }
      RSF_Put_bytes(h, meta, data, data_size) ; i0++ ;
  //     s = RSF_Put_bytes(h, meta, data, data_size) ;
  //     fprintf(stderr,"slot = %16.16lx\n", s);
    }
    RSF_Dump_dir(h) ;
    RSF_Close_file(h) ;
//     fprintf(stderr,"after RSF_Close_file, k = %d\n", k);
//     RSF_Dump(names[k]) ;
  }
exit(0) ;
#if 0
//   bzero(mask, META_SIZE*sizeof(uint32_t)) ;  // search mask (ignore everything for match purposes)
  system("cat demo[1-3].rsf >demo0.rsf") ;
  fprintf(stderr,"=========== concatenation test ===========\n") ;
  RSF_Dump("demo0.rsf") ;
// exit(0) ;
  fprintf(stderr,"=========== add records test ===========\n") ;
  meta_dim = 0 ;
  h = RSF_Open_file("demo0.rsf", RSF_RW, &meta_dim, "DeMo", NULL);
  fprintf(stderr,"meta_dim = %d\n", meta_dim) ;
exit(0) ;
#endif
// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=//
#if 0
  RSF_Dump_dir(h) ;
exit(0) ;
  for(i = 0 ; i < NREC ; i++){
    for(j=1 ; j < meta_dim-1 ; j++) {
      meta[j] = (j << 16) + i + (0xF << 8) ;
    }
    ndata = NDATA + i0 ;
    data_size = ndata * sizeof(data[0]) ;
    for(j=0 ; j < ndata    ; j++) {
      data[j] = j+i0 ;
    }
    RSF_Put_bytes(h, meta, data, data_size) ; i0++ ;
  }
  fprintf(stderr,"=========== dump memory directory test ===========\n") ;
  RSF_Dump_dir(h) ;

  fprintf(stderr,"=========== scan test ===========\n") ;
  key0 = 0 ;
//   fprintf(stderr," <%12.12lx> ",key0) ;
  bzero(keys, sizeof(keys)) ;
//   for(j0 = 0 ; j0 < META_SIZE ; j0++) mask[j0] = 0 ;
//   bzero(mask, META_SIZE*sizeof(uint32_t)) ;  // search mask (ignore everything for match purposes)
  i0 = 0 ;
  for(j0 = 0 ; j0 < META_SIZE ; j0++) fprintf(stderr,"|%8.8x|",mask[j0]); fprintf(stderr,"\n");
fprintf(stderr,"calling RSF_Lookup, crit = %8.8x @%p, mask = %8.8x @%p\n", criteria[0], criteria, mask[0], mask);
  key0 = RSF_Lookup(h, key0, &criteria[0], &mask[0]) ;
//   while(i0 < 2) {
//     keys[i0++] = key0 ;
//     key0 = RSF_Lookup(h, key0, crit, mask) ;
//   }
//   keys[i0++] = key0 ;
  while(key0 > 0) {
    keys[i0++] = key0 ;
    key0 = RSF_Lookup(h, key0, &criteria[0], &mask[0]) ;
  }
  for(j0 = 0 ; j0 < i0 ; j0++) fprintf(stderr,"%12.12lx ",keys[j0]) ;
//   for(j0 = 0 ; keys[j0] != 0 ; j0++) fprintf(stderr,"%12.12lx ",keys[j0]) ;
  fprintf(stderr,"\n") ;
// exit(0) ;
  fprintf(stderr,"h = %p\n",h);
//   metaptr = (int32_t *) RSF_get_meta(h, keys[0], &meta_size, &data_size) ;
  for(j0 = 0 ; keys[j0] != 0 ; j0++){
    metaptr = (uint32_t *) RSF_Get_meta(h, keys[j0], &meta_size, &data_size) ;
//     dataptr = RSF_Get(h, keys[j0], NULL, &data_size) ;
  }
  RSF_Close_file(h) ;
// exit(0) ;

  fprintf(stderr,"=========== dump file test ===========\n") ;
//   RSF_Dump("demo0.rsf") ;
#endif
}
#endif

#if defined(TEST2)

#include <rsf_int.h>

// advisory lock with wait test
#include <mpi.h>
#include <errno.h>

static int32_t Lock(int fd, int lock){
  struct flock file_lock ;
  int status ;

  file_lock.l_type = (lock == 1) ? F_WRLCK : F_UNLCK ;
  file_lock.l_whence = SEEK_SET ;
  file_lock.l_start = 0 ;
  file_lock.l_len = sizeof(start_of_segment) -1 ;
  status = fcntl(fd, F_SETLKW, &file_lock) ;
// fprintf(stderr,"fd = %d, start = %ld, end = %ld, lock = %d, status = %d\n",fd, file_lock.l_start, file_lock.l_len, lock, status) ;
// if(status != 0) perror("LOCK ");
  return status ;
}
int the_test(int argc, char **argv){
  int fd ;
  int rank ;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank) ;
  fd = open("demo.lock", O_RDWR) ;
  if(rank == 0) {
    Lock(fd, 1) ;
    fprintf(stderr,"rank = %d, fd = %d locked\n",rank,fd) ;
    sleep(5) ;
    Lock(fd, 0) ;
    fprintf(stderr,"rank = %d, fd = %d unlocked\n",rank,fd) ;
  }
  if(rank > 0) {
    sleep(1) ;
    Lock(fd, 1) ;
    sleep(1) ;
    fprintf(stderr,"rank = %d, fd = %d locked\n",rank,fd) ;
    sleep(3) ;
    Lock(fd, 0) ;
    fprintf(stderr,"rank = %d, fd = %d unlocked\n",rank,fd) ;
  }
  MPI_Finalize() ;
}
#endif

#if defined(TEST5)

#include <rsf_int.h>
int RSF_Create_empty_file(RSF_File *fp, int32_t meta_dim, const char *appl) ;
// advisory lock with wait test
#include <mpi.h>
#include <errno.h>
#include <stdint.h>

// test that only one process will be able to create an empty RSF file
// mpirun -n N ./test5.exe  new_file_name  # N > 1
// one process should get a status of 0, all the others should get a status of -1
int the_test(int argc, char **argv){
  int fd ;
  int rank ;
  int32_t status ;
  RSF_File fp ;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank) ;
  fd = open(argv[1], O_RDWR | O_CREAT, 0777) ;
  if(fd < 0) {
    fprintf(stderr, "rank %d cannot open '%s'\n",rank, argv[1]) ;
    goto END ;
  }
  RSF_File_init(&fp) ;            // initialize structure
  fp.fd = fd ;                    // fd will be the only useful member for this test
  MPI_Barrier(MPI_COMM_WORLD) ;   // maximize the probability of a race condition
  status = RSF_Create_empty_file(&fp, 6, "abcd") ;
  fprintf(stderr, "rank %d, RSF_Create_empty_file '%s', status = %d\n", rank, argv[1], status) ;
  close(fd) ;
  MPI_Barrier(MPI_COMM_WORLD) ;   // wait to make sure file has been created
  if(rank ==  0){
    RSF_Dump(argv[1], 0) ;
  }
END :
  MPI_Finalize() ;
}
#endif

#if defined(TEST4)
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <mpi.h>

int the_test(int argc, char **argv){
  int my_rank, nprocs ;
  int fd , i ;
  uint32_t buffer[1024*1024] ;
  off_t pos ;
  ssize_t nc = 0 ;
  int nrecords ;
  double t0, t1 ;

  MPI_Init(&argc, &argv) ;
  system("hostname");
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank) ;
  MPI_Comm_size(MPI_COMM_WORLD, &nprocs) ;
  for(i=0 ; i<sizeof(buffer)/4 ; i++) buffer[i] = i*3 ;
  MPI_Barrier(MPI_COMM_WORLD) ;
  t0 = MPI_Wtime() ;
  nrecords = atoi(argv[2]) ;
  fd = open(argv[1], O_RDWR+O_CREAT, 0777) ;
  if(fd > 0) {
   pos = my_rank * sizeof(buffer) * nrecords ;
   lseek(fd, pos, SEEK_SET) ;
   for(i=0 ; i<nrecords-1 ; i++){
     nc += write(fd, buffer, sizeof(buffer)) ;
   }
   close(fd) ;
  }
  MPI_Barrier(MPI_COMM_WORLD) ;
  t1 = MPI_Wtime() ;
  printf("process %d of %d, nc = %ld MB at offset %ld MB, records = %d\n", my_rank+1, nprocs, nc/1024/1024, pos/1024/1024, nrecords-1) ;
  if(my_rank == 0) printf("TIME = %f\n", t1-t0) ;
  MPI_Finalize() ;
}

#endif

int main(int argc, char **argv){
  return the_test(argc, argv) ;
}

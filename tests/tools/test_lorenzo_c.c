#include <stdio.h>
#include <stdint.h>
#include <sys/time.h>

#include <rmn/timers.h>
#include <rmn/test_helpers.h>

#if !defined(NPTS)
#define NPTS 67
#endif

#define NTIMES 100000

#include <rmn/lorenzo.h>

// NOTE: the C test is not as exhaustive as the Fortran test, the in place mode is not tested

int main(int argc, char **argv){
  (void)(argc) ;
  (void)(argv) ;
  int32_t data[NPTS+1][NPTS] ;
  int32_t data0[NPTS+1][NPTS] ;
  int32_t data2[NPTS+1][NPTS] ;
  int32_t pred[NPTS+1][NPTS] ;
  int i, j, errors ;
  uint64_t tmin, tmax, freq ;
  double nano, tavg ;
  char buf[1024] ;
  size_t bufsiz = sizeof(buf) ;

  start_of_test("lorenzo (C)");
  freq = cycles_counter_freq() ;
  nano = 1000000000.0 ;
  fprintf(stderr, "nano = %8.2G\n", nano) ;
  nano = nano / freq ;
  fprintf(stderr, "nano = %8.2G\n", nano) ;

  fprintf(stderr, "LorenzoPredict (%dx%d) theoretical test (i+j)\n", NPTS-5, NPTS-3) ;
  for(j=0 ; j<NPTS ; j++){
    for(i=0 ; i<NPTS ; i++){
      data[j][i]  = i + j + 1000 ;
    }
  }
  LorenzoPredict(&data[0][0], &pred[0][0], NPTS-5, NPTS, NPTS, NPTS-3) ;
  errors = 0 ;
  if(pred[0][0] != data[0][0]) errors++ ;  // first value
  for(j=1 ; j<NPTS-3 ; j++){               // first column except first value
    if(pred[j][0] != 1) errors++ ;         // should predict to 1
  }
  for(i=1 ; i<NPTS-5 ; i++){               // first row except first value
    if(pred[0][i] != 1) errors++ ;         // should predict to 1
  }
  for(j=1 ; j<NPTS-3 ; j++){               // inner values, excluding first row and first column
    for(i=1 ; i<NPTS-5 ; i++){
      if(pred[j][i] != 0) errors++ ;       // should predict to 0
    }
  }
  fprintf(stderr, "LorenzoPredict    : errors = %d\n\n",errors);
  if(errors > 0) goto fail ;

  for(j=0 ; j<NPTS+1 ; j++){
    for(i=0 ; i<NPTS ; i++){
      data[j][i]  = (2*i + 3*j + 5) ;
      data0[j][i] = data[j][i] ;
      data2[j][i] = 999999 ;
      pred[j][i]  = 999999 ;
    }
  }

  fprintf(stderr, "LorenzoPredict (%dx%d) not in place\n", NPTS, NPTS) ;
  LorenzoPredict(&data[0][0], &pred[0][0], NPTS, NPTS, NPTS, NPTS) ;
  LorenzoUnpredict(&data2[0][0], &pred[0][0], NPTS, NPTS, NPTS, NPTS) ;
  errors = 0 ;
  for(j=0 ; j<NPTS ; j++){              // check restored data vs original
    for(i=0 ; i<NPTS ; i++){
      if(data2[j][i] != data0[j][i]) errors++;
    }
  }
  fprintf(stderr, "LorenzoUnpredict    : errors = %d\n\n",errors);
  if(errors > 0) goto fail ;
  fprintf(stderr, "SUCCESS\n") ;

  fprintf(stderr, "LorenzoPredict (%dx%d) in place\n", NPTS, NPTS) ;
  LorenzoPredict(&data[0][0], &data[0][0], NPTS, NPTS, NPTS, NPTS) ;
  errors = 0 ;
  for(j=0 ; j<NPTS ; j++){              // check predicted data using previous step prediction
    for(i=0 ; i<NPTS ; i++){
      if(data[j][i] != pred[j][i]) errors++ ;
    }
  }
  fprintf(stderr, "LorenzoPredict    : errors = %d\n\n",errors);
  if(errors > 0) goto fail ;
  LorenzoUnpredict(&data[0][0], &data[0][0], NPTS, NPTS, NPTS, NPTS) ;
  errors = 0 ;
  for(j=0 ; j<NPTS ; j++){              // check restored data vs original
    for(i=0 ; i<NPTS ; i++){
      if(data[j][i] != data0[j][i]) errors++;
    }
  }
  fprintf(stderr, "LorenzoUnpredict    : errors = %d\n\n",errors);
  if(errors > 0) goto fail ;
  fprintf(stderr, "SUCCESS\n") ;

  fprintf(stderr, "LorenzoPredict (%dx%d) not in place\n", 7, 5) ;
  for(j=0 ; j<NPTS+1 ; j++){
    for(i=0 ; i<NPTS ; i++){
      pred[j][i]  = 999999 ;
    }
  }
  LorenzoPredict(&data[0][0], &pred[0][0], 7, NPTS, NPTS, 5) ;
  LorenzoUnpredict(&data[0][0], &pred[0][0], 7, NPTS, NPTS, 5) ;
  errors = 0 ;
  for(j=0 ; j<NPTS ; j++){              // check restored data vs original
    for(i=0 ; i<NPTS ; i++){
      if(data[j][i] != data0[j][i]) errors++;
    }
  }
  fprintf(stderr, "LorenzoUnpredict_   : errors = %d\n\n",errors);
  if(errors > 0) goto fail ;

  fprintf(stderr, "LorenzoPredict (%dx%d) in place\n", 7, 5) ;
  LorenzoPredict(&data[0][0], &data[0][0], 7, NPTS, NPTS, 5) ;
  errors = 0 ;
  for(j=0 ; j<5 ; j++){                 // check predicted data using previous step prediction
    for(i=0 ; i<7 ; i++){
      if(data[j][i] != pred[j][i]) errors++ ;
    }
  }
  for(j=0 ; j<NPTS ; j++){              // re check predicted data
    for(i=0 ; i<NPTS ; i++){            // points out of 7x5 area should be equal to original data
      if(data[j][i] != pred[j][i] && data[j][i] != data0[j][i]) errors++ ;
    }
  }
  fprintf(stderr, "LorenzoPredict    : errors = %d\n\n",errors);
  if(errors > 0) goto fail ;
  LorenzoUnpredict(&data[0][0], &data[0][0], 7, NPTS, NPTS, 5) ;
  errors = 0 ;
  for(j=0 ; j<NPTS ; j++){
    for(i=0 ; i<NPTS ; i++){
      if(data[j][i] != data0[j][i]) errors++;
    }
  }
  fprintf(stderr, "LorenzoUnpredict_   : errors = %d\n\n",errors);
  if(errors > 0) goto fail ;

  fprintf(stderr, "SUCCESS\n") ;

  fprintf(stderr, "=============================== timing tests ===============================\n") ;

  TIME_LOOP(tmin, tmax, tavg, NTIMES, (NPTS*NPTS), buf, bufsiz, LorenzoPredict(&data[0][0], &pred[0][0], NPTS, NPTS, NPTS, NPTS) )
  fprintf(stderr, "LorenzoPredict    : %s\n",buf);

  TIME_LOOP(tmin, tmax, tavg, NTIMES, (NPTS*NPTS), buf, bufsiz, LorenzoUnpredict(&data[0][0], &pred[0][0], NPTS, NPTS, NPTS, NPTS) )
  fprintf(stderr, "LorenzoUnpredict  : %s\n",buf);

  TIME_LOOP(tmin, tmax, tavg, NTIMES, (NPTS*NPTS), buf, bufsiz, LorenzoUnpredict(&data[0][0], &pred[0][0], NPTS, NPTS, NPTS, NPTS) )
  fprintf(stderr, "LorenzoUnpredict_ : %s\n",buf);

  if(tmax == 0) fprintf(stderr, "tmax == 0, should not happen\n") ;

  return 0 ;

fail:
  fprintf(stderr, "FAIL\n") ;
  return 1 ;
}

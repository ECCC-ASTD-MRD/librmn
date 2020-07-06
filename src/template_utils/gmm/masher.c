// See the link bellow for the explanation of why this is necessary
// https://stackoverflow.com/questions/39679689/concatenate-strings-in-a-macro-using-gfortran
#ifdef __GFORTRAN__
#define PASTER3(x,y,z) x/**/y/**/z
#else
#define PASTER3(x,y,z) x##y##z
#endif

#define EVALUATOR3(x,y,z) PASTER3(x,y,z)
#define EXTENSION EVALUATOR3(DATACODE, DATALENGTH, DIM)
#define FNCNAME(name) EVALUATOR3(name , _, EXTENSION)

#include <rmn/c_ccard.h>
#include <stdio.h>
#include <string.h>

#define GLOBAL_VAL
#ifdef GLOBAL_VAL
char val[8][CCARD_NCARMAX];
#endif
int test_with_equal(){
    fprintf(stderr, "%s()\n", __func__);
    const char *argv[] = { "myprog", "-key1","=value1", NULL, };
    int argc = 3; // sizeof(argv)/sizeof(argv[0]);

    const char *keyNames[] = { "key1." };
    const int nbKeys = 1; //sizeof(keyNames)/sizeof(keyNames[0]);

    const char *def[] = { "key1_default" };

    int npos = 0;

#ifndef GLOBAL_VAL
    char val[1][CCARD_NCARMAX];
#endif

    c_ccard(argv, argc, keyNames, val, def, nbKeys, &npos);
    fprintf(stderr, "val[0] = '%s'\n", val[0]);

    return strcmp(val[0], "value1");
}

int test_with_space(){
    fprintf(stderr, "%s()\n", __func__);
    const char *argv[] = { "myprog", "-key1", "value1", NULL, };
    int argc = 3; // sizeof(argv)/sizeof(argv[0]);

    const char *keyNames[] = { "key1." };
    const int nbKeys = 1; //sizeof(keyNames)/sizeof(keyNames[0]);

    const char *def[] = { "key1_default" };

#ifndef GLOBAL_VAL
    char val[1][CCARD_NCARMAX];
#endif
    int npos = 0;

    c_ccard(argv, argc, keyNames, val, def, nbKeys, &npos);
    fprintf(stderr, "val[0] = '%s'\n", val[0]);

    return strcmp(val[0], "value1");
}
int main(void){

#define EQUAL
#define SPACE
#if defined(EQUAL)
    if(test_with_equal() != 0){
        fprintf(stderr, "test_with_equal failed\n");
    } else {
        fprintf(stderr, "test_with_equal SUCCESS\n");
    }
#endif

#if defined(SPACE)
#ifdef GLOBAL_VAL
    strncpy(val[0], "", CCARD_NCARMAX);
#endif
    if(test_with_space() != 0){
        fprintf(stderr, "test_with_space failed\n");
        return 1;
    } else {
        fprintf(stderr, "test_with_space SUCCESS\n");
    }
#endif
}



//! \file version.c Version information for the project

#include <stdio.h>

#include <rmn/version.h>

char PROJECT_VERSION[] = VERSION;

//! EC_ARCH environment variable value when this was compiled
char CAT(PROJECT_NAME,_ec_arch)[] = EC_ARCH;


void CAT(print_,PROJECT_VERSION)() {
    printf("%s version: %s\n", PROJECT_NAME_STRING, PROJECT_VERSION);
}

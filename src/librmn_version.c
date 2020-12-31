//! This program prints version information on the standard output.

#include <stdio.h>

int main(int argc, char **argv) {
    //! \todo These 2 definitions should be in the library top-level header file
    extern char librmn_version[];
    extern char librmn_ec_arch[];

    printf("Version: %s\n", librmn_version);
    printf("EC_ARCH: %s\n", librmn_ec_arch);

    return 0;
};
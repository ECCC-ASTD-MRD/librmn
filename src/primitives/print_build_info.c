#include <stdio.h>
#include <rmn_build_info.h>
#include "rmn/primitives.h"

// These symbols aren't static in order to make them accessible
// Therefore in order to prevent collisions, the are prefixed with the project's name
char rmn_project_name[] = PROJECT_NAME_STRING;
char rmn_project_version[] = PROJECT_VERSION_STRING;
char rmn_project_description[] = PROJECT_DESCRIPTION_STRING;
char rmn_version[] = VERSION;
char rmn_git_commit[] = GIT_COMMIT;
char rmn_git_commit_timestamp[] = GIT_COMMIT_TIMESTAMP;
char rmn_git_status[] = GIT_STATUS;
char rmn_ec_arch[] = EC_ARCH;
char rmn_build_user[] = BUILD_USER;
char rmn_build_timestamp[] = BUILD_TIMESTAMP;
char rmn_c_compiler_id[] = C_COMPILER_ID;
char rmn_c_compiler_version[] = C_COMPILER_VERSION;
char rmn_cxx_compiler_id[] = CXX_COMPILER_ID;
char rmn_cxx_compiler_version[] = CXX_COMPILER_VERSION;
char rmn_fortran_compiler_id[] = FORTRAN_COMPILER_ID;
char rmn_fortran_compiler_version[] = FORTRAN_COMPILER_VERSION;

//! Print build information
void rmn_print_build_info(void) {
    printf("Name: %s\n", rmn_project_name);
    printf("Version: %s\n", rmn_version);
    printf("Description: %s\n", rmn_project_description);
    printf("Git commit: %s\n", rmn_git_commit);
    printf("Git commit timestamp: %s\n", rmn_git_commit_timestamp);
    printf("EC_ARCH: %s\n", rmn_ec_arch);
    printf("Build user: %s\n", rmn_build_user);
    printf("Build timestamp: %s\n", rmn_build_timestamp);
    printf("C compiler id: %s\n", rmn_c_compiler_id);
    printf("C compiler version: %s\n", rmn_c_compiler_version);
    printf("CXX compiler id: %s\n", rmn_cxx_compiler_id);
    printf("CXX compiler version: %s\n", rmn_cxx_compiler_version);
    printf("Fortran compiler id: %s\n", rmn_fortran_compiler_id);
    printf("Fortran compiler version: %s\n", rmn_fortran_compiler_version);
}

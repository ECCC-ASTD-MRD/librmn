#define PASTE(a,b) a##b
#define CAT(a,b) PASTE(a,b)

//! Project version
//!
//! This is defined with the PROJECT_NAME and VERSION macros that are defined
//! by the git_version CMake module in cmake_shared.  This ensures that the
//! version information is taken from the version management system.
//!
//! The symbol name is built by concatenating the CMake PROJECT_NAME variable
//! with "-version".
char CAT(PROJECT_NAME,_version)[] = VERSION;


#ifndef EC_ARCH
#define EC_ARCH "Undefined"
#endif

//! EC_ARCH environment variable value when this was compiled
char CAT(PROJECT_NAME,_ec_arch)[] = EC_ARCH;
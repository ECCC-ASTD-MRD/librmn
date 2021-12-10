#ifndef VERSION_H
#define VERSION_H

#include <cat.h>
#include <librmn_build_info.h>

//! Project version
//!
//! This is defined with the PROJECT_NAME and VERSION macros that are defined
//! by the git_version CMake module in cmake_shared.  This ensures that the
//! version information is taken from the version management system.
//!
//! The symbol name is built by concatenating the CMake PROJECT_NAME variable
//! with "_version".
#define PROJECT_VERSION CAT(PROJECT_NAME,_version)

#ifndef EC_ARCH
#define EC_ARCH "Undefined"
#endif

void CAT(print_,PROJECT_VERSION)();

#endif

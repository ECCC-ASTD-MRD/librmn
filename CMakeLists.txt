include("${CMAKE_SOURCE_DIR}/cmake_shared/base.cmake")

# Given our wacky compiler setups, languages must be activated after the
# compieler has been configured
project(librmn VERSION ${VERSION} DESCRIPTION "Library of functions for numerical weather forecasting" LANGUAGES NONE)

option(COMPILER "Compiler collection to use for the build.  Can be one of the following: gnu, intel" "gnu")

# TODO: Configure the compiler
include("${CMAKE_SOURCE_DIR}/cmake_shared/compiler.cmake")

enable_language(C)
enable_language(Fortran)

# TODO: Set compiler flags based on the OS/Distro and requested compiler

# CMAKE_BUILD_TYPE can be one of Debug, Release, RelWithDebInfo, MinSizeRel
set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Build type (Debug, Release, RelWithDebInfo, MinSizeRel)")
message(STATUS "Configuring for ${CMAKE_BUILD_TYPE}")

set(CMAKE_VERBOSE_MAKEFILE TRUE)

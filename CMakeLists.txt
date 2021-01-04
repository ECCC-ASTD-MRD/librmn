# This must be repeated in each CMake file; having it in an included file is not
# sufficient
cmake_minimum_required(VERSION 3.16)

include("${CMAKE_SOURCE_DIR}/cmake_shared/git_version.cmake")

# Given our wacky compiler setups, languages must be activated after the
# compieler has been configured
project(librmn VERSION ${VERSION} DESCRIPTION "Library of functions for numerical weather forecasting" LANGUAGES NONE)

option(BUILD_SHARED_LIBS "Build shared libraries instead of static ones." FALSE)

# CMAKE_BUILD_TYPE can be one of Debug, Release, RelWithDebInfo, MinSizeRel
set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Build type (Debug, Release, RelWithDebInfo, MinSizeRel)")

# enable_language can not be called here since the compiler must be configured
# first.  We therefore make a list of languages that will be enabled by the
# compiler_presets module
set(LANGUAGES C Fortran)

include("${CMAKE_SOURCE_DIR}/cmake_shared/compiler_presets.cmake")
include("${CMAKE_SOURCE_DIR}/cmake_shared/OpenMP.cmake")
include("${CMAKE_SOURCE_DIR}/cmake_shared/doxygen.cmake")

set(CMAKE_VERBOSE_MAKEFILE TRUE)

# Figure out the correct ach_dependant include path
set(BUILD_ARCH "${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")
message(STATUS "Build architecture: ${BUILD_ARCH}")

include_directories(
   include
   include/${BUILD_ARCH}-${COMPILER_SUITE}
   src/PUBLIC_INCLUDES
)

file(GLOB PROJECT_C_FILES
   src/*/*.c
)
file(GLOB PROJECT_F_FILES
   src/*/*.F90
   src/*/*.F
   src/*/*.f
   src/*/*.f90
)
file(GLOB PROJECT_INCLUDE_FILES src/PUBLIC_INCLUDES/*)

add_library(rmn ${PROJECT_C_FILES} ${PROJECT_F_FILES})
add_executable(librmn_version src/librmn_version.c)
add_dependencies(librmn_version rmn)
target_link_libraries(librmn_version rmn)

set_target_properties(rmn PROPERTIES VERSION ${PROJECT_VERSION})
set_target_properties(rmn PROPERTIES PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}")

install(TARGETS rmn librmn_version)

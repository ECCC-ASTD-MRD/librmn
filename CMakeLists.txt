# This must be repeated in each CMake file; having it in an included file is not
# sufficient
cmake_minimum_required(VERSION 3.16)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake_rpn)
include(ec_init)
ec_git_version()

# Given our wacky compiler setups, languages must be activated after the
# compieler has been configured
project(librmn DESCRIPTION "Library of functions for numerical weather forecasting")
set(PROJECT_VERSION ${GIT_VERSION})

option(BUILD_SHARED_LIBS "Build shared libraries instead of static ones." FALSE)

# CMAKE_BUILD_TYPE can be one of Debug, Release, RelWithDebInfo, MinSizeRel
set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Build type (Debug, Release, RelWithDebInfo, MinSizeRel)")

enable_language(C Fortran)
include(ec_compiler_presets)

set(WITH_OPENMP TRUE CACHE BOOL "Control whether to use OpenMP")
include(OpenMP)
include(ec_doxygen)

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

ec_build_info()

add_library(rmn ${PROJECT_C_FILES} ${PROJECT_F_FILES})
add_dependencies(rmn build_info)
add_executable(librmn_version src/librmn_version.c)
add_dependencies(librmn_version rmn)
target_link_libraries(librmn_version rmn)


# This really isn't the ideal solution; the compiler should be invoqued with all
# the definitions and options as if we it was processing a source file

# We need to creat a file here because gfortran won't process a file nammmed *.hf
file(GLOB GMM_HEADER_DEPS ${CMAKE_SOURCE_DIR}/src/gmm/*.inc ${CMAKE_SOURCE_DIR}/src/gmm/*.hf)
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/gmm_header.F90 "#include <gmm.hf>\n")
add_custom_command(
    TARGET rmn
    POST_BUILD
    COMMAND ${CMAKE_Fortran_COMPILER} -I${CMAKE_SOURCE_DIR}/src/gmm -P -E ${CMAKE_CURRENT_BINARY_DIR}/gmm_header.F90 > ${CMAKE_CURRENT_BINARY_DIR}/gmm.hf
    COMMENT "Generating gmm.hf"
    MAIN_DEPENDENCY ${CMAKE_SOURCE_DIR}/src/gmm/gmm.hf
    DEPENDS ${GMM_HEADER_DEPS}
    BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/gmm.hf
    USES_TERMINAL
)

file(GLOB WHITEBOARD_HEADER_DEPS ${CMAKE_SOURCE_DIR}/src/whiteboard/*.tmpl90 ${CMAKE_SOURCE_DIR}/src/whiteboard/*.hf)
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/whiteboard_header.F90 "#include <WhiteBoard.hf>\n")
add_custom_command(
    TARGET rmn
    POST_BUILD
    COMMAND ${CMAKE_Fortran_COMPILER} -I${CMAKE_SOURCE_DIR}/include -I${CMAKE_SOURCE_DIR}/src/whiteboard -P -E ${CMAKE_CURRENT_BINARY_DIR}/whiteboard_header.F90 > ${CMAKE_CURRENT_BINARY_DIR}/WhiteBoard.hf
    COMMENT "Generating WhiteBoard.hf"
    MAIN_DEPENDENCY ${CMAKE_SOURCE_DIR}/src/whiteboard/WhiteBoard.hf
    DEPENDS ${WHITEBOARD_HEADER_DEPS}
    BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/WhiteBoard.hf
    USES_TERMINAL
)

list(APPEND PROJECT_INCLUDE_FILES ${CMAKE_CURRENT_BINARY_DIR}/gmm.hf ${CMAKE_CURRENT_BINARY_DIR}/WhiteBoard.hf)

set_target_properties(rmn PROPERTIES VERSION ${GIT_VERSION})
set_target_properties(rmn PROPERTIES PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}")

install(TARGETS rmn librmn_version)

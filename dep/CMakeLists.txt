cmake_minimum_required(VERSION 3.16)

message(STATUS "(EC) Generating external dependencies")

project(ExternalDependencies NONE)

include(ExternalProject)

set_property(GLOBAL PROPERTY ALLOW_DUPLICATE_CUSTOM_TARGETS 1)
add_subdirectory(json-c json-c)
add_subdirectory(udunits2 udunits2)

#option(BUILD_JSON "Build json-c dependency" ON)
#if(BUILD_JSON)
#    ExternalProject_Add(json-c
#    GIT_REPOSITORY    https://github.com/json-c/json-c
#    GIT_TAG           master
#    SOURCE_DIR        "${CMAKE_BINARY_DIR}/dep/json-c"
#    BINARY_DIR        "${CMAKE_BINARY_DIR}/build"
#    CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX})
#endif()
#ExternalProject_Get_Property(json-c install_dir)

#option(BUILD_UDUNITS2 "Build udunits2 dependency" ON)
#if(BUILD_UDUNITS2)
#    ExternalProject_Add(udunits2
#    GIT_REPOSITORY    git@gitlab.science.gc.ca:RPN-SI/udunits2
#    GIT_TAG           master
#    SOURCE_DIR        "${CMAKE_BINARY_DIR}/udunits2"
#    BINARY_DIR        "${CMAKE_BINARY_DIR}/build"
#    CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX})
#endif()

# This must be repeated in each CMake file; having it in an included file is not
# sufficient
cmake_minimum_required(VERSION 3.16)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake_rpn)
# Initialise compilers and ec specific functions
include(ec_init)
# Get version from git
ec_git_version()
# Parse MANIFEST file
ec_parse_manifest()

# Given our wacky compiler setups, languages must be activated after the
# compieler has been configured
project(${NAME} DESCRIPTION "${DESCRIPTION}")
set(PROJECT_VERSION ${VERSION}${STATE})
set(RMN_VERSION ${PROJECT_VERSION} CACHE INTERNAL "RMN version" FORCE) # Needed for cascaded version identification

enable_language(C Fortran)
include(ec_compiler_presets)

#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -diag-disable=6916")

set(WITH_OPENMP TRUE CACHE BOOL "Control whether to use OpenMP")
include(ec_openmp)
include(ec_doxygen)

# This is required when using GNU compilers without OpenMP.
# It has no adverse effect with other compilers and/or when OpenMP is enabled.
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -ldl -pthread")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ldl -pthread")
set(CMAKE_EXE_LINKER_FLAGS_INIT "${CMAKE_EXE_LINKER_FLAGS_INIT} -ldl -pthread")

set(CMAKE_SKIP_RPATH TRUE)
set(CMAKE_VERBOSE_MAKEFILE TRUE)
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_EXTENSIONS OFF)

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

# Generate build include file (optional)
ec_build_info()

add_library(rmn STATIC ${PROJECT_C_FILES} ${PROJECT_F_FILES})
add_library(rmnshared SHARED ${PROJECT_C_FILES} ${PROJECT_F_FILES})
add_dependencies(rmn rmn_build_info)
add_dependencies(rmnshared rmn rmn_build_info)

#TODO: Could get rid of this binary a it's availble through rmn_config
add_executable(librmn_version src/librmn_version.c)
add_dependencies(librmn_version rmn rmn_build_info)
target_link_libraries(librmn_version rmn)

# This really isn't the ideal solution; the compiler should be invoqued with all
# the definitions and options as if we it was processing a source file

# We need to create a file here because gfortran won't process a file nammmed *.hf
file(GLOB GMM_HEADER_DEPS ${CMAKE_CURRENT_SOURCE_DIR}/src/gmm/*.inc ${CMAKE_CURRENT_SOURCE_DIR}/src/gmm/*.hf)
add_custom_command(
    TARGET rmn
    PRE_BUILD
    COMMAND sed -e "'s/ \"/ </g'" -e "'s/\"/>/g'" ${CMAKE_CURRENT_SOURCE_DIR}/src/gmm/gmm.hf > ${CMAKE_CURRENT_BINARY_DIR}/gmm_header.F90
    COMMAND ${CMAKE_Fortran_COMPILER} -I${CMAKE_CURRENT_SOURCE_DIR}/src/gmm -P -E ${CMAKE_CURRENT_BINARY_DIR}/gmm_header.F90 > ${CMAKE_CURRENT_BINARY_DIR}/gmm.hf
    COMMENT "Generating gmm.hf"
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/src/gmm/gmm.hf
    DEPENDS ${GMM_HEADER_DEPS}
    BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/gmm.hf
    USES_TERMINAL
)

file(GLOB WHITEBOARD_HEADER_DEPS ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/*.tmpl90 ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/*.hf)

# Commands to generate the compiled WhiteBoard.hf in the working directory
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/WhiteBoard.hf
    PRE_BUILD
    COMMAND sed -e "'s/ \"/ </g'" -e "'s/\"/>/g'" ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/WhiteBoard.hf > ${CMAKE_CURRENT_BINARY_DIR}/whiteboard_header.F90
    COMMAND rm -f ${CMAKE_CURRENT_BINARY_DIR}/WhiteBoard.hf
    COMMAND cp -av ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/WhiteBoard_constants.h ${CMAKE_CURRENT_BINARY_DIR}/WhiteBoard.hf
    COMMAND ${CMAKE_Fortran_COMPILER} -I${CMAKE_CURRENT_SOURCE_DIR}/include -I${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard -P -E ${CMAKE_CURRENT_BINARY_DIR}/whiteboard_header.F90 >> ${CMAKE_CURRENT_BINARY_DIR}/WhiteBoard.hf
    COMMENT "Generating WhiteBoard.hf"
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/WhiteBoard.hf
    DEPENDS ${WHITEBOARD_HEADER_DEPS}
    BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/WhiteBoard.hf
    USES_TERMINAL
)

# Add a custom target to regenerate WhiteBoard.hf
add_custom_target(make_WhiteBoard DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/WhiteBoard.hf)

# Add the WhiteBoard-generating target to rmn, ensuring WhiteBoard.hf is kept up to date
add_dependencies(rmn make_WhiteBoard)

list(APPEND PROJECT_INCLUDE_FILES
    ${CMAKE_CURRENT_BINARY_DIR}/gmm.hf
    ${CMAKE_CURRENT_BINARY_DIR}/WhiteBoard.hf
)

list(APPEND PROJECT_INCLUDE_FILES
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}_build_info.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/rpnmacros.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/rpnmacros_global.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/ftn2c_helper.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/${BUILD_ARCH}-${COMPILER_SUITE}/rpn_macros_arch.h
    ${CMAKE_CURRENT_SOURCE_DIR}/include/${BUILD_ARCH}-${COMPILER_SUITE}/arch_specific.hf
)
# Modules Fortran qui devraient être publiés comme des includes
#   jar_module

string(REGEX MATCH "([0-9]+)" SOVERSION ${PROJECT_VERSION})

set_target_properties(rmn PROPERTIES
    VERSION ${PROJECT_VERSION}
    PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
    POSITION_INDEPENDENT_CODE ON
    OUTPUT_NAME rmn)
set_target_properties(rmnshared PROPERTIES
    VERSION ${PROJECT_VERSION}
    PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
    POSITION_INDEPENDENT_CODE ON
    SOVERSION ${SOVERSION}
    OUTPUT_NAME rmn)

install(TARGETS rmn rmnshared librmn_version)

add_custom_target(
    cprotos
    ${CMAKE_CURRENT_SOURCE_DIR}/extractProtos.bash ${CMAKE_CURRENT_SOURCE_DIR}/src ${PROJECT_C_FILES}
    #WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Extract function prototypes from C source code"
)

# Generate the config file for the project to be usable via cmake's find_package command
set(INCLUDE_INSTALL_DIR include)
set(LIB_INSTALL_DIR lib)
set(CONFIG_INSTALL_DIR "${LIB_INSTALL_DIR}/cmake/${PROJECT_NAME}-${PROJECT_VERSION}")

include(CMakePackageConfigHelpers)
configure_package_config_file(
    "Config.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
    INSTALL_DESTINATION "${CONFIG_INSTALL_DIR}"
    PATH_VARS INCLUDE_INSTALL_DIR LIB_INSTALL_DIR
)
write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
    COMPATIBILITY SameMajorVersion
)
install(
    FILES
        "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
        "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
    DESTINATION "${CONFIG_INSTALL_DIR}"
)

# Packaging
# Define package prefix
ec_install_prefix(${NAME} ${PROJECT_VERSION} PACKAGE_PREFIX)
# Create build configuration script
ec_build_config()
# Prepare ssm packaging files
ec_prepare_ssm()

set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGE_VENDOR "ECCC")
set(CPACK_PACKAGE_CONTACT "${MAINTAINER}")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_OUTPUT_FILE_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/package")
set(CPACK_PACKAGE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set(CPACK_PACKAGE_FILE_NAME "${PACKAGE_PREFIX}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "${NAME}_${PROJECT_VERSION}")
include(CPack)

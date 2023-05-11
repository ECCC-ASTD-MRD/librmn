# The minimum version can not be 3.16 because of the LINK_LANGUAGE generator expression in Config.cmake.in
cmake_minimum_required(VERSION 3.20)

message(STATUS "(EC) Generating librmn Makefile")

# Append EC specific module path
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake_rpn/modules $ENV{EC_CMAKE_MODULE_PATH})

include(ec_init)           # Initialise compilers and ec specific functions
ec_git_version()           # Get version from git state
ec_parse_manifest()        # Parse MANIFEST file

project(${NAME} DESCRIPTION "${DESCRIPTION}" LANGUAGES C Fortran)
set(PROJECT_VERSION ${VERSION}${STATE})
set(rmn_VERSION ${PROJECT_VERSION} CACHE INTERNAL "rmn version" FORCE) # Needed for cascaded version identification
message(STATUS "(EC) ${PROJECT_NAME} version = ${PROJECT_VERSION}")
string(REGEX MATCH "([0-9]+)" SOVERSION ${PROJECT_VERSION})

ec_build_info()            # Generate build information

include(ec_compiler_presets)

# Need to add HAVE_RMN definition for auto-registering librmn within App
add_compile_definitions(HAVE_RMN=\"${PROJECT_VERSION}\")
add_subdirectory(App App)

option(WITH_OMPI "Compile with OpenMP/MPI support" TRUE) # Enable with(-DWITH_OMPI=TRUE)
if (WITH_OMPI)
   find_package(MPI REQUIRED)
   find_package(OpenMP REQUIRED)
endif()

include(ec_doxygen)         # Doxygen target doc

# Fortran compiler options required to build this project
set(Intel_FFLAGS -convert big_endian -align array32byte -assume byterecl)
set(GNU_FFLAGS -fconvert=big-endian -fcray-pointer -frecord-marker=4 -fno-second-underscore)
set(Flang_FFLAGS -Mbyteswapio)
set(PGI_FFLAGS -byteswapio)

# This is required when using GNU compilers without OpenMP.
# It has no adverse effect with other compilers and/or when OpenMP is enabled.
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -ldl -pthread")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ldl -pthread")
set(CMAKE_EXE_LINKER_FLAGS_INIT "${CMAKE_EXE_LINKER_FLAGS_INIT} -ldl -pthread")

#set(CMAKE_VERBOSE_MAKEFILE TRUE)
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_EXTENSIONS OFF)

add_compile_definitions(_${CMAKE_SYSTEM_NAME}_ _GNU_SOURCE)

option(EXTRA_DEBUG_OUTPUT "Enable extra debug output" OFF)
if(EXTRA_DEBUG_OUTPUT)
    add_compile_definitions(DEBUG)
endif()

# Figure out the correct arch_dependent include path
set(BUILD_ARCH "${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")
message(STATUS "Build architecture: ${BUILD_ARCH}")

# TODO Adapt Config.cmake.in to be ready for this
#set(CMAKE_DEBUG_POSTFIX "d")

include_directories(
   include
   src/PUBLIC_INCLUDES
   src/PUBLIC_INCLUDES/rmn
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
file(GLOB PROJECT_H_FILES
   src/*/*.h
   src/*/*.inc
   src/*/*.hf
   src/*/*.cdk
   src/*/*.tpl*
)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/rmn)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/include)
set(MODULE_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)

# We need to create a file here because gfortran won't process a file nammmed *.hf
file(GLOB WHITEBOARD_HEADER_DEPS ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/*.tmpl90 ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/*.hf)
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf
    PRE_BUILD
    COMMAND sed -e "'s/ \"/ </g'" -e "'s/\"/>/g'" ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/WhiteBoard.hf > ${CMAKE_CURRENT_BINARY_DIR}/whiteboard_header.F90
    COMMAND rm -f ${CMAKE_CURRENT_BINARY_DIR}/WhiteBoard.hf
    COMMAND cp -av ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/WhiteBoard_constants.h ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf
    COMMAND ${CMAKE_Fortran_COMPILER} -I${CMAKE_CURRENT_SOURCE_DIR}/include -I${CMAKE_CURRENT_SOURCE_DIR}/src/PUBLIC_INCLUDES -I${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard -P -E ${CMAKE_CURRENT_BINARY_DIR}/whiteboard_header.F90 >> ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf
    COMMENT "Generating WhiteBoard.hf"
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/WhiteBoard.hf
    DEPENDS ${WHITEBOARD_HEADER_DEPS}

    USES_TERMINAL
)
# Add a custom target to regenerate WhiteBoard.hf
add_custom_target(WhiteBoardFortranHeader DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf)

# Build static library from source files
add_library(rmn-static STATIC ${PROJECT_C_FILES} ${PROJECT_F_FILES})
target_link_libraries(rmn-static PUBLIC App-static)
target_include_directories(rmn-static INTERFACE $<BUILD_INTERFACE:${MODULE_DIR}>)
add_dependencies(rmn-static rmn_build_info)

# Build shared library from object of rmn
add_library(rmn-shared SHARED $<TARGET_OBJECTS:rmn-static>)
target_link_libraries(rmn-shared PUBLIC App-shared)

set_target_properties(rmn-static rmn-shared PROPERTIES
    VERSION ${PROJECT_VERSION}
    Fortran_MODULE_DIRECTORY ${MODULE_DIR}
    PUBLIC_HEADER ${CMAKE_CURRENT_SOURCE_DIR}/src/PUBLIC_INCLUDES/rmn.h
    INTERFACE_COMPILE_DEFINITIONS HAVE_RMN="${PROJECT_VERSION}"
    INTERFACE_INCLUDE_DIRECTORIES $<BUILD_INTERFACE:${MODULE_DIR}>
    POSITION_INDEPENDENT_CODE ON
    OUTPUT_NAME rmn
    SOVERSION ${SOVERSION})

set(TARGETS rmn-static rmn-shared)
set(shared_targets rmn-shared)
set(static_targets rmn-static)

#----- Needed when used as sbumodule/subtree
add_library(rmn::rmn-shared ALIAS rmn-shared)
add_library(rmn::rmn-static ALIAS rmn-static)
if(BUILD_SHARED_LIBS)
    add_library(rmn::rmn ALIAS rmn-shared)
else()
    add_library(rmn::rmn ALIAS rmn-static)
endif()

if(WITH_OMPI AND OpenMP_FOUND AND MPI_FOUND) 
   set(TARGETS rmn-static rmn-shared rmn-ompi-static rmn-ompi-shared)

   # Build static library from source files
   add_library(rmn-ompi-static STATIC  ${PROJECT_C_FILES} ${PROJECT_F_FILES})
   set_target_properties(rmn-ompi-static PROPERTIES
       POSITION_INDEPENDENT_CODE ON
       Fortran_MODULE_DIRECTORY ${MODULE_DIR}/ompi
   )
   target_link_libraries(rmn-ompi-static PUBLIC App-ompi-static OpenMP::OpenMP_C OpenMP::OpenMP_Fortran MPI::MPI_C MPI::MPI_Fortran)
   target_compile_definitions(rmn-ompi-static PUBLIC HAVE_MPI HAVE_OPENMP)
   add_dependencies(rmn-ompi-static rmn_build_info)

   # Build shared library from objects of rmn-ompi
   add_library(rmn-ompi-shared SHARED $<TARGET_OBJECTS:rmn-ompi-static>)
   target_link_libraries(rmn-ompi-shared PUBLIC App-ompi-shared OpenMP::OpenMP_C OpenMP::OpenMP_Fortran MPI::MPI_C MPI::MPI_Fortran)
   target_compile_definitions(rmn-ompi-shared PUBLIC HAVE_MPI HAVE_OPENMP)

   set_target_properties(rmn-ompi-static rmn-ompi-shared PROPERTIES
      VERSION ${PROJECT_VERSION}
      PUBLIC_HEADER ${CMAKE_CURRENT_SOURCE_DIR}/src/PUBLIC_INCLUDES/rmn.h
      INTERFACE_COMPILE_DEFINITIONS HAVE_RMN="${PROJECT_VERSION}"
      INTERFACE_INCLUDE_DIRECTORIES $<BUILD_INTERFACE:${MODULE_DIR}/ompi>
      OUTPUT_NAME rmn-ompi
      SOVERSION ${SOVERSION})

   # Symbolic link for archive numbered version
   add_custom_command(TARGET rmn-static POST_BUILD
       COMMAND ln -sfv lib${PROJECT_NAME}-ompi.a lib${PROJECT_NAME}-ompi_${PROJECT_VERSION}.a)
   install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lib${PROJECT_NAME}-ompi_${PROJECT_VERSION}.a DESTINATION lib)

   list(APPEND shared_targets rmn-ompi-shared)
   list(APPEND static_targets rmn-ompi-static)

   #----- Needed when used as sbumodule/subtree
   add_library(rmn::rmn-ompi-shared ALIAS rmn-ompi-shared)
   add_library(rmn::rmn-ompi-static ALIAS rmn-ompi-static)
   if(BUILD_SHARED_LIBS)
       add_library(rmn::rmn-ompi ALIAS rmn-ompi-shared)
   else()
       add_library(rmn::rmn-ompi ALIAS rmn-ompi-static)
   endif()
endif()

foreach(TARGET IN ITEMS ${TARGETS})
    target_compile_options(${TARGET}
        PUBLIC $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:${GNU_FFLAGS}>
               $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:${Intel_FFLAGS}>
               $<$<COMPILE_LANG_AND_ID:Fortran,Flang>:${Flang_FFLAGS}>
               $<$<COMPILE_LANG_AND_ID:Fortran,PGI>:${PGI_FFLAGS}>
               $<$<COMPILE_LANG_AND_ID:Fortran,NVHPC>:${PGI_FFLAGS}>)
    add_dependencies(${TARGET}
        ${PROJECT_NAME}_build_info
        WhiteBoardFortranHeader)
    target_include_directories(${TARGET} INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/PUBLIC_INCLUDES>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/> # Fortran modules
    )
endforeach()

install(TARGETS ${shared_targets}
    EXPORT rmn-shared-targets
    INCLUDES DESTINATION include
    PUBLIC_HEADER DESTINATION include)

install(TARGETS ${static_targets}
    EXPORT rmn-static-targets
    INCLUDES DESTINATION include
    PUBLIC_HEADER DESTINATION include)

# Symbolic link for archive numbered version
add_custom_command(TARGET rmn-static POST_BUILD
    COMMAND ln -sfv lib${PROJECT_NAME}.a lib${PROJECT_NAME}_${PROJECT_VERSION}.a)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lib${PROJECT_NAME}_${PROJECT_VERSION}.a DESTINATION lib)

# Fortran modules are installed in the include directory for now
install(FILES
    ${MODULE_DIR}/rmn_jar.mod
    ${MODULE_DIR}/rmn_gmm.mod
    ${MODULE_DIR}/convert_ip123_int.mod
    DESTINATION include)

file(GLOB PROJECT_INCLUDE_FILES src/PUBLIC_INCLUDES/rmn/*)
# WhiteBoard.hf is generated by make and doesn't exist when cmake is executed
# It must therefore be added explicitly
list(APPEND PROJECT_INCLUDE_FILES ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf)
install(FILES ${PROJECT_INCLUDE_FILES} DESTINATION include/rmn)
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

install(EXPORT rmn-shared-targets
    NAMESPACE rmn::
    DESTINATION ${CONFIG_INSTALL_DIR})

install(EXPORT rmn-static-targets
    NAMESPACE rmn::
    DESTINATION ${CONFIG_INSTALL_DIR})

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
ec_package_name()                 # Define package prefix  
ec_build_config()                 # Create build configuration script
ec_prepare_ssm()                  # Prepare ssm packaging files

set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGE_VENDOR "ECCC")
set(CPACK_PACKAGE_CONTACT "${MAINTAINER}")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_OUTPUT_FILE_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/package")
set(CPACK_PACKAGE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set(CPACK_PACKAGE_FILE_NAME "${PACKAGE_NAME}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "${NAME}_${PROJECT_VERSION}")
include(CPack)

if (EC_INIT_DONE LESS 2)
   include(CTest)
   add_subdirectory(tests)
endif()



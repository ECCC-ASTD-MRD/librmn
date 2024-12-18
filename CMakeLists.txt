# The minimum version can not be 3.16 because of the LINK_LANGUAGE generator expression in Config.cmake.in
cmake_minimum_required(VERSION 3.20)

message(STATUS "(EC) Generating librmn Makefiles")

# Append EC specific module path
foreach(DIR_PATH ${CMAKE_SOURCE_DIR}/cmake_rpn/modules $ENV{EC_CMAKE_MODULE_PATH})
    if (EXISTS ${DIR_PATH} AND IS_DIRECTORY ${DIR_PATH})
        message(STATUS "(EC) Adding ${DIR_PATH} to CMAKE_MODULE_PATH")
        list(APPEND CMAKE_MODULE_PATH ${DIR_PATH})
    endif()
endforeach()
if (NOT CMAKE_MODULE_PATH)
    message(STATUS "(EC) CMAKE_MODULE_PATH=\"${CMAKE_MODULE_PATH}\"")
    message(FATAL_ERROR "No paths were added to CMAKE_MODULE_PATH; cmake_rpn will not be found!")
endif()

include(ec_init)           # Initialise compilers and ec specific functions
ec_git_version()           # Get version from git
ec_parse_manifest()        # Parse MANIFEST file

project(${NAME} DESCRIPTION "${DESCRIPTION}" LANGUAGES C Fortran)
# The project command sets the PROJECT_VERSION variable, but it doesn't accept anything that isn't of the form
# <major>[.<minor>[.<patch>[.<tweak>]]]], so we set it again afterwards
set(PROJECT_VERSION ${VERSION}${STATE})
set(rmn_VERSION ${PROJECT_VERSION} CACHE INTERNAL "rmn version" FORCE) # Needed for cascaded version identification
message(STATUS "(EC) ${PROJECT_NAME} version = ${PROJECT_VERSION}")
string(REGEX MATCH "([0-9]+)" SOVERSION ${PROJECT_VERSION})

include(GNUInstallDirs)
set(CMAKE_INSTALL_LIBDIR lib CACHE INTERNAL "Install dir for libs" FORCE)  # Avoid using lib64, cache when in super-project

ec_build_info()            # Generate build information
include(ec_compiler_presets)

# Need to add HAVE_RMN definition for auto-registering librmn within App
add_compile_definitions(HAVE_RMN=\"${PROJECT_VERSION}\")
add_subdirectory(dep dep)
add_subdirectory(App App)

option(WITH_OMPI "Compile with OpenMP/MPI support" TRUE) # Enable with(-DWITH_OMPI=TRUE)
if (WITH_OMPI)
   find_package(MPI REQUIRED)
   find_package(OpenMP REQUIRED)
endif()

option(RMN_LLVM_ADDRESS_SANITIZER "Compile with LLVM address sanitizer (Intel 2024+ only)" OFF)
if (NOT ("${CMAKE_C_COMPILER_ID}" STREQUAL "IntelLLVM"))
    set(RMN_LLVM_ADDRESS_SANITIZER OFF)
endif()

option(BUILD_PYTHON "Build Python interface" OFF)

set(RMN_BUILD_SHARED ON)
if (${RMN_LLVM_ADDRESS_SANITIZER})
    set(RMN_BUILD_SHARED OFF)
endif()

#set(FIND_LIBRARY_USE_LIB64_PATHS TRUE)
#find_package(json-c CONFIG REQUIRED)
#find_package(UDUNITS2)
find_package(LibXml2 REQUIRED)

include(ec_doxygen)         # Doxygen target doc

# set(CMAKE_VERBOSE_MAKEFILE TRUE)

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

set(PROJECT_PUBLIC_INCLUDE_FILES
    src/Struct/Vector.h
    src/Struct/QTree.h
)
 
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/rmn)
set(MODULE_DIR ${CMAKE_CURRENT_BINARY_DIR}/include/mod)
set(MODULE_DIR_OMPI ${CMAKE_CURRENT_BINARY_DIR}/include/mod_ompi)

# A bunch of fst_record functions (generated)
set(ARRAY_GETTER_SOURCE ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/rmn_fst24_record_get_data_array.hf)
set(GEN_ARRAY_GETTER_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/src/fst/gen_array_getters.sh)
add_custom_command(
    OUTPUT ${ARRAY_GETTER_SOURCE}
    PRE_BUILD
    COMMAND ${GEN_ARRAY_GETTER_SCRIPT} > ${ARRAY_GETTER_SOURCE}
    DEPENDS ${GEN_ARRAY_GETTER_SCRIPT}
    COMMENT "Generating array include rmn_fst24_record_get_data_array.hf")
list(APPEND PROJECT_H_FILES ${ARRAY_GETTER_SOURCE})
add_custom_target(ArrayGetter DEPENDS ${ARRAY_GETTER_SOURCE})

# We need to create a file here because gfortran won't process a file nammmed *.hf
file(GLOB WHITEBOARD_HEADER_DEPS ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/*.tmpl90 ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/*.hf)
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard_2.hf
    BYPRODUCTS ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf_ ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf__
    PRE_BUILD
    COMMAND sed -e "'s/ \"/ </g'" -e "'s/\"/>/g'" ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/WhiteBoard.hf > ${CMAKE_CURRENT_BINARY_DIR}/whiteboard_header.F90
    COMMAND cp -av ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/WhiteBoard_constants.h   ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf
    COMMAND cp -av ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/WhiteBoard_constants_2.h ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard_2.hf
    COMMAND cat ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/whiteboard_type.hf >> ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf
    COMMAND cat ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/whiteboard_type.hf >> ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard_2.hf
    COMMAND ${CMAKE_Fortran_COMPILER} -I${CMAKE_CURRENT_SOURCE_DIR}/include -I${CMAKE_CURRENT_SOURCE_DIR}/src/PUBLIC_INCLUDES -I${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard -E ${CMAKE_CURRENT_BINARY_DIR}/whiteboard_header.F90 > ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf_
    COMMAND grep -Ev "'^#|^[ ]*$|^[ ]*!'" ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf_ > ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf__
    COMMAND cat ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf__ >>${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf
    COMMAND cat ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf__ >>${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard_2.hf
    COMMENT "Generating WhiteBoard.hf and WhiteBoard_2.hf"
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/src/whiteboard/WhiteBoard.hf
    DEPENDS ${WHITEBOARD_HEADER_DEPS}

    USES_TERMINAL
)
# Add a custom target to regenerate WhiteBoard.hf
add_custom_target(WhiteBoardFortranHeader DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf)

set(TARGETS rmn-static)
if (${RMN_BUILD_SHARED})
    set(TARGETS ${TARGETS} rmn-shared)
endif()

# Build static library from source files
add_library(rmn-static STATIC ${PROJECT_C_FILES} ${PROJECT_F_FILES} ${PROJECT_H_FILES})
target_link_libraries(rmn-static PUBLIC App-static json-c-static libudunits2-static LibXml2::LibXml2)
target_include_directories(rmn-static INTERFACE $<BUILD_INTERFACE:${MODULE_DIR}> ${CMAKE_CURRENT_SOURCE_DIR}/src/Struct)
add_dependencies(rmn-static rmn_build_info json-c libudunits2-static)

# Build shared library from object of rmn
if (${RMN_BUILD_SHARED})
    add_library(rmn-shared SHARED $<TARGET_OBJECTS:rmn-static>)
    target_link_libraries(rmn-shared PUBLIC App-shared json-c libudunits2-shared LibXml2::LibXml2)
endif()

set_target_properties(${TARGETS} PROPERTIES
    VERSION ${PROJECT_VERSION}
    Fortran_MODULE_DIRECTORY ${MODULE_DIR}
    PUBLIC_HEADER ${CMAKE_CURRENT_SOURCE_DIR}/src/PUBLIC_INCLUDES/rmn.h
    INTERFACE_COMPILE_DEFINITIONS HAVE_RMN="${PROJECT_VERSION}"
    INTERFACE_INCLUDE_DIRECTORIES $<BUILD_INTERFACE:${MODULE_DIR}>
    POSITION_INDEPENDENT_CODE ON
    OUTPUT_NAME rmn
    SOVERSION ${SOVERSION}
    C_STANDARD 11
    C_EXTENSIONS OFF)

set(shared_targets rmn-shared)
set(static_targets rmn-static)

#----- Needed when used as sbumodule/subtree
add_library(rmn::rmn-static ALIAS rmn-static)
add_library(rmn::rmn ALIAS rmn-static) # Default to static
if (${RMN_BUILD_SHARED})
    add_library(rmn::rmn-shared ALIAS rmn-shared)
endif()

if(WITH_OMPI AND OpenMP_FOUND AND MPI_FOUND) 
    set (OMPI_TARGETS rmn-ompi-static)
    if (${RMN_BUILD_SHARED})
        set (OMPI_TARGETS ${OMPI_TARGETS} rmn-ompi-shared)
    endif()
    set(TARGETS ${TARGETS} ${OMPI_TARGETS})

   # Build static library from source files
   add_library(rmn-ompi-static STATIC ${PROJECT_C_FILES} ${PROJECT_F_FILES})
   set_target_properties(rmn-ompi-static PROPERTIES
       POSITION_INDEPENDENT_CODE ON
       Fortran_MODULE_DIRECTORY ${MODULE_DIR_OMPI}
   )
   target_link_libraries(rmn-ompi-static PUBLIC App-ompi-static json-c-static libudunits2-static LibXml2::LibXml2 OpenMP::OpenMP_C OpenMP::OpenMP_Fortran MPI::MPI_C MPI::MPI_Fortran)
   target_compile_definitions(rmn-ompi-static PUBLIC HAVE_MPI HAVE_OPENMP)
   add_dependencies(rmn-ompi-static rmn_build_info json-c libudunits2-static)

    # Build shared library from objects of rmn-ompi
    if (${RMN_BUILD_SHARED})
        add_library(rmn-ompi-shared SHARED $<TARGET_OBJECTS:rmn-ompi-static>)
        target_link_libraries(rmn-ompi-shared PUBLIC App-ompi-shared json-c libudunits2-shared LibXml2::LibXml2 OpenMP::OpenMP_C OpenMP::OpenMP_Fortran MPI::MPI_C MPI::MPI_Fortran)
        target_compile_definitions(rmn-ompi-shared PUBLIC HAVE_MPI HAVE_OPENMP)
    endif()

    set_target_properties(${OMPI_TARGETS} PROPERTIES
        VERSION ${PROJECT_VERSION}
        PUBLIC_HEADER ${CMAKE_CURRENT_SOURCE_DIR}/src/PUBLIC_INCLUDES/rmn.h
        INTERFACE_COMPILE_DEFINITIONS HAVE_RMN="${PROJECT_VERSION}"
        INTERFACE_INCLUDE_DIRECTORIES $<BUILD_INTERFACE:${MODULE_DIR_OMPI}>
        OUTPUT_NAME rmn-ompi
        SOVERSION ${SOVERSION}
        C_STANDARD 11
        C_EXTENSIONS OFF)

   # Symbolic link for archive numbered version
   add_custom_command(TARGET rmn-static POST_BUILD
       COMMAND ln -sfv lib${PROJECT_NAME}-ompi.a lib${PROJECT_NAME}-ompi_${PROJECT_VERSION}.a
       BYPRODUCTS lib${PROJECT_NAME}-ompi_${PROJECT_VERSION}.a)
   install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lib${PROJECT_NAME}-ompi_${PROJECT_VERSION}.a DESTINATION lib)

   list(APPEND shared_targets rmn-ompi-shared)
   list(APPEND static_targets rmn-ompi-static)

    #----- Needed when used as sbumodule/subtree
    add_library(rmn::rmn-ompi-static ALIAS rmn-ompi-static)
    add_library(rmn::rmn-ompi ALIAS rmn-ompi-static)    # Default to static
    if (${RMN_BUILD_SHARED})
        add_library(rmn::rmn-ompi-shared ALIAS rmn-ompi-shared)
    endif()
endif()

foreach(TARGET IN ITEMS ${TARGETS})

    target_compile_options(${TARGET} PUBLIC
        $<$<COMPILE_LANG_AND_ID:Fortran,GNU>: -fconvert=big-endian -fcray-pointer -frecord-marker=4 -fno-second-underscore -pthread>
        $<$<COMPILE_LANG_AND_ID:C,GNU>: -pthread>
        $<$<COMPILE_LANG_AND_ID:Fortran,Intel>: -convert big_endian -align array32byte -assume byterecl>
        $<$<COMPILE_LANG_AND_ID:Fortran,Flang>: -Mbyteswapio>
        $<$<COMPILE_LANG_AND_ID:Fortran,PGI>: -byteswapio>
        $<$<COMPILE_LANG_AND_ID:Fortran,NVHPC>:-byteswapio>
    )
    target_link_options(${TARGET} PUBLIC
               $<$<COMPILE_LANG_AND_ID:C,GNU>:-ldl -pthread>
               $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-ldl -pthread>
    )

    if (${RMN_LLVM_ADDRESS_SANITIZER})
        target_compile_options(${TARGET} PUBLIC
                $<$<COMPILE_LANG_AND_ID:C,IntelLLVM>:-fsanitize=address>
        )
        target_link_options(${TARGET} PUBLIC
                $<$<COMPILE_LANG_AND_ID:C,IntelLLVM>:-fsanitize=address>
                $<$<COMPILE_LANG_AND_ID:Fortran,IntelLLVM>:-fsanitize=address>
        )
    endif()

    # nvhpc doesn't recognize shiftl	  
    target_compile_definitions(${TARGET}
        PUBLIC $<$<COMPILE_LANG_AND_ID:Fortran,NVHPC>:SHIFTL=LSHIFT>)
    # for IntelLLVM > 2024.0 - TODO: move in source code
    if (("${CMAKE_C_COMPILER_ID}" STREQUAL "IntelLLVM") AND (${CMAKE_C_COMPILER_VERSION} VERSION_GREATER_EQUAL 2024.1))
      target_compile_definitions(${TARGET} PUBLIC FORTRAN_202X_SUPPORTED)
    endif()
    add_dependencies(${TARGET}
        ${PROJECT_NAME}_build_info
        WhiteBoardFortranHeader
        ArrayGetter)
    target_include_directories(${TARGET} INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src/PUBLIC_INCLUDES>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/> # Fortran modules
    )
    target_include_directories(${TARGET} PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/include
        ${CMAKE_CURRENT_SOURCE_DIR}/src
        ${CMAKE_CURRENT_SOURCE_DIR}/src/PUBLIC_INCLUDES
        ${CMAKE_CURRENT_SOURCE_DIR}/src/Meta
        ${CMAKE_CURRENT_SOURCE_DIR}/src/Struct
        ${CMAKE_CURRENT_BINARY_DIR}/include
    )
    target_include_directories(${TARGET} PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/dep>
    )

endforeach()

foreach(RMN_TARGET ${TARGETS})
    target_compile_options(${RMN_TARGET} PRIVATE
        $<$<COMPILE_LANG_AND_ID:C,Intel>:-diag-disable=3180>
    )
endforeach()


# Compilation options for specific files
set_source_files_properties(src/fst/xdf98.c            PROPERTIES COMPILE_FLAGS "$<$<COMPILE_LANG_AND_ID:C,Intel>:-diag-disable=1899> $<$<COMPILE_LANG_AND_ID:C,GNU>:-Wno-multichar> $<$<COMPILE_LANG_AND_ID:C,PGI,NVHPC>:-Mnovect>")
if(COMPILER_SUITE MATCHES "intel" AND CMAKE_C_COMPILER_VERSION MATCHES "2025.0.0")
#   message(STATUS "(ICX) C_COMPILER_VERSION = ${CMAKE_C_COMPILER_VERSION}")
  set_source_files_properties(src/fst/fst98.c            PROPERTIES COMPILE_FLAGS "-O1")
endif()
if(COMPILER_SUITE MATCHES "aocc" AND CMAKE_C_COMPILER_VERSION MATCHES "17")
#   message(STATUS "(AOCC) C_COMPILER_VERSION = ${CMAKE_C_COMPILER_VERSION}")
  set_source_files_properties(src/fst/excdes_new.c     PROPERTIES COMPILE_FLAGS "-O0")
endif()
set_source_files_properties(src/primitives/wkoffit.c   PROPERTIES COMPILE_FLAGS "$<$<COMPILE_LANG_AND_ID:C,Intel>:-diag-disable=1899> $<$<COMPILE_LANG_AND_ID:C,GNU>:-Wno-multichar>")
set_source_files_properties(src/primitives/constnt.F90 PROPERTIES COMPILE_FLAGS "$<$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>:-diag-disable=6477,7028,7416>")
set_source_files_properties(src/base/fft_m4.f          PROPERTIES COMPILE_FLAGS "$<$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>:-diag-disable=6477,7342>")
set_source_files_properties(src/base/fft_m8.f          PROPERTIES COMPILE_FLAGS "$<$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>:-diag-disable=7342>")
set_source_files_properties(src/readlx/readlx.F90      PROPERTIES COMPILE_FLAGS "$<$<COMPILE_LANG_AND_ID:Fortran,Intel,IntelLLVM>:-diag-disable=6477,7028,7416>")

if (${RMN_BUILD_SHARED})
    install(TARGETS ${shared_targets}
        EXPORT rmn-shared-targets
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
endif()

install(TARGETS ${static_targets}
    EXPORT rmn-static-targets
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
 
# Symbolic link for archive numbered version
add_custom_command(TARGET rmn-static POST_BUILD
    COMMAND ln -sfv lib${PROJECT_NAME}.a lib${PROJECT_NAME}_${PROJECT_VERSION}.a
    BYPRODUCTS lib${PROJECT_NAME}_${PROJECT_VERSION}.a)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lib${PROJECT_NAME}_${PROJECT_VERSION}.a DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Fortran modules are installed in the include directory for now
install(FILES
    ${MODULE_DIR}/f_c_strings_mod.mod
    ${MODULE_DIR}/rmn_circular_buffer_module.mod
    ${MODULE_DIR}/rmn_common.mod
    ${MODULE_DIR}/rmn_date.mod
    ${MODULE_DIR}/rmn_fst_common.mod
    ${MODULE_DIR}/rmn_fst24.mod
    ${MODULE_DIR}/rmn_fst24_record.mod
    ${MODULE_DIR}/rmn_fst98.mod
    ${MODULE_DIR}/rmn_jar.mod
    ${MODULE_DIR}/rmn_gmm.mod
    ${MODULE_DIR}/rmn_libc.mod
    ${MODULE_DIR}/rmn_meta.mod
    ${MODULE_DIR}/convert_ip123_int.mod
    ${MODULE_DIR}/whiteboard_mod.mod
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

file(GLOB PROJECT_INCLUDE_FILES src/PUBLIC_INCLUDES/rmn/*)
# WhiteBoard.hf is generated by make and doesn't exist when cmake is executed
# WhiteBoard_2.hf is generated by make and doesn't exist when cmake is executed
# they must therefore be added explicitly
list(APPEND PROJECT_INCLUDE_FILES ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard.hf)
list(APPEND PROJECT_INCLUDE_FILES ${CMAKE_CURRENT_BINARY_DIR}/include/rmn/WhiteBoard_2.hf)
list(APPEND PROJECT_INCLUDE_FILES ${PROJECT_PUBLIC_INCLUDE_FILES})
install(FILES ${PROJECT_INCLUDE_FILES} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/rmn)
add_custom_target(
    cprotos
    ${CMAKE_CURRENT_SOURCE_DIR}/extractProtos.bash ${CMAKE_CURRENT_SOURCE_DIR}/src ${PROJECT_C_FILES}
    #WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Extract function prototypes from C source code"
)

# Generate the config file for the project to be usable via cmake's find_package command
set(CONFIG_INSTALL_DIR "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}-${PROJECT_VERSION}")

if (${RMN_BUILD_SHARED})
    install(EXPORT rmn-shared-targets
        NAMESPACE rmn::
        DESTINATION ${CONFIG_INSTALL_DIR})
endif()

install(EXPORT rmn-static-targets
    NAMESPACE rmn::
    DESTINATION ${CONFIG_INSTALL_DIR})

# udunits2 does not saves it's targets configuration correctly
install(EXPORT udunits2-targets
        NAMESPACE udunits2::
        DESTINATION ${CONFIG_INSTALL_DIR})
    
include(CMakePackageConfigHelpers)
configure_package_config_file(
    "Config.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
    INSTALL_DESTINATION "${CONFIG_INSTALL_DIR}"
    PATH_VARS CMAKE_INSTALL_INCLUDEDIR CMAKE_INSTALL_LIBDIR
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

add_subdirectory(utils)
if(BUILD_PYTHON)
    message(STATUS "(EC) Python interface will be built")
    find_package(Python 3 REQUIRED COMPONENTS Development NumPy Interpreter)
    add_subdirectory(python/rmn)
else()
    message(STATUS "(EC) Python interface will NOT be built")
endif()
add_subdirectory(tests)

# Packaging
ec_package_name()                 # Define package prefix  
ec_build_config()                 # Create build configuration script
ec_prepare_ssm()                  # Prepare ssm packaging files

set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGE_VENDOR "ECCC-ASTD-MRD")
set(CPACK_PACKAGE_CONTACT "${MAINTAINER}")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_OUTPUT_FILE_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/package")
set(CPACK_PACKAGE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
#set(CPACK_PACKAGE_VERSION     "${PROJECT_VERSION}")
#set(CPACK_PACKAGE_VERSION_MAJOR     "${PROJECT_VERSION_MAJOR}")
#set(CPACK_PACKAGE_VERSION_MINOR     "${PROJECT_VERSION_MINOR}")
#set(CPACK_PACKAGE_VERSION_PATCH     "${PROJECT_VERSION_PATCH}")
set(CPACK_PACKAGE_FILE_NAME "${PACKAGE_NAME}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "${NAME}_${PROJECT_VERSION}")
include(CPack)

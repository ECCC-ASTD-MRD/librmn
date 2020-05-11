cmake_minimum_required(VERSION 3.16)

# Append EC specific module path
foreach(PATH $ENV{EC_CMAKE_MODULE_PATH})
   list(APPEND CMAKE_MODULE_PATH ${PATH})
endforeach()
 
# Set the compiler by either
#    - Uncommenting one of the following 3 lines
#    - Using the flag -DEC_COMPILER=...
#    - Specifying the "EC_COMPILER: ..." in the VERSION file
#set(EC_COMPILER gnu CACHE INTERNAL compiler)
#set(EC_COMPILER intel CACHE INTERNAL compiler)
#set(EC_COMPILER pgi CACHE INTERNAL compiler)

include(ec_utils)   # Include EC specific cmake utils
ec_parse_version()  # Parse VERSION file
ec_getvar()         # Get EC specific var
ec_build_info()     # Generate build include file

project(${NAME} VERSION ${VERSION} DESCRIPTION "${DESCRIPTION}")
set(CMAKE_INSTALL_PREFIX "" CACHE PATH "..." FORCE)

# Enable the two languages that are used
enable_language(C)
enable_language(Fortran)

set(CMAKE_ARCH "${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")
message(STATUS "Build architecture: ${CMAKE_ARCH}")

add_compile_options(${OPTIMIZ_FLAG} )

option(WITH_OPENMP "Build with OpenMP support" no)
if (WITH_OPENMP)
   find_package(OpenMP REQUIRED)
      if (OPENMP_FOUND)
         add_definitions(-DWITH_OpenMP)
         add_compile_options(${OpenMP_C_FLAGS})
      else() 
         add_definitions(-DWITHOUT_OpenMP)
  endif()
endif()

#find_package(MPI) (rpncomm has its own)

# Include an architecture dependent file that contains settings for a
# particular architecture and compiler, using the default value of the
# compiler defined above or the argument given to cmake command.
#include("${CMAKE_ARCH}-${CMAKE_C_COMPILER_ID}")
message(STATUS "Compiler rules: ${CMAKE_ARCH}-${EC_COMPILER}")
include("${CMAKE_ARCH}-${EC_COMPILER}")

set(FTNSRC "" CACHE INTERNAL "" FORCE)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${BUILD}/modules CACHE STRING "Where Fortran modules go")
set(CMAKE_Fortran_SOURCE_FILE_EXTENSIONS f;f90;F90 CACHE STRING "Fortran extension files")

include_directories(include
                    include/${CMAKE_ARCH}-${CMAKE_C_COMPILER_ID}
                    src/PUBLIC_INCLUDES
                    ${CMAKE_CURRENT_BINARY_DIR}/${BUILD}/modules
                    ${MPI_Fortran_INCLUDE_PATH}
                    )

# Uncomment to link to the local libraries already installed
#link_directories(libs/${ARCH}-${EC_COMPILER})

# The following is to install the library in a location librmn/libs/*
if (WITH_OPENMP)
   set(CMAKE_INSTALL_PREFIX ${CMAKE_CURRENT_SOURCE_DIR}/libs/${EC_OS}-${EC_COMPILER}-${EC_COMPILER_VERSION}-OpenMP)
else()
   set(CMAKE_INSTALL_PREFIX ${CMAKE_CURRENT_SOURCE_DIR}/libs/${EC_OS}-${EC_COMPILER}-${EC_COMPILER_VERSION})
endif()

add_subdirectory(src)

#----- Build config information script
install(CODE "execute_process(COMMAND sed -e \"s/CMAKE_CC/\\\"${EC_COMPILER} ${EC_COMPILER_VERSION}\\\"/\" -e \"s/CMAKE_FTN/\\\"${EC_COMPILER} ${EC_COMPILER_VERSION}\\\"/\" 
   -e \"s/CMAKE_NAME/${NAME}/\" -e \"s/CMAKE_VERSION/${VERSION}${STATE}/\"
   ../config OUTPUT_FILE ${NAME}-config)")
install(PROGRAMS ${CMAKE_BINARY_DIR}/${NAME}-config DESTINATION bin)

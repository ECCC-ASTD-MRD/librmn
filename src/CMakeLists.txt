message(STATUS "Starting generation of librmn Makefile")

#----- Insert version info
execute_process(COMMAND bash rmnlib_version ${VERSION}${STATE} WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/primitives")

file(GLOB PROJECT_C_FILES  */*.c template_utils/whiteboard/*.c)
file(GLOB PROJECT_F_FILES  */*.F90 */*.F */*.f */*.f90 template_utils/gmm/gmm.f90 template_utils/whiteboard/whiteboard_omp.f90 template_utils/whiteboard/whiteboard_st.f90)
file(GLOB PROJECT_INCLUDE_FILES PUBLIC_INCLUDES/*.*)

include_directories(INTRALIB_INCLUDES)
if (OPENMP)
    find_package(OpenMP)
    add_definitions(-DWITH_OpenMP)
    add_compile_options(${OPTIMIZ_FLAG} ${OPENMP_FLAG})
else()
    add_definitions(-DWITHOUT_OpenMP)
    add_compile_options(${OPTIMIZ_FLAG} )
endif()

if (SHARED)
   add_library (rmn SHARED ${PROJECT_F_FILES} ${PROJECT_C_FILES})
else()
   add_library (rmn STATIC ${PROJECT_F_FILES} ${PROJECT_C_FILES})
endif()

add_dependencies(rmn build_info)

set_target_properties(rmn PROPERTIES VERSION ${PROJECT_VERSION})
set_target_properties(rmn PROPERTIES PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}")

install(TARGETS rmn
   LIBRARY DESTINATION lib
   ARCHIVE DESTINATION lib
   PUBLIC_HEADER DESTINATION include)

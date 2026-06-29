
file(GLOB PROJECT_PYTHON_FILES
    rmn/*.py
)

set(PYTHON_BUILD_FILES ${PROJECT_PYTHON_FILES})
list(TRANSFORM PYTHON_BUILD_FILES REPLACE "${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_CURRENT_BINARY_DIR}")

if (TARGET rmn-shared)
    add_custom_command(
        OUTPUT ${PYTHON_BUILD_FILES}
        COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/rmn/
        COMMAND ${CMAKE_COMMAND} -E copy ${PROJECT_PYTHON_FILES} ${CMAKE_CURRENT_BINARY_DIR}/rmn/
        DEPENDS ${PROJECT_PYTHON_FILES}
        COMMENT "Copying python files to build directory")
    add_custom_target(python-files DEPENDS ${PYTHON_BUILD_FILES})
    add_dependencies(rmn-shared python-files)
    if (TARGET rmn-ompi-shared)
        add_dependencies(rmn-ompi-shared python-files)
    endif()
endif()

install(DIRECTORY rmn DESTINATION lib/python FILES_MATCHING PATTERN *.py)

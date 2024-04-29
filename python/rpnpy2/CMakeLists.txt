Python_add_library(_librmn MODULE _librmn.c)

file(GLOB python_files *.py)
set(python_files
    __init__.py
)

foreach(f IN LISTS python_files)
    file(CREATE_LINK ${CMAKE_CURRENT_SOURCE_DIR}/${f} ${CMAKE_CURRENT_BINARY_DIR}/${f} SYMBOLIC)
endforeach()

target_link_libraries(_librmn PRIVATE rmn-ompi-shared ${PYTHON_LIBRARIES})
target_include_directories(_librmn PRIVATE ${PYTHON_INCLUDE_DIRS})

install(FILES ${python_files} DESTINATION lib/python/rpnpy)
install(TARGETS _librmn LIBRARY DESTINATION lib/python/rpnpy)

# Temporary convenience test
add_custom_target(test_py
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/tests
    # The -u is for Python to operate without buffering stderr
    # which is helpful for printf debugging
    COMMAND bash -c "PYTHONPATH=${CMAKE_BINARY_DIR}/python python3 -u test_rpnpy.py"
)
add_dependencies(test_py _librmn)

configure_file(setup.in.sh setup.sh @ONLY)

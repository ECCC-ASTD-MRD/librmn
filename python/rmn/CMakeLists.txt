Python_add_library(_rmn MODULE _rmn.c indexing.c)

file(GLOB python_files *.py)
set(python_files
    __init__.py
)

foreach(f IN LISTS python_files)
    file(CREATE_LINK ${CMAKE_CURRENT_SOURCE_DIR}/${f} ${CMAKE_CURRENT_BINARY_DIR}/${f} SYMBOLIC)
endforeach()

target_link_libraries(_rmn PRIVATE rmn-ompi-shared ${PYTHON_LIBRARIES})
target_include_directories(_rmn PRIVATE ${PYTHON_INCLUDE_DIRS})

install(FILES ${python_files} DESTINATION lib/python/rmn)
install(TARGETS _rmn LIBRARY DESTINATION lib/python/rmn)

# Temporary convenience test
add_custom_target(test_py
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/tests
    # The -u is for Python to operate without buffering stderr
    # which is helpful for printf debugging
    COMMAND bash -c "PYTHONPATH=${CMAKE_BINARY_DIR}/python ${Python_EXECUTABLE} -u test_rmn.py"
)
add_dependencies(test_py _rmn)

configure_file(setup.in.sh setup.sh @ONLY)

add_executable(test_indexing indexing.c test_indexing.c)
target_link_libraries(test_indexing PRIVATE rmn-ompi-static)

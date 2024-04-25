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

configure_file(setup.in.sh setup.sh @ONLY)

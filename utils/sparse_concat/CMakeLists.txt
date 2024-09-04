
add_executable(sparse_concat sparse_concat.c)
target_link_libraries(sparse_concat PUBLIC rmn::rmn App::App)
install(TARGETS sparse_concat)

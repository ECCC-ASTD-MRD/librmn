
add_executable(rsf_dump rsf_dump.c)
target_link_libraries(rsf_dump PUBLIC rmn::rmn App::App)
install(TARGETS rsf_dump)

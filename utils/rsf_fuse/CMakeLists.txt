
add_executable(rsf_fuse rsf_fuse.c)
target_link_libraries(rsf_fuse PUBLIC rmn::rmn App::App)
install(TARGETS rsf_fuse)

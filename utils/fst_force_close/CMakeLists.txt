
add_executable(fst_force_close fst_force_close.c)
target_link_libraries(fst_force_close PUBLIC rmn::rmn)
install(TARGETS fst_force_close)

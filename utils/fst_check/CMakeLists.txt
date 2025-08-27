
add_executable(fst_check fst_check.c)
target_link_libraries(fst_check PUBLIC rmn::rmn)
install(TARGETS fst_check)


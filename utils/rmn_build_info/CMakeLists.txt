add_executable(rmn_build_info_util rmn_build_info.c)
set_property(TARGET rmn_build_info_util PROPERTY OUTPUT_NAME rmn_build_info)
target_link_libraries(rmn_build_info_util PUBLIC rmn::rmn)
install(TARGETS rmn_build_info_util RENAME rmn_build_info)

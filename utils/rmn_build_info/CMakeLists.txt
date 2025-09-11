add_executable(rmn_build_info_util rmn_build_info.c)
set_property(TARGET rmn_build_info_util PROPERTY OUTPUT_NAME rmn_build_info)
target_link_libraries(rmn_build_info_util PUBLIC rmn::rmn)
target_include_directories(rmn_build_info_util PRIVATE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../>
)
install(TARGETS rmn_build_info_util RENAME rmn_build_info)

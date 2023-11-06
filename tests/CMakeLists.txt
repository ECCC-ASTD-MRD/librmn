if(BUILD_TESTING)
    add_custom_target(check COMMAND CTEST_OUTPUT_ON_FAILURE=true ${CMAKE_CTEST_COMMAND})

    message(STATUS "(EC) Generating librmn tests")

    # Helper module
    add_library(test_helper EXCLUDE_FROM_ALL rmn_test_helper.F90 generate_fstd_module.F90)
    target_link_libraries(test_helper rmn::rmn)

    add_executable(clib_isalpha EXCLUDE_FROM_ALL ${CMAKE_CURRENT_SOURCE_DIR}/clib_isalpha.F90)
    target_link_libraries(clib_isalpha rmn::rmn)
    add_test(NAME clib_isalpha COMMAND ${CMAKE_CURRENT_BINARY_DIR}/clib_isalpha ${CMAKE_CURRENT_SOURCE_DIR}/spp.dict)
    add_dependencies(check clib_isalpha)

    foreach(DATATYPE float double)
        add_executable(fst_${DATATYPE} EXCLUDE_FROM_ALL fst.c)
        target_compile_definitions(fst_${DATATYPE} PRIVATE DATATYPE=${DATATYPE})
        target_link_libraries(fst_${DATATYPE} PRIVATE rmn::rmn m)
        add_test(NAME fst_${DATATYPE} COMMAND ${CMAKE_CURRENT_BINARY_DIR}/fst_${DATATYPE})
        add_dependencies(check fst_${DATATYPE})
    endforeach()

    add_executable(wb_read EXCLUDE_FROM_ALL wb_read.F90)
    target_link_libraries(wb_read rmn::rmn)
    add_test(NAME wb_read COMMAND ${CMAKE_CURRENT_BINARY_DIR}/wb_read ${CMAKE_CURRENT_SOURCE_DIR}/spp.dict)
    add_dependencies(check wb_read)

    add_executable(wb_putget EXCLUDE_FROM_ALL wb_putget.F90)
    target_link_libraries(wb_putget rmn::rmn)
    target_include_directories(wb_putget PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../src/whiteboard)
    add_test(NAME wb_putget COMMAND ${CMAKE_CURRENT_BINARY_DIR}/wb_putget)
    add_dependencies(check wb_putget)

    add_executable(wb_test EXCLUDE_FROM_ALL wb_test.c)
    target_link_libraries(wb_test rmn::rmn)
    target_include_directories(wb_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../src/whiteboard)
    add_test(NAME wb_test COMMAND ${CMAKE_CURRENT_BINARY_DIR}/wb_test)
    add_dependencies(check wb_test)

    add_executable(wb_test_f90 EXCLUDE_FROM_ALL wb_test.F90)
    target_link_libraries(wb_test_f90 rmn::rmn)
    target_include_directories(wb_test_f90 PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../src/whiteboard)
    add_test(NAME wb_test_f90  COMMAND ${CMAKE_CURRENT_BINARY_DIR}/wb_test_f90)
    add_dependencies(check wb_test_f90)

# This test is weird and I need moretime to understand it
#    add_executable(wb_get EXCLUDE_FROM_ALL wb_get.c)
#    target_link_libraries(wb_get rmn::rmn)
#    target_include_directories(wb_get PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../src/whiteboard)
#    add_test(NAME wb_get COMMAND ${CMAKE_CURRENT_BINARY_DIR}/wb_get)
#    add_dependencies(check wb_get)

    add_executable(prog_filename EXCLUDE_FROM_ALL prog_filename.c)
    target_link_libraries(prog_filename rmn::rmn)
    add_test(NAME prog_filename COMMAND ${CMAKE_CURRENT_BINARY_DIR}/prog_filename)
    add_dependencies(check prog_filename)

    add_executable(gmm EXCLUDE_FROM_ALL gmm.F90)
    target_link_libraries(gmm rmn::rmn)
    add_test(NAME gmm COMMAND ${CMAKE_CURRENT_BINARY_DIR}/gmm)
    add_dependencies(check gmm)

    add_executable(c_ccard EXCLUDE_FROM_ALL c_ccard.c)
    target_link_libraries(c_ccard rmn::rmn)
    add_test(NAME c_ccard COMMAND ${CMAKE_CURRENT_BINARY_DIR}/c_ccard)
    add_dependencies(check c_ccard)

    foreach(FST_BACKEND rsf xdf)
        add_executable(fst_missing_${FST_BACKEND} EXCLUDE_FROM_ALL fst_missing.F90)
        target_link_libraries(fst_missing_${FST_BACKEND} rmn::rmn test_helper)
        target_compile_options(fst_missing_${FST_BACKEND} PRIVATE
            $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-diag-disable 5268>)
        add_test(NAME fst_missing_${FST_BACKEND} COMMAND ${CMAKE_CURRENT_BINARY_DIR}/fst_missing_${FST_BACKEND})
        add_dependencies(check fst_missing_${FST_BACKEND})

        add_executable(fst_interface_${FST_BACKEND} EXCLUDE_FROM_ALL fst_interface.F90)
        target_link_libraries(fst_interface_${FST_BACKEND} rmn::rmn test_helper)
        add_test(NAME fst_interface_${FST_BACKEND} COMMAND ${CMAKE_CURRENT_BINARY_DIR}/fst_interface_${FST_BACKEND})
        add_dependencies(check fst_interface_${FST_BACKEND})

        if (${FST_BACKEND} STREQUAL xdf)
            target_compile_definitions(fst_missing_xdf PRIVATE FST_TEST_IS_RSF=.false.)
            target_compile_definitions(fst_interface_xdf PRIVATE FST_TEST_IS_RSF=.false.)
        endif()
    endforeach()

    add_executable(generate_fstd EXCLUDE_FROM_ALL generate_fstd.F90)
    target_link_libraries(generate_fstd test_helper)

    add_executable(fst_thorough EXCLUDE_FROM_ALL fst_thorough.F90)
    target_link_libraries(fst_thorough rmn::rmn test_helper)
    add_test(NAME fst_thorough COMMAND ${CMAKE_CURRENT_BINARY_DIR}/fst_thorough)
    add_dependencies(check fst_thorough)

    add_executable(rsf_dump EXCLUDE_FROM_ALL test_rsf.c)
    target_link_libraries(rsf_dump rmn::rmn)
    target_compile_definitions(rsf_dump PRIVATE RSF_DUMP)

    add_executable(burp_f EXCLUDE_FROM_ALL burp_f.f)
    target_link_libraries(burp_f rmn::rmn)
    add_test(NAME burp_f COMMAND ${CMAKE_CURRENT_BINARY_DIR}/burp_f)
    add_dependencies(check burp_f)

    add_executable(burp_c EXCLUDE_FROM_ALL burp_c.c)
    target_link_libraries(burp_c rmn::rmn)
    add_test(NAME burp_c COMMAND ${CMAKE_CURRENT_BINARY_DIR}/burp_c)
    add_dependencies(check burp_c)

    add_executable(serializer EXCLUDE_FROM_ALL serializer.F90)
    target_link_libraries(serializer rmn::rmn)
    add_test(NAME serializer COMMAND ${CMAKE_CURRENT_BINARY_DIR}/serializer)
    add_dependencies(check serializer)

    add_executable(serializer_large EXCLUDE_FROM_ALL serializer_large.F90)
    target_link_libraries(serializer_large rmn::rmn)
    # add_test(NAME serializer_large COMMAND ${CMAKE_CURRENT_BINARY_DIR}/serializer_large)
    # add_dependencies(check serializer_large)

    add_executable(fst23_interface_c fst23_interface.c)
    target_link_libraries(fst23_interface_c rmn::rmn)
    add_test(NAME fst23_interface_c COMMAND $<TARGET_FILE:fst23_interface_c>)
    add_dependencies(check fst23_interface_c)

    add_executable(fst23_interface_f fst23_interface.F90)
    target_link_libraries(fst23_interface_f rmn::rmn)
    add_test(NAME fst23_interface_f COMMAND $<TARGET_FILE:fst23_interface_f>)
    add_dependencies(check fst23_interface_f)
endif()

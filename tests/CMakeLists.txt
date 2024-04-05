if (EC_INIT_DONE LESS 2)
   include(CTest)
   add_custom_target(check COMMAND CTEST_OUTPUT_ON_FAILURE=true ${CMAKE_CTEST_COMMAND})

   if(BUILD_TESTING)

        message(STATUS "(EC) Generating librmn tests")
        
        # Helper module
        add_library(test_helper EXCLUDE_FROM_ALL rmn_test_helper.F90 fst/generate_fstd_module.F90)
        target_link_libraries(test_helper rmn::rmn)

        add_executable(clib_isalpha EXCLUDE_FROM_ALL ${CMAKE_CURRENT_SOURCE_DIR}/clib_isalpha.F90)
        target_link_libraries(clib_isalpha rmn::rmn)
        add_test(NAME clib_isalpha COMMAND ${CMAKE_CURRENT_BINARY_DIR}/clib_isalpha ${CMAKE_CURRENT_SOURCE_DIR}/spp.dict)
        add_dependencies(check clib_isalpha)

        foreach(DATATYPE float double)
            add_executable(fst_${DATATYPE} EXCLUDE_FROM_ALL fst/fst.c)
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

        add_executable(fst98_missing fst/fst98_missing.F90)
        target_link_libraries(fst98_missing rmn::rmn test_helper)
        target_compile_options(fst98_missing PRIVATE $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-diag-disable 5268>)
        add_test(NAME fst98_missing COMMAND ${CMAKE_CURRENT_BINARY_DIR}/fst98_missing)
        add_dependencies(check fst98_missing)

        add_executable(generate_fstd EXCLUDE_FROM_ALL fst/generate_fstd.F90)
        target_link_libraries(generate_fstd test_helper)

        add_executable(rsf_dump test_rsf.c)
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

        add_executable(fst24_interface_c fst/fst24_interface.c)
        target_link_libraries(fst24_interface_c rmn::rmn)
        add_test(NAME fst24_interface_c COMMAND $<TARGET_FILE:fst24_interface_c>)
        add_dependencies(check fst24_interface_c)
        set_property(TEST fst24_interface_c PROPERTY ENVIRONMENT "META_PROFPATH=${CMAKE_CURRENT_SOURCE_DIR}/../src/Meta" "META_VALIDATE=TRUE")

        add_executable(fst24_interface_f fst/fst24_interface.F90)
        target_link_libraries(fst24_interface_f rmn::rmn)
        add_test(NAME fst24_interface_f COMMAND $<TARGET_FILE:fst24_interface_f>)
        add_dependencies(check fst24_interface_f)
        set_property(TEST fst24_interface_f PROPERTY ENVIRONMENT "META_PROFPATH=${CMAKE_CURRENT_SOURCE_DIR}/../src/Meta" "META_VALIDATE=TRUE")

        add_executable(fst98_interface fst/fst98_interface.F90)
        target_link_libraries(fst98_interface rmn::rmn test_helper)
        add_test(NAME fst98_interface COMMAND $<TARGET_FILE:fst98_interface>)
        add_dependencies(check fst98_interface)

        add_executable(fstlnk fst/fstlnk.F90)
        target_link_libraries(fstlnk rmn::rmn test_helper)
        add_test(NAME fstlnk COMMAND $<TARGET_FILE:fstlnk>)
        add_dependencies(check fstlnk)

        add_executable(fst24_meta_c fst/fst24_meta.c)
        target_link_libraries(fst24_meta_c rmn::rmn)
        add_test(NAME fst24_meta_c COMMAND $<TARGET_FILE:fst24_meta_c>)
        add_dependencies(check fst24_meta_c)
        set_property(TEST fst24_meta_c PROPERTY ENVIRONMENT "META_PROFPATH=${CMAKE_CURRENT_SOURCE_DIR}/../src/Meta" "META_VALIDATE=TRUE")

        add_executable(fst24_meta_f90 fst/fst24_meta.F90)
        target_link_libraries(fst24_meta_f90 rmn::rmn)
        add_test(NAME fst24_meta_f90 COMMAND $<TARGET_FILE:fst24_meta_f90>)
        add_dependencies(check fst24_meta_f90)
        set_property(TEST fst24_meta_f90 PROPERTY ENVIRONMENT "META_PROFPATH=${CMAKE_CURRENT_SOURCE_DIR}/../src/Meta" "META_VALIDATE=TRUE")

        add_executable(fst_parallel_write fst/fst_parallel_write.c)
        target_link_libraries(fst_parallel_write rmn::rmn-ompi)
        # add_test(NAME fst_parallel_write COMMAND mpirun -n 4 $<TARGET_FILE:fst_parallel_write>)
        add_dependencies(check fst_parallel_write)
        # set_property(TEST fst_parallel_write PROPERTY ENVIRONMENT "META_PROFPATH=${CMAKE_CURRENT_SOURCE_DIR}/../src/Meta" "META_VALIDATE=TRUE")

        add_executable(fst_compression fst/fst_compression.c)
        target_link_libraries(fst_compression rmn::rmn)
        add_test(NAME fst_compression COMMAND $<TARGET_FILE:fst_compression>)
        add_dependencies(check fst_compression)

        add_executable(moduledate EXCLUDE_FROM_ALL moduledate.F90)
        target_link_libraries(moduledate rmn::rmn)
        add_test(NAME moduledate COMMAND $<TARGET_FILE:moduledate>)
        add_dependencies(check moduledate)

        add_executable(fst_ipall fst/fst_ipall.F90)
        target_link_libraries(fst_ipall rmn::rmn)
        add_test(NAME fst_ipall COMMAND $<TARGET_FILE:fst_ipall>)
        add_dependencies(check fst_ipall)

        add_executable(rsf_empty_file fst/rsf_empty_file.c)
        target_link_libraries(rsf_empty_file rmn::rmn)
        add_test(NAME rsf_empty_file COMMAND $<TARGET_FILE:rsf_empty_file>)
        add_dependencies(check rsf_empty_file)

        add_executable(fst24_read_cube_c fst/fst24_read_cube.c)
        target_link_libraries(fst24_read_cube_c rmn::rmn)
        add_test(NAME fst24_read_cube_c COMMAND $<TARGET_FILE:fst24_read_cube_c>)
        add_dependencies(check fst24_read_cube_c)

        add_executable(fst24_read_cube_f fst/fst24_read_cube.F90)
        target_link_libraries(fst24_read_cube_f rmn::rmn)
        add_test(NAME fst24_read_cube_f COMMAND $<TARGET_FILE:fst24_read_cube_f>)
        add_dependencies(check fst24_read_cube_f)

        add_executable(rsf_cat fst/rsf_cat.c)
        target_link_libraries(rsf_cat rmn::rmn)
        add_test(NAME rsf_cat COMMAND $<TARGET_FILE:rsf_cat>)
        add_dependencies(check rsf_cat)

        add_executable(fst24_search_params_f fst/fst24_search_params.F90)
        target_link_libraries(fst24_search_params_f rmn::rmn)
        add_test(NAME fst24_search_params_f COMMAND $<TARGET_FILE:fst24_search_params_f>)
        add_dependencies(check fst24_search_params_f)

    endif()
endif()

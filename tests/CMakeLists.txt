if (EC_INIT_DONE LESS 2)
    include(CTest)
    add_custom_target(check COMMAND CTEST_OUTPUT_ON_FAILURE=true ${CMAKE_CTEST_COMMAND})

    add_custom_target(test_execs)
    add_dependencies(check test_execs)

    if(BUILD_TESTING)

        message(STATUS "(EC) Generating librmn tests")

        set(TEST_SCRIPTS_SOURCE
            fst/fst_force_close.sh
            fst/fst_open_close_link.py
            readlx/get_diff.sh
            readlx/inp_readlx_02.txt
            readlx/output_readlx_02.txt
            spp.dict
            test_rmn.py
        )
        list(TRANSFORM TEST_SCRIPTS_SOURCE REPLACE "^(.+)" "${CMAKE_CURRENT_SOURCE_DIR}/\\1")
        set(TEST_SCRIPTS_BUILD ${TEST_SCRIPTS_SOURCE})
        list(TRANSFORM TEST_SCRIPTS_BUILD REPLACE "${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_CURRENT_BINARY_DIR}")

        add_custom_command(
            OUTPUT ${TEST_SCRIPTS_BUILD}
            COMMAND ${CMAKE_COMMAND} -E copy ${TEST_SCRIPTS_SOURCE} ${CMAKE_CURRENT_BINARY_DIR}/
            DEPENDS ${TEST_SCRIPTS_SOURCE}
            COMMENT "Copying test script files to test build directory")
        add_custom_target(test-script-files DEPENDS ${TEST_SCRIPTS_BUILD})
        add_dependencies(test-script-files python-files)
        add_dependencies(test_execs test-script-files)

        if (TARGET rmn::rmn-ompi)
            # Check MPI implementation to be able to give the proper flags when running tests
            find_program(RPN_MPIRUN mpirun)
            if (${RPN_MPIRUN} STREQUAL "RPN_MPIRUN-NOTFOUND")
                message(FATAL_ERROR "mpirun not found. Won't be able to configure the version.")
            else()
                execute_process(
                    COMMAND ${RPN_MPIRUN} --version 2>&1
                    COMMAND grep "Open MPI"
                    COMMAND grep -o "[0-9]\\.[0-9]"
                    OUTPUT_VARIABLE MPI_IMPLEMENTATION
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
                if (MPI_IMPLEMENTATION MATCHES "[0-9]\\.[0-9]")
                    set(EXTRA_MPI_FLAGS ${EXTRA_MPI_FLAGS} --oversubscribe)
                    if (CMAKE_Fortran_COMPILER_ID MATCHES ".*Flang")
                        set(EXTRA_MPI_FLAGS ${EXTRA_MPI_FLAGS} -x LD_LIBRARY_PATH)
                    else()
                        set(EXTRA_MPI_FLAGS ${EXTRA_MPI_FLAGS} --bind-to none)
                    endif()
                endif()
            endif()
        endif()

        function(get_test_name SOURCE_FILE TEST_NAME)
            get_filename_component(BASE_NAME ${SOURCE_FILE} NAME_WLE)
            set(${TEST_NAME} ${BASE_NAME} PARENT_SCOPE)
        endfunction(get_test_name)

        function(gen_test_target)
            set(options DISABLE)
            set(oneValueArgs MPI_NUM NAME)
            set(multiValueArgs SOURCES LIBRARIES ARGUMENTS INCLUDES DEFINES ENVIRONMENT)
            cmake_parse_arguments(GEN_TEST "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

            if (NOT DEFINED GEN_TEST_SOURCES)
                message(WARNING "Missing sources")
                return()
            endif()

            if (DEFINED GEN_TEST_NAME)
                set(TEST_TARGET ${GEN_TEST_NAME})
            else()
                list (GET GEN_TEST_SOURCES 0 FIRST_SOURCE)
                get_test_name(${FIRST_SOURCE} TEST_TARGET)
            endif()

            # The target
            add_executable(${TEST_TARGET} EXCLUDE_FROM_ALL ${GEN_TEST_SOURCES})

            # Modules
            get_target_property(LIBRARY_DIR ${TEST_TARGET} BINARY_DIR)
            set_target_properties(${TEST_TARGET} PROPERTIES Fortran_MODULE_DIRECTORY ${LIBRARY_DIR}/mod)
            target_include_directories(${TEST_TARGET} INTERFACE ${LIBRARY_DIR}/mod)
            target_include_directories(${TEST_TARGET} PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
            # target_compile_definitions(${TEST_TARGET} PRIVATE _POSIX_C_SOURCE=199309L)

            # Includes
            foreach(DIR ${GEN_TEST_INCLUDES})
                target_include_directories(${TEST_TARGET} PRIVATE ${DIR})
            endforeach()

            # Defines
            foreach(DEF ${GEN_TEST_DEFINES})
                target_compile_definitions(${TEST_TARGET} PRIVATE ${DEF})
            endforeach()

            # Libraries
            foreach(LIB ${GEN_TEST_LIBRARIES})
                target_link_libraries(${TEST_TARGET} ${LIB})
            endforeach()
            if (NOT DEFINED GEN_TEST_LIBRARIES)
                if (NOT DEFINED GEN_TEST_MPI_NUM)
                    target_link_libraries(${TEST_TARGET} rmn::rmn)
                else()
                    target_link_libraries(${TEST_TARGET} rmn::rmn-ompi)
                endif()
            endif()

            set(MPI_PREFIX "")
            if (DEFINED GEN_TEST_MPI_NUM)
                set(MPI_PREFIX ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${GEN_TEST_MPI_NUM} ${EXTRA_MPI_FLAGS})
            endif()

            if (NOT ${GEN_TEST_DISABLE})
                add_test(NAME ${TEST_TARGET} COMMAND ${MPI_PREFIX} $<TARGET_FILE:${TEST_TARGET}> ${GEN_TEST_ARGUMENTS})
            endif()

            if (DEFINED GEN_TEST_ENVIRONMENT)
                foreach(VAR ${GEN_TEST_ENVIRONMENT})
                    set(FULL_ENV "${FULL_ENV};${VAR}")
                endforeach()
                # message(WARNING "Setting env to ${FULL_ENV} for ${TEST_TARGET}")
                set_property(TEST ${TEST_TARGET} PROPERTY ENVIRONMENT "${FULL_ENV}")
            endif()

            add_dependencies(test_execs ${TEST_TARGET})
        endfunction()


        # Helper module
        add_library(test_helper STATIC EXCLUDE_FROM_ALL rmn_test_helper.F90 fst/generate_fstd_module.F90)
        target_link_libraries(test_helper rmn::rmn)
        add_executable(generate_fstd EXCLUDE_FROM_ALL fst/generate_fstd.F90)
        target_link_libraries(generate_fstd test_helper)

        # This test is weird and I need more time to understand it
        #    add_executable(wb_get EXCLUDE_FROM_ALL wb_get.c)
        #    target_link_libraries(wb_get rmn::rmn)
        #    target_include_directories(wb_get PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../src/whiteboard)
        #    add_test(NAME wb_get COMMAND ${CMAKE_CURRENT_BINARY_DIR}/wb_get)
        #    add_dependencies(test_execs wb_get)

        gen_test_target(SOURCES fst/fst.c NAME fst_float DEFINES "DATATYPE=float")
        gen_test_target(SOURCES fst/fst.c NAME fst_double DEFINES "DATATYPE=double")
        gen_test_target(SOURCES test_copy_swap.c)
        gen_test_target(SOURCES clib_isalpha.F90 ARGUMENTS ${CMAKE_CURRENT_BINARY_DIR}/spp.dict)
        gen_test_target(SOURCES wb_read.F90 ARGUMENTS ${CMAKE_CURRENT_BINARY_DIR}/spp.dict)
        gen_test_target(SOURCES wb_putget.F90 INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/../src/whiteboard)
        gen_test_target(SOURCES wb_test.c INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/../src/whiteboard)
        gen_test_target(SOURCES wb_test.F90 NAME wb_test_f90 INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/../src/whiteboard)
        gen_test_target(SOURCES prog_filename.c)
        gen_test_target(SOURCES gmm.F90)
        gen_test_target(SOURCES c_ccard.c)
        gen_test_target(SOURCES fst/fst98_missing.F90 LIBRARIES rmn::rmn test_helper)
        gen_test_target(SOURCES serializer.F90)
        gen_test_target(DISABLE SOURCES serializer_large.F90)
        gen_test_target(SOURCES fst/fst24_interface.c NAME fst24_interface_c ENVIRONMENT "META_PROFPATH=${CMAKE_CURRENT_SOURCE_DIR}/../src/Meta" "META_VALIDATE=TRUE")
        gen_test_target(SOURCES fst/fst24_interface.F90 NAME fst24_interface_f ENVIRONMENT "META_PROFPATH=${CMAKE_CURRENT_SOURCE_DIR}/../src/Meta" "META_VALIDATE=TRUE")
        gen_test_target(SOURCES fst/fst98_interface.F90 LIBRARIES test_helper)
        gen_test_target(SOURCES fst/fstlnk.F90 LIBRARIES test_helper)
        gen_test_target(SOURCES fst/fst24_meta.c ENVIRONMENT "META_PROFPATH=${CMAKE_CURRENT_SOURCE_DIR}/../src/Meta" "META_VALIDATE=TRUE")
        gen_test_target(SOURCES fst/fst24_meta.F90 NAME fst24_meta_f ENVIRONMENT "META_PROFPATH=${CMAKE_CURRENT_SOURCE_DIR}/../src/Meta" "META_VALIDATE=TRUE")
        gen_test_target(SOURCES fst/fst24_meta_find.c ENVIRONMENT "META_PROFPATH=${CMAKE_CURRENT_SOURCE_DIR}/../src/Meta" "META_VALIDATE=TRUE")
        gen_test_target(SOURCES fst/fst_compression.c)
        gen_test_target(SOURCES moduledate.F90)
        gen_test_target(SOURCES fst/fst_ipall.F90)
        gen_test_target(SOURCES fst/rsf_empty_file.c)
        gen_test_target(SOURCES fst/fst24_read_cube.c)
        gen_test_target(SOURCES fst/fst24_read_cube.F90 NAME fst24_read_cube_f)
        gen_test_target(SOURCES fst/rsf_cat.c)
        gen_test_target(SOURCES fst/fst24_search_params.F90)
        gen_test_target(SOURCES fst/fst24_delete.c)
        gen_test_target(SOURCES fst/fst24_delete.F90 NAME fst24_delete_f)
        gen_test_target(SOURCES fst/fst_write_in_read_only.c)
        gen_test_target(SOURCES fst/excdes.c)
        gen_test_target(SOURCES fst/fst_cross_link.F90)
        gen_test_target(SOURCES fst/fst24_by_index.c)
        gen_test_target(SOURCES fst/fst_rewrite_with_filter.c)
        gen_test_target(SOURCES fst/fstouv.c)
        gen_test_target(SOURCES fst/fst24_skip_descriptors.c)
        gen_test_target(SOURCES fst/fst24_read_into_bigger_size.c)
        gen_test_target(SOURCES circular_buffer/cb_unaligned_input.c)
        gen_test_target(SOURCES circular_buffer/circular_buffer_single_thread.F90)
        gen_test_target(SOURCES fst/fst_edge_case_1.c)
        gen_test_target(SOURCES fst/fst24_record_offset_size.c)
        gen_test_target(SOURCES fst/fstinf_datev.c)
        gen_test_target(SOURCES packers/compact_integer.c)
        gen_test_target(SOURCES interp.c)
        gen_test_target(SOURCES fst/fst98_edit_dir.c)
        gen_test_target(SOURCES fst/fst24_rewrite_meta.c ENVIRONMENT "META_PROFPATH=${CMAKE_CURRENT_SOURCE_DIR}/../src/Meta")
        gen_test_target(SOURCES fst/fst_rewrite_skip.c)
        gen_test_target(SOURCES fst/fst24_no_dateo.c)
        gen_test_target(SOURCES fst/fst24_flush.c)
        gen_test_target(SOURCES fst/fst_data_size.c)
        gen_test_target(SOURCES fst/fst_large_record.c)
        gen_test_target(SOURCES fst/fst24_data_map.c)
        gen_test_target(SOURCES fst/fst_wildcard.c)
        gen_test_target(SOURCES fst/fst24_open_link.F90)
        gen_test_target(DISABLE SOURCES fst/fst_old_file.c)
        gen_test_target(DISABLE SOURCES fst/fst_ignore_msglvl.c)
        gen_test_target(DISABLE SOURCES interp_interface.F90)

        gen_test_target(SOURCES fst/samples/fst24_open_close.F90)
        gen_test_target(SOURCES fst/samples/fst24_open_close.c NAME fst24_open_close_c)
        gen_test_target(SOURCES fst/samples/fst24_create_and_write.F90)
        gen_test_target(SOURCES fst/samples/fst24_create_and_write.c NAME fst24_create_and_write_c)

        if(IS_DIRECTORY $ENV{ECCI_DATA_DIR})
            gen_test_target(SOURCES burp.F90 NAME burp_f ENVIRONMENT "CMCCONST=$ENV{ECCI_DATA_DIR}/rpn-tools")
            gen_test_target(SOURCES burp_c.c ENVIRONMENT "CMCCONST=$ENV{ECCI_DATA_DIR}/rpn-tools")
            gen_test_target(SOURCES fst/rsf_read_old.c ENVIRONMENT "ECCI_DATA_DIR=$ENV{ECCI_DATA_DIR}")
        else()
            message(WARNING "ECCI_DATA_DIR environment variable does not contain a valid path to a directory; skipping burp tests!")
        endif()

        if (TARGET rmn::rmn-ompi)
            gen_test_target(SOURCES fst/fst_parallel_write.c MPI_NUM 4 ENVIRONMENT "FST_OPTIONS=SEGMENT_SIZE_MB=10" "META_PROFPATH=${CMAKE_CURRENT_SOURCE_DIR}/../src/Meta" "META_VALIDATE=TRUE")
            gen_test_target(SOURCES fst/fst_parallel_write_threads.c LIBRARIES rmn::rmn-ompi ENVIRONMENT "FST_OPTIONS=SEGMENT_SIZE_MB=10" "META_PROFPATH=${CMAKE_CURRENT_SOURCE_DIR}/../src/Meta" "META_VALIDATE=TRUE")
            gen_test_target(SOURCES fst/multi_open.c MPI_NUM 4)
            gen_test_target(SOURCES fst/fst_multithread.c LIBRARIES rmn::rmn-ompi)
            gen_test_target(SOURCES fnom.c LIBRARIES rmn::rmn-ompi)

            gen_test_target(SOURCES circular_buffer/circular_buffer_fill.c MPI_NUM 8)
            gen_test_target(SOURCES circular_buffer/circular_buffer_concurrent.F90 MPI_NUM 8)
            gen_test_target(SOURCES circular_buffer/circular_buffer_timeout.F90 MPI_NUM 2)
        endif()

        add_executable(readlx_base EXCLUDE_FROM_ALL readlx/readlx_base.F90)
        target_link_libraries(readlx_base rmn::rmn)
        # file(READ ${CMAKE_CURRENT_SOURCE_DIR}/readlx/output_readlx_02.txt EXPECTED_OUTPUT_02)
        add_test(NAME readlx_base COMMAND sh -c "env -u APP_VERBOSE_COLOR $<TARGET_FILE:readlx_base> ${CMAKE_CURRENT_BINARY_DIR}/inp_readlx_02.txt > ${CMAKE_CURRENT_BINARY_DIR}/output_readlx_02_current.txt")
        add_test(NAME readlx_compare_02
                 COMMAND ${CMAKE_CURRENT_BINARY_DIR}/get_diff.sh
                            ${CMAKE_CURRENT_BINARY_DIR}/output_readlx_02_current.txt
                            ${CMAKE_CURRENT_BINARY_DIR}/output_readlx_02.txt
                 DEPENDS readlx_base)
        add_dependencies(test_execs readlx_base)

        # Python tests
        if (TARGET rmn::rmn-shared)
            find_package(Python3 REQUIRED COMPONENTS Interpreter)
            add_dependencies(test_execs rmn::rmn-shared)
            set(set_pythonpath "PYTHONPATH=${CMAKE_BINARY_DIR}/python\${PYTHONPATH:+:\${PYTHONPATH}}")
            add_test( NAME python_import_package COMMAND bash -c "${set_pythonpath} python3 -c 'import rmn'")
            add_test( NAME python_unit_tests COMMAND bash -c "${set_pythonpath} python3 ${CMAKE_CURRENT_BINARY_DIR}/test_rmn.py")
            add_test( NAME python_fst_open_close_link COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/fst_open_close_link.py )
            set_property(
                TEST python_fst_open_close_link PROPERTY
                ENVIRONMENT "PYTHONPATH=${CMAKE_BINARY_DIR}/python")
        endif()

        add_executable(gen_abort_file EXCLUDE_FROM_ALL fst/gen_abort_file.c)
        target_link_libraries(gen_abort_file rmn::rmn)
        add_dependencies(test_execs gen_abort_file)
        add_dependencies(gen_abort_file fst_force_close fst_check)
        add_test(
            NAME fst_force_close
            COMMAND ${CMAKE_CURRENT_BINARY_DIR}/fst_force_close.sh
                    $<TARGET_FILE:gen_abort_file> $<TARGET_FILE:fst_check> $<TARGET_FILE:fst_force_close>
        )

        if(BUILD_PYTHON)
            add_dependencies(test_execs _rmn)
            add_test( NAME python_import_package_compiled COMMAND bash -c "${set_pythonpath} ${Python_EXECUTABLE} -c 'import compiled-rmn'")
        endif()

        # Stop gap measure to delete files generated by the testing with make clean
        # It would be nicer if we could use wildcards, it doesn't look like it's supported by CMake
        # Furthermore, the files might not even exist when cmake is executed
        # The ideal solution would be to add a shell command to the clean target, but
        # despite what I've seen online, adding a dependency to the "clean" target doesn't work
        set_target_properties(check PROPERTIES
            ADDITIONAL_CLEAN_FILES "1.rsf;2.rsf;abcde;abcde.rsf;abcde.xdf;abc.test;abort.rsf;abort.xdf;\
BRPFIL1;brpfil1;BRPFIL2;brpfil2;by_index.fst;compression.rsf;compression.xdf;cube.fst;\
delete.rsf;delete.xdf;diff_size.rsf;diff_size.xdf;dummy.fst;dummy.rsf;edge_case_1.rsf;\
edge_case_1.xdf;excdes.rsf;excdes.xdf;ezscint.fst;file1.fst;file2.fst;flush.rsf;flush.xdf;\
fst123_meta.fst;fst24_interface1.fst;fst24_interface2.fst;fst24_interface3.fst;\
fst24_rewrite_meta.rsf;fst24_rewrite_meta_solution.rsf;fst24_rewrite_meta_solution.xdf;\
fst24_rewrite_meta.xdf;fst98_ipall_module.smod;fst_cross_link_module.smod;fst_edit_dir.rsf;\
fst_edit_dir_solution.rsf;fst_edit_dir_solution.xdf;fst_edit_dir.xdf;fstinf_datev.rsf;\
fstinf_datev.xdf;fst_interface_2.fst;fst_interface.fst;fst_link_mod.smod;ip1.fst;ip2.fst;\
missing.fst;my_file.fst;no_dateo.rsf;no_dateo.xdf;output_readlx_02_current.txt;read_only.fst;\
record_offset_size.rsf;record_offset_size.xdf;rewrite_skip.rsf;rewrite_skip.xdf;rsf1.fst;rsf2.fst;\
search_params.fst;skip_descriptors.rsf;skip_descriptors.xdf;test_double.fst;test_float.fst;\
test_fst24_meta_find.fst;test_fst98_interface_module.smod;Whiteboard.ckpt;xdf1.fst;xdf2.fst"
        )
      endif()
endif()


integer function gnthread()
	 use app
	 implicit none
	 character(len=15) omp_var_S
	 character(len=4) result_S
     integer resu2	

	 omp_var_S='OMP_NUM_THREADS'

	 call getenvc(omp_var_S, result_S)

	 if(result_S == '') then
	   gnthread = 1
	   return
	 endif
	 read(result_S(1:3),10) gnthread
 10	 format(I3)
     read(result_S(1:4),20) resu2
 20  format(I4)

     if(resu2 .ne. gnthread) then
        call lib_log(APP_LIBRMN,APP_ERROR,'gnthread: Invalid value for OMP_NUM_THREADS')
	    gnthread = -1
	 endif
	 return

	 end


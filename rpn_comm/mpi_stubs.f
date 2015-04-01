	subroutine MPI_INITIALIZED(mpi_started,ierr)
	implicit none
*
	include 'rpn_comm.h'
	include 'mpif.h'
*
	logical mpi_started
	integer ierr
*
	data pe_tot /-1/
*
	ierr=MPI_SUCCESS
	mpi_started=pe_tot .ne. -1
	return
	end
	subroutine MPI_COMM_SIZE(comm,pe_tot,ierr)
	implicit none
*
	include 'mpif.h'
*
	integer comm,pe_tot,ierr
	ierr=MPI_SUCCESS
	pe_tot=1
	return
	end
	subroutine MPI_COMM_RANK(comm,pe_me,ierr)
	implicit none
*
	include 'mpif.h'
*
	integer comm,pe_me,ierr
	ierr=MPI_SUCCESS
	pe_me=0
	return
	end
	subroutine MPI_init(ierr)
	implicit none
*
	include 'rpn_comm.h'
	include 'mpif.h'
*
	integer ierr
	ierr=0
	if(pe_tot .ne. -1) then
	  print *,'MPI_init called twice, ... ABORTING'
	  call ABORT
	endif
	pe_tot=0
	return
	end
	subroutine MPI_bcast(i,j,k,l,m,ierr)
	implicit none
*
	include 'mpif.h'
*
	integer i,j,k,l,m,ierr
	ierr=MPI_SUCCESS
	return
	end
	subroutine MPI_barrier(i,ierr)
	implicit none
	include 'mpif.h'
	integer i,ierr
	ierr=MPI_SUCCESS
	return
	end
	subroutine MPI_allreduce(send,recv,ni,l,m,n,ierr)
	implicit none
*
	include 'mpif.h'
*
	integer i,l,m,n,ierr
	integer send(*),recv(*),ni
	do i=1,ni
	  recv(i)=send(i)
	enddo
	ierr=MPI_SUCCESS
	return
	end
	subroutine MPI_reduce(send,recv,ni,l,m,n,o,ierr)
	implicit none
*
	include 'mpif.h'
*
	integer i,l,m,n,o,ierr
	integer send(*),recv(*),ni
	do i=1,ni
	  recv(i)=send(i)
	enddo
	ierr=MPI_SUCCESS
	return
	end
	subroutine MPI_finalize
	return
	end
	subroutine MPI_recv
	print *,'MPI_recv called, ... ABORTING'
	call ABORT
	return
	end
	subroutine MPI_send
	print *,'MPI_send called, ... ABORTING'
	call ABORT
	return
	end
	subroutine MPI_ssend
	print *,'MPI_ssend called, ... ABORTING'
	call ABORT
	return
	end
	REAL*8 function  MPI_wtime()
	MPI_wtime=0
	return
	end
	subroutine MPI_comm_split(i,j,k,newcomm,ierr)
	implicit none
	integer i,j,k,newcomm,ierr
*
	include 'mpif.h'
*
	newcomm=-1
	ierr=MPI_SUCCESS
	return
	end
	subroutine MPI_comm_group(i,group,ierr)
	implicit none
	integer i,group,ierr
*
	include 'mpif.h'
*
	group=-1
	ierr=MPI_SUCCESS
	return
	end
	subroutine MPI_alltoall
	print *,'MPI_alltoall called, ... ABORTING'
	call ABORT
	return
	end
	subroutine MPI_sendrecv
	print *,'MPI_sendrecv called, ... ABORTING'
	call ABORT
	return
	end
	subroutine MPI_allgather
	print *,'MPI_allgather called, ... ABORTING'
	call ABORT
	return
	end
	subroutine MPI_gather
	print *,'MPI_gather called, ... ABORTING'
	call ABORT
	return
	end

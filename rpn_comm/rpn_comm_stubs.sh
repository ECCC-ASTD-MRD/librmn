#!/bin/sh
cat <<EOT >fortran_stubs.f
	subroutine MPI_abort
	write(6,*) 'MPI_abort is called, ABORT'
	call ABORT
	return
	end
*
	subroutine MPI_allgather
	write(6,*) 
     %         'MPI_allgather not authorized in non-mpi mode, ABORT'
	call ABORT
	return
	end
*
	subroutine MPI_allgatherv
	write(6,*) 
     %         'MPI_allgatherv not authorized in non-mpi mode, ABORT'
	call ABORT
	return
	end
*
	subroutine MPI_allreduce(send,recv,ni,l,m,n,ierr)
	implicit none
*
	include 'mpif.h'
*
	integer MPI_STUBS_length
	integer i,l,m,n,ierr
	integer send(*),recv(*),ni
	do i=1,ni*MPI_STUBS_length(l)
	  recv(i)=send(i)
	enddo
	ierr=MPI_SUCCESS
	return
	end
*
	subroutine MPI_alltoall
	write(6,*)
     %         'MPI_alltoall not authorized in non-mpi mode, ABORT'
	call ABORT
	return
	end
*
	subroutine MPI_alltoallv
	write(6,*)
     %         'MPI_alltoallv not authorized in non-mpi mode, ABORT'
	call ABORT
	return
	end
*
	subroutine MPI_barrier(i,ierr)
	implicit none
	include 'mpif.h'
	integer i,ierr
	ierr=MPI_SUCCESS
	return
	end
*
	subroutine MPI_bcast(i,j,k,l,m,ierr)
	implicit none
*
	include 'mpif.h'
*
	integer i,j,k,l,m,ierr
	ierr=MPI_SUCCESS
	return
	end
*
	subroutine mpi_comm_create(pe_indomm,pe_gr_blocmaster, 
     &            pe_blocmaster, ierr)
	implicit none
	include 'mpif.h'
	integer pe_indomm,pe_gr_blocmaster, 
     &            pe_blocmaster, ierr
	pe_blocmaster = 0
	ierr = MPI_SUCCESS
	return
	end
*
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
*
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
*
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
*
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
*
	subroutine MPI_finalize
	return
	end
*
	subroutine MPI_gather(a, cnt, b, c, cnt2, d, e, f,ierr )
	implicit none
	include 'mpif.h'
	integer MPI_STUBS_length
	integer a(*),cnt,b,c(*),cnt2,d,e,f,ierr,i
	do i=1,min(cnt,cnt2)*MPI_STUBS_length(b)
	   c(i)=a(i)
	enddo
	ierr=MPI_SUCCESS
	return
	end
*
	subroutine MPI_gatherv(a, cnt, b, c, cnt2s, displ, d, e, f,ierr )
	implicit none
	include 'mpif.h'
	integer MPI_STUBS_length
	integer a(*),cnt,b,c(*),cnt2s(*),cnt2,d,e,f,ierr,i,displ(*)
        cnt2=cnt2s(1)
	do i=1,min(cnt,cnt2)*MPI_STUBS_length(b)
	   c(i+displ(1))=a(i+displ(1))
	enddo
	ierr=MPI_SUCCESS
	return
	end
*
	subroutine MPI_get_count
	write(6,*) 
     %         'MPI_get_count not authorized in non-mpi mode, ABORT'
	call ABORT
	return
	end
*
	subroutine MPI_GROUP_incl(pe_gr_wcomm, pe_dommtot,proc_indomm,
     %   	   pe_gr_indomm,ierr)
	implicit none
	include 'mpif.h'
	integer pe_gr_wcomm, pe_dommtot,proc_indomm,
     %   	   pe_gr_indomm,ierr
	pe_gr_indomm = 0
	ierr = MPI_SUCCESS
	return
	end
*
	subroutine MPI_GROUP_RANK(comm,pe_me,ierr)
	implicit none
*
	include 'mpif.h'
*
	integer comm,pe_me,ierr
	ierr=MPI_SUCCESS
	pe_me=0
	return
	end
*
	subroutine MPI_GROUP_SIZE(comm,pe_tot,ierr)
	implicit none
*
	include 'mpif.h'
*
	integer comm,pe_tot,ierr
	ierr=MPI_SUCCESS
	pe_tot=1
	return
	end
*
	subroutine MPI_init(ierr)
	implicit none
	integer pe_tot
	save pe_tot
*
	include 'mpif.h'
*
	integer ierr
	data pe_tot / -1/
	ierr=0
c	if(pe_tot .ne. -1) call ABORT
	pe_tot=0
	return
	end
*
	subroutine MPI_INITIALIZED(mpi_started,ierr)
	implicit none
*
	include 'mpif.h'
*
	logical mpi_started
	integer ierr
*
	ierr=MPI_SUCCESS
	mpi_started=.false.
	return
	end
*
	subroutine MPI_irecv
	write(6,*) 
     %         'MPI_irecv not authorized in non-mpi mode, ABORT'
	call ABORT
	return
	end
*
	subroutine MPI_isend
	write(6,*) 
     %         'MPI_isend not authorized in non-mpi mode, ABORT'
	call ABORT
	return
	end
*
	subroutine MPI_recv
	write(6,*) 
     %         'MPI_recv not authorized in non-mpi mode, ABORT'
	call ABORT
	return
	end
*
	subroutine MPI_reduce(send,recv,ni,l,m,n,o,ierr)
	implicit none
*
	include 'mpif.h'
*
	integer MPI_STUBS_length
	integer i,l,m,n,o,ierr
	integer send(*),recv(*),ni
	do i=1,ni*MPI_STUBS_length(l)
	  recv(i)=send(i)
	enddo
	ierr=MPI_SUCCESS
	return
	end
*
	subroutine MPI_scatterv
	write(6,*) 
     %         'MPI_scatterv not authorized in non-mpi mode, ABORT'
	call ABORT
	return
	end
*
	subroutine MPI_send
	write(6,*) 
     %         'MPI_send not authorized in non-mpi mode, ABORT'
	call ABORT
	return
	end
*
	subroutine MPI_sendrecv
	write(6,*) 
     %         'MPI_sendrecv not authorized in non-mpi mode, ABORT'
	call ABORT
	return
	end
*
	subroutine MPI_ssend
	write(6,*) 
     %         'MPI_ssend not authorized in non-mpi mode, ABORT'
	call ABORT
	return
	end
*
	integer function MPI_STUBS_length(itype)
	implicit none
	include 'mpif.h'
	integer itype

	MPI_STUBS_length=0

	if(itype .eq. MPI_DOUBLE_PRECISION) MPI_STUBS_length=2
	if(itype .eq. MPI_2DOUBLE_PRECISION) MPI_STUBS_length=2
	if(itype .eq. MPI_REAL8) MPI_STUBS_length=2
	if(itype .eq. MPI_INTEGER8) MPI_STUBS_length=2

	if(itype .eq. MPI_LOGICAL) MPI_STUBS_length=1

	if(itype .eq. MPI_INTEGER) MPI_STUBS_length=1
	if(itype .eq. MPI_INTEGER4) MPI_STUBS_length=1
	if(itype .eq. MPI_2INTEGER) MPI_STUBS_length=1
	if(itype .eq. MPI_REAL) MPI_STUBS_length=1
	if(itype .eq. MPI_REAL4) MPI_STUBS_length=1
	if(itype .eq. MPI_2REAL) MPI_STUBS_length=1

        if(MPI_STUBS_length.eq.0)then
	  write(6,*)'MPI_STUBS_length ERROR: unrecognized type'
	  call abort
	endif
	return
	end
*
	subroutine MPI_wait
	return
	end
*
	subroutine MPI_waitall
	return
	end

*
	REAL*8 function  MPI_wtime()
	MPI_wtime=0
	return
	end
*
EOT
cat <<EOT >c_stubs.c
#include <stdio.h>
#include <mpi.h>
int MPI_Comm_rank(MPI_Comm comm, int *rank){
   *rank = 0;
   return(MPI_SUCCESS);
}
int MPI_Comm_size(MPI_Comm comm, int *size){
   *size = 1;
   return(MPI_SUCCESS);
}
int MPI_Allgather(void *outx, int nout, MPI_Datatype outtype, void *inx, int nin, MPI_Datatype intype, MPI_Comm comm){
   int *out = outx;
   int *in = inx;
   *out = *in;
   if( nin != 1 || nout != 1 || outtype != MPI_INTEGER || intype != MPI_INTEGER ) {
      fprintf(stderr,"ERROR: MPI_Allgather, number of elements is not one or type is not MPI_INTEGER\n");
      exit(1);
   }
   return(MPI_SUCCESS);
}
EOT
if [ "$1" != "-full" ] ; then
cat <<EOT >fortran_stubs.f
      subroutine no_mpi_ftn_stubs
      return
      end
EOT
cat <<EOT >c_stubs.c
int no_mpi_c_stubs(){
   return(0);
}
EOT
fi
r.compile -mpi -src fortran_stubs.f c_stubs.c
echo =====================================
echo
echo fortran_stubs.o and c_stubs.o created
echo
echo =====================================
nm fortran_stubs.o  c_stubs.o | grep " T "
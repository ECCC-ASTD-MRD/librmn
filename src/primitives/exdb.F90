! RMNLIB - Library of useful routines for C and FORTRAN programming
! Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
!                          Environnement Canada
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation,
! version 2.1 of the License.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the
! Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.


!> \file


module exdb_helper
    implicit none

    real, save :: t1
end module


!> Print start of execution box
integer function exdb(in_titre, revis, flag)
    implicit none

    character(len = *) :: in_titre, revis, flag

    integer, external :: exdbplus

    character(len = 90) :: unusedstring
    exdb = exdbplus(in_titre, revis, flag, unusedstring, 0)
end


!> Print start of execution box
integer function exdbplus(in_titre, revis, flag, supp, nsup)
    use exdb_helper, only: t1
#if __INTEL_LLVM_COMPILER >= 20250001
    use ifport, only: fdate
#endif
    implicit none

    !> Number of extra information lines
    integer, intent(in) :: nsup
    !> Title. Only the first 90 characters will be printed
    character(len = *), intent(in) :: in_titre
    !> Product version or end message when called from exfin
    character(len = *), intent(in) :: revis
    !> Unused. Kept for backward compatibility
    character(len = *), intent(in) :: flag
    !> Extra information lines
    character(len = *), intent(in) :: supp(nsup)

    character(len = 24) :: cdatim
    character(len = 105) :: version, titre, tempstring
    integer :: i

    external :: rmnlib_version

    character(*), parameter :: sup_fmt = "(3x,'*',107X,'*',/3x,'*',2x,a105,'*')"
    character(*), parameter :: box_top_fmt = "('1', &
        &/,3X,'*',107('*'),'*', &
        &/,3X,'*',107X,'*', &
        &/,3X,'*',3X,A57,10X,A10,27X,'*', &
        &/,3X,'*',107X,'*', &
        &/,3X,'*',1X,A105,1X,'*', &
        &/,3X,'*',107X,'*', &
        &/,3X,'*',107X,'*', &
        &/,3X,'*',3X,A24,46X,34X,'*')"
    character(*), parameter :: box_bottom_fmt = "(3X,'*',107X,'*', &
        &/,3X,'*',3X,A20,84X,'*', &
        &/,3X,'*',107X,'*', &
        &/,3X,'*',107('*'),'*')"

    ! Suppress unused argument warning without changing the interface
    if (flag == 'a') continue

    titre = ' '
    titre(1:min(len(in_titre), 90)) = in_titre(1:min(len(in_titre), 90))
    call fdate(cdatim)
    ! Obtenir la version de rmnlib utilisee
    call rmnlib_version(version, .false.)
    write(*, box_top_fmt) titre, revis, version, cdatim
    do i = 1, nsup
        tempstring = supp(i)
        write(*, sup_fmt) tempstring
    enddo
    write(*, box_bottom_fmt) 'BEGIN  EXECUTION     '

    call cpu_time(t1)
    exdbplus = 0
end


!> Print end of execution box
integer function exfin(in_titre, revis, flag)
    use exdb_helper, only: t1
#if __INTEL_LLVM_COMPILER >= 20250001
    use ifport, only: fdate
#endif
    implicit none

    !> Title. Only the first 90 characters will be printed
    character(len = *), intent(in) :: in_titre
    !> End message when called from exfin
    character(len = *), intent(in) :: revis
    !> Unused. Kept for backward compatibility
    character(len = *), intent(in) :: flag

    external :: flush_stdout

    character(*), parameter :: endBox_fmt = "( &
        &/,3X,'*',107('*'),'*', &
        &/,3X,'*',107X,'*', &
        &/,3X,'*',3X,A57,3X,A10,34X,'*', &
        &/,3X,'*',107X,'*', &
        &/,3X,'*',3X,A24,46X,34X,'*', &
        &/,3X,'*',107X,'*', &
        &/,3X,'*',3X,A20,84X,'*', &
        &/,3X,'*',107X,'*', &
        &/,3X,'*',3X,'CP SECS = ',F10.3,84X,'*', &
        &/,3X,'*',107X,'*', &
        &/,3X,'*',107('*'),'*')"

    real :: t2
    character(len = 105) :: titre
    character(len = 24) :: cdatim

    ! Suppress unused argument warning without changing the interface
    if (flag == 'a') continue

    call flush_stdout()
    titre = ' '
    titre(1:min(len(in_titre), 90)) = in_titre(1:min(len(in_titre), 90))
    call fdate(cdatim)
    call cpu_time(t2)
    write(*, endBox_fmt) titre, revis, cdatim, 'END EXECUTION       ', T2 - T1

    exfin = 0
end

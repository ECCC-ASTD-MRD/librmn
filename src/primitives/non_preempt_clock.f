*/* RMNLIB - Library of useful routines for C and FORTRAN programming
* * Copyright (C) 1975-2001  Division de Recherche en Prevision Numerique
* *                          Environnement Canada
* *
* * This library is free software; you can redistribute it and/or
* * modify it under the terms of the GNU Lesser General Public
* * License as published by the Free Software Foundation,
* * version 2.1 of the License.
* *
* * This library is distributed in the hope that it will be useful,
* * but WITHOUT ANY WARRANTY; without even the implied warranty of
* * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* * Lesser General Public License for more details.
* *
* * You should have received a copy of the GNU Lesser General Public
* * License along with this library; if not, write to the
* * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
* * Boston, MA 02111-1307, USA.
* */
       module rmn_clock_helper
           use rmn_common
           implicit none
           save

           real(kind = real64) :: the_clock = 0.0
           integer :: is_running = 0
       end module rmn_clock_helper

       real(kind = real64) function non_preempt_clock()
           use rmn_common
!
!       read the current value of the non preempt/suspend clock
!
       non_preempt_clock = the_clock
       return
       end

       subroutine run_non_preempt_clock
          use app
          use rmn_common
          use rmn_clock_helper
!
!       auxiliary thread used by the non preempt/suspend clock
!
       real(kind = real64) :: time1, time2, f_gettimeofday
       real(kind = real64) :: sleep_tick, elapsed
!
!       set micro sleep interval
!       set running flag
!
        sleep_tick = 1.0
       is_running = 1
       write(app_msg,*) 'non_preempt_clock: Clock started, id=',id_thread()
       call lib_log(APP_LIBRMN,APP_INFO,app_msg)
       time1=f_gettimeofday()
1      call micro_sleep(sleep_tick)
       time2=f_gettimeofday()
       elapsed=time2-time1
!
!       if elapsed is more than 15 sleep_tick, probably 
!       suspended/preempted
!       do not increase the running time counter
!
       if(elapsed .lt. sleep_tick*15.0) the_clock=the_clock+elapsed
       time1=time2
!
!       if run flag has been zeroed, stop
!
       if(is_running .eq. 1) goto 1
       write(app_msg,*) 'non_preempt_clock: Clock stopped'
       call lib_log(APP_LIBRMN,APP_INFO,app_msg)
       return
       end
       subroutine do_non_preempt_clock(start_stop)
           use rmn_common
           use rmn_clock_helper
!
!       start / stop non preemption real time counter
!       start_stop=1 means start, start_stop=0 means stop
!
       integer, save       :: running = 0
       integer, save       :: taskid = 0
       integer             :: start_stop
       integer             :: create_thread, join_thread
       external            :: create_thread, join_thread
       external            :: run_non_preempt_clock

       if(start_stop .eq. 1) then  ! start the counting thread
         if(running .eq.1) return
         is_running = 1
         taskid=create_thread(run_non_preempt_clock,0)
         running=1 ! set running flag to started
       else
         if(running .eq.0) return
         is_running=0 ! tell counting thread to stop
         ! wait for counting thread to terminate
         running=join_thread(taskid)
         running=0 ! set running flag to stopped
       endif
       return
       end

#=======================================================
# Read the timebase register and return the result.
# For 64-bit powerpc in 64-bit mode.
#
# C example:
#   long long tbvalue;
#   time_base(&tbvalue);
#
# Fortran example: 
#   integer *8 tbvalue
#   tbvalue = time_base()
#=======================================================

.globl	.time_base
.globl	.time_base_

.globl  .hpm_count_1
.globl  .hpm_count_1_
.globl  .hpm_count_2
.globl  .hpm_count_2_
.globl  .hpm_count_3
.globl  .hpm_count_3_
.globl  .hpm_count_4
.globl  .hpm_count_4_
.globl  .hpm_count_5
.globl  .hpm_count_5_
.globl  .hpm_count_6
.globl  .hpm_count_6_
.globl  .hpm_count_7
.globl  .hpm_count_7_
.globl  .hpm_count_8
.globl  .hpm_count_8_

.globl  .hpm_counters
.globl  .hpm_counters_

.globl	time_base{DS}               
.llong  time_base{PR}

.machine  "ppc64"

.csect    time_base{PR}, 4    

.time_base:
.time_base_:	     #  integer*8 = time_base()
   mftb    3         # read timebase register into gr3 (return value)
   bclr    20,0      # return
.llong     0

.hpm_count_1:	     #  integer*4 = hpm_count_1()
.hpm_count_1_:
   mfspr   3,771     # read counter set 1
   bclr    20,0      # return
.llong     0

.hpm_count_2:
.hpm_count_2_:
   mfspr   3,772     # read counter set 2
   bclr    20,0      # return
.llong     0

.hpm_count_3:
.hpm_count_3_:
   mfspr   3,773     # read counter set 3
   bclr    20,0      # return
.llong     0

.hpm_count_4:
.hpm_count_4_:
   mfspr   3,774     # read counter set 4
   bclr    20,0      # return
.llong     0

.hpm_count_5:
.hpm_count_5_:
   mfspr   3,775     # read counter set 5
   bclr    20,0      # return
.llong     0

.hpm_count_6:
.hpm_count_6_:
   mfspr   3,776     # read counter set 6
   bclr    20,0      # return
.llong     0

.hpm_count_7:
.hpm_count_7_:
   mfspr   3,777     # read counter set 7
   bclr    20,0      # return
.llong     0

.hpm_count_8:
.hpm_count_8_:
   mfspr   3,778     # read counter set 8
   bclr    20,0      # return
.llong     0

.hpm_counters:	     # integer*4 = hpm_counters(values)   integer *4 values(8)
.hpm_counters_:
   mfspr   4,771                  # performance counter 1
   mfspr   5,772                  # performance counter 2
   mfspr   6,773                  # performance counter 3
   mfspr   7,774                  # performance counter 4
   mfspr   8,775                  # performance counter 5
   mfspr   9,776                  # performance counter 6
   mfspr   10,777                 # performance counter 7
   mfspr   11,778                 # performance counter 8
   stw     4,0(3)
   stw     5,4(3)
   stw     6,8(3)
   stw     7,12(3)
   stw     8,16(3)
   stw     9,20(3)
   stw     10,24(3)
   stw     11,28(3)
   or      3,8,8                # return counter 5 as function value
   bclr    20,0      # return
.llong     0


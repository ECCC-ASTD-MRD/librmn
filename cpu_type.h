/* capability flags used by cpu_type.c */
#define FLAG_SSE    1
#define FLAG_SSE2   2
#define FLAG_SSE3   4
#define FLAG_SSE4   8
#define FLAG_AVX   16
#define FLAG_AVX2  32
#define FLAG_FMA   64
#define FLAG_BMI  128

#if defined(IN_FORTRAN_CODE)
  interface
    function cpu_has_feature(feature) result(status)
      import :: C_INT
      integer(C_INT), intent(IN), value :: feature
      integer(C_INT) :: status
    end function cpu_has_feature

    function get_cpu_id() result(id)
      import :: C_INT
      integer(C_INT) :: id
    end function get_cpu_id

    function get_cpu_freq() result(freq)
      import C_INT64_T
      integer(C_INT64_T) :: freq
    end function get_cpu_freq

    function rdtsc() result(tsc)
      import C_INT64_T
      integer(C_INT64_T) :: tsc
    end function rdtsc

    function rdtscp() result(tsc)
      import C_INT64_T
      integer(C_INT64_T) :: tsc
    end function rdtscp

    function wall_clock_seconds(ticks) result(seconds)
      import C_INT64_T, C_DOUBLE
      integer(C_INT64_T), intent(IN), value :: ticks
      real(C_DOUBLE) :: seconds
    end function wall_clock_seconds

    function rdtscp_seconds() result(seconds)
      import C_DOUBLE
      real(C_DOUBLE) :: seconds
    end function rdtscp_seconds

    function rdtsc_seconds() result(seconds)
      import C_DOUBLE
      real(C_DOUBLE) :: seconds
    end function rdtsc_seconds
  end interface
#else
  int cpu_has_feature(int feature);
  int get_cpu_id(void);
  uint64_t get_cpu_freq(void);
  uint64_t rdtsc(void);
  uint64_t rdtscp(void);
  double wall_clock_seconds(uint64_t ticks);
  double rdtsc_seconds(void);
  double rdtscp_seconds(void);
#endif
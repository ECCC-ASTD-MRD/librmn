#ifndef _App_Timer_h
#define _App_Timer_h

#include <stdint.h>
#include <stdlib.h>
#include <time.h>

//! Timer that can accumulate microsecond intervals
typedef struct {
  uint64_t Start;     //! Timestamp when the timer was started
  uint64_t TotalTime; //! How many clock ticks have been recorded (updates every time the timer stops)
} TApp_Timer;

static const clockid_t APP_CLOCK_ID = CLOCK_MONOTONIC;

//! Get current system time in microseconds, wraps around approximately every year
static inline uint64_t get_current_time_us() {

  struct timespec now;
  clock_gettime(APP_CLOCK_ID, &now);

  // Wraps around every year or so. Not sure why you would need microsecond precision for longer
  const uint64_t now_us = ((uint64_t)now.tv_sec % (1 << 25)) * 1000000 + (uint64_t)now.tv_nsec / 1000;

  return(now_us);
}

static inline void App_TimerInit(TApp_Timer* Timer) {

  Timer->Start = 0;
  Timer->TotalTime = 0;
}

static inline TApp_Timer* App_TimerCreate() {

   TApp_Timer* timer = (TApp_Timer*)malloc(sizeof(TApp_Timer));

   if (timer)
      App_TimerInit(timer);

   return(timer);
}

//! Record the current timestamp
static inline void App_TimerStart(TApp_Timer* Timer) {
   Timer->Start = get_current_time_us();
}

//! Increment total time with number of ticks since last start
static inline void App_TimerStop(TApp_Timer* Timer) {
   Timer->TotalTime += get_current_time_us() - Timer->Start;
}

//! Retrieve the accumulated time in number of milliseconds, as a double
static inline double App_TimerTime_ms(const TApp_Timer* Timer) {
   // If we only count microseconds in a year, this conversion to double does not lose any precision (about 2^31 us/year)
   return(Timer->TotalTime / 1000.0);
}

static inline double App_TimerTime_SinceStart(const TApp_Timer* Timer) {
   // If we only count microseconds in a year, this conversion to double does not lose any precision (about 2^31 us/year)
   return((get_current_time_us() - Timer->Start) / 1000.0);
}

#endif


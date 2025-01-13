#ifndef IO_SERVER_circular_buffer_GEN_H
#define IO_SERVER_circular_buffer_GEN_H

/**
 \file
 \brief circular buffer package (C and Fortran)

 \verbatim
           circular buffer data layout

   (IN = OUT) (bufer empty) (LIMIT - FIRST -1 free slots)

 FIRST                                                   LIMIT
   |                                                       |
   v                                                       v
   +------------------------------------------------------+
   ........................................................
   ^------------------------------------------------------+
   |
 IN/OUT
   +------------------------------------------------------+
   ........................................................
   +--------------------^---------------------------------+
                        |
                      IN/OUT

   (IN = OUT - 1) (buffer full)

 FIRST                                                   LIMIT
   |                                                       |
   v                                                       v
   +------------------------------------------------------+
   xxxxxxxxxxxxxxxxxxxx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
   +-------------------^^---------------------------------+
                       ||
                     IN  OUT
   +------------------------------------------------------+
   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.
   ^------------------------------------------------------^
   |                                                      |
  OUT                                                     IN

   (OUT < IN) (LIMIT - IN -1) free, (IN - OUT) data
 FIRST                                                   LIMIT
   |                                                       |
   v                                                       v
   +------------------------------------------------------+
   xxxxxxxxxxxxxx..........................................
   ^-------------^----------------------------------------+
   |             |
  OUT            IN

   (IN < OUT) (OUT - IN -1) free, (LIMIT - OUT + IN - FIRST) data
 FIRST                                                   LIMIT
   |                                                       |
   v                                                       v
   +------------------------------------------------------+
   xxxxxxxxxxxxxx................................xxxxxxxxxx
   +-------------^-------------------------------^--------+
                 |                               |
                 IN                             OUT
   x = useful data       . = free space


      With partial insertion/extraction

  avail. data            available space           avail. data
   ...________          _________________         ____...
              |        |                 |       |
   +------------------------------------------------------+
   xxxxxxxxxxxx::::::::...................;;;;;;;xxxxxxxx
   +-----------^-------^------------------^------^--------+
               |       |                  |      |
               IN      |                 OUT     |
                  PARTIAL_IN                PARTIAL_OUT

   x = useful data    : = partially inserted data
   . = free space     ; = partially extracted data

 \endverbatim
*/
#include <stdlib.h>
#include <string.h>

#include <App.h>

#include "cb_data.h"

static const size_t CB_MIN_BUFFER_SIZE = 128 * 4; //!> Minimum size of a circular buffer, in bytes

//! Circular buffer management variables
//! Only use 64-bit members in that struct. Better for alignment
//! in == out means buffer is empty
//! in == out-1 (or in=limit-1 && out==0) means buffer is full
typedef struct {
    uint64_t version; //!< version marker
    uint64_t first;   //!< should be 0, because the feature has not been implemented yet
    uint64_t in[2];   //!< Start inserting data at data[in]
    uint64_t out[2];  //!< Start reading data at data[out]
    uint64_t limit;   //!< Size of data buffer (last available index + 1)
    uint64_t capacity_byte; //!< Size of data buffer in bytes
    int32_t  lock;    //!< To be able to perform thread-safe operations (not necessarily used)
    int32_t  dummy; //!< So that the struct has size multiple of 64 bits
} fiol_management;

//! pointer to circular buffer management part
typedef fiol_management* fiol_management_p;

//! Set of statistics we want to record as a circular buffer is used
//! Only use 64-bit members in that struct. Better for alignment
typedef struct {
    uint64_t num_reads;
    uint64_t num_unique_reads;
    uint64_t num_read_elems;
    uint64_t num_fractional_reads;
    double   total_read_wait_time_ms;
    double   total_read_time_ms;
    uint64_t max_fill;

    uint64_t num_writes;
    uint64_t num_write_elems;
    uint64_t num_fractional_writes;
    double   total_write_wait_time_ms;
    double   total_write_time_ms;
} cb_stats;

typedef cb_stats* cb_stats_p;

//! \brief A first-in, first-out queue that can have one producer and one consumer.
//! \copydoc circular_buffer.c
typedef struct {
    fiol_management m;      //!< Management structure
    cb_stats        stats;  //!< Set of recorded statistics
    data_element    data[]; //!< Data buffer (contains at most limit - 1 useful data elements)
} circular_buffer;

//! pointer to circular buffer
typedef circular_buffer* circular_buffer_p;

//! \brief Compute how much data is stored in a circular buffer, given of set of indices and a limit.
//! The caller is responsible for making sure that the inputs have been properly read (i.e. not cached by the compiler)
//! \return Number of elements stored in the buffer (available for reading)
static inline uint64_t available_data_elem(
    const uint64_t in,   //!< [in] Index of insertion location in the buffer
    const uint64_t out,  //!< [in] Index of extraction location in the buffer
    const uint64_t limit //!< [in] Number of elements that the buffer can hold
) {
    return (in >= out) ? in - out : limit - out + in;
}

//! \brief Compute how much space is available in a circular buffer, given a set of indices and a limit.
//! The caller is responsible for making sure that the inputs have been properly read (i.e. not cached by the compiler)
//! \return Available space in the buffer, in number of elements
static inline size_t available_space_elem(
    const uint64_t in,   //!< [in] Index of insertion location in the buffer
    const uint64_t out,  //!< [in] Index of extraction location in the buffer
    const uint64_t limit //!< [in] Number of elements that the buffer can hold
) {
    return limit - available_data_elem(in, out, limit) - 1;
}

enum
{
    CB_FULL    = 0, //!< Array index corresponding to the circular buffer _full_ index (in or out)
    CB_PARTIAL = 1  //!< Array index corresponding to the circular buffer _partial_ index (in or out)
};

//! \brief Compute the smallest number of elements that can fit the given number of bytes.
//! Basically, it's num_bytes / elem_size rounded up.
static inline size_t num_bytes_to_num_elem(const size_t num_bytes)
{
    const size_t remainder = num_bytes % sizeof(data_element) > 0 ? 1 : 0;
    return num_bytes / sizeof(data_element) + remainder;
}

static inline size_t num_bytes_to_num_elem_64(const size_t num_bytes) {
    const size_t num_elem = num_bytes_to_num_elem(num_bytes);
    const size_t add = (num_elem * sizeof(data_element)) % 8 == 0 ? 0 : 1;
    return num_elem + add;
}

/**
 * \brief Copy buffer elements into another array (either into or out of the buffer)
 */
static inline void copy_bytes(
    void*        dst,      //!< [out] Where to copy the elements
    const void*  src,      //!< [in]  The elements to copy
    const size_t num_bytes //!< [in] How many bytes we want to copy
) {
    memcpy(dst, src, num_bytes);
}

//! Compute the space in kilobytes taken by the given number of elements
static inline double num_elem_to_kb(const size_t num_elements) {
    return num_elements * sizeof(data_element) / 1024.0;
}

int CB_get_elem_size(void);
void CB_print_header(circular_buffer_p b);
void CB_dump_data(circular_buffer_p buffer, const int64_t num_bytes);
circular_buffer_p CB_init_bytes(circular_buffer_p p, size_t num_bytes);
circular_buffer_p CB_create_shared_bytes(int32_t* const shmid, size_t num_bytes);
int32_t CB_detach_shared(circular_buffer_p p);
circular_buffer_p CB_create_bytes(size_t num_bytes);
circular_buffer_p CB_from_pointer_bytes(void*  p, size_t num_bytes);
size_t CB_get_available_space_bytes(const circular_buffer_p buffer);
size_t CB_get_available_data_bytes(const circular_buffer_p buffer);
size_t CB_get_capacity_bytes(const circular_buffer_p buffer);
int64_t CB_wait_space_available_bytes(circular_buffer_p p, size_t num_bytes_wanted, int timeout_ms);
int64_t CB_wait_data_available_bytes(circular_buffer_p p, size_t num_bytes_wanted, int timeout_ms);
int CB_get(circular_buffer_p buffer, void* dest, size_t num_bytes, int operation, int timeout_ms);
int CB_cancel_get(circular_buffer_p buffer);
int CB_put(circular_buffer_p buffer, void* src, size_t num_bytes, int operation, int timeout_ms, int thread_safe);
int CB_cancel_put(circular_buffer_p buffer);
int CB_check_integrity(const circular_buffer_p buffer);
const char* CB_error_code_to_string(const int error_code);
void readable_element_count(const double num_elements, char* buffer);
void CB_print_stats(const circular_buffer_p buffer, int buffer_id, int with_header);

#endif // IO_SERVER_circular_buffer_GEN_H

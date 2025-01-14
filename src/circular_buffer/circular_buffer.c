#include <math.h>
#include <stdio.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/types.h>
#include <unistd.h>

#include <stdlib.h>
#include <string.h>

#include <App.h>

#include "rmn/circular_buffer.h"

//!> version marker
#define FIOL_VERSION 0x1BAD

//! Number of microseconds to wait between reads of the IN/OUT indices of a buffer when waiting for data to arrive
static const int CB_DATA_CHECK_DELAY_US = 10;
//! Number of microseconds to wait between reads of the IN/OUT indices of a buffer when waiting for space to be freed
static const int CB_SPACE_CHECK_DELAY_US = 10;

int CB_get_elem_size(void) { 
    return sizeof(data_element);
}

cb_stats_p CB_get_stats(circular_buffer_p b) {
    return &(b->stats);
}

//! Print buffer header (to help debugging)
void CB_print_header(
    circular_buffer_p b //!< [in] Pointer to the buffer to print
) {
    printf(
        "version %ld, first %ld, in %ld, in partial %ld, out %ld, out partial %ld, limit %ld, capacity (bytes) %ld\n",
        (long)b->m.version,
        (long)b->m.first, (long)b->m.in[CB_FULL], (long)b->m.in[CB_PARTIAL], (long)b->m.out[CB_FULL],
        (long)b->m.out[CB_PARTIAL], (long)b->m.limit, (long)b->m.capacity_byte);
}

int might_contain_chars(const int num) {
    const char* uchars = (char*)&num;
    for (int i = 0; i < 4; ++i) {
        if (uchars[i] < 32 || uchars[i] > 122) return 0;
    }

    return 1;
}

void CB_dump_data(
    circular_buffer_p buffer,   //!< [in] Pointer to the buffer to print
    const int64_t     num_bytes //!< [in] How many bytes of data to print
) {
    const int LINE_LENGTH = 10;
    const uint64_t num_elements = num_bytes >= 0 && (uint64_t)num_bytes <= buffer->m.limit * sizeof(data_element) ?
        num_bytes_to_num_elem_64(num_bytes) :
        buffer->m.limit;
  
    printf("Buffer data:");
    for (uint64_t i = 0; i < num_elements; ++i)
    {
        if (i % LINE_LENGTH == 0) printf("\n[%5ld] ", i / LINE_LENGTH);

        int add_space = 0;
        if (i == buffer->m.in[CB_PARTIAL]) { printf("\nPARTIAL IN"); add_space = 1; }
        if (i == buffer->m.in[CB_FULL]) { printf("\nFULL IN"); add_space = 1; }
        if (i == buffer->m.out[CB_PARTIAL]) { printf("\nPARTIAL OUT"); add_space = 1; }
        if (i == buffer->m.out[CB_FULL]) { printf("\nFULL OUT"); add_space = 1; }

        if (add_space) {
            printf("\n[     ] ");
            for (uint64_t j = 0; j < i % LINE_LENGTH; ++j) printf("                   ");
        }

        const int64_t elem = buffer->data[i];
        const int32_t* elems = (int32_t*)(&elem);

        for (int j = 0; j < 2; ++j) {
        if (might_contain_chars(elems[j])) {
            const char* letters = (const char*)(&elems[j]);
            printf("    %c%c%c%c ", letters[0], letters[1], letters[2], letters[3]);
        }
        else if (elems[j] > 99999999)
            printf("?%07d ", elems[j] % 10000000);
        else if (elems[j] < -9999999)
            printf("-?%06d ", -elems[j] % 1000000);
        else
            printf("%8d ", elems[j]);
        }
        printf(" ");
    }
    printf("\n");
}

//! initialize a circular buffer
//! \return pointer to buffer upon success, NULL upon error
circular_buffer_p CB_init_bytes(
    circular_buffer_p p,        //!< [in]  pointer to a circular buffer
    size_t            num_bytes //!< [in]  size in bytes of the circular buffer
) {
    if (p == NULL) {
        printf("ERROR: Given null pointer for initializing a circular_buffer\n");
        return NULL;
    }

    if (num_bytes < CB_MIN_BUFFER_SIZE) {
        printf("ERROR: not requesting enough elements for circular_buffer initialization! Requested %ld, but min buffer size is %ld bytes\n", num_bytes, CB_MIN_BUFFER_SIZE);
        return NULL; // area is too small
    }

    p->m.version         = FIOL_VERSION;
    p->m.first           = 0;
    p->m.in[CB_FULL]     = 0;
    p->m.in[CB_PARTIAL]  = 0;
    p->m.out[CB_FULL]    = 0;
    p->m.out[CB_PARTIAL] = 0;
    p->m.lock            = 0; // Unlocked

    // Memory is already allocated so to get the number of full elements we round down
    const int num_elements = num_bytes / sizeof(data_element);
    // Header size in number of elements
    const data_element header_size  = num_bytes_to_num_elem(sizeof(circular_buffer));

    p->m.limit = num_elements - header_size;
    p->m.capacity_byte = (p->m.limit - 1) * sizeof(data_element);

    p->stats.num_reads               = 0;
    p->stats.num_unique_reads        = 0;
    p->stats.num_read_elems          = 0;
    p->stats.num_fractional_reads    = 0;
    p->stats.total_read_time_ms      = 0.0;
    p->stats.total_read_wait_time_ms = 0.0;
    p->stats.max_fill                = 0;

    p->stats.num_writes               = 0;
    p->stats.num_write_elems          = 0;
    p->stats.num_fractional_writes    = 0;
    p->stats.total_write_time_ms      = 0.0;
    p->stats.total_write_wait_time_ms = 0.0;

    return p;
}

//! Create and initialize a circular buffer of size num_bytes in "shared memory",
//! shmid will be set to the shared memory id of the "shared memory segment upon success, -1 otherwise
//! (see man shmget)
//! \return pointer to buffer upon success, NULL upon error
circular_buffer_p CB_create_shared_bytes(
    int32_t* const shmid,    //!< [out] identifier of shared memory area (see man shmget) (-1 upon error)
    size_t   num_bytes //!< [in]  size in bytes of the circular buffer
) {
    *shmid = -1;

    if (num_bytes < CB_MIN_BUFFER_SIZE)
        return NULL;

    int id = shmget(IPC_PRIVATE, num_bytes, IPC_CREAT); // create shared memory segment
    if (id == -1)
        return NULL;          // error occurred

    void* t = shmat(id, NULL, 0); // attach shared memory segment
    if (t == (void*)-1)
        return NULL;                      // error occurred

    struct shmid_ds ds;
    int status = shmctl(id, IPC_RMID, &ds); // mark segment for deletion (ONLY SAFE ON LINUX)
    if (status != 0)
        return NULL; // this should not fail

    *shmid = id;
    return CB_init_bytes((circular_buffer_p)t, num_bytes);
}

//! Detach "shared memory segment" used by circular buffer
//! \return 0 upon success, nonzero upon error
int32_t CB_detach_shared(
    circular_buffer_p p //!< [in] pointer to a circular buffer
) {
    if (p == NULL)
        return CB_ERROR;
    return shmdt(p); // detach from "shared memory segment" creeated by CB_create_shared
}

//! Create and initialize a circular buffer of size num_bytes in process memory
//! \return address of the circular buffer upon success, NULL otherwise
circular_buffer_p CB_create_bytes(
    size_t num_bytes //!< [in]  size in bytes of the circular buffer
) {
    if (num_bytes < CB_MIN_BUFFER_SIZE)
        return NULL;
    circular_buffer_p buffer = (circular_buffer_p)malloc(num_bytes);
    return CB_init_bytes(buffer, num_bytes);
}

//! create and initialize a circular buffer, using supplied space
//! \return address of the circular buffer upon success, NULL otherwise
circular_buffer_p CB_from_pointer_bytes(
    void*  p,        //!< [in] Pointer to user supplied memory space
    size_t num_bytes //!< [in] Size in bytes of the circular buffer
) {
    if (num_bytes < CB_MIN_BUFFER_SIZE) {
        printf("NOT ASKING FOR A BIG ENOUGH CIRCULAR BUFFER\n");
        return NULL;
    }

    return CB_init_bytes((circular_buffer_p)p, num_bytes);
}

//! Compute how much space (in bytes) is available in a given circular buffer
//! \return How many bytes can still be added
size_t CB_get_available_space_bytes(
    const circular_buffer_p buffer //!< [in] The buffer we want to query
) {
    // Make sure that the values are really read by accessing them through a volatile pointer
    volatile uint64_t* in  = &buffer->m.in[CB_PARTIAL];
    volatile uint64_t* out = &buffer->m.out[CB_FULL];
    return available_space_elem(*in, *out, buffer->m.limit) * sizeof(data_element);
}

//! Compute how much data (in bytes) is stored in a given circular buffer
//! \return How many bytes are stored in the buffer
size_t CB_get_available_data_bytes(
    const circular_buffer_p buffer //!< [in] The buffer we want to query
) {
    // Make sure that the values are really read by accessing them through a volatile pointer
    volatile uint64_t* in  = &buffer->m.in[CB_FULL];
    volatile uint64_t* out = &buffer->m.out[CB_PARTIAL];
    return available_data_elem(*in, *out, buffer->m.limit) * sizeof(data_element);
}

//! Compute the maximum number of bytes the buffer can hold
size_t CB_get_capacity_bytes(
    const circular_buffer_p buffer //!< [in] The buffer we want to query
) {
    return (buffer->m.limit - buffer->m.first - 1) * sizeof(data_element);
}

//! wait until at least num_bytes_wanted empty slots are available for inserting data
//! \return actual number of bytes available, a negative error code on error
int64_t CB_wait_space_available_bytes(
    circular_buffer_p p,                //!< [in]  pointer to a circular buffer
    size_t            num_bytes_wanted, //!< [in]  needed number of available bytes
    int               timeout_ms        //!< [in]  How long to wait (in ms) before giving up and returning an error. Wait (almost) forever if negative
) {
    const int status = CB_check_integrity(p);
    if (status != CB_SUCCESS)
        return status;

    // Check whether there is enough space in the entire CB
    if (num_bytes_wanted > p->m.capacity_byte)
        return CB_ERROR_INSUFFICIENT_SPACE;

    const size_t num_removable_data = CB_get_available_data_bytes(p);
    size_t num_available = CB_get_available_space_bytes(p);

    // Check whether there is enough fillable space in the CB
    if (num_available + num_removable_data < num_bytes_wanted)
        return CB_ERROR_INSUFFICIENT_SPACE;

    // Wait until there is enough space (up to the specified amount of time)
    const size_t max_num_waits = timeout_ms < 0 ? (size_t)(-1) : (size_t)timeout_ms * 1000 / CB_SPACE_CHECK_DELAY_US;
    size_t num_waits = 0;
    for (num_waits = 0; num_waits < max_num_waits && num_available < num_bytes_wanted; ++num_waits) {
        sleep_us(CB_SPACE_CHECK_DELAY_US);
        num_available = CB_get_available_space_bytes(p);
    }

    // Check if there still isn't enough space
    if (num_available < num_bytes_wanted) return CB_ERROR_TIMEOUT;

    return (int64_t)num_available;
}

//! wait until at least num_bytes_wanted are available for extracting data
//! \return actual number of bytes available, a negative error code if error
int64_t CB_wait_data_available_bytes(
    circular_buffer_p p,                //!< [in] pointer to a circular buffer
    size_t            num_bytes_wanted, //!< [in] needed number of available bytes
    int               timeout_ms        //!< [in] How long to wait (in ms) before giving up and returning an error. Wait (almost) forever if negative
) {
    const int status = CB_check_integrity(p);
    if (status != CB_SUCCESS)
        return status;

    if (num_bytes_wanted > p->m.capacity_byte)
        return CB_ERROR_INSUFFICIENT_SPACE;

    // Wait until data becomes available (up until the specified amount of time)
    const size_t max_num_waits = timeout_ms < 0 ? (size_t)(-1) : (size_t)timeout_ms * 1000 / CB_DATA_CHECK_DELAY_US;
    size_t num_available = CB_get_available_data_bytes(p);
    size_t num_waits = 0;
    for (num_waits = 0; num_waits < max_num_waits && num_available < num_bytes_wanted; ++num_waits) {
        sleep_us(CB_DATA_CHECK_DELAY_US);
        num_available = CB_get_available_data_bytes(p);
    }

    // Check whether there still isn't enough data
    if (num_available < num_bytes_wanted) return CB_ERROR_TIMEOUT;

    return (int64_t)num_available;
}

//! wait until num_bytes are available then extract them into dst
//! \return CB_SUCCESS on success, a negative error code on error
int CB_get(
    circular_buffer_p buffer,     //!< [in]  Pointer to a circular buffer
    void*             dest,       //!< [out] Destination array for data extraction
    size_t            num_bytes,  //!< [in]  Number of #data_element data items to extract
    int               operation,  //!< [in]  Whether to update the buffer, do a partial read, or simply peek at the next values
    int               timeout_ms  //!< [in]  How long to wait (in ms) before giving up and returning an error
) {
    TApp_Timer timer = NULL_TIMER;
    App_TimerStart(&timer);

    const int64_t num_available = CB_wait_data_available_bytes(buffer, num_bytes, timeout_ms);
    buffer->stats.total_read_wait_time_ms += App_TimerTimeSinceStart_ms(&timer); // Update wait stats
    if (num_available < 0)
        return num_available;

    uint64_t       out   = buffer->m.out[CB_PARTIAL];
    const uint64_t limit = buffer->m.limit;
    data_element*  data  = buffer->data;

    const size_t num_elements = num_bytes_to_num_elem(num_bytes);

    const size_t num_elem_1  = num_elements > (limit - out) ? (limit - out) : num_elements;
    const size_t num_bytes_1 = num_elem_1 < num_elements ? num_elem_1 * sizeof(data_element) : num_bytes;
    copy_bytes(dest, (void*)(data + out), num_bytes_1);
    out += num_elem_1;

    if (out >= limit)
        out = buffer->m.first;

    if (num_elem_1 < num_elements) {
        const size_t num_elem_2  = num_elements - num_elem_1;
        const size_t num_bytes_2 = num_bytes - num_bytes_1;
        copy_bytes((char*)dest + num_bytes_1, (void*)(data + out), num_bytes_2);
        out += num_elem_2;
    }

    if (operation != CB_PEEK) {
        buffer->m.out[CB_PARTIAL] = out;
    }

    if (operation == CB_COMMIT) {
        full_memory_fence(); // memory fence, make sure everything fetched and stored before adjusting the "out" pointer
        uint64_t volatile* outp = &(buffer->m.out[CB_FULL]);
        *outp                   = out;
    }

    if (operation != CB_PEEK) {
        buffer->stats.num_read_elems += num_elements;
        buffer->stats.num_unique_reads++;
    }

    buffer->stats.num_reads++;

    App_TimerStop(&timer);
    buffer->stats.total_read_time_ms += App_TimerLatestTime_ms(&timer);

    // Only count fractional reads that won't be read again (so no CB_PEEK)
    if (num_bytes_1 != num_elements * sizeof(data_element) && operation != CB_PEEK) buffer->stats.num_fractional_reads++;

    return CB_SUCCESS;
}

//! Revert any reading operation that was not yet committed (basically reset the OUT index to what it was
//! before we started doing and uncommitted/partial read)
int CB_cancel_get(
    circular_buffer_p buffer //!< The buffer on which we're trying to cancel
) {
    const int status = CB_check_integrity(buffer);
    if (status != CB_SUCCESS)
        return status;

    uint64_t volatile* out_full    = &(buffer->m.out[CB_FULL]);
    uint64_t volatile* out_partial = &(buffer->m.out[CB_PARTIAL]);
    *out_partial = *out_full;

    return CB_SUCCESS;
}

//! wait until num_bytes are available then insert from src array
//! \return CB_SUCCESS upon success, a negative error code upon error
int CB_put(
    circular_buffer_p buffer,    //!< [in] Pointer to a circular buffer
    void*             src,       //!< [in] Source array for data insertion
    size_t            num_bytes, //!< [in] Number of bytes to insert
    int operation,    //!< [in] Whether to update the IN pointer so that the newly-inserted data can be read right away
    int timeout_ms,   //!< [in] How long to wait (in ms) before giving up and returning an error
    int thread_safe   //!< [in] If 1, perform operation in a thread-safe way
) {
    TApp_Timer timer = NULL_TIMER;
    App_TimerStart(&timer);

    if (thread_safe == 1 && operation != CB_COMMIT) {
        printf("WARNING: Trying to put data in a CB in a thread safe way, but not committing the message. We don't allow that for now.\n");
        return CB_ERROR_NOT_ALLOWED;
    }

    if (thread_safe == 1) acquire_lock(&buffer->m.lock);

    const int64_t num_available = CB_wait_space_available_bytes(buffer, num_bytes, timeout_ms);
    if (num_available < 0) {
        if (thread_safe == 1) release_lock(&buffer->m.lock);
        return num_available;
    }

    buffer->stats.total_write_wait_time_ms += App_TimerTimeSinceStart_ms(&timer); // Update wait stats

    data_element*  data       = buffer->data;
    uint64_t       current_in = buffer->m.in[CB_PARTIAL];
    const uint64_t limit      = buffer->m.limit;

    const size_t num_elements = num_bytes_to_num_elem(num_bytes);

    const size_t num_elem_1  = num_elements > (limit - current_in) ? (limit - current_in) : num_elements;
    const size_t num_bytes_1 = num_elem_1 < num_elements ? num_elem_1 * sizeof(data_element) : num_bytes;
    copy_bytes((void*)(data + current_in), src, num_bytes_1);
    current_in += num_elem_1;

    if (current_in >= limit)
        current_in = buffer->m.first;

    if (num_elements > num_elem_1) {
        const size_t num_elem_2  = num_elements - num_elem_1;
        const size_t num_bytes_2 = num_bytes - num_bytes_1;
        copy_bytes(data, (char*)src + num_bytes_1, num_bytes_2);
        current_in += num_elem_2;
    }

    buffer->m.in[CB_PARTIAL] = current_in;

    if (operation == CB_COMMIT) {
        write_fence(); // make sure everything is in memory before adjusting the "in" pointer
        uint64_t volatile* inp = &(buffer->m.in[CB_FULL]);
        *inp                   = current_in;
    }

    App_TimerStop(&timer);

    const uint64_t new_fill = limit - num_bytes_to_num_elem(num_available) + num_elements - 1;
    if (new_fill > buffer->stats.max_fill) {
        buffer->stats.max_fill = new_fill;
    }
    buffer->stats.num_write_elems += num_elements;
    buffer->stats.num_writes++;
    buffer->stats.total_write_time_ms += App_TimerLatestTime_ms(&timer);

    if (num_bytes != num_elements * sizeof(data_element)) buffer->stats.num_fractional_writes++;

    if (thread_safe == 1) release_lock(&buffer->m.lock);

    return CB_SUCCESS;
}

//! Revert any uncompleted write operation (no commit yet). Basically resets the IN index to what it was
//! before the uncommitted write started.
int CB_cancel_put(
    circular_buffer_p buffer //!< The buffer on which we're trying to cancel
) {
    const int status = CB_check_integrity(buffer);
    if (status != CB_SUCCESS)
        return status;

    uint64_t volatile* in_full    = &(buffer->m.in[CB_FULL]);
    uint64_t volatile* in_partial = &(buffer->m.in[CB_PARTIAL]);
    *in_partial = *in_full;

    return CB_SUCCESS;
}

/**
 * \brief Verify the header of the given buffer is self-consistent (correct version, first = 0, in/out within limits)
 * \return CB_SUCCESS if the Buffer is consistent, an error code otherwise
 */
int CB_check_integrity(
    const circular_buffer_p buffer  //!< [in] The buffer we want to check
) {
    if (buffer == NULL) {
        return CB_ERROR_INVALID_POINTER;
    }

    if (buffer->m.version != FIOL_VERSION) {
        return CB_ERROR_INVALID_VERSION;
    }

    if (buffer->m.first != 0) {
        return CB_ERROR_INVALID_FIRST;
    }

    if (buffer->m.in[CB_FULL] < buffer->m.first || buffer->m.in[CB_FULL] >= buffer->m.limit) {
        return CB_ERROR_INVALID_FULL_IN;
    }

    if (buffer->m.out[CB_FULL] < buffer->m.first || buffer->m.out[CB_FULL] >= buffer->m.limit) {
        return CB_ERROR_INVALID_FULL_OUT;
    }

    if (buffer->m.in[CB_PARTIAL] < buffer->m.first || buffer->m.in[CB_PARTIAL] >= buffer->m.limit) {
        return CB_ERROR_INVALID_PARTIAL_IN;
    }

    if (buffer->m.out[CB_PARTIAL] < buffer->m.first || buffer->m.out[CB_PARTIAL] >= buffer->m.limit) {
        return CB_ERROR_INVALID_PARTIAL_OUT;
    }

    return CB_SUCCESS;
}

const char* CB_error_code_to_string(
    const int error_code  //!< [in] The error code we want to translate into a string
) {
    switch (error_code)
    {
    case CB_SUCCESS:                    return "CB_SUCCESS";
    case CB_ERROR:                      return "CB_ERROR";
    case CB_ERROR_INVALID_POINTER:      return "CB_ERROR_INVALID_POINTER";
    case CB_ERROR_INVALID_VERSION:      return "CB_ERROR_INVALID_VERSION";
    case CB_ERROR_INVALID_FIRST:        return "CB_ERROR_INVALID_FIRST";
    case CB_ERROR_INVALID_FULL_IN:      return "CB_ERROR_INVALID_FULL_IN";
    case CB_ERROR_INVALID_FULL_OUT:     return "CB_ERROR_INVALID_FULL_OUT";
    case CB_ERROR_INVALID_PARTIAL_IN:   return "CB_ERROR_INVALID_PARTIAL_IN";
    case CB_ERROR_INVALID_PARTIAL_OUT:  return "CB_ERROR_INVALID_PARTIAL_OUT";
    case CB_ERROR_INSUFFICIENT_SPACE:   return "CB_ERROR_INSUFFICIENT_SPACE";
    case CB_ERROR_TIMEOUT:              return "CB_ERROR_TIMEOUT";
    case CB_ERROR_NOT_ALLOWED:          return "CB_ERROR_NOT_ALLOWED";
    case DCB_ERROR_INVALID_CAPACITY:    return "DCB_ERROR_INVALID_CAPACITY";
    case DCB_ERROR_INVALID_RANK:        return "DCB_ERROR_INVALID_RANK";
    case DCB_ERROR_INVALID_INSTANCE:    return "DCB_ERROR_INVALID_INSTANCE";
    default:                            return "[UNKNOWN CB ERROR CODE]";
    }
}

//! Provide a string representation of a number in a human readable way (with the k, M or G suffix if needed)
void readable_element_count(
    const double num_elements, //!< [in]  Number we want to represent
    char*        buffer        //!< [out] Buffer where the string will be stored. Must contain at least 8 bytes
) {
    double amount = num_elements;
    int    unit   = 0;

    const char UNITS[] = {'\0', 'k', 'M', 'G', 'T', 'P', 'E'};

    while (amount > 1900.0 && unit < 6) {
        amount /= 1000.0;
        unit++;
    }

    if (unit == 0) {
        if (ceil(amount) == amount)
        sprintf(buffer, "%7.0f", amount);
        else
        sprintf(buffer, "%7.2f", amount);
    }
    else if (amount < 10000.0) {
        sprintf(buffer, "%6.1f%c", amount, UNITS[unit]);
    }
    else {
        sprintf(buffer, "    %s", "inf");
    }
}

//! Provide a string representation of a time in a human readable way, with units
void readable_time(
    const double time_ms, //!< [in]  Time we want to print, in ms
    char*        buffer   //!< [out] Buffer where the string will be stored. Must 
) {
    const char* UNITS[] = {"ns", "us", "ms", "s ", "m ", "H "};
    double amount = time_ms;
    int    unit   = 2;

    if (time_ms < 1000.0) {
        while (amount < 1.0 && unit > 0) {
        amount *= 1000.0;
        unit--;
        }
    }
    else {
        amount /= 1000.0;
        unit++;
        while (amount > 120.0 && unit < 5) {
        amount /= 60.0;
        unit++;
        }
    }
    sprintf(buffer, "%5.1f%s", amount, UNITS[unit]);
}

void CB_print_stats(
    const circular_buffer_p buffer,     //!< [in] Buffer whose stats we want to print
    int                     buffer_id,  //!< [in] ID of the buffer (displayed at beginning of line)
    int                     with_header //!< [in] Whether to print a header for the values
) {
    const int status = CB_check_integrity(buffer);
    if (status != CB_SUCCESS) {
        printf("ERROR: Cannot print CB because CB is not valid: %s\n", CB_error_code_to_string(status));
        return;
    }

    const cb_stats_p stats = &buffer->stats;

    const uint64_t num_writes = stats->num_writes;
    const uint64_t num_reads  = stats->num_unique_reads;

    const uint64_t num_write_elems = stats->num_write_elems;
    const uint64_t num_read_elems  = stats->num_read_elems;

    char total_in_s[8], avg_in_s[8], total_out_s[8], avg_out_s[8], read_per_sec_s[8], write_per_sec_s[8], max_fill_s[8];
    char total_write_time_s[8], total_write_wait_time_s[8], avg_write_wait_time_s[8], total_read_time_s[8], total_read_wait_time_s[8], avg_read_wait_time_s[8];

    const double avg_bytes_in  = num_writes > 0 ? (double)stats->num_write_elems * sizeof(data_element) / num_writes : 0.0;
    const double avg_bytes_out = num_reads > 0 ? (double)stats->num_read_elems * sizeof(data_element) / num_reads : 0.0;

    readable_element_count(stats->num_write_elems * sizeof(data_element), total_in_s);
    readable_element_count(avg_bytes_in, avg_in_s);
    readable_element_count(stats->num_read_elems * sizeof(data_element), total_out_s);
    readable_element_count(avg_bytes_out, avg_out_s);

    const double avg_wait_w       = num_writes > 0 ? stats->total_write_wait_time_ms / num_writes : 0.0;
    const double avg_wait_r       = num_reads > 0 ? stats->total_read_wait_time_ms / num_reads : 0.0;
    const double total_write_time = stats->total_write_time_ms + 1e-24; // Lazy trick to avoid division by zero later on
    const double total_read_time  = stats->total_read_time_ms  + 1e-24; // Lazy trick to avoid division by zero later on
    readable_time(total_write_time, total_write_time_s);
    readable_time(total_read_time, total_read_time_s);
    readable_time(stats->total_write_wait_time_ms, total_write_wait_time_s);
    readable_time(stats->total_read_wait_time_ms, total_read_wait_time_s);
    readable_time(avg_wait_w, avg_write_wait_time_s);
    readable_time(avg_wait_r, avg_read_wait_time_s);

    readable_element_count(num_write_elems / total_write_time * 1000.0 * sizeof(data_element), write_per_sec_s);
    readable_element_count(num_read_elems  / total_read_time  * 1000.0 * sizeof(data_element), read_per_sec_s);

    readable_element_count(stats->max_fill * sizeof(data_element), max_fill_s);
    const int max_fill_percent = (int)(stats->max_fill * 100.0 / (CB_get_capacity_bytes(buffer) / sizeof(data_element)));

    const int frac_write_percent = num_writes > 0 ? (int)(stats->num_fractional_writes * 100.0 / num_writes) : 0;
    const int frac_read_percent  = num_reads  > 0 ? (int)(stats->num_fractional_reads  * 100.0 / num_reads)  : 0;

    if (with_header) {
        printf("     "
            "                       Write                               |"
            "                       Read                                |\n"
            "rank "
            "  #bytes  (B/call) : tot. time (B/sec) :  wait    (/call)  |"
            "  #bytes  (B/call) : tot. time (B/sec) :  wait    (/call)  | "
            "max fill B (%%)| frac. writes, reads (%%)\n");
    }

    printf(
        "%04d: "
        "%s (%s) : %s (%s) : %s (%s) | "
        "%s (%s) : %s (%s) : %s (%s) | "
        "%s (%3d) | %3d, %3d\n",
        buffer_id, total_in_s, avg_in_s, total_write_time_s, write_per_sec_s, total_write_wait_time_s, avg_write_wait_time_s,
        total_out_s, avg_out_s, total_read_time_s, read_per_sec_s, total_read_wait_time_s, avg_read_wait_time_s, max_fill_s,
        max_fill_percent, frac_write_percent, frac_read_percent);
}


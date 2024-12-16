#include <stdio.h>

#include <mpi.h>

#include <rmn/circular_buffer.h>

void init_array(data_element* array, const int num_elements, const int rank) {
  for (int i = 0; i < num_elements; i++)
    array[i] = (rank + 1) * 10000 + i;
}

int fill_test(int argc, char** argv) {
  const int NUM_BUFFER_BYTES = 128 * 8;
  const int NUM_TEST_ELEM    = NUM_BUFFER_BYTES / 8 * 2;
  const int NUM_TEST_BYTES   = NUM_TEST_ELEM * 8;
  const int READ_DELAY_US    = 1000;
  const int WRITE_DELAY_US   = 1000;

  data_element* local_data    = (data_element*)malloc(NUM_TEST_BYTES);
  data_element* received_data = (data_element*)malloc(NUM_TEST_BYTES);

  int num_errors = 0;

  // Init MPI
  int num_procs;
  int my_rank;
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &num_procs);
  MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

  // Allocate MPI window in shared memory
  MPI_Win        window;
  data_element*  base_mem_ptr;
  const int      disp_unit   = sizeof(data_element);
  const MPI_Aint window_size = NUM_BUFFER_BYTES;
  MPI_Win_allocate_shared(window_size, disp_unit, MPI_INFO_NULL, MPI_COMM_WORLD, &base_mem_ptr, &window);

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  circular_buffer_p  local_buffer = CB_from_pointer_bytes(base_mem_ptr, NUM_BUFFER_BYTES);
  circular_buffer_p* all_buffers  = NULL;
  const int          success      = (local_buffer != NULL);

  // Retrieve local address of every CB as seen on the root process
  if (my_rank == 0) {
    all_buffers = (circular_buffer_p*)malloc((size_t)num_procs * sizeof(circular_buffer_p));
    for (int i = 1; i < num_procs; ++i) {
      MPI_Aint target_size;
      int      target_disp_unit;
      MPI_Win_shared_query(window, i, &target_size, &target_disp_unit, &all_buffers[i]);
    }
  }

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  if (!success)
    return ++num_errors;

  const size_t max_num_bytes = CB_get_available_space_bytes(local_buffer);
  const size_t capacity      = CB_get_capacity_bytes(local_buffer);
  init_array(local_data, NUM_TEST_ELEM, my_rank);

  if (my_rank != 0) {
    if (capacity != max_num_bytes) {
      printf("Ahhhh inconsistency between free space (%ld) and capacity (%ld)!\n", max_num_bytes, capacity);
      num_errors++;
      return num_errors;
    }

    const int expected_error = CB_put(local_buffer, local_data, capacity + 1, CB_COMMIT, -1, 0);
    if (expected_error == CB_SUCCESS) {
      printf("Wrong return value (%s) after trying to put more than max into the buffer!\n", CB_error_code_to_string(expected_error));
      num_errors++;
      return num_errors;
    }

    const int status = CB_put(local_buffer, local_data, capacity, CB_COMMIT, -1, 0);
    if (status != CB_SUCCESS)
    {
      printf("ERROR: put failed (capacity) with %s\n", CB_error_code_to_string(status));
      num_errors++;
      return num_errors;
    }
  }

  // Now all buffers except the root one are full

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  {
    TApp_Timer put_time = NULL_TIMER;
    App_TimerStart(&put_time);

    //---------------------------
    MPI_Barrier(MPI_COMM_WORLD);
    //---------------------------

    if (my_rank == 0) {
      for (int i = 1; i < num_procs; ++i) {
        sleep_us(READ_DELAY_US);
        CB_get(all_buffers[i], received_data, NUM_BUFFER_BYTES / 2, CB_COMMIT, -1);
      }
    }
    else {
      // At first, buffer is full, so we can't put anything in it (hence the delay)
      CB_put(local_buffer, local_data + NUM_BUFFER_BYTES / 4, 1*8, CB_COMMIT, -1, 0);
      App_TimerStop(&put_time);

      const double t = App_TimerLatestTime_ms(&put_time);
      //      printf("Put data after %f ms (rank %d)\n", t, my_rank);

      if (t * 1000 < READ_DELAY_US * my_rank)
      {
        printf("Error in delay (READ)\n");
        num_errors++;
        return num_errors;
      }
    }
  }

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  {
    TApp_Timer read_time = {0, 0, 0};
    App_TimerStart(&read_time);

    //---------------------------
    MPI_Barrier(MPI_COMM_WORLD);
    //---------------------------

    if (my_rank == 0) {
      for (int i = 1; i < num_procs; ++i) {
        CB_get(all_buffers[i], received_data, max_num_bytes - NUM_BUFFER_BYTES / 2 + 2*8, CB_COMMIT, -1);
        App_TimerStop(&read_time);
        // IO_timer_start(&read_time);
        const double t = App_TimerLatestTime_ms(&read_time);
        if (t * 1000 < WRITE_DELAY_US * i)
        {
          printf("Error in delay! (WRITE) t = %f ms, i = %d\n", t, i);
          num_errors++;
          return num_errors;
        }

        //        printf("Read in %f ms\n", t);

        const int expected_error = CB_get(all_buffers[i], received_data, capacity + 1, CB_COMMIT, -1);
        if (expected_error == CB_SUCCESS) {
          printf("Wrong return code after trying to read more than max buffer size! (%s)\n", CB_error_code_to_string(expected_error));
          num_errors++;
          return num_errors;
        }
      }
    }
    else {
      sleep_us(WRITE_DELAY_US * my_rank);
      CB_put(local_buffer, local_data, 1*8, CB_COMMIT, -1, 0);
    }
  }

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  if (my_rank != 0) {
    if (CB_get_available_data_bytes(local_buffer) != 0)
    {
      printf("Should be 0 available bytes!!\n");
      num_errors++;
      return num_errors;
    }
  }

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  const int tmp_errors = num_errors;
  MPI_Reduce(&tmp_errors, &num_errors, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD);

  if (my_rank == 0) {
    for (int i = 1; i < num_procs; ++i) {
      CB_print_stats(all_buffers[i], i, i == 1);
    }

    free(all_buffers);

    if (num_errors > 0) {
      printf("THERE WERE %d ERRORS\n", num_errors);
    }
    else {
      printf("Circular buffer fill and wait test successful\n");
    }
  }

  //---------------------------
  MPI_Barrier(MPI_COMM_WORLD);
  //---------------------------

  free(local_data);
  free(received_data);

  MPI_Win_free(&window);
  MPI_Finalize();

  return num_errors;
}

int main(int argc, char** argv) {
  return fill_test(argc, argv);
}

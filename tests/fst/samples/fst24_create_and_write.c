#include <rmn.h>

int main(void) {
  
  float my_data[100][200];
  
  // Initialize data
  for (int i = 0; i < 100; i++)
    for (int j = 0; j < 200; j++)
        my_data[i][j] = 100.0;
  
  fst_file* my_file = fst24_open("my_file.fst", "R/W+RSF");
  if (my_file == NULL) return -1;
  
  fst_record my_record = default_fst_record;

  my_record.data = my_data;
  my_record.ni = 100;
  my_record.nj = 200;
  my_record.nk = 1;
  my_record.data_type = FST_TYPE_REAL;
  my_record.data_bits = 32;
  my_record.pack_bits = 32;

  my_record.deet = 0;
  my_record.npas = 0;
  my_record.ip1 = 0;
  my_record.ip2 = 0;
  my_record.ip3 = 0;
  my_record.ig1 = 0;
  my_record.ig2 = 0;
  my_record.ig3 = 0;
  my_record.ig4 = 0;
  
  if (fst24_write(my_file, &my_record, 0) <= 0) return -1;
  
  if (fst24_close(my_file) <= 0) return -1;
  
  free(my_file);

  return 0;
}

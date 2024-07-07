program create_and_write
  
  use rmn_fst24
  implicit none
  
  type(fst_file)   :: my_file
  type(fst_record) :: my_record
  logical :: success
  
  real, dimension(100, 200), target :: my_data
  
  ! Initialize data
  my_data=100.
  
  success = my_file % open('my_file.fst', 'R/W+RSF')
  if (.not. success) error stop 1
  
  my_record % data = c_loc(my_data)
  my_record % ni = 100
  my_record % nj = 200
  my_record % nk = 1
  my_record % data_type = FST_TYPE_REAL
  my_record % data_bits = 32
  my_record % pack_bits = 32
  my_record % deet = 0
  my_record % npas = 0
  my_record % ip1 = 0
  my_record % ip2 = 0
  my_record % ip3 = 0
  my_record % ig1 = 0
  my_record % ig2 = 0
  my_record % ig3 = 0
  my_record % ig4 = 0
  
  success = my_file % write(my_record)
  if (.not. success) error stop 1
  
  success = my_file % close()
  if (.not. success) error stop 1
  
end program create_and_write

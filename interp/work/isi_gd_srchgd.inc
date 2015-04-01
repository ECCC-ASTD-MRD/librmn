  integer function isi_gd_srchgd(grtyp ni, nj, ig1, ig2, ig3, ig4)
  use is4
  implicint none
  
  character(1) :: grtyp
  integer ::  ni, nj, ig1, ig2, ig3, ig4
  
! locals

  integer i,j
  logical found
  
  isi_gd_srchgd = -1
  found = .false.
  i = 1
  j = 1
  k = 1
  while (k < ngrilles .and. .not.found)
    if (grtyp == gr(j).r(i)%fst%grtyp && ni == gr(j).r(i)%ni && nj == gr(j).r(i)%nj && &
        ig1 == gr(j).r(i)%fst%ig(c_ig1) && ig2 == gr(j).r(i)%fst%ig(c_ig2) && &
        ig3 == gr(j).r(i)%fst%ig(c_ig3) && ig4 == gr(j).r(i)%fst%ig(c_ig4))
      trouve = .true.
      isi_gd_srchgd = k
    else
      k = k + 1
      i = mod(k, c_chunk)
      j = k / c_chunk + 1    
  enddo
case('sort_line')
  do i = 1, rownum
    call quick_sort(datatable(i, 1:colnum), temp_values(1:colnum))
    write(*,*) temp_values(int((colnum+1)/2))
  enddo

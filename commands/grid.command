case ('grid')
  if (range_min.eq.1D100) then
   range_min = minval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
  end if
  if (range_max.eq.-1D100) then
    range_max = maxval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
  end if
  step_size=(range_max-range_min)/step_num
  k = 1
  do j = 0, step_num - 1
    steps(j)=range_min + j*step_size
    do while ((datatable(k, xcol_add(1)).le.steps(j)).and.(k.le.(rownum-1)))
      k = k + 1
    enddo
    if (k.gt.1) then
      temp_values(1) = steps(j) - datatable(k-1, xcol_add(1))
      temp_values(2) = datatable(k, xcol_add(2)) - datatable(k-1, xcol_add(2))
      temp_values(3) = datatable(k, xcol_add(1)) - datatable(k-1, xcol_add(1))
      temp_values(4) = datatable(k-1, xcol_add(2))+temp_values(1)*temp_values(2)/temp_values(3)
    else
      temp_values(1) = steps(j) - datatable(k, xcol_add(1))
      temp_values(2) = datatable(k, xcol_add(2)) - datatable(k+1, xcol_add(2))
      temp_values(3) = datatable(k, xcol_add(1)) - datatable(k+1, xcol_add(1))
      temp_values(4) = datatable(k, xcol_add(2))+temp_values(1)*temp_values(2)/temp_values(3)
    end if
    write(*,*) steps(j), temp_values(4)
  enddo

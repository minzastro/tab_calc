case('int', 'integrate')
!integrating values
  if ((xcol_num.lt.1).or.(ycol_num.lt.1)) then
    write(*,*) cComment//'Error! X or Y column not specified'
  endif
  temp_values(1) = 0D0
  do j = 2, rownum
    temp_values(1) = temp_values(1) + &
                     0.5D0*(datatable(j, xcol_add(1))-datatable(j-1, xcol_add(1))) * &
                    (datatable(j-1, ycol_add(1))+datatable(j, ycol_add(1)))
  enddo
  write(*,*) temp_values(1)
case('int2') !integrate with Simpson's rule
  if ((xcol_num.lt.1).or.(ycol_num.lt.1)) then
    write(*,*) cComment//'Error! X or Y column not specified'
  endif
  temp_values(1) = 0D0
  do j = 2, rownum-1, 2
    temp_values(1) = temp_values(1) + &
                    (datatable(j+1, xcol_add(1))-datatable(j-1, xcol_add(1))) * &
                    (datatable(j-1, ycol_add(1))+4d0*datatable(j, ycol_add(1))+datatable(j+1, ycol_add(1)))/6d0
  enddo
  if (rownum - rownum/2*2 .eq. 0) then
    temp_values(1) = temp_values(1) + &
                     0.5D0*(datatable(rownum, xcol_add(1))-datatable(rownum-1, xcol_add(1))) * &
                    (datatable(rownum-1, ycol_add(1))+datatable(rownum, ycol_add(1)))
  endif
  write(*,*) temp_values(1)
case('int_avg')
  if ((xcol_num.lt.1).or.(ycol_num.lt.1)) then
    write(*,*) cComment//'Error! X or Y column not specified'
  endif
  temp_values(1:ycol_num) = 0D0
  do j = 2, rownum
    temp_values(1:ycol_num) = temp_values(1:ycol_num) + &
                     0.5D0*(datatable(j, xcol_add(1))-datatable(j-1, xcol_add(1))) * &
                     (datatable(j-1, ycol_add(1:ycol_num))+datatable(j, ycol_add(1:ycol_num)))
  enddo
  write(*,*) temp_values(1:ycol_num)/(datatable(rownum, xcol_add(1))-datatable(1, xcol_add(1)))

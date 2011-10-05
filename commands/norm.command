!normalize data
  case('norm', 'norm_max')
    temp_values(1:xcol_num)=maxval(datatable(1:rownum,xcol_add(1:xcol_num)),dim=2)
    do i = 1, rownum
      if (bSingleValue) then
        write(*,*) datatable(i, xcol_add(1:xcol_num))/temp_values(1:xcol_num)
      else
        datatable(i, xcol_add(1:xcol_num)) = datatable(i, xcol_add(1:xcol_num))/temp_values(1:xcol_num)
        write(*,*) datatable(i, 1:colnum)
      end if
    enddo !i
  case('norm_min')
    temp_values(1:xcol_num)=minval(datatable(1:rownum,xcol_add(1:xcol_num)),dim=2)
    do i = 1, rownum
      if (bSingleValue) then
        write(*,*) datatable(i, xcol_add(1:xcol_num))/temp_values(1:xcol_num)
      else
        datatable(i, xcol_add(1:xcol_num)) = datatable(i, xcol_add(1:xcol_num))/temp_values(1:xcol_num)
        write(*,*) datatable(i, 1:colnum)
      end if
    enddo !i
  case('norm_avg')
    temp_values(1:xcol_num)=sum(datatable(1:rownum,xcol_add(1:xcol_num)),dim=2)/rownum
    do i = 1, rownum
      if (bSingleValue) then
        write(*,*) datatable(i, xcol_add(1:xcol_num))/temp_values(1:xcol_num)
      else
        datatable(i, xcol_add(1:xcol_num)) = datatable(i, xcol_add(1:xcol_num))/temp_values(1:xcol_num)
        write(*,*) datatable(i, 1:colnum)
      end if
    enddo !i        
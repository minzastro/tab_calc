!statistics: average and dispersion
case ('stat')
  !Grouping is not yet supported
    temp_values2(1,1:xcol_num)=sum(transpose(datatable(1:rownum,xcol_add(1:xcol_num))),dim=2)/rownum
    do j = 1, rownum
      temp_values(1:xcol_num) = temp_values(1:xcol_num)+(datatable(j, xcol_add(1:xcol_num))-temp_values2(1,1:xcol_num))**2
    enddo !rownum
    temp_values2(2,1:xcol_num) = dsqrt(temp_values(1:xcol_num))/rownum
    call PrepareRealFormat(2*xcol_num)
    write(*,sFormat) temp_values2(1:2, 1:xcol_num)

!statistics: median and dispersion around it.
case ('stat_med')
    do j = 1, xcol_num
      call quick_sort(datatable(1:rownum, xcol_add(j)), long_values(1:rownum))
      temp_values2(1,j) = long_values((rownum+1)/2)
    enddo
    do j = 1, rownum
      temp_values(1:xcol_num) = temp_values(1:xcol_num)+(datatable(j, xcol_add(1:xcol_num))-temp_values2(1,1:xcol_num))**2
    enddo !rownum
    temp_values2(2,1:xcol_num) = dsqrt(temp_values(1:xcol_num))/rownum
    call PrepareRealFormat(2*xcol_num)
    write(*,sFormat) temp_values2(1:2, 1:xcol_num)

case('many')
    temp_values(1:xcol_num)=sum(transpose(datatable(1:rownum,xcol_add(1:xcol_num))),dim=2)/rownum
    k2 = min(len(xSubCommands),10)
    do i = 1, xcol_num
      do j = 1, k2
        call TStringArrayGet(xSubCommands, j, sKey)
        subcomm_case:select case (trim(sKey))
          case('avg')
            temp_values2(j, i) = temp_values(i)
          case('min')
            temp_values2(j, i) = minval(datatable(1:rownum, xcol_add(i)))
          case('max')
            temp_values2(j, i) = maxval(datatable(1:rownum, xcol_add(i)))
          case('dis')
            do k = 1, rownum
              temp_values2(j,i) = temp_values2(j,i)+(datatable(k, xcol_add(i))-temp_values(i))**2
            enddo !rownum
            temp_values2(j,i) = dsqrt(temp_values2(j,i))/rownum
        end select subcomm_case
      enddo
    enddo
    call PrepareRealFormat(k2*xcol_num)
    write(*,sFormat) temp_values2(1:k2, 1:xcol_num)

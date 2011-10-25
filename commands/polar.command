  case('polar')
    if ((xcol_num.ne.2).and.(xcol_num.ne.4)) then
      write(*,*) 'Please specify 2 or 4 columns with -x switch'
    end if
    if (bSingleValue) then
      call PrepareRealFormat(xcol_num)
    else
      call PrepareFormatAllExt()
    endif
    do i = 1, rownum
      temp_values(1:xcol_num) = datatable(i, xcol_add(1:xcol_num))
      temp_values(5) = dsqrt(temp_values(1)**2 + temp_values(2)**2)
      datatable(i, xcol_add(1)) = temp_values(5)
      datatable(i, xcol_add(2)) = datan2(temp_values(1)/temp_values(5), temp_values(2)/temp_values(5))
      if (xcol_num.eq.4) then
        temp_values(6) = datatable(i, xcol_add(3))**2 + datatable(i, xcol_add(4))**2
        temp_values(7) = datatable(i, xcol_add(3))*dcos(datatable(i, xcol_add(2))) -   &
                         datatable(i, xcol_add(4))*dsin(datatable(i, xcol_add(2)))
        temp_values(8) =  datatable(i, xcol_add(3))*dsin(datatable(i, xcol_add(2))) + &
                          datatable(i, xcol_add(4))*dcos(datatable(i, xcol_add(2)))
        datatable(i, xcol_add(3)) = temp_values(7)
        datatable(i, xcol_add(4)) = temp_values(8)
      endif
      if (bSingleValue) then
        write(*, sFormat) datatable(i, xcol_add(1:xcol_num))
      else
        call WriteFormattedLineX(datatable(i, 1:colnum), xFormat)
      endif
    enddo
      
      

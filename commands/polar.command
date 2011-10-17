  case('polar')
    if (xcol_num.ne.2) then
      write(*,*) 'Please specify 2 columns with -x switch'
    end if
    if (bSingleValue) then
      call PrepareRealFormat(2)
    else
      call PrepareFormatAllExt()
    endif
    do i = 1, rownum
      temp_values(1:2) = datatable(i, xcol_add(1:2))
      temp_values(3) = dsqrt(temp_values(1)**2 + temp_values(2)**2)
      datatable(i, xcol_add(1)) = temp_values(3)
      datatable(i, xcol_add(2)) = datan2(temp_values(1)/temp_values(3), temp_values(2)/temp_values(3))
      if (bSingleValue) then
        write(*, sFormat) datatable(i, xcol_add(1:2))
      else
        call WriteFormattedLineX(datatable(i, 1:colnum), xFormat)
      endif
    enddo
      
      

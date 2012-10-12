case('relative_error') !calculate error relative to the last value
    do i = 1, rownum
      if (bSingleValue) then
        write(*,*) dabs(datatable(i, xcol_add(1:xcol_num)) - datatable(rownum, xcol_add(1:xcol_num))) &
                   / datatable(rownum, xcol_add(1:xcol_num))
      else
        call PrepareFormatAllExt()
        datatable(i, xcol_add(1:xcol_num)) = dabs(datatable(i, xcol_add(1:xcol_num)) - &
                                                  datatable(rownum, xcol_add(1:xcol_num)))/datatable(rownum, xcol_add(1:xcol_num))
        call WriteFormattedLineX(datatable(i, 1:colnum), xFormat)
      end if
    enddo !i

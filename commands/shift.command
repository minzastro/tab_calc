!+ shift_center # shift column values so that average is 0
!+ shift_center_med # shift column values so that median is 0
    case('shift_center')
      do j = 1, xcol_num
        temp_values(j) = sum(datatable(1:rownum, xcol_add(j)))/rownum
        datatable(1:rownum, xcol_add(j)) = datatable(1:rownum, xcol_add(j)) - temp_values(j)
      enddo
      if (bSingleValue) then
        call PrepareFormatXcol()
        do j = 1, rownum
          write(*, sFormat) datatable(j, xcol_add(1:xcol_num))
        enddo
      else
        call PrepareFormatAll()
        do j = 1, rownum
          call WriteFormattedLineX(datatable(j, 1:colnum), xFormat)
        enddo
      endif
    case('shift_center_med')
      do j = 1, xcol_num
        call quick_sort_index(datatable(1:rownum, xcol_add(j)), &
                              long_values(1:rownum), &
                              index_values(1:rownum))
        temp_values(j) = long_values((rownum + 1)/2)   !50%
        datatable(1:rownum, xcol_add(j)) = datatable(1:rownum, xcol_add(j)) - temp_values(j)
      enddo
      if (bSingleValue) then
        call PrepareFormatXcol()
        do j = 1, rownum
          write(*, sFormat) datatable(j, xcol_add(1:xcol_num))
        enddo
      else
        call PrepareFormatAll()
        do j = 1, rownum
          call WriteFormattedLineX(datatable(j, 1:colnum), xFormat)
        enddo
      endif

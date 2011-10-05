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

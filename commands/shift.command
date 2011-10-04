	case('shift_center')
	  do j = 1, xcol_num
	    temp_values(j) = sum(datatable(xcol_add(j), 1:rownum))/rownum
	    datatable(xcol_add(j), 1:rownum) = datatable(xcol_add(j), 1:rownum) - temp_values(j)
	  enddo
	  if (bSingleValue) then
	    call PrepareFormatXcol()
	    do j = 1, rownum
	      write(*, sFormat) datatable(xcol_add(1:xcol_num), j)
	    enddo
	  else
	    call PrepareFormatAll()
	    do j = 1, rownum
	      write(*, sFormat) datatable(1:colnum, j)
	    enddo
	  endif

    case ('slope') !!todo
      do j = 1, colnum
        call quick_sort(datatable(1:rownum, j), long_values(1:rownum))
        temp_values2(1,j) = long_values(2*(rownum+1)/3)
        temp_values2(2,j) = long_values((rownum+1)/3)
      enddo
      if (bSingleValue) then
        if (xcol_num.gt.0) then
          write(*,*) temp_values2(1:2,xcol_add(1:xcol_num))
	else
          write(*,*) temp_values2(1:2,xcol_add(1):xcol_add(1))
	endif
      else
        write(*,*) temp_values2(1:2,1:colnum)
      end if
      
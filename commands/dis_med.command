case('dis_med') !Dispersion around median value
      do j = 1, xcol_num
        call quick_sort(datatable(1:rownum, xcol_add(j)), long_values(1:rownum))
        temp_values2(1,j) = long_values((rownum+1)/2)
      enddo
      do j = 1, rownum
        temp_values(1:xcol_num) = temp_values(1:xcol_num)+(datatable(j, xcol_add(1:xcol_num))-temp_values2(1,1:xcol_num))**2
      enddo !rownum
      temp_values(1:xcol_num) = dsqrt(temp_values(1:xcol_num))/rownum
      if (mode.eq.0) then
          write(*,*) temp_values(1:xcol_num)
      else
          write(*,*) temp_values(1:xcol_num), temp_values2(1,1:xcol_num)
      endif

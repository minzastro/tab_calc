    case ('hist2') !new histogramming
      call quick_sort(datatable(1:rownum, xcol_add(1)), long_values(1:rownum))
      density=rownum/(maxval(long_values)-minval(long_values))
      do j=rownum-step_num, 1+step_num, -1
        if (mode.eq.0) then
          temp_values(1)=(2D0*real(step_num)+1D0)/(long_values(j-step_num)-long_values(j+step_num))
        else
          !temp_values(1) = 0D0
          temp_values(2) = sum(long_values(j-step_num:j-1))
          temp_values(3) = sum(long_values(j+1:j+step_num))
          temp_values(1)=  dabs(real(step_num)**2/(temp_values(3)-temp_values(2)))
        end if
        write(*,*) long_values(j), temp_values(1)/density
      enddo

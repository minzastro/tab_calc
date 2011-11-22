    case('extremum', 'ext_min', 'ext_max', 'ext_all')
      do i = 2, rownum -1
        if (xcol_num.eq.1) then
          do j = 1, 3
            temp_values2(1, j) = i - 2 + j
          enddo
          temp_values2(2, 1:3) = datatable(i-1:i+1, xcol_add(1))
        else
          do j = 1, 3
            temp_values2(1:2, j) = datatable(j, xcol_add(1:2))
          enddo
        endif
        call fitParabola(temp_values2(1:2, 1:3), temp_values(1:3))
        temp_values(4) = -0.5d0*temp_values(2) / temp_values(1) ! Location of the minimum
        if (verbose) then
          write(*,*) 'Local parabola: ', temp_values(1:3)
        endif
        if ((temp_values(4).gt.temp_values2(1, 1)).and.(temp_values(4).lt.temp_values2(1, 3))) then
          if ((trim(sCommand).eq.'extremum').or.(trim(sCommand).eq.'ext_all').or. &
              ((temp_values(1).gt.0d0).and.(trim(sCommand).eq.'ext_min')).or. &
              ((temp_values(1).lt.0d0).and.(trim(sCommand).eq.'ext_max'))) then
            write(*, *) temp_values(4), temp_values(3) - temp_values(1)*temp_values(4)**2
          endif
        endif
      enddo
    

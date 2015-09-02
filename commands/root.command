    case ('root')
      if (datatable(1, xcol_add(2))-threshold .gt. 0D0) then
        iTemp=+1
      else
        iTemp=-1
      end if
      do j=2,rownum
        rTemp = (datatable(j, xcol_add(2))-threshold)*real(iTemp)
        if (rTemp .lt. 0D0) then
          x_1 =  datatable(j-1, xcol_add(1))
          x_2 =  datatable(j  , xcol_add(1))
          y_1 =  abs(datatable(j-1, xcol_add(2))-threshold)
          y_2 =  abs(datatable(j  , xcol_add(2))-threshold)
          rTemp = x_1 + abs((x_2-x_1)*y_1/(y_1 + y_2))
          write(*,*) rTemp
          iTemp=-iTemp
        endif
      enddo

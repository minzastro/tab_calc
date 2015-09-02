!+ deriv # first derivative
case('deriv')
    call PrepareFormatAllExt()
    !long_values(1:rownum) = datatable(1:rownum, xcol_add(2))
    do j = 2, rownum
      temp_values(1:colnum) = datatable(j, 1:colnum)
      do k = 2, xcol_num
        temp_values(xcol_add(k)) = ( datatable(j, xcol_add(k)) - datatable(j-1, xcol_add(k))) &
                                  / (datatable(j, xcol_add(1)) - datatable(j-1, xcol_add(1)))
      enddo
      write(*, *) temp_values(1:colnum)
    enddo

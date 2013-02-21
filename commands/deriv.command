!+ deriv # first derivative
case('deriv')
    call PrepareFormatAllExt()
    long_values(1:rownum) = datatable(1:rownum, xcol_add(2))
    do j = 2, rownum
      datatable(j, xcol_add(2)) = ( long_values(j) - long_values(j - 1)) / (datatable(j, xcol_add(1)) - datatable(j-1, xcol_add(1)))
      write(*, *) datatable(j, 1:colnum)
    enddo

!+cumul#Create a cumulative distribution
case('cumul')
    call quick_sort(datatable(1:rownum, xcol_add(1)), long_values(1:rownum))
    do j = 1, rownum
        write(*, *) long_values(j), dble(j)/dble(rownum), dble(j)
    enddo
!+cumul_back#Create an inverse cumulative distribution
case('cumul_back')
    call quick_sort(datatable(1:rownum, xcol_add(1)), long_values(1:rownum))
    do j = 1, rownum
        write(*, *) long_values(rownum - j + 1), dble(j)/dble(rownum), dble(j)
    enddo

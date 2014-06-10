!+ distr_med # distribution of medians
    case('distr_med')
      hist_data(:)=0
      distr_sum(:)=0
      !changind data limits
      if (range_min.eq.1D100) then
        range_min = minval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
      end if
      if (range_max.eq.-1D100) then
        range_max = maxval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
      end if
      step_size=(range_max-range_min)/step_num
      if (verbose) then
        print*, '# Distribution ', range_min, range_max
      endif
      do j = 0, step_num
        steps(j)=range_min + j*step_size+0.5D0*step_size
      enddo
      iGroupByIndex = int((datatable(:, xcol_add(1))-range_min)/step_size)
      do j = 0, step_num - 1
        bGroupByMask(1:rownum) = iGroupByIndex(1:rownum).eq.j
        k2 = count(bGroupByMask(1:rownum))
        call quick_sort(pack(datatable(1:rownum, xcol_add(1)), bGroupByMask(1:rownum)), &
                        long_values(1:k2))
        temp_value = long_values((k2+1)/2)
        write(*, *) steps(j), temp_value
      enddo

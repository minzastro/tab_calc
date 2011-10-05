    case ('fit_power')
      call quick_sort(datatable(1:rownum, xcol_add(1)), long_values(1:rownum))
      write(*,*) find_alpha(long_values(int(rownum/2)), long_values(1), long_values(rownum), 1d-5)
    case ('fit_power_tails')
      call quick_sort(datatable(1:rownum, xcol_add(1)), long_values(1:rownum))
      if (threshold.eq.0d0) then
        if (verbose) then
          write(*,*) cComment//'Zero threshold, substituting 0.5'
        end if
        threshold = 0.5d0
      end if
      k2 = int(rownum*threshold*0.5)
      write(*,*) find_alpha(long_values(k2), long_values(1), &
                            long_values(k2*2), 1d-5), &
                 find_alpha(long_values(rownum-k2), long_values(rownum-k2*2), &
                            long_values(rownum), 1d-5)

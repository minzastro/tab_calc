!this option is to be upgraded for good binning
    case('distr', 'distr_avg', 'distr_min', 'distr_max') !distribution
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
      do j = 1, rownum
        if ((datatable(j, xcol_add(1)).ge.range_min).and.(datatable(j, xcol_add(1)).le.range_max)) then
          k = int((datatable(j, xcol_add(1))-range_min)/step_size)
          hist_data(k) = hist_data(k) + 1
          distr_sum(k) = distr_sum(k) + datatable(j, xcol_add(2))
        endif
      enddo !j
      do j = 0, step_num-1
        if (hist_data(j).gt.0) then
          write(*,*) steps(j), distr_sum(j)/real(hist_data(j)), hist_data(j)
        else
          write(*,*) steps(j), 0D0, hist_data(j)
        endif
      enddo !j

!this option is to be upgraded for good binning
    case('distr2d') !2D distribution
      hist_data_2d(:,:)=0
      distr_sum_2d(:,:)=0
      !changind data limits
      if (range_min.eq.1D100) then
        range_min = minval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
        range_min_2 = minval(datatable(1:rownum, xcol_add(2):xcol_add(2)))
      else
        range_min_2 = range_min
      end if
      if (range_max.eq.-1D100) then
        range_max = maxval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
        range_max_2 = maxval(datatable(1:rownum, xcol_add(2):xcol_add(2)))
      else
        range_max_2 = range_max
      end if
      step_size=(range_max-range_min)/step_num
      step_size_2=(range_max_2-range_min_2)/step_num
      if (verbose) then
        print*, '# 2D-Distribution'
      endif
      do j = 0, step_num
        steps(j)=range_min + real(j)*step_size
        steps_2(j)=range_min_2 + real(j)*step_size_2
      enddo
      do j = 1, rownum
        if ((datatable(j, xcol_add(1)).ge.range_min  ).and. &
            (datatable(j, xcol_add(1)).le.range_max  ).and. &
            (datatable(j, xcol_add(2)).ge.range_min_2).and. &
            (datatable(j, xcol_add(2)).le.range_max_2))     then
          k  = int((datatable(j, xcol_add(1))-range_min)/step_size)
          k2 = int((datatable(j, xcol_add(2))-range_min_2)/step_size_2)
          hist_data_2d(k,k2) = hist_data_2d(k, k2) + 1
          distr_sum_2d(k,k2) = distr_sum_2d(k, k2) + datatable(j, xcol_add(3))
        endif
      enddo !j
      call PrepareCustomFormat('FFFI')
      do j = 0, step_num-1
        do k = 0, step_num-1
          if (hist_data_2d(j,k).gt.0) then
            write(*,sFormat) steps(j), steps_2(k), distr_sum_2d(j,k)/real(hist_data_2d(j,k)), hist_data_2d(j,k)
          else
            write(*,sFormat) steps(j), steps_2(k), 0D0, hist_data_2d(j,k)
          endif
        enddo !k
      enddo !j

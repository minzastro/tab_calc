!+ distr_dis # Data dispersion distribution (maximum y value in bins of x)
!this option is to be upgraded for good binning
  case('distr_dis') !distribution
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
      call quick_sort_index(datatable(1:rownum, xcol_add(1)), &
                            long_values(1:rownum), &
                            index_values(1:rownum))
       k = 1
       do j = 0, step_num
         iTemp = k
         do while ((datatable(index_values(k), xcol_add(1)).lt.steps(j)).and.(k.lt.rownum))
           k = k + 1
         enddo
         !write(*, *) iTemp, k, datatable(index_values(k), xcol_add(1)), steps(j)
         if (k.eq.iTemp) then
           cycle
         endif
         k = k - 1
         hist_data(j) = k - iTemp + 1
         distr_sum(j) = dsqrt(moment(datatable(index_values(iTemp:k), xcol_add(2)), &
                                     k-iTemp + 1, 2))
       enddo ! j
      call PrepareCustomFormat('FFI')
      do j = 0, step_num-1
        write(*,sFormat) steps(j), distr_sum(j), hist_data(j)
      enddo !j

    case ('hist_max', 'hist_min') !histogramming with subcommands
      if (trim(sCommand).eq.'hist_max') then
        distr_sum(:) = -huge(1d0)
      else
        distr_sum(:) = huge(1d0)
      endif
      call PrepareSteps()
      do j = 1, rownum
        if ((datatable(j, xcol_add(1)).ge.range_min).and.(datatable(j, xcol_add(1)).le.range_max)) then
          k = int((datatable(j, xcol_add(1))-range_min)/step_size)
          if ((trim(sCommand).eq.'hist_max').and.(datatable(j, xcol_add(2)).gt.distr_sum(k))) then
            distr_sum(k) = datatable(j, xcol_add(2))
            hist_data(k) = j
          else if ((trim(sCommand).eq.'hist_min').and.(datatable(j, xcol_add(2)).lt.distr_sum(k))) then
            distr_sum(k) = datatable(j, xcol_add(2))
            hist_data(k) = j
          endif
        endif
      enddo
      do j = 0, step_num - 1
        write(*,*) steps(j)+0.5*step_size, distr_sum(j), datatable(hist_data(j), xcol_add(1))
      enddo

!Strange power-law fitting
    case ('fit_log', 'fit_log_filter', 'fit_log_procent_filter') !fitting (logarithmic)
      if (range_min.eq.1D100) then
        range_min = minval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
      end if
      if (range_max.eq.-1D100) then
        range_max = maxval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
      end if
      call ApplyDataCuts(range_min, range_max, xcol_add(1))
      if (xcol_add(2).eq.XCOL_NULL) then
        if (ycol_add(1).eq.XCOL_NULL) then
          write(*,*) cComment//'Error! Second column for fitting not set'
	      else !get from Y columns
	        xcol_add(2) = ycol_add(1)
	      endif
      endif
      if (verbose) then
        if (any(datatable(1:rownum, xcol_add(1:2)).le.0D0)) then
          write(*,*) cComment//'Non-positive values from the input data will be omitted!'
        endif
      endif
      j = 1
      do while (j.le.rownum)
        if ((datatable(j, xcol_add(1)).le.0D0).or.(datatable(j, xcol_add(2)).le.0D0)) then
          datatable(j:rownum, xcol_add(1)) = datatable(j+1:rownum+1, xcol_add(1))
          datatable(j:rownum, xcol_add(2)) = datatable(j+1:rownum+1, xcol_add(2))
          rownum = rownum - 1
        else
          j = j + 1
        endif
      enddo
      if (trim(sCommand).eq.'fit_log') then
        call LinearFit(log(datatable(1:rownum, xcol_add(1):xcol_add(1))), &
                       log(datatable(1:rownum, xcol_add(2):xcol_add(2))), rownum, fit_a, fit_b)
      else if (trim(sCommand).eq.'fit_log_filter') then
        if (threshold.le.0d0) then
          if (verbose) then
            write(*,*) 'Threshold must be positive. Setting to default (2d0)'
          end if
          threshold = 2d0
        endif
        call LinearFitFilter(log(datatable(1:rownum, xcol_add(1):xcol_add(1))), &
                      log(datatable(1:rownum, xcol_add(2):xcol_add(2))), rownum, threshold, fit_a, fit_b)
      else !if (trim(sCommand).eq.'fit_log_procent_filter') then
        if (threshold.le.0d0) then
          if (verbose) then
            write(*,*) 'Threshold must be positive. Setting to default (2d0)'
          end if
          threshold = 2d0
        endif
        call LinearFitPercFilter(log(datatable(1:rownum, xcol_add(1):xcol_add(1))), &
                      log(datatable(1:rownum, xcol_add(2):xcol_add(2))), rownum, threshold, fit_a, fit_b)
      end if
      if (verbose) then
        write(*,*) 'Fitting performed on ', rownum, ' data points'
        write(*,*) 'y = ', exp(fit_b), '*x**', fit_a
      else
        write(*,*) fit_a, exp(fit_b)
      endif

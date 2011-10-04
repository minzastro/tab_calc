    case ('xhist') !histogramming
      hist_data(:)=0
      !changind data limits
      if (range_min.eq.1D100) then
        range_min = minval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
      end if
      if (range_max.eq.-1D100) then
        range_max = maxval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
      end if
      if (verbose) then
        if (trim(filename).eq.'') then
          write(*,*) '# Generated from $stdin'
	else
          write(*,*) '# Generated from: ', trim(filename)
	endif
        write(*,*) '# Minimum= ', range_min, ' Maximum=', range_max, ' Step=', step_size
      end if
      call iterativeHistBuilder
      do j = 0, step_num - 1
        xhist_modes: select case (mode)
        case(0)
          write(*,*) steps(j)+0.5*step_size, hist_data(j)
        case(1)
          write(*,*) steps(j)+0.5*step_size, real(hist_data(j))/maxval(hist_data(0:step_num)), hist_data(j)
        case(2)
          write(*,*) steps(j)+0.5*step_size, real(hist_data(j))/rownum, hist_data(j)
        case(3)
          write(*,*) steps(j)+0.5*step_size, real(hist_data(j))*(range_max-range_min)/rownum, hist_data(j)
        end select xhist_modes
      enddo

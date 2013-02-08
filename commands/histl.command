!+ histl # histogram with logarithmic binning
    case ('histl') !histogramming
      hist_data(:)=0
      !changind data limits
      if (range_min.eq.1D100) then
        range_min = minval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
      end if
      if (range_max.eq.-1D100) then
        range_max = maxval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
      end if
      step_size=(log(range_max)-log(range_min))/step_num
      if (verbose) then
        if (trim(filename).eq.'') then
          write(*,*) '# Generated from $stdin'
	else
          write(*,*) '# Generated from: ', trim(filename)
	endif
        write(*,*) '# Minimum= ', range_min, ' Maximum=', range_max, ' Step=', step_size
      end if
      do j = 0, step_num
        steps(j)=range_min * exp(j*step_size)
      enddo
      do j = 1, rownum
        if ((datatable(j, xcol_add(1)).ge.range_min).and.(datatable(j, xcol_add(1)).le.range_max)) then
          k = int((log(datatable(j, xcol_add(1)))-log(range_min))/step_size)
          hist_data(k) = hist_data(k) + 1
	endif
      enddo
      do j = 0, step_num - 1
        histl_modes: select case (mode)
        case(0)
!          if (j.eq.0) then
!            write(*,*) steps(j), real(hist_data(j))*(range_max-range_min)/(step_num*steps(1))
!          else
            write(*,*) steps(j), real(hist_data(j))*(range_max-range_min)/(step_num*(steps(j+1)-steps(j)))
!          endif
        case(1)
          write(*,*) steps(j), real(hist_data(j))/maxval(hist_data(0:step_num))
        case(2)
          write(*,*) steps(j), real(hist_data(j))/rownum
        case(3)
          write(*,*) steps(j), real(hist_data(j))*(range_max-range_min)/rownum
        case(4)
          write(*,*) steps(j), hist_data(j)
        end select histl_modes
      enddo

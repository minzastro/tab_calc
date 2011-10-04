    case ('fit_ext', 'fit_filter') !fitting (linear) with extended output
      if (xcol_add(2).eq.XCOL_NULL) then
        if (ycol_add(1).eq.XCOL_NULL) then
          write(*,*) 'Error! Second column for fitting not set'
	else !get from Y columns
	  xcol_add(2) = ycol_add(1)
	endif
      endif
      if (trim(sCommand).eq.'fit_ext') then
        call LinearFit(log(datatable(1:rownum, xcol_add(1):xcol_add(1))), &
                      log(datatable(1:rownum, xcol_add(2):xcol_add(2))), rownum, fit_a, fit_b)
      else                
        call LinearFitFilter(log(datatable(1:rownum, xcol_add(1):xcol_add(1))), &
                      log(datatable(1:rownum, xcol_add(2):xcol_add(2))), rownum, fit_a, fit_b, threshold)
      end if
      fit_modes: select case (mode)
        case(0)
          write(*,*) fit_a, fit_b
        case(1)
          do j = 1, rownum
            write(*,*) datatable(j, xcol_add(1)), datatable(j, xcol_add(2)),  datatable(j, xcol_add(1))*fit_a + fit_b, &
                                                  datatable(j, xcol_add(2)) - datatable(j, xcol_add(1))*fit_a - fit_b
          enddo
      end select fit_modes

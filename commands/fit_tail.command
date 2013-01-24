!+ fit_tail # power-law fits to a upper and lower 1/5 parts of a dataset
    case('fit_tail', 'fit_tails')
        call fit_power_tails(datatable(1:rownum, xcol_add(1)), rownum, temp_values(1), &
                             temp_values(2), temp_values(3), temp_values(4), temp_values(5))
        if (mode.eq.0) then
          call PrepareRealFormat(5)
          write(*,sFormat) temp_values(1:5)
        else
          call PrepareRealFormat(3)
          write(*,sFormat) temp_values(1), temp_values(3), temp_values(5)
        endif

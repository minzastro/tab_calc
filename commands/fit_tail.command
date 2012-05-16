    case('fit_tail')
        call fit_power_tails(datatable(1:rownum, xcol_add(1)), rownum, temp_values(1), &
                             temp_values(2), temp_values(3), temp_values(4), temp_values(5))
        call PrepareRealFormat(5)
        write(*,sFormat) temp_values(1:5)

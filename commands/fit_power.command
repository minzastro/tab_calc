!+ fit_power # negative power-law fit to a dataset
!+ fit_positive_power # positive power-law fit to a dataset
!+ fit_power_tail # power-law fits to a upper and lower 1/5 parts of a dataset
    case ('fit_power')
      write(*,*) find_alpha(datatable(1:rownum, xcol_add(1)), rownum, minval(datatable(1:rownum, xcol_add(1))))
    case ('fit_positive_power')
      write(*,*) find_positive_alpha(datatable(1:rownum, xcol_add(1)), rownum, maxval(datatable(1:rownum, xcol_add(1))))
    case ('fit_power_limit')
      write(*,*) find_alpha(datatable(1:rownum, xcol_add(1)), rownum, threshold)

    case ('fit_power')
      write(*,*) find_alpha(datatable(1:rownum, xcol_add(1)), rownum, minval(datatable(1:rownum, xcol_add(1))))

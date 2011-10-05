    case ('fit') !fitting (linear) with extended output
      call RequireXCols(2)
      call TabCalcFit(.false., .false., 0, .false., fit_a, fit_b)
    case ('fit_filter')
      call RequireXCols(2)
      call TabCalcFit(.false., .false., 1, .false., fit_a, fit_b)
    case ('fit_percent_filter')
      call RequireXCols(2)
      call TabCalcFit(.false., .false., 2, .false., fit_a, fit_b)
    case ('fit_weight', 'fitw') ! weighted fitting
      call RequireXCols(3)
      call TabCalcFit(.false., .false., 0, .true., fit_a, fit_b)
    case ('fit_scan')
      call RequireXCols(2)
      call TabCalcFit(.false., .false., 3, .false., fit_a, fit_b)

    case ('fit_log')
      call RequireXCols(2)
      call TabCalcFit(.true., .true., 0, .false., fit_a, fit_b)
    case ('fit_log_filter')
      call RequireXCols(2)
      call TabCalcFit(.true., .true., 1, .false., fit_a, fit_b)
    case ('fit_log_percent_filter')
      call RequireXCols(2)
      call TabCalcFit(.true., .true., 2, .false., fit_a, fit_b)
    case ('fit_log_weight', 'fit_logw') ! weighted fitting
      call RequireXCols(3)
      call TabCalcFit(.true., .true., 0, .true., fit_a, fit_b)
    case ('fit_log_scan')
      call RequireXCols(2)
      call TabCalcFit(.true., .true., 3, .false., fit_a, fit_b)

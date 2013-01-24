!+ fit # fitting (linear) with extended output
!+ fit_filter # fitting (linear) with outliers removed
!+ fit_percent_filter # fitting (linear) with outliers removed
!+ fit_weight # weighted fitting (linear)
!+ fit_scan # ???
!+ fit_log # fitting (logarithmic) with extended output
!+ fit_log_filter # fitting (logarithmic) with outliers removed
!+ fit_log_percent_filter # fitting (logarithmic) with outliers removed
!+ fit_log_weight # weighted fitting (logarithmic)
!+ fit_log_scan # ???

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

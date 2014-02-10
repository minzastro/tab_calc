module tcUtils !Special utility functions for TabCalc
!
!
!
use tcGlobals
use tcOutput
use tcFit
use quickSort
use array_works
implicit none

contains

subroutine PrepareSteps() ! Fill steps data according to a given range
integer j
  !changind data limits
  if (range_min.eq.1D100) then
    range_min = minval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
  end if
  if (range_max.eq.-1D100) then
    range_max = maxval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
  end if
  step_size=(range_max-range_min)/step_num
  if (verbose) then
    if (trim(filename).eq.'') then
      write(*,*) '# Generated from $stdin'
    else
      write(*,*) '# Generated from: ', trim(filename)
    endif
    write(*,*) '# Minimum= ', range_min, ' Maximum=', range_max, ' Step=', step_size
  end if
  do j = 0, step_num - 1
    steps(j)=range_min + j*step_size
  enddo
end subroutine PrepareSteps

subroutine ApplyDataCuts(xMin, xMax, iCol) !Removes all data outside a given range
real*8, intent(in) :: xMin ! Lower-limit
real*8, intent(in) :: xMax ! Upper-limit
integer, intent(in) :: iCol ! Column index
integer i, iCountOk
  i = 1
  iCountOk = count(datatable(1:rownum, iCol).le.xMax)
  index_values(1:iCountOk) = pack(datatable(1:rownum, 0), datatable(1:rownum, iCol).le.xMax)
  datatable(1:iCountOk, 1:colnum) = datatable(index_values(1:iCountOk), 1:colnum)
  rownum = iCountOk
  iCountOk = count(datatable(1:rownum, iCol).ge.xMin)
  index_values(1:iCountOk) = pack(datatable(1:rownum, 0), datatable(1:rownum, iCol).ge.xMin)
  datatable(1:iCountOk, 1:colnum) = datatable(index_values(1:iCountOk), 1:colnum)
  rownum = iCountOk
  !iCountOk = count(datatable(1:rownum, iCol).ge.xMin)
  !datatable(1:iCountOk, 1:colnum) = pack(datatable(1:rownum, 1:colnum), &
  !                                       datatable(1:rownum, iCol).ge.xMin)
  !rownum = iCountOk
!  do while (i.le.rownum)
!    if ((datatable(i, iCol).gt.xMax).or.(datatable(i, iCol).lt.xMin)) then
!      datatable(i:rownum-1, :) = datatable(i+1:rownum, :)
!      rownum = rownum - 1
!    else
!      i = i + 1
!    endif
!  enddo
end subroutine ApplyDataCuts


subroutine TabCalcFit(bLogX, bLogY, iFilterMode, bWeighted, a, b)
logical, intent(in) :: bLogX, bLogY ! Use logarithmic scale
integer, intent(in) :: iFilterMode ! Filter: 0-none, 1-sigma, 2-percent, 3-window
logical, intent(in) :: bWeighted ! Use third column as weights
real*8, intent(out) :: a, b ! y = a*x + b fit
real*8 x_data(rownum), y_data(rownum), w_data(rownum)
real*8 a_set(rownum), b_set(rownum)
integer j
  if (bLogX.or.bLogY) then !Clear data from negative values
    if (verbose) then
      if (any(datatable(1:rownum, xcol_add(1:2)).le.0D0)) then
        write(*,*) cComment//'Non-positive values from the input data will be omitted!'
      endif
    endif
    j = 1
    do while (j.le.rownum)
      if ((bLogX.and.(datatable(j, xcol_add(1)).le.0D0)).or. &
          (bLogY.and.(datatable(j, xcol_add(2)).le.0D0))) then
        datatable(j:rownum, xcol_add(1)) = datatable(j+1:rownum+1, xcol_add(1))
        datatable(j:rownum, xcol_add(2)) = datatable(j+1:rownum+1, xcol_add(2))
        rownum = rownum - 1
      else
        j = j + 1
      endif
    enddo
  endif

  if (bLogX) then
    x_data(1:rownum) = dlog10(datatable(1:rownum, xcol_add(1)))
  else
    x_data(1:rownum) = datatable(1:rownum, xcol_add(1))
  endif
  if (bLogY) then
    y_data(1:rownum) = dlog10(datatable(1:rownum, xcol_add(2)))
  else
    y_data(1:rownum) = datatable(1:rownum, xcol_add(2))
  endif
  if (bWeighted) then
    w_data(1:rownum) = datatable(1:rownum, xcol_add(3))
  else
    w_data(1:rownum) = 1d0
  endif
  fit_case: select case (iFilterMode)
    case (0)
      call LinearFitWeight(x_data(1:rownum), y_data(1:rownum), w_data(1:rownum), rownum, a, b)
    case (1)
      if (threshold.le.0d0) then
        if (verbose) then
          write(*,*) cComment//'Threshold must be positive. Setting to default (2d0)'
        endif
        threshold = 2d0
      endif
      call LinearFitFilter(x_data(1:rownum), y_data(1:rownum), rownum, threshold, a, b)
    case (2)
      if (threshold.le.0d0) then
        if (verbose) then
          write(*,*) cComment//'Threshold must be positive. Setting to default (50%)'
        endif
        threshold = 50d0
      endif
      call LinearFitPercFilter(x_data(1:rownum), y_data(1:rownum), rownum, threshold, a, b)
    case (3)
      a = 0d0
      b = 0d0
      call LinearFitScanned(x_data(1:rownum), y_data(1:rownum), rownum, step_num, a_set, b_set)
  end select fit_case

  if (iFilterMode.ne.3) then
    fit_modes: select case (mode)
      case(0)
        call PrepareRealFormat(2)
        write(*,sFormat) a, b
      case(1)
        call PrepareRealFormat(4)
        do j = 1, rownum
          write(*,sFormat) datatable(j, xcol_add(1)),  &
                           datatable(j, xcol_add(2)),  &
                           datatable(j, xcol_add(1))*a + b, &
                           datatable(j, xcol_add(2)) - datatable(j, xcol_add(1))*a - b
        enddo
    end select fit_modes
  else
    call PrepareRealFormat(3)
    do j = 1, rownum
      write(*, sFormat) datatable(j, xcol_add(1)), a_set(j), b_set(j)
    enddo
  endif
end subroutine TabCalcFit

subroutine FillGroupBySums() !Fills Group-by columns
integer j,k
logical flag
  aGroupByValues(:,:) = 0D0
  iGroupByCount = 0
  do j = 1, rownum
    flag = .false.
    do k = 1, iGroupByCount
      if (count(aGroupByValues(k, 1:iGroupByColumns) .ne. datatable(j, aGroupByColumns(1:iGroupByColumns))).eq.0) then
        aGroupByValues(k, -2) = aGroupByValues(k, -2) + datatable(j, xcol_add(1))
        aGroupByValues(k, -1) = aGroupByValues(k, -1) + 1
        iGroupByIndex(j) = k
        flag = .true.
        exit
      endif
    enddo
    if (.not.flag) then
      iGroupByCount = iGroupByCount + 1
      iGroupByIndex(j) = iGroupByCount
      aGroupByValues(iGroupByCount, 1:iGroupByColumns) = datatable(j, aGroupByColumns(1:iGroupByColumns))
      aGroupByValues(iGroupByCount, -2) = aGroupByValues(iGroupByCount, -2) + datatable(j, xcol_add(1))
      aGroupByValues(iGroupByCount, -1) = aGroupByValues(iGroupByCount, -1) + 1
    endif
  enddo
end subroutine FillGroupBySums

subroutine RemoveNanRows() !Remove rows with NaN values in columns under consideration
integer iR
  iR = 1
  do while (iR.le.rownum)
    if (any(datatable(iR, xcol_add(1:xcol_num)).eq.NANVALUE)) then
      datatable(iR:rownum-1, 1:colnum) = datatable(iR+1:rownum, 1:colnum)
      rownum = rownum - 1
    else
      iR = iR + 1
    endif
  enddo
end subroutine RemoveNanRows

end module tcUtils

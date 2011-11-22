module tcUtils !Special utility functions for TabCalc
!
!
!
use tcGlobals
use tcOutput
use quickSort
use array_works
implicit none

contains
subroutine ApplyDataCuts(xMin, xMax, iCol) !Removes all data outside a given range
real*8, intent(in) :: xMin ! Lower-limit
real*8, intent(in) :: xMax ! Upper-limit
integer, intent(in) :: iCol ! Column index
integer i
  i = 1
  do while (i.le.rownum)
    if ((datatable(i, iCol).gt.xMax).or.(datatable(i, iCol).lt.xMin)) then
      datatable(i:rownum-1, :) = datatable(i+1:rownum, :)
      rownum = rownum - 1
    else
      i = i + 1
    endif
  enddo
end subroutine ApplyDataCuts

!! Least square fitting subroutine
subroutine LinearFit(x, y, asize, a, b)
real*8, intent(in) :: x(asize), y(asize) !! input data
integer, intent(in) :: asize !! size of input arrays
real*8, intent(out):: a,b !! y = a*x + b : result
!local variables
real*8 xx, x2, xy, yy
real*8 xy0, yy0, y_(asize)
real*8 det, a0, b0
  xx = sum(x(:))
  yy = sum(y(:))
  x2 = sum(x(:)*x(:))
  xy = sum(x(:)*y(:))
  det = 1D0/(asize*x2-xx*xx)
  a = (asize*xy-xx*yy)*det
  b = (yy*x2 - xx*xy)*det
end subroutine LinearFit

subroutine LinearFitWeight(x, y, w, asize, a, b)
real*8, intent(in) :: x(asize), y(asize) !! input data
real*8, intent(in) :: w(asize) ! Weights
integer, intent(in) :: asize !! size of input arrays
real*8, intent(out):: a,b !! y = a*x + b : result
!local variables
real*8 xx, x2, xy, yy, ww
real*8 xy0, yy0, y_(asize)
real*8 det, a0, b0
  xx = sum(x(:)*w(:))
  yy = sum(y(:)*w(:))
  ww = sum(w(:)) !*dble(asize)
  x2 = sum(x(:)*x(:)*w(:))
  xy = sum(x(:)*y(:)*w(:))
  det = 1D0/(ww*x2-xx*xx)
  a = (ww*xy - xx*yy)*det
  b = (yy*x2 - xx*xy)*det
end subroutine LinearFitWeight

subroutine LinearFitFilter(x, y, asize, xSigma, a, b)
! Performs linear fit, then cuts off all outliers (more than xSigma
! dispersions) and re-fits.
real*8, intent(in) :: x(asize), y(asize) !! input data
integer, intent(in) :: asize !! size of input arrays
real*8, intent(in) :: xSigma ! Cut-off limit
real*8, intent(out):: a,b !! y = a*x + b : result

real*8 sigma, xx(asize), yy(asize)
integer i, arrsize
  call LinearFit(x, y, asize, a, b)
  sigma = 0D0
  do i = 1, asize
    sigma = sigma + (y(i) - a*x(i)-b)**2
  enddo
  sigma = dsqrt(sigma)/dble(asize)
  arrsize = asize
  i = 1
  xx(:) = x(:)
  yy(:) = y(:)
  do while (i.le.arrsize)
    if (dabs(yy(i)-a*xx(i)-b).gt.(xSigma*sigma)) then
      xx(i:arrsize-1) = xx(i+1:arrsize)
      yy(i:arrsize-1) = yy(i+1:arrsize)
      arrsize = arrsize - 1
    else
      i = i + 1
    endif
  enddo
  if (arrsize.gt.1) then
    call LinearFit(xx(1:arrsize), yy(1:arrsize), arrsize, a, b)
  else if (verbose) then
    write(*,*) 'Filter is too hard. No data left'
  endif
end subroutine LinearFitFilter

subroutine LinearFitPercFilter(x, y, asize, xSigma, a, b)
! Performs linear fit, then cuts off all outliers (leaving xSigma
! procents of best-fit data, and then re-fits.
real*8, intent(in) :: x(asize), y(asize) !! input data
integer, intent(in) :: asize !! size of input arrays
real*8, intent(in) :: xSigma ! Percentage of data to keep
real*8, intent(out):: a,b !! y = a*x + b : result

real*8 sigma, xx(asize), yy(asize), dy(asize)
integer i, arrsize, iDy(asize)
  call LinearFit(x, y, asize, a, b)
  xx(:) = x(:)
  yy(:) = y(:)
  dy(:) = abs(y(:) - a*x(:) - b)
  call quick_sort_index(dy(:), dy(:), iDy(:))
  arrsize = int(asize*xSigma*0.01D0)+1
  if (arrsize.gt.asize) then
    arrsize = asize
  endif
  xx(1:arrsize) = x(iDy(1:arrsize))
  yy(1:arrsize) = y(iDy(1:arrsize))
  if (arrsize.gt.1) then
    call LinearFit(xx(1:arrsize), yy(1:arrsize), arrsize, a, b)
  else if (verbose) then
    write(*,*) 'Filter is too hard. No data left'
  endif
end subroutine LinearFitPercFilter

real*8 function ScanWeight(i, w)
integer, intent(in) :: i, w
  ScanWeight = exp(-10d0*dble(i*i)/dble(w*w))
end function ScanWeight

subroutine LinearFitScanned(x, y, asize, window, a, b)
real*8, intent(in) :: x(asize), y(asize) !! input data
integer, intent(in) :: asize !! size of input arrays
integer, intent(in) :: window ! Percentage of data to keep
real*8, intent(out):: a(asize),b(asize) !! y = a*x + b : result
real*8 weight(asize)
integer i, j, w
do i = 1, window
  do j = 1, i+window
    weight(j) = ScanWeight(j-i, window)
    write(33,*) x(j), y(j), weight(j)
  enddo
  weight(i+window+1:asize) = 0
  call LinearFitWeight(x(1:i+window), y(1:i+window), weight(1:i+window), i+window, a(i), b(i))
  write(33,*) a(i), b(i), i
enddo
do i = window + 1, asize - window
  do j = i - window, i + window
    weight(j - i + window + 1) = ScanWeight(j - i, window)
  enddo
  call LinearFitWeight(x(i-window:i+window), y(i-window:i+window), &
                       weight(1:2*window+1), 2*window+1, a(i), b(i))
enddo
do i = asize-window + 1, asize
  do j = i, asize
    weight(j) = ScanWeight(j-i, window)
  enddo
  call LinearFitWeight(x(i:asize), y(i:asize), weight(1:asize-i+1), asize-i + 1, a(i), b(i))
enddo
end subroutine LinearFitScanned


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
        flag = .true.
        exit
      endif
    enddo
    if (.not.flag) then
      iGroupByCount = iGroupByCount + 1
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

recursive function calcMatrix(a, n) result (x)
real*8, intent(in) :: a(n, n)
integer, intent(in) :: n
real*8 :: x
real*8 temp, b(n-1, n-1)
integer i, iSig
  if (n.eq.2) then
    x = a(1, 1)*a(2, 2) - a(1, 2)*a(2, 1)
    return
  endif
  iSig = 1
  temp = 0d0
  do i = 1, n
    b(1:n-1, 1:i-1) = a(2:n, 1:i-1)
    b(1:n-1, i:n-1) = a(2:n, i+1:n)
    temp = temp + dble(iSig)*a(1, i)*calcMatrix(b, n-1)
    iSig = -iSig
  enddo
  x = temp
end function calcMatrix

subroutine solveSystem(a, b, n, x)
real*8, intent(in) :: a(n, n), b(n)
integer, intent(in) :: n
real*8, intent(out) :: x(n)
integer i
real*8 da, dx, a1(n, n)
  da = calcMatrix(a, n)
  do i = 1, n
    a1 = a
    a1(:, i) = b(:)
    dx = calcMatrix(a1, n)
    x(i) = dx/da
  enddo
end subroutine solveSystem

subroutine fitParabola(pts, coeff)
real*8, intent(in) :: pts(2, 3) ! x & y coordinates for 3 points
real*8, intent(out) :: coeff(3) ! a, b, c for y = ax**2 + bx + c
real*8 aaa(3, 3), bbb(3)
integer i
  aaa(:, 3) = 1d0
  bbb(:) = pts(2, :)
  do i = 1, 3
    aaa(i, 1) = pts(1, i)**2
    aaa(i, 2) = pts(1, i)
  enddo
  call solveSystem(aaa, bbb, 3, coeff)
end subroutine fitParabola

end module tcUtils

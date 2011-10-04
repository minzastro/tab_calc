module tcUtils !Special utility functions for TabCalc
!
!
!
use tcGlobals
use quickSort
use array_works
implicit none 

contains
subroutine ApplyDataCuts(xMin, xMax, iCol)
real*8, intent(in) :: xMin
real*8, intent(in) :: xMax
integer, intent(in) :: iCol
integer i
  i = 1
  do while (i.le.rownum)
    if ((datatable(i, iCol).gt.xMax).or.(datatable(i, iCol).lt.xMin)) then
      datatable(i:rownum-1, :) = datatable(i+1:rownum, :) 
      rownum = rownum - 1
    else
      i = i + 1
    end if
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

  y_(:) = -a*x(:) - b + y(:)
  yy0 = sum(y_(:))
  xy0 = sum(x(:)*y_(:))
  a0 = (asize*xy0-xx*yy0)*det
  b0 = (yy0*x2 - xx*xy0)*det
  a = a + a0
  b = b + b0
end subroutine LinearFit

subroutine LinearFitFilter(x, y, asize, xSigma, a, b)
real*8, intent(in) :: x(asize), y(asize) !! input data
integer, intent(in) :: asize !! size of input arrays
real*8, intent(in) :: xSigma
real*8, intent(out):: a,b !! y = a*x + b : result

real*8 sigma, xx(asize), yy(asize)
integer i, arrsize
  call LinearFit(x, y, asize, a, b)
  sigma = 0D0
  do i = 1, asize
    sigma = sigma + (y(i) - a*x(i)-b)**2
  enddo
  sigma = dsqrt(sigma)/dble(asize)
!   write(*,*) a, b, sigma
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
    end if
  enddo
  if (arrsize.gt.1) then
    call LinearFit(xx(1:arrsize), yy(1:arrsize), arrsize, a, b)
  else if (verbose) then
    write(*,*) 'Filter is too hard. No data left'
  end if
end subroutine LinearFitFilter

subroutine LinearFitPercFilter(x, y, asize, xSigma, a, b)
real*8, intent(in) :: x(asize), y(asize) !! input data
integer, intent(in) :: asize !! size of input arrays
real*8, intent(in) :: xSigma
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
  end if
  xx(1:arrsize) = x(iDy(1:arrsize))
  yy(1:arrsize) = y(iDy(1:arrsize))
  do i = 1, arrsize
    write(44, *) exp(xx(i)), exp(yy(i))
  enddo
  if (arrsize.gt.1) then
    call LinearFit(xx(1:arrsize), yy(1:arrsize), arrsize, a, b)
  else if (verbose) then
    write(*,*) 'Filter is too hard. No data left'
  end if
end subroutine LinearFitPercFilter

subroutine FillGroupBySums
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
      end if
    enddo
    if (.not.flag) then
      iGroupByCount = iGroupByCount + 1
      aGroupByValues(iGroupByCount, 1:iGroupByColumns) = datatable(j, aGroupByColumns(1:iGroupByColumns))
      aGroupByValues(iGroupByCount, -2) = aGroupByValues(iGroupByCount, -2) + datatable(j, xcol_add(1))
      aGroupByValues(iGroupByCount, -1) = aGroupByValues(iGroupByCount, -1) + 1
    end if
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

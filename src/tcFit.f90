module tcFit
use tcGlobals
use tcOutput
use tcMath
use quickSort
use array_works
implicit none

contains

!! Least square fitting subroutine
subroutine LinearFit(x, y, asize, a, b)
integer, intent(in) :: asize !! size of input arrays
real*8, intent(in) :: x(asize), y(asize) !! input data
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
integer, intent(in) :: asize !! size of input arrays
real*8, intent(in) :: x(asize), y(asize) !! input data
real*8, intent(in) :: w(asize) ! Weights
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
integer, intent(in) :: asize !! size of input arrays
real*8, intent(in) :: x(asize), y(asize) !! input data
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
integer, intent(in) :: asize !! size of input arrays
real*8, intent(in) :: x(asize), y(asize) !! input data
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

function ScanWeight(i, w)
real*8 ScanWeight
integer, intent(in) :: i, w
  ScanWeight = exp(-10d0*dble(i*i)/dble(w*w))
end function ScanWeight


subroutine LinearFitScanned(x, y, asize, window, a, b)
integer, intent(in) :: asize !! size of input arrays
real*8, intent(in) :: x(asize), y(asize) !! input data
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

subroutine errorEllipse(posX, posY, axisA, axisB, tilt)
real*8, intent(out) :: posX, posY, tilt, axisA, axisB
real*8 det(2, 2), disp(2), axes(2)
real*8 a, b
  posX = sum(datatable(1:rownum, xcol_add(1)))/rownum
  posY = sum(datatable(1:rownum, xcol_add(2)))/rownum
  disp(1) = sqrt(sum((datatable(1:rownum, xcol_add(1)) - posX)**2)/rownum)
  disp(2) = sqrt(sum((datatable(1:rownum, xcol_add(2)) - posY)**2)/rownum)
  call LinearFit(datatable(1:rownum, xcol_add(1)), datatable(1:rownum, xcol_add(2)), rownum, a, b)
  tilt = datan(a)
  write(*, *) posX, posY, disp(:), a, b, tilt
  det(1, 1) = dcos(tilt)
  det(1, 2) = -dsin(tilt)
  det(2, 2) = det(1, 1)
  det(2, 1) = -det(1, 2)
  call solveSystem(det, disp, 2, axes)
  axes(:) = dabs(axes(:))
  if (axes(1).gt.axes(2)) then
    axisA = axes(1)
    axisB = axes(2)
  else
    axisA = axes(2)
    axisB = axes(1)
  endif
end subroutine errorEllipse
end module tcFit

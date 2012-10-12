module tcMath
implicit none

interface moment
  module procedure moment1d
  module procedure moment2d
end interface moment

contains

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

real*8 function moment1d(arr, asize, order, average)
integer, intent(in) :: asize
real*8, intent(in) :: arr(asize)
integer, intent(in) :: order
real*8, intent(in), optional :: average
integer i
real*8 tmp, avg
  if (present(average)) then
    avg = average
  else
    avg = sum(arr(1:asize))/asize
  endif
  tmp = 0d0
  do i = 1, asize
    tmp = tmp + (arr(i) - avg) ** order
  enddo
  moment1d = tmp/asize
end function moment1d

function moment2d(arr, asize, asize2, order, average) result (res)
integer, intent(in) :: asize
integer, intent(in) :: asize2
real*8, intent(in) :: arr(asize, asize2)
integer, intent(in) :: order
real*8, intent(in), optional :: average(asize2)
real*8 :: res(asize2)

integer i
real*8 tmp(asize2), avg(asize2)
  if (present(average)) then
    avg = average
  else
    avg = sum(arr(1:asize, :), dim=1)/asize
  endif
  tmp(:) = 0d0
  do i = 1, asize
    tmp(:) = tmp(:) + (arr(i,:) - avg(:)) ** order
  enddo
  res(:) = tmp(:)/asize
end function moment2d

end module tcMath

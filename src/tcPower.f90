module tcPower
use tcGlobals
use quickSort
use tcFit

implicit none

contains

function find_alpha(in_x, asize, x_cut) result (alpha)
integer, intent(in) :: asize
real*8, intent(in) :: in_x(asize), x_cut
real*8 tmp, alpha
integer i, n
  tmp = 0d0
  n = 0
  do i = 1, asize
    if (in_x(i).ge.x_cut) then
      n = n + 1
      tmp = tmp + dlog(in_x(i)/x_cut)
    endif
  enddo
  alpha = -(1d0 + n/tmp)
end function find_alpha

function find_positive_alpha(in_x, asize, x_cut) result (alpha)
integer, intent(in) :: asize
real*8, intent(in) :: in_x(asize), x_cut
real*8 tmp, alpha
integer i, n
  tmp = 0d0
  n = 0
  do i = 1, asize
    if (in_x(i).le.x_cut) then
      n = n + 1
      tmp = tmp + dlog(in_x(i)/x_cut)
    endif
  enddo
  alpha = -(1d0 + n/tmp)
end function find_positive_alpha

subroutine fit_power_tails(in_x, asize, out_a1, out_b1, out_a2, out_b2, out_med)
integer, intent(in) :: asize
real*8, intent(in) :: in_x(asize)
real*8, intent(out) :: out_a1, out_a2, out_b1, out_b2, out_med

real*8 in_(asize), part(asize/2), diff(asize/2)
real*8 dummy
integer isize, i

    call quick_sort(in_x, in_)

    out_med = in_(asize/2)
    isize = asize/5
    part(1:isize) = in_(1:isize)
    do i = 2, isize
      diff(i) = dlog10(part(i) - part(i - 1))
    enddo
    part(1:isize) = dlog10(part(1:isize))
    call LinearFit(part(1:isize), diff(1:isize), isize, out_a1, out_b1)

    part(1:isize) = in_(2*isize : 3*isize-1+isize)
    do i = 2, isize
      diff(i) = dlog10(part(i) - part(i - 1))
    enddo
    part(1:isize) = dlog10(part(1:isize))
    call LinearFit(part(1:isize), diff(1:isize), isize, out_a2, out_b2)
    out_a1 = -out_a1
    out_b1 = -out_b1
    out_a2 = -out_a2
    out_b2 = -out_b2
end subroutine fit_power_tails
end module tcPower

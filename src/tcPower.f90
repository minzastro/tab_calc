module tcPower
use tcGlobals

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

end module tcPower

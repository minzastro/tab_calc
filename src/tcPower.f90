module tcPower
use tcGlobals

implicit none

real*8 x, m0, m1
contains

real*8 function get_value(a)
real*8 a
  get_value = m1**a + m0**a - 2*x**a
end function get_value

function get_h(x, m0, m1, a) result (h)
real*8, intent(in) :: x, m0, m1, a
real*8 h, h1, h2
  h1 = m1**a + m0**a - 2*x**a
  !write(22,*) m1**a, dlog(m1), dlog(m1)*m1**a, - 2*dlog(x)*x**a
  h2 = dlog(m1)*m1**a + dlog(m0)*m0**a - 2*dlog(x)*x**a
  h = -h1/h2
  if (dabs(h).gt.dabs(a)) then
    h = -a*0.5
  endif
  !write(*,*) a, h1, h2, h
end function get_h

subroutine get_next_estimate(a1, a2) ! trying to move closer to zero, by updating approximation points
real*8, intent(inout) :: a1, a2
real*8 f1, f2, f3, ax
  f1 = get_value(a1)
  f2 = get_value(a2)
  if (verbose) then
    write(*,*) a1, f1, '#', a2, f2
  endif
  if (f1*f2.gt.0d0) then !same signs on both estimates.
    if (dabs(f1).gt.dabs(f2)) then !go right
      ax = 2*a2 - a1
      if (ax*a2.le.0d0) then
        ax = a2*0.75d0
      endif
      a1 = a2
      a2 = ax
    else !go left
      ax = 2*a1 - a2
      if (ax*a1.le.0d0) then
        ax = a1*0.75
      endif
      a2 = a1
      a1 = ax
    endif
  else
    f3 = get_value(0.5d0*(a1+a2))
    if (f3*f1.gt.0d0) then
      a1 = 0.5d0*(a1+a2)
    else
      a2 = 0.5d0*(a1+a2)
    endif
  endif
end subroutine get_next_estimate

function find_alpha(in_x, in_m0, in_m1, epsil) result (alpha)
real*8, intent(in) :: in_x, in_m0, in_m1, epsil
real*8 alpha, alpha2, h
  x = in_x
  m0 = in_m0
  m1 = in_m1
  alpha = 1d-3
  alpha2 = 30d0
  do while (dabs(alpha2 - alpha).gt.epsil)
    !write(*,*) m0, x, m1, alpha, alpha2
    call get_next_estimate(alpha, alpha2)
  enddo
  if (alpha.le.2*epsil) then
    alpha = -30d0
    alpha2 = -1d-3
    do while (dabs(alpha2 - alpha).gt.epsil)
      !write(*,*) m0, x, m1, alpha, alpha2
      call get_next_estimate(alpha, alpha2)
    enddo
  endif
  !write(*,*) x, m0, m1
end function find_alpha
end module tcPower

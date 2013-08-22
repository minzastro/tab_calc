program fitPower
use quickSort
implicit none

real*8 fit_values(1000)
integer fit_value_count, i

fit_value_count = 1000
do i = 1, fit_value_count
  fit_values(i) = rand()**(-6.25d0)
enddo
call quick_sort(fit_values(:), fit_values(:))
do i = 1, fit_value_count
  write(88, *) fit_values(i)
enddo
write(66, *) get_solution(1d0)
contains

function get_estimates(alpha, k) result (derivs)
real*8, intent(in) :: k, alpha
real*8 derivs(3)

real*8 x1_p, x1_p_log, sum_k, sum_alpha, sum_e, tmp, one_alpha, xtmp, n_inv
integer i
  x1_p = fit_values(1)**(alpha+1)
  x1_p_log = x1_p * dlog(fit_values(1))
  one_alpha = 1d0/(alpha + 1d0)
  sum_alpha = 0d0
  sum_k = 0d0
  sum_e = 0d0
  n_inv = 1d0/fit_value_count
  do i = 1, fit_value_count
    xtmp = fit_values(i)**(alpha+1)
    tmp = k * one_alpha * (xtmp - x1_p) - dble(i-1)*n_inv
    sum_k = sum_k + tmp*one_alpha*(xtmp - x1_p)
    sum_e = sum_e + dabs(tmp)**2
    sum_alpha = sum_alpha + tmp* k * one_alpha * ( k * one_alpha * (xtmp - x1_p) + (dlog(fit_values(i))*xtmp - x1_p_log))
  enddo
  derivs(1) = sum_alpha
  derivs(2) = sum_k
  derivs(3) = sum_e
end function get_estimates

function get_solution(eps2) result (values)
real*8, intent(in) :: eps2
real*8 a, k, deriv(3)
real*8 values(3), eps, e0
integer i, j

  a = -1.3d0
  k = 1.0d0
  eps = eps2
  deriv(:) = get_estimates(a, k)
  i = 0
  e0 = deriv(3)
  do j = 1, 5
      do while ((deriv(3).gt.eps).and.(deriv(3).le.e0))
          write(*, *) i, a, k, deriv(:), eps
          a = a - deriv(3)*eps / deriv(1)
          k = (a + 1d0)/ ((fit_values(fit_value_count)**(a+1) - fit_values(1)**(a+1)))
          i = i + 1
          e0 = deriv(3)
          deriv(:) = get_estimates(a, k)
      enddo
      eps = eps / 4
  enddo
  values(1) = k
  values(2) = a
end function get_solution

end program

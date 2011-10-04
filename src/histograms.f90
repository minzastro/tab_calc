module histograms
!This module contains subroutines to build histograms
use tcGlobals

  integer, parameter :: MAX_ITERATIONS = 10 !Maximum number of itegations for optimizing histogram
  integer mode                       ! 0=absolute; 1={max=1}; 2={sum=1};
  !character*(6) align                ! bin value alignment ('left', 'right', 'center'), UNUSED
  real*8, save :: steps(0:MAX_STEPS)         ! positions of each step
  real*8, save :: range_min, range_max       ! minimum & maximum values of data
  real*8, save :: step_size                  ! size of 1 step = (range_max-range_min)/step_num
  integer, save :: step_num                   ! number of steps
  integer, save :: hist_data(0:MAX_STEPS)     ! number of points in each bin
  real*8 , save :: distr_sum(0:MAX_STEPS)     ! sum of data per bin

contains

subroutine histBuilder() !Build histogram of a given column 
real*8 step_size_inv !inversed step size
integer k
  step_size=(range_max-range_min)/step_num
  step_size_inv = 1D0/step_size
  do j = 0, step_num - 1
    steps(j)=range_min + j*step_size
  enddo
  hist_data(:) = 0
  do j = 1, rownum
    if ((datatable(j, xcol_add(1)).ge.range_min).and.(datatable(j, xcol_add(1)).le.range_max)) then
      k = int((datatable(j, xcol_add(1))-range_min)*step_size_inv)
      hist_data(k) = hist_data(k) + 1
    endif
  enddo
end subroutine histBuilder

real*8 function get_derivatives_estimate() !Gives an estimate of the derivatives
integer ii
real*8 tmp, td, one12th
  tmp = 0D0
  one12th = 1D0/12D0
  tmp = ((hist_data(1) - hist_data(0))/step_size)**2
  tmp = tmp+((hist_data(2) - hist_data(0))*0.5D0/step_size)**2
  do ii = 2, step_num - 2
    td = (hist_data(ii-2) - 8D0*hist_data(ii-1) + 8D0*hist_data(ii+1) - hist_data(ii+2))*one12th
    tmp = tmp + (td/step_size)**2
  enddo
  tmp = tmp + ((hist_data(step_num) - hist_data(step_num-2))*0.5D0/step_size)**2
  tmp = tmp + ((hist_data(step_num) - hist_data(step_num-1))/step_size)**2
  get_derivatives_estimate = tmp*step_size
end function get_derivatives_estimate

subroutine iterativeHistBuilder() !Builds optimized histogram
integer iter, step_num_old
real*8 gprime2
  iter = 0
  step_num_old = -1
  do while ((iter.le.MAX_ITERATIONS).and.(step_num.ne.step_num_old))
    iter = iter + 1
    call histBuilder()
    gprime2 = get_derivatives_estimate()
    if (gprime2.gt.0D0) then
      step_num_old = step_num
      step_num = int((rownum*gprime2/6D0)**0.33333)+1
      if (step_num.ge.MAX_STEPS) then
        step_num = MAX_STEPS ! - 1
      end if
    else
      iter = MAX_ITERATIONS+1
    end if
  enddo
  if (verbose) then
    write(*,*) cComment//'Converged after ', iter, ' iterations. Step count:', step_num
  end if
  call histBuilder
end subroutine iterativeHistBuilder

end module histograms

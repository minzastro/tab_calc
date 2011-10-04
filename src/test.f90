program test
use bbb
integer k
call LoadFromFileX('data_harris', colnum, rownum)
write(*,*) colnum, rownum
do k = 1, rownum
  write(*,*) datatable(k, 1:colnum)
enddo
end program test

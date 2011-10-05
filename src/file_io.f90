!!@author A.A.Mints
!!@version 0.5
!!Module with functions and subroutines for working with files
module file_io
implicit none

! type TFile
!   character*(30) sFilename
!   integer iUnit
!   logical bOpened
! end type TFile

contains
!! Find an unused unit number for input or output. Unit n=start is used
!! if available; otherwise n is incremented until an unused unit is found.
!! Unit numbers are limited to the range 1-100; if n reaches 100 the
!! search starts again at 1.
integer function find_io(start)
implicit none
integer, intent(in) :: start
logical :: in_use, exists
character(len=40) :: string
integer :: n, n0
integer, parameter :: max_unit=99

  n0 = start
  if (n0 .le. 1 .or. n0 .gt. max_unit) then
    n0 = 1
  end if
  n = n0
  in_use = .true.
  do while (in_use)
    inquire(n, opened=in_use, exist=exists)
    if (exists) then
      if (.not. in_use) exit
    else
      write (unit=string, fmt='(a,i3,a)') 'unit number', n, ' out of range'
    endif
    n = n + 1
  end do
  find_io = n
end function find_io

end module file_io

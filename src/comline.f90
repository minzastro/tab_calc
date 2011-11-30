!! Module to work with command-line parameters
!!@author Alexey A. Mints
!!@version 1.1 (12/10/2009)
module comline
use operators
use logs

integer, parameter :: COMLINE_MAX = 40
logical, parameter :: DO_DEBUG = .false.
integer, parameter :: PAR_LENGTH = 100

character*(PAR_LENGTH), save :: comline_param(COMLINE_MAX)
integer, save :: comline_count !number of parameters
integer, save :: iFirstFreeParameter ! index of the first non-prefixed parameter

interface clCheckParam
  module procedure clCheckParam1
  module procedure clCheckParam2
end interface clCheckParam

interface clGetParamValue
  module procedure clGetParamValueC
  module procedure clGetParamValueI
  module procedure clGetParamValueR
  module procedure clGetParamValueC2
  module procedure clGetParamValueI2
  module procedure clGetParamValueR2
end interface

contains

!reads all parameters from command line into an array
subroutine clReadParams
integer i
  comline_count = iargc()
  iFirstFreeParameter = 1
  if (comline_count.gt.COMLINE_MAX) then
     if (DO_DEBUG) then
      write(*,*) 'Too many parameters: ',comline_count, ' keeping only ', COMLINE_MAX
    endif
    comline_count = COMLINE_MAX
  endif
  do i = 1, comline_count
    call GetArg(i, comline_param(i))
    if (comline_param(i)(1:1).eq.'-') then
      iFirstFreeParameter = i + 2
    endif
  enddo
  if (DO_DEBUG) then
    write(*,*) comline_count, 'command-line parameters'
  endif
end subroutine clReadParams

!checks if parameter 's' is present
logical function clCheckParam1(s)
character*(*), intent(in) :: s
integer i
logical bResult
  i = 1
  bResult = .false.
  do while (i.le.comline_count)
    if (comline_param(i).eq.s) then
      bResult = .true.
      exit
    endif
    i = i + 1
  enddo
  clCheckParam1 = bResult
end function clCheckParam1

!checks if parameter 's' is present
logical function clCheckParam2(s1, s2)
character*(*), intent(in) :: s1, s2
integer i
logical bResult
  clCheckParam2 = clCheckParam1(s1).or.clCheckParam1(s2)
end function clCheckParam2

function clGetFreeParam(i) result (sResult) ! Returns i-th free parameter (counts from 1)
integer, intent(in) :: i
character*(PAR_LENGTH) sResult
  sResult = comline_param(iFirstFreeParameter + i - 1)
end function clGetFreeParam


function clGetParamValueC(s, sDefault) result(sResult) ! Returns the value of the parameter with key 's'
character*(*), intent(in) :: s
character*(*), intent(in), optional :: sDefault
character*(PAR_LENGTH) sResult
integer i
  i = 1
  if (present(sDefault)) then
    sResult = sDefault
  else
    sResult = ''
  endif
  do while (i.le.comline_count-1)
    if (trim(comline_param(i)).eq.s) then
      sResult = trim(comline_param(i+1))
      exit
    endif
    i = i + 1
  enddo
end function clGetParamValueC

function clGetParamValueC2(s, s2, sDefault) result(sResult)
character*(*), intent(in) :: s, s2
character*(*), intent(in) :: sDefault
character*(PAR_LENGTH) sResult
integer i
  i = 1
  sResult = sDefault
  do while (i.le.comline_count-1)
    if ((trim(comline_param(i)).eq.s).or.(trim(comline_param(i)).eq.s2)) then
      sResult = trim(comline_param(i+1))
      exit
    endif
    i = i + 1
  enddo
end function clGetParamValueC2

function clGetParamValueR(s, xDefault) result(xResult)
character*(*), intent(in) :: s
real*8, intent(in) :: xDefault
real*8 xResult
integer i
  i = 1
  xResult = xDefault
  do while (i.lt.comline_count-1)
    if (trim(comline_param(i)).eq.s) then
      if (isReal(comline_param(i+1))) then
        xResult = comline_param(i+1)
      else
        write(iLogFile, *) 'Error! Not a real value for parameter: ', trim(s),' : ', comline_param(i+1)
        stop
      end if
      exit
    endif
    i = i + 1
  enddo
end function clGetParamValueR

function clGetParamValueR2(s, s2, xDefault) result(xResult)
character*(*), intent(in) :: s, s2
real*8, intent(in) :: xDefault
real*8 xResult
integer i
  i = 1
  xResult = xDefault
  do while (i.lt.comline_count-1)
    if ((trim(comline_param(i)).eq.s).or.(trim(comline_param(i)).eq.s2)) then
      if (isReal(comline_param(i+1))) then
        xResult = comline_param(i+1)
      else
        write(iLogFile, *) 'Error! Not a real value for parameter: ', trim(s),' : ', comline_param(i+1)
        stop
      end if
      exit
    endif
    i = i + 1
  enddo
end function clGetParamValueR2

function clGetParamValueI(s, iDefault) result(iResult)
character*(*), intent(in) :: s
integer, intent(in) :: iDefault
integer iResult
integer i
  i = 1
  iResult = iDefault
  do while (i.lt.comline_count-1)
    if (trim(comline_param(i)).eq.s) then
      if (isInteger(comline_param(i+1))) then
        iResult = comline_param(i+1)
      else
        write(iLogFile, *) 'Error! Not an integer value for parameter: ', trim(s),' : ', comline_param(i+1)
        stop
      end if
      exit
    endif
    i = i + 1
  enddo
end function clGetParamValueI

function clGetParamValueI2(s, s2, iDefault) result(iResult)
character*(*), intent(in) :: s, s2
integer, intent(in) :: iDefault
integer iResult
integer i
  i = 1
  iResult = iDefault
  do while (i.lt.comline_count-1)
    if ((trim(comline_param(i)).eq.s).or.(trim(comline_param(i)).eq.s2)) then
      if (isInteger(comline_param(i+1))) then
        iResult = comline_param(i+1)
      else
        write(iLogFile, *) 'Error! Not an integer value for parameter: ', trim(s),' : ', comline_param(i+1)
        stop
      end if
      exit
    endif
    i = i + 1
  enddo
end function clGetParamValueI2

end module comline

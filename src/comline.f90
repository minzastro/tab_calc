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

interface clGetParamValue
  module procedure clGetParamValueC
  module procedure clGetParamValueI
  module procedure clGetParamValueR
end interface

contains

!reads all parameters from command line into an array
subroutine clReadParams
integer i
  comline_count = iargc()
  if (comline_count.gt.COMLINE_MAX) then
     if (DO_DEBUG) then
      write(*,*) 'Too many parameters: ',comline_count, ' keeping only ', COMLINE_MAX
    endif
    comline_count = COMLINE_MAX
  endif
  do i = 1, comline_count
    call GetArg(i, comline_param(i))
  enddo
  if (DO_DEBUG) then
    write(*,*) comline_count, 'command-line parameters'
  endif
end subroutine clReadParams

!checks if parameter 's' is present
logical function clCheckParam(s)
character*(PAR_LENGTH), intent(in) :: s
integer i
logical bResult
  i = 1
  bResult = .false.
  do while (i.lt.comline_count)
    if (comline_param(i).eq.s) then
      bResult = .true.
      exit
    endif
    i = i + 1
  enddo
  clCheckParam = bResult
end function clCheckParam

function clGetParamValueC(s, sDefault) result(sResult)
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
        xResult = Char2Real(comline_param(i+1))
      else
        write(iLogFile, *) 'Error! Not a real value for parameter: ', trim(s),' : ', comline_param(i+1)
        stop
      end if
      exit
    endif
    i = i + 1
  enddo
end function clGetParamValueR

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
        iResult = Char2Int(comline_param(i+1))
      else
        write(iLogFile, *) 'Error! Not an integer value for parameter: ', trim(s),' : ', comline_param(i+1)
        stop
      end if
      exit
    endif
    i = i + 1
  enddo
end function clGetParamValueI

end module comline
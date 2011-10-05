module logs

character*(100), save :: sLogFile
integer, save :: iLogFile

interface WriteCheckResultsToLog
  module procedure WriteCheckResultsToLogI
  module procedure WriteCheckResultsToLogR
end interface

contains

subroutine OpenLogFile()
logical bExists
character*(9) dir_acc, rw_sts
integer iErr
if (sLogFile.eq.'') then
  iLogFile = 6
else
  inquire(FILE=sLogFile, EXIST=bExists, DIRECT=dir_acc, READWRITE=rw_sts)
  if (bExists) then
    open(unit=99, file = sLogFile, action='WRITE', status = 'UNKNOWN', access='APPEND', iostat=iErr)
  else
    open(unit=99, file = sLogFile, action='WRITE', status = 'UNKNOWN', iostat=iErr)
  end if
  iLogFile = 99
end if
end subroutine OpenLogFile

function getDateTime() result (s) !Returns date-time stamp as a string
character*(23) s
integer value(8)
  call date_and_time(values=value)
  write(s,'(I2,"/",I2,"/",I4," ",I2,":",I2,":",I2,".",I3)') value(3),value(2),value(1), value(5:8)
end function getDateTime

subroutine WriteToLog(s)
character*(*) s
  if (iLogFile.ne.6) then
    write(*,*) s
  end if
  write(iLogFile, *) getDateTime()//' '//s
end subroutine WriteToLog

subroutine WriteCheckResultsToLogI(s1, i1, s2, i2)
character*(*), intent(in):: s1, s2
integer, intent(in) :: i1, i2
  if (iLogFile.ne.6) then
    write(*,*) s1, i1, s2, i2
  end if
  write(iLogFile, *) getDateTime()//' '//s1, i1,s2, i2
end subroutine WriteCheckResultsToLogI

subroutine WriteCheckResultsToLogR(s1, i1, s2, i2)
character*(*), intent(in):: s1, s2
real*8, intent(in) :: i1
integer, intent(in) :: i2
  if (iLogFile.ne.6) then
    write(*,*) s1, i1, s2, i2
  end if
  write(iLogFile, *) getDateTime()//' '//s1, i1 ,s2, i2
end subroutine WriteCheckResultsToLogR

end module logs
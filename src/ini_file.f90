!! Module to work with ini-files
!!@author Alexey A. Mints
!!@version 1.0
module ini_file
use operators
implicit none

!! This type is used to store one entry in .ini file
!! It might be better not to use this type directly, but via TIniFile...
type TIniItem
  character*30 name   !! entry name
  character*256 value !! entry value
  logical exists      !! existance flag
end type TIniItem

!! This type contains whole information about .ini filename
!! contents is read into TIniItem-array whith TIniFileOpen command.
!! It can then be accessed with TIniGet... functions.
!! If You're trying to access a non-existent item, You can get
!! zero values or an error, so use TIniFileCheck to check it
!!@see TIniFileCheck
type TIniFile
  character*30 name           !! filename
  type (TIniItem) items(100)  !! array of ini-items
  integer item_count          !! number of ini-items
end type TIniFile

!INTEGER FUNCTION find_io(start)
!                 Find an unused unit number for input or output.
!
!subroutine TIniFileOpen(xIniFile, filename)
!           Opens .ini-file and reads its contents into xIniFile
!
!logical function TIniFileCheck(xIniFile, sName)
!           Returns .true. if sName-entry exists in .ini-file
!
!integer function TIniFileGetInteger(xIniFile, sName)
!           Returns value of an integer-type entry from .ini-file
!
!real*8 function TIniFileGetReal(xIniFile, sName)
!           Returns value of a real-type entry from .ini-file
!
!subroutine TIniFileGetCharacter(xIniFile, sName, sChar)
!           Gets value of a character-type entry into sChar

contains

!! Find an unused unit number for input or output. Unit n=start is used
!! if available; otherwise n is incremented until an unused unit is found.
!! Unit numbers are limited to the range 1-100; if n reaches 100 the
!! search starts again at 1.
INTEGER FUNCTION find_io(start)
IMPLICIT NONE
INTEGER, INTENT(IN) :: start
LOGICAL :: in_use, exists
CHARACTER(LEN=40) :: string
INTEGER :: n, n0
INTEGER, PARAMETER :: max_unit=99

n0=start
if (n0 .le. 1 .or. n0 .gt. max_unit) n0=1
n=n0
in_use=.true.
do while (in_use)
  inquire(n,opened=in_use,exist=exists)
  if (exists) then
    if (.not. in_use) exit
  else
    write (unit=string,fmt="(a,i3,a)") "Unit number", n, " out of range"
  endif
  n=n+1
end do
find_io=n
END FUNCTION find_io

logical function TIniFileOpen(xIniFile, filename)
type (TIniFile), intent(inout) :: xIniFile
character*(*), intent(in) :: filename
integer unit_no, ini_no, istat, ind, iFileLine
character*140 sLine
character*30 name
character*100 value
logical file_exists
  unit_no = find_io(7)
  xIniFile%name=filename
  ini_no = 0
  istat=0
  iFileLine=0
  inquire(file=trim(filename), exist=file_exists)
  if (file_exists) then
    open(unit=unit_no, file=trim(filename)) !, access='READ')
      do while (istat.eq.0)
        read(unit_no, '(a)', iostat=istat) sLine
        iFileLine=iFileLine+1
        if ((istat.eq.0).and.(sLine(1:1).ne.';').and.(len(trim(sLine)).gt.0)) then
          ind = index(sLine, '=')
          if (ind.le.1) then
            print *, 'Error reading ', trim(filename), ' line ', iFileLine
          endif
          ini_no = ini_no + 1
          xIniFile%items(ini_no)%name =trim(sLine(1:ind-1))
          xIniFile%items(ini_no)%value=trim(sLine(ind+1:))
          xIniFile%items(ini_no)%exists=.true.
        endif
      enddo
    close(unit_no)
  endif
  xIniFile%item_count = ini_no
  TIniFileOpen = file_exists
end function TIniFileOpen

!! special subroutine to get xIniItem... Internal use only.
subroutine TIniFileGetItem(xIniFile, sName, xIniItem)
type (TIniFile), intent(in) :: xIniFile
character*(*), intent(in) :: sName      !! Name of ini-item to be searched for
type (TIniItem), intent(out):: xIniItem !! Found ini-item
integer i
  i = 1
  do while ((i.le.xIniFile%item_count).and.(trim(xIniFile%items(i)%name).ne.trim(sName)))
    i=i+1
  enddo
  if (i.gt.xIniFile%item_count) then
    !print *, 'Error! Ini-item ', trim(sName), ' not found!'
    xIniItem%name=''
    xIniItem%value=''
    xIniItem%exists=.false.
  else
    xIniItem = xIniFile%items(i)
  endif
  return
end subroutine TIniFileGetItem

logical function TIniFileCheck(xIniFile, sName)
type (TIniFile), intent(in) :: xIniFile
character*(*), intent(in) :: sName
type (TIniItem) xIniItem
  call TIniFileGetItem(xIniFile, sName, xIniItem)
  TIniFileCheck = xIniItem%exists
end function TIniFileCheck

integer function TIniFileGetInteger(xIniFile, sName, iDefault)
type (TIniFile), intent(in) :: xIniFile
character*(*), intent(in) :: sName
integer, intent(in), optional :: iDefault
type (TIniItem) xIniItem
  call TIniFileGetItem(xIniFile, sName, xIniItem)
  if (xIniItem%exists) then
    TIniFileGetInteger = xIniItem%value
  else
    if (Present(iDefault)) then
      TIniFileGetInteger = iDefault
    else
      TIniFileGetInteger = 0
    endif
  endif
end function TIniFileGetInteger

real*8 function TIniFileGetReal(xIniFile, sName,rDefault)
type (TIniFile), intent(in) :: xIniFile
character*(*), intent(in) :: sName
real*8, intent(in), optional :: rDefault
type (TIniItem) xIniItem
  call TIniFileGetItem(xIniFile, sName, xIniItem)
  if (xIniItem%exists) then
    TIniFileGetReal = xIniItem%value
  else
    if (Present(rDefault)) then
      TIniFileGetReal = rDefault
    else
      TIniFileGetReal = 0D0
    endif
  endif
end function TIniFileGetReal

logical function TIniFileGetLogical(xIniFile, sName, bDefault)
type (TIniFile), intent(in) :: xIniFile
character*(*), intent(in) :: sName
logical, intent(in), optional :: bDefault
type (TIniItem) xIniItem
  call TIniFileGetItem(xIniFile, sName, xIniItem)
  if (xIniItem%exists) then
    if ((xIniItem%value.eq.'yes').or.(xIniItem%value.eq.'y')) then
      TIniFileGetLogical = .true.
    else if ((xIniItem%value.eq.'no').or.(xIniItem%value.eq.'n')) then
      TIniFileGetLogical = .false.
    else
      write(*,*) 'Only "yes"/"y" for thue and "no"/"n" for false are allowed'
      stop
    end if
  else
    if (Present(bDefault)) then
      TIniFileGetLogical = bDefault
    else
      TIniFileGetLogical = .false.
    endif
  endif
end function TIniFileGetLogical



subroutine TIniFileGetCharacter(xIniFile, sName, sChar, sDefault)
type (TIniFile), intent(in) :: xIniFile
character*(*), intent(in) :: sName
character*(*), intent(in), optional :: sDefault
character*(*) sChar
type (TIniItem) xIniItem
  call TIniFileGetItem(xIniFile, sName, xIniItem)
  if (xIniItem%exists) then
    sChar = trim(xIniItem%value)
  else
    if (present(sDefault)) then
      sChar = sDefault
    else
      sChar = ''
    endif
  endif
  return
end subroutine TIniFileGetCharacter

end module
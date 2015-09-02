module StringArray
!! Set of strings handling
!!@author Alexey A. Mints
!!@version 0.2 (12.10.2009)
use operators
use logs

implicit none

integer, parameter :: ARRAY_SIZE = 60 !Maximum size of array

type TString !! fixed-legnth type
  character*(256) chars
end type TString

type TStringArray !! array of strings
  integer length !!number of strings in array
  type(TString) member(ARRAY_SIZE)
end type TStringArray

interface assignment(=)
  module procedure TStringAssign
  module procedure TStringArrAssign
  module procedure TStringAssignBack
end interface

interface operator(+)
  module procedure TStringSum
end interface

interface operator(.in.)
  module procedure TStringArrayIn
end interface

interface len
  module procedure len_array
end interface len

interface TStringArraySplit
  module procedure TStringArraySplitPlain
  module procedure TStringArraySplitDup
end interface TStringArraySplit

interface TStringArraySplitX
  module procedure TStringArraySplitX1
  module procedure TStringArraySplitX2
end interface TStringArraySplitX

contains

subroutine TStringAssign(s1, c) ! Assign character array to string
type(TString), intent(out) :: s1
character*(*), intent(in)  :: c
  s1%chars = c
end subroutine TStringAssign

subroutine TStringArrAssign(s1, c)
type(TString), dimension(:), intent(out) :: s1
character*(*), intent(in)  :: c
  s1(:)%chars = c
end subroutine TStringArrAssign

subroutine TStringAssignBack(c, s1)
type(TString), intent(in) :: s1
character*(*), intent(out)  :: c
  c = s1%chars
end subroutine TStringAssignBack

logical function TStringArrayIn(s, x)
character*(*), intent(in) :: s
type(TStringArray), intent(in) :: x
integer i
  do i = 1, x%length
    if (trim(s).eq.trim(x%member(i)%chars)) then
      TStringArrayIn = .true.
      return
    end if
  enddo
  TStringArrayIn = .false.
end function TStringArrayIn

function TStringSum(s1, s2) result(s3) !! TString concatenation
type(TString), intent(in) :: s1, s2
type(TString) s3
  if ((len(trim(s1%chars))+len(trim(s2%chars))).lt.256) then
    s3%chars = trim(s1%chars)//trim(s2%chars)
  else
    print*, 'Warning! lines are too long!'
    s3%chars=''
  endif
end function TStringSum

subroutine TStringArraySplitPlain(c, delimiter, sa1) !! converts delimited string into a string array
character*(*), intent(in) :: c
character*(1), intent(in) :: delimiter
type(TStringArray), intent(out) :: sa1
integer pos1, pos2, i
  i=1
  pos1=0 !index(c, delimiter)
  pos2 = index(c(pos1+1:), delimiter)
  do while ((pos2.ne.0).and.(i.lt.ARRAY_SIZE-1))
    sa1%member(i)%chars=c(pos1+1:pos2+pos1-1)
    pos1=pos1+pos2
    i = i + 1
    pos2 = index(c(pos1+1:), delimiter)
  enddo
  if (len(trim(c(pos1+1:))).gt.0) then
    sa1%member(i)%chars = c(pos1+1:)
    sa1%length = i
  else
    sa1%length = i - 1
  endif
end subroutine TStringArraySplitPlain

subroutine TStringArraySplitDup(c, delimiter, sa1, bRemoveDelimiter)
! converts delimited string into a string array,
! but removes repeated delimiters
character*(*), intent(in) :: c
character*(1), intent(in) :: delimiter
logical, intent(in) :: bRemoveDelimiter
type(TStringArray), intent(out) :: sa1
integer pos1, pos2, i
  i=1
  pos1 = 0 !index(c, delimiter)
  pos2 = index(c(pos1+1:), delimiter)
  do while ((pos2.ne.0).and.(i.lt.ARRAY_SIZE-1))
    sa1%member(i)%chars=c(pos1+1:pos2+pos1-1)
    pos1=pos1+pos2
    i = i + 1
    pos2 = index(c(pos1+1:), delimiter)
    if (bRemoveDelimiter) then
      do while (pos2.eq.1)
        pos1 = pos1 + 1
        pos2 = index(c(pos1+1:), delimiter)
      end do
    end if
  enddo
  if (len(trim(c(pos1+1:))).gt.0) then
    sa1%member(i)%chars = c(pos1+1:)
    sa1%length = i
  else
    sa1%length = i - 1
  endif  
end subroutine TStringArraySplitDup

function TStringArraySplitX1(c, delimiter) result (sa1)
character*(*), intent(in) :: c
character*(1), intent(in) :: delimiter
type(TStringArray) :: sa1
  call TStringArraySplit(c, delimiter, sa1)
end function TStringArraySplitX1

function TStringArraySplitX2(c, delimiter, b) result (sa1)
character*(*), intent(in) :: c
character*(1), intent(in) :: delimiter
logical, intent(in) :: b
type(TStringArray) :: sa1
  call TStringArraySplit(c, delimiter, sa1, b)
end function TStringArraySplitX2

subroutine TStringArrayGet(sa, i, c) !! Returns string array member as a string
type(TStringArray), intent(in) :: sa
integer, intent(in)            :: i
character*(*), intent(out)     :: c
  c = trim(sa%member(i)%chars)
end subroutine TStringArrayGet

subroutine TStringArrayAdd(sa, s)
type(TStringArray), intent(inout) :: sa
character*(*), intent(in) :: s
  sa%length = sa%length + 1
  sa%member(sa%length)%chars = s
end subroutine TStringArrayAdd

subroutine TStringArrayJoin(sa, sep, s)
type(TStringArray), intent(in) :: sa  !input array
character*(*), intent(in)      :: sep !symbol between elements
character*(*), intent(out)     :: s   !result
integer i
  s = ''
  do i = 1, sa%length-1
    s = trim(s)//trim(sa%member(i)%chars)//trim(sep)
  enddo
  s = trim(s)//trim(sa%member(i)%chars)
end subroutine TStringArrayJoin

integer function len_array(sa) ! Length of string array
type(TStringArray), intent(in) :: sa  !input array
  len_array = sa%length
end function len_array

subroutine toIntegerArray(sa, iL, iA, iAsize) !Convert TStringArray to integer array
type(TStringArray), intent(in) :: sa  !input array
integer, intent(in) :: iL !output array size
integer, intent(out) :: iA(iL)
integer, intent(out) :: iAsize
integer j
character*(10) sJ
  iAsize = min(iL,sa%length)
  do j=1,iAsize
    if (isInteger(trim(sa%member(j)%chars))) then
      iA(j) = trim(sa%member(j)%chars)
    else
      call Integer_to_Chars(sJ, j)
      call WriteToLog('ERROR! Wrong integer in the array: '//trim(sa%member(j)%chars)//' at position '//trim(sJ))
      close(iLogFile)
      stop
    end if
  enddo
end subroutine toIntegerArray

end module

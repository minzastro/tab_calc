!!special subroutines for handling stings
!!@author by A.A.Mints
module StringUtils

!int2char()          alternative converter from integer to char values
!        subroutine int2char(int_value, char_value)

!LastChar()          returns the last character in the string
!        character function LastChar(input)

!PrintFile()         simply prints out file contents
!        subroutine PrintFile(filename)

!replace_substring() replace all occurances of a target substring by a replacement
!        subroutine replace_substring( string, substring, repstring)

!SplitString()       returns N'th item from the string
!        subroutine SplitString(input, Nindex, delimiter, output)

!sqeeze()            removes repeating character 'symbol' from 'string'
!        subroutine sqeeze(string, symbol)

!trimleft()          removes leading spaces from a string
!        subroutine TrimLeft(input, output)

contains

integer function index2(str, substr, begin)
character*(*), intent(in) :: str
character*(*), intent(in) :: substr
integer, intent(in) :: begin
integer iX
  iX = index(str(begin:len(str)), substr)
  if (iX.ne.0) then
    index2 = iX+begin-1
  else
    index2 = 0
  end if
end function index2

subroutine TrimLeft(input, output) !! Removes leading spaces from a string
character*(*), intent(IN)::input
character*(*), intent(OUT)::output
integer i
  i = 1
  do while (input(i:i).eq.' ')
    i = i + 1
  end do
  output = input(i:len(input))
end subroutine TrimLeft

pure character function LastChar(input) !! Returns the last character in the string
character*(*), intent(in) :: input
integer i
  i = len(input)
  LastChar = input(i:i)
end function LastChar

!! Replace all occurances of a target substring by a replacement
subroutine replace_substring( string, substring, repstring)
character( len= *), intent( inout) :: string !! the string containing the
character( len= *), intent( in) :: substring !! the target substring
character( len= *), intent( in) :: repstring !! the replacement string
integer :: i                                 !! loop index
   i = index( trim(string), substring)                                     ! try to find substring
   scan_string: do while( i > 0)                   ! find all substrings
      string = string( 1: i - 1) // repstring // string( i + len( substring): )
      i = index( trim(string), substring)                                  ! find next substring, if any
   enddo scan_string                                                 ! loop thru quoted string
return                                                               ! replace_substring()
end subroutine replace_substring

!! Removes repeating character 'symbol' from 'string'
subroutine sqeeze(string, symbol)
character( len= *), intent( inout) :: string
character*(1), intent(in):: symbol
character*(2) double_symbol
integer i, l
  double_symbol=symbol//symbol
  i = 1
  l = len(trim(string))
  do while (i.le.l)
    if (string(i:i+1).eq.double_symbol) then
      string = string(1:(i-1))//string(i+1:)
      i=i-1
      l=l-1
    endif
    i=i+1
  enddo
  return
end subroutine Sqeeze

!! Simply prints out file contents
subroutine PrintFile(filename)
character*(*) filename !! name of the file to print
character*(100) fileline
integer istat, n
logical exists
  inquire(file=trim(filename),exist=exists)
  if (exists) then
    open(unit=90, file=trim(filename))
    istat=0
    do while (istat.eq.0)
      read(90, '(a)', iostat=istat) fileline
      if (istat.eq.0) then
        write(*,'(a)') trim(fileline)
      endif
    enddo
    close(90)
  else
    write(*,*) 'Warning! File ', trim(filename), ' not exists!'
  endif
end subroutine PrintFile

subroutine PrintLongString(LongString, StringLength, LineSeparator)
integer StringLength, Pos
character*(StringLength) LongString, SubStr
character*(1) LineSeparator
  Pos = Scan(LongString, LineSeparator)
  do while (Pos.gt.0)
    SubStr = LongString(1:Pos-1)
    LongString = LongString(Pos+1 : Len(LongString))
    print *, trim(SubStr)
    Pos = Scan(LongString, LineSeparator)
  end do
end subroutine PrintLongString

subroutine WriteLongString(LongString, StringLength, LineSeparator, filenum)
integer StringLength, Pos, filenum
character*(StringLength) LongString, SubStr
character*(1) LineSeparator
  Pos = Scan(LongString, LineSeparator)
  SubStr = ''
  do while (Pos.gt.0)
    SubStr = LongString(1:Pos-1)
    LongString = LongString(Pos+1 : Len(LongString))
    write (filenum, 100) trim(SubStr)
	100 FORMAT(A)
    Pos = Scan(LongString, LineSeparator)
  end do
end subroutine WriteLongString

subroutine PrintChars(LongString, StringLength)
integer StringLength, Pos
character*(StringLength) LongString, SubStr
  do Pos = 1, StringLength-1
    print *, LongString(pos:pos)
  end do
end subroutine PrintChars

!IsInteger & IsReal are moved to Operators

end module StringUtils

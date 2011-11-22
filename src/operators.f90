!! Extend assignment and + operators, & define two new ops
!!@version 1.0
!!@author Alexey Mints
module operators
!
  implicit none

  logical, save :: operatorsChecking = .true.

! Override operators:
  interface assignment(=)
    module procedure Chars_to_Integer
    module procedure Chars_to_Real
    module procedure Integer_to_Chars
  end interface

! Similarly the next interface will arrange for Add_logicals
! to be called if two logical values are added together. The
! procedure named must be a function with two intent(in)
! arguments. The result of the addition is the result of the
! function.

  interface operator(+)
    module procedure Add_logicals
  end interface

  interface operator(.in.) !true if a == any symbol in b
    module procedure StringIn
    module procedure IntegerIn
  end interface

contains

! Define procedures contained in this module

  logical function StringIn(a,b)
  character*(*), intent(in) :: a, b
  integer i
    StringIn = .false.
    do i = 1, len(trim(b))
      if (a.eq.b(i:i)) then
        StringIn = .true.
        exit
      end if
    end do
    return
  end function StringIn

  logical function IntegerIn(a,b)
  integer, intent(in) :: a
  integer, intent(in) :: b(:)
  integer i
    IntegerIn = .false.
    do i = 1, size(b)
      if (a.eq.b(i)) then
        IntegerIn = .true.
        exit
      end if
    end do
    return
  end function IntegerIn

  subroutine Chars_to_Integer(inte, int_as_chars)
    ! Subroutine to convert a character string containing
  ! digits to an integer.

    Character*(*), intent(in)  :: int_as_chars
    Integer, intent(OUT)          :: inte
    integer iSize
    character*(5) sFormat
    if (operatorsChecking) then
      if (.not.IsInteger(int_as_chars)) then
        write(*,*) 'ERROR! Not an  integer: '//trim(int_as_chars)
        inte = 0
        stop
      end if
    end if
    iSize = len(trim(int_as_chars))+48
    sFormat = '(I'//achar(iSize)//')'
    Read (int_as_chars, FMT = sFormat) inte
  end subroutine Chars_to_Integer


  subroutine Chars_to_Real(reale, int_as_chars)
    ! Subroutine to convert a character string containing
  ! digits to an integer.

    Character*(*), intent(in)  :: int_as_chars
    real*8, intent(OUT)          :: reale
    if (operatorsChecking) then
      if (.not.IsReal(int_as_chars)) then
        write(*,*) 'ERROR! Not a real: '//trim(int_as_chars)
        reale = 0
        stop
      end if
    end if
    Read (int_as_chars, *) reale
  end subroutine Chars_to_Real


subroutine Integer_to_Chars(char_value, int_value) !! Integer to character conversion without using READ statement (based on character codes!!!)
integer, intent(IN):: int_value
character*(*), intent(OUT)::char_value
character*(20) char_temp
integer i_temp, k
  i_temp = ABS(int_value)
  char_temp = ""
  if (i_temp.eq.0) then
    char_temp = "0"
  end if
  do while (i_temp.gt.0)
    k = MOD(i_temp, 10)
    char_temp = CHAR(48+k)//trim(char_temp)
    i_temp = i_temp/10
  end do
  if (int_value.lt.0) then
    char_temp = '-'//trim(char_temp)
  end if
  char_value = trim(char_temp)
end subroutine Integer_to_Chars

function getCharsFromInt(int_value) result (char_temp) !! Integer to character conversion without using READ statement (based on character codes!!!)
integer, intent(IN):: int_value
character*(20) char_temp
integer i_temp, k
  i_temp = ABS(int_value)
  char_temp = ""
  if (i_temp.eq.0) then
    char_temp = "0"
  end if
  do while (i_temp.gt.0)
    k = MOD(i_temp, 10)
    char_temp = CHAR(48+k)//trim(char_temp)
    i_temp = i_temp/10
  end do
  if (int_value.lt.0) then
    char_temp = '-'//trim(char_temp)
  end if
end function getCharsFromInt


  Function Add_logicals(a, b) result(c)
  ! Description:
  ! Function to implement addition of logical values as an
  ! OR operation.

    logical, intent(in)  :: a
    logical, intent(in)  :: b
    logical              :: c

    c = a .OR. b

  end function Add_logicals


logical function IsInteger(sLine) !Check if the character value is an integer number
character*(*), intent(in):: sLine
character*(1) sChar
integer iC
integer i, istat
logical bStart
  istat=0
  bStart = .false.
  do i = 1, len(trim(sLine))
    sChar = sLine(i:i)
    iC = iachar(sChar)
    if ((sChar.eq.'-').or.(sChar.eq.'+')) then
      cycle
    else if ((.not.bStart).and.(sChar.eq.' ')) then
      cycle
    else if ((iC.lt.48).or.(iC.gt.57)) then
      istat = 1
      bStart = .true.
      exit
    endif
  enddo
  if (istat.eq.0) then
    IsInteger = .true.
  else
    IsInteger = .false.
  endif
end function IsInteger

logical function IsReal(sLine) !Check if the character value is a real number 0.0E+0
character*(*), intent(in) :: sLine
character*(1) sChar
integer i
logical l, bWasExp, bWasDot
  l = .true.
  bWasExp = .false.
  bWasDot = .false.
  do i = 1, len(trim(sLine))
    sChar = sLine(i:i)
    if (.not.IsInteger(sChar)) then
      if ((sChar.eq.'D').or.(sChar.eq.'E').or.(sChar.eq.'d').or.(sChar.eq.'e')) then
        if (bWasExp) then
          l = .false.
          exit
        endif
        bWasExp = .true.
      else if (sChar.eq.'.') then
        if ((bWasExp).or.(bWasDot)) then
          l = .false.
          exit
        endif
        bWasDot = .true.
      else
        l = .false.
        exit
      endif
    endif
  enddo
  IsReal = l
end function IsReal

end module Operators

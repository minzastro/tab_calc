module tcOutput
use tcGlobals
use StringArray
use stringUtils

contains

character function GetColumnPreFormat(iCol) !Returns column type
!Returns column type: 
! I for intger
! R for float/real
integer, intent(in) :: iCol
  if (iCol.in.int_columns(1:int_col_num)) then
		GetColumnPreFormat = 'I'
	else
	  GetColumnPreFormat = 'R'
	endif
end function GetColumnPreFormat

subroutine PrepareColumnPreFormat(iCol) !makes format for one column only
integer, intent(in) :: iCol
	sFormat = GetColumnPreFormat(iCol)
end subroutine PrepareColumnPreFormat

function GetPreFormatSymForAll() result(c)
integer j
character*(MAX_COLUMN) :: c
  c =''
  do j = 1, xcol_num
    c(j:j) = GetColumnPreFormat(j)
  enddo
end function GetPreFormatSymForAll

subroutine PrepareRealFormat(nn)
integer, intent(in) :: nn
integer j
  sFormat = '('//trim(GetRealFormat(nn))//')'
end subroutine PrepareRealFormat

function GetRealFormat(nn) result (s) !returns format for nn real values
integer, intent(in) :: nn !number of values
character*(300) :: s !resulting format
integer j
  s = sRealFormat
  do j = 2, nn
    s = trim(s)//',1X,'//sRealFormat
  enddo
end function GetRealFormat

subroutine PrepareFormatAllExt() !prepare format for all columns with adaptive integer column width
integer j
real*8 xx
type(TStringArray) sa
  sa%length = 0
  do j = 1, colnum
    if (j.in.xcol_ignore(1:xcol_ignore_num)) then
      cycle
    endif
    if (j.in.int_columns(1:int_col_num)) then
      xx = maxval(datatable(1:rownum, j))
      call TStringArrayAdd(sa, 'I'//getCharsFromInt(int(log10(xx))+3))
    else
      call TStringArrayAdd(sa, sRealFormat)
    endif
  enddo
  call TStringArrayJoin(sa, ',1X,', sFormat)
  xFormat = sa
  sFormat = '('//trim(sFormat)//')'
end subroutine PrepareFormatAllExt

subroutine PrepareFormatAll()
integer j
type(TStringArray) sa
  sa%length = 0
  do j = 1, colnum
    if (j.in.int_columns(1:int_col_num)) then
      call TStringArrayAdd(sa, sIntegerFormat)
    else
      call TStringArrayAdd(sa, sRealFormat)
    endif
  enddo
  xFormat = sa
  call TStringArrayJoin(sa, ',1X,', sFormat)
  sFormat = '('//trim(sFormat)//')'
end subroutine PrepareFormatAll

subroutine WriteFormattedLineX(xdata, xFmt) !write a line with a given array of formats
real*8, dimension(:), intent(in) :: xdata
type(TStringArray), intent(in) :: xFmt
character*(LINE_LENGTH) sLine, sField, sFmt
integer j
  sLine = ''
  do j = 1, ubound(xdata, 1)
    sField = ''
    call TStringArrayGet(xFmt, j, sFmt)
    if (sFmt(1:1).eq.'I') then
      write(sField, '('//trim(sFmt)//',a)') int(xdata(j)), '!'
    else
      write(sField, '('//trim(sFmt)//',a)') xdata(j), '!'
    endif
    sLine = trim(sLine)//' '//trim(sField)
  enddo
  call replace_substring(sLine, '!', '')
  write(*,*) trim(sLine)
end subroutine WriteFormattedLineX

subroutine WriteFormattedLine(xdata, sFmt) !write a line with a given preformat
real*8, dimension(:), intent(in) :: xdata
character*(*), intent(in) :: sFmt
character*(LINE_LENGTH) sLine, sField
integer j
  sLine  = ''
  do j = 1, ubound(xdata,1)
    sField = ''
    if (sFmt(j:j).eq.'I') then
      write(sField, '('//sIntegerFormat//')') int(xdata(j))
    else
      write(sField, '('//sRealFormat//')') xdata(j)
    endif
    sLine = trim(sLine)//' '//trim(sField)
  enddo
  write(*,*) trim(sLine)
end subroutine WriteFormattedLine

subroutine PrepareFormatXcolFixed() !prepare format for all xcol's, with the default int format
integer j
type(TStringArray) sa
  sa%length = 0
  do j = 1, xcol_num
    if (xcol_add(j).in.int_columns(1:int_col_num)) then
      call TStringArrayAdd(sa, sIntegerFormat)
    else
      call TStringArrayAdd(sa, sRealFormat)
    endif
  enddo
  call TStringArrayJoin(sa, ',1X,', sFormat)
  xFormat = sa
  sFormat = '('//trim(sFormat)//')'
end subroutine PrepareFormatXcolFixed

subroutine PrepareFormatXcol() !prepare format for all xcol's, with the dynamic int format
integer j
type(TStringArray) sa
real*8 xx
  sa%length = 0
  do j = 1, xcol_num
    if (xcol_add(j).in.int_columns(1:int_col_num)) then
      xx = maxval(datatable(1:rownum, xcol_add(j)))
      call TStringArrayAdd(sa, 'I'//getCharsFromInt(int(log10(xx)) + 3))
    else
      call TStringArrayAdd(sa, sRealFormat)
    endif
  enddo
  call TStringArrayJoin(sa, ',1X,', sFormat)
  xFormat = sa
  sFormat = '('//trim(sFormat)//')'
end subroutine PrepareFormatXcol


end module tcOutput

module aaa
use tcGlobals
!use quickSort ! here is sorting routine SSORT
use file_io ! io-file routines
use stringUtils

real*8, parameter :: NANVALUE = HUGE(1d0)
integer, save :: field_begin(MAX_COLUMN)
integer, save :: field_end(MAX_COLUMN)
integer, save :: iIgnoranceMode = 1 ! 1 - set to huge (default); 0 - set to zero

integer, private, save :: iColTotalEstimate
contains

subroutine ParseDataLineSpaces(sSpaces, iLength, iRow, iCol)
character*(*) sSpaces
integer, intent(in) :: iRow
integer, intent(in) :: iLength
integer, intent(inout) :: iCol
integer i, iInd, istat, iBegin
  iBegin = field_end(iCol-1)
  write(*,'(a, i4, i4, 1x,a)') 'Spaces:', iCol, iLength, sSpaces
  
  if (trim(sSpaces).eq.'') then
    return
  else
    i = 1
    do while(i.le.len(trim(sSpaces)))
      do while(sSpaces(i:i).eq.' ') !find first non-empty character
        i = i + 1
      enddo
      iInd = index2(sSpaces, ' ', i) !find the end of the number
      if ((iInd.eq.0).or.(iInd.ge.iLength)) then
        write(*,*) 'Warning!', i, iInd, iLength, sSpaces(iLength:iLength)
        iInd = len(trim(sSpaces))
        field_begin(iCol) = i+iBegin
        write(*,*) '$$$', iCol, field_begin(iCol), field_end(iCol)
        write(*,*) '$$$', iCol, field_begin(iCol+1), field_end(iCol+1)
        return
      end if
      write(*,*) i, iInd, iCol, iColTotalEstimate, iLength
      !Shift arrays to the right
      field_begin(iCol+1:iColTotalEstimate+1) = field_begin(iCol:iColTotalEstimate)
      field_end(iCol+1:iColTotalEstimate+1)   = field_end(iCol:iColTotalEstimate)
      datatable(1:iRow, iCol+1:iColTotalEstimate+1) = datatable(1:iRow, iCol:iColTotalEstimate)
      if (iIgnoranceMode.eq.0) then
        datatable(1:iRow-1, iCol) = 0d0
      else
        datatable(1:iRow-1, iCol) = NANVALUE
      end if
      !Insert new column boundaries
      field_begin(iCol) = i + iBegin
      field_end  (iCol) = iInd + iBegin
      !Read a new value
      read(sSpaces(i:iInd), *, iostat=istat) datatable(iRow, iCol)
      write(*,*) '++', iCol, iColTotalEstimate, datatable(iRow, iCol), iLength
      iColTotalEstimate = iColTotalEstimate + 1
      iCol = iCol + 1
      i = iInd
    enddo
  end if
  !iColTotalEstimate = iColTotalEstimate - 1
  !iCol = iCol - 1
  write(*,*) iCol, datatable(iRow, 1:iColTotalEstimate), 'parsed'
end subroutine ParseDataLineSpaces

subroutine ParseDataLine(sLine, iRow)
character*(*) sLine
character*(50) sField
integer iRow, j, iTemp, iL, iPos
  j = 1
  do iTemp = 1, iColTotalEstimate+1
    write(*,*) field_begin(iTemp), field_end(iTemp)
  enddo
  sField = sLine(1:field_begin(1)-1)
  iL = field_begin(1)
  call ParseDataLineSpaces(sField, iL, iRow, j)
  do while (j.le.iColTotalEstimate)
    iPos = field_end(j)
    do while (sLine(iPos:iPos).ne.' ')
      iPos = iPos + 1
      field_end(j) = field_end(j) + 1
    enddo
    sField = sLine(field_begin(j):field_end(j))
    if (trim(sField).eq.'') then
      if (iIgnoranceMode.eq.0) then
        datatable(iRow, j) = 0D0
      else
        datatable(iRow, j) = NANVALUE
      end if       
    else
      read(sField,fmt=*, iostat=iTemp) datatable(iRow,j)
      if (iTemp.gt.0) then
        if (verbose) then
          write(*,*) cComment//' Warning! Bad line: ', iLine,', skipped'
        end if
        return
      else
        write(*,*) '+', j, field_begin(j), field_end(j), datatable(iRow, j)
      endif
    end if
    !if (j.ne.iColTotalEstimate) then
    !end if
    j = j + 1
    sField = sLine(field_end(j-1)+1:field_begin(j)-1)
    iL = field_begin(j)-field_end(j-1)-1
    call ParseDataLineSpaces(sField, iL, iRow, j)
  enddo  
!  sField = sLine(field_end(j)+1:len(trim(sLine)))
!  call ParseDataLineSpaces(sField, iRow, j)
end subroutine ParseDataLine

subroutine LoadFromFileX(sFilename, iColNum, iSize) !Load data from ASCII file
!                               Input parameters
character(*), intent(in) :: sFilename
!                               Output parameters
integer, intent(out) :: iColNum !Number of columns in file
integer, intent(out) :: iSize   !Number of lines in file

integer unit_id
logical file_exists, flag
integer i, istat, iTemp, iLine
character*(LINE_LENGTH) sLine   !! one line from file
character*(512) sErrorMsg
  if (trim(sFilename).eq.'') then
    if (verbose) then
      write(*,*) cComment//' No filename specified, proceeding with STDIN'
    end if
    unit_id = 5
  else
    !checking for input file existance
    inquire(file=trim(sFilename), exist=file_exists)
    if (file_exists) then
      unit_id=find_io(7)
      open(unit=unit_id, file=trim(sFilename), status="UNKNOWN")
    else
      print *, 'Error! File "', trim(sFilename), '" does not exists! '
      stop
    endif
  end if
  !loading data from file======================================
    i = 0
    istat = 0
    !detect number of columns
    flag=.false.
    iLine = 0
  datatable(:,:)=0D0
  field_begin(1) = LINE_LENGTH
  field_end(1) = LINE_LENGTH
  field_begin(2) = LINE_LENGTH
  field_end(2) = LINE_LENGTH
  iColTotalEstimate = 1
    do while ((istat .eq. 0).and.(i.le.MAX_ROW))
      read(unit_id,'(a)', iostat=istat) sLine
      iLine = iLine + 1
      if (istat.eq.0) then
        if (sLine(1:1) .ne. cComment) then !Looking for first non-commented line
          call ParseDataLine(sLine, iLine)
        end if
      endif
    enddo
    iColNum = iColTotalEstimate-1
    iSize   = iLine
end subroutine LoadFromFileX

end module aaa

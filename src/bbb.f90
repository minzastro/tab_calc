module bbb
use tcGlobals
!use quickSort ! here is sorting routine SSORT
use file_io ! io-file routines
use stringUtils

real*8, parameter :: NANVALUE = HUGE(1d0)
integer, save :: iFieldBreaks(MAX_COLUMN)

contains

real*8 function get_empty_value()
  if (iIgnoranceMode.eq.0) then
    get_empty_value = 0d0
  else
    get_empty_value = NANVALUE
  end if
end function get_empty_value

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
character*(50) sField
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
    iLine = 0
    istat = 0
    !detect number of columns
    flag=.false.
    iLine = 0
  datatable(:,:)=0D0
  iFieldBreaks(1) = 1
  iColTotalEstimate = 1
  !Parsing first non-comment line - should contain field separators
  do while (istat.eq.0)
    read(unit_id,'(a)', iostat=istat) sLine
    if (istat.eq.0) then
      if ((sLine(1:1).ne.cComment).and.(len(trim(sLine)).gt.0)) then
        do iTemp = 1, len(trim(sLine))
          if (sLine(iTemp:iTemp).eq.cDelimiter) then
            iColTotalEstimate = iColTotalEstimate + 1
            iFieldBreaks(iColTotalEstimate) = iTemp
          end if
        enddo
        iColTotalEstimate = iColTotalEstimate - 1
        ! iFieldBreaks(iColTotalEstimate+1) = len(trim(sLine))
        istat = 1
      end if
    end if
  enddo
  istat = 0
  do while ((istat .eq. 0).and.(i.le.MAX_ROW))
    read(unit_id,'(a)', iostat=istat) sLine
    if (istat.eq.0) then
      if ((sLine(1:1).eq.cComment).or.(trim(sLine).eq.'')) then
        cycle !skip comments and empty lines
      end if
      iLine = iLine + 1
      datatable(iLine, 0) = iLine
      do iTemp = 1, iColTotalEstimate
        sField = sLine(iFieldBreaks(iTemp):iFieldBreaks(iTemp+1))
        if (trim(sField).eq.'') then
          datatable(iLine, iTemp) = get_empty_value()
        else
          read(sField, *, iostat=istat) datatable(iLine, iTemp)
          if (istat.ne.0) then
            datatable(iLine, iTemp) = get_empty_value()
            istat = 0
          end if
        end if
      enddo
    end if
  enddo
  iColNum = iColTotalEstimate
  iSize   = iLine
end subroutine LoadFromFileX

end module bbb

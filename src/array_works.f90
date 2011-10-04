! module for reading and populating arrays
! version 1.2 /17.12.2010/
module array_works
use tcGlobals
use file_io ! io-file routines
use stringUtils
use StringArray

implicit none

real*8, parameter :: NANVALUE = HUGE(1d0) !not-a-number 
integer, save :: iFieldBreaks(MAX_COLUMN) !field border locations
integer, save :: iIgnoranceMode = 1

contains
! Subroutine to load data from text file into double-precision array.
! Number of lines/columns in file is detected,
! Comments are also supported as lines, starting with 'cComment' character (default to ';')
subroutine LoadFromFile(sFilename, datatable, iColNum, iSize) !Load data from ASCII file
!                               Input parameters
character(*), intent(in) :: sFilename
!                               Output parameters
real*8, intent(out) :: datatable(MAX_ROW, 0:MAX_COLUMN) !Data from file will be placed here
integer, intent(out) :: iColNum !Number of columns in file
integer, intent(out) :: iSize   !Number of lines in file

integer unit_id
logical file_exists, flag
integer i, istat, iTemp, iLine
character*(LINE_LENGTH) sLine                    !! one line from file
character*(512) sErrorMsg
  datatable(:,:)=0D0

  if (trim(sFilename).eq.'') then
    if (verbose) then
      write(*,*) cComment//' No filename specified, proceeding with STDIN'
    endif
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
  endif

	!loading data from file======================================
	i = 0
	istat = 0
	!detect number of columns
	flag = .false.
	iLine = 0
	do while (flag.eqv..false.)
	  read(unit_id,'(a)', iostat=istat) sLine
	  iLine = iLine + 1
	  if (istat.ne.0) then
		write(*,*) 'Error! No data to proceed!'
		stop
	  endif
	  if (sLine(1:1) .ne. cComment) then !Looking for first non-commented line
		call replace_substring(sLine, '	', ' ') !replacing TABS with SPACEs
		call TrimLeft(sLine, sLine) !adjust left
		call sqeeze(sLine,' ') !remove double SPACEs
		if (len(trim(sLine)).gt.0) then
		  iColnum=1 !first column
		endif
		do i = 1, len(trim(sLine)) !counting remaining spaces
		  if (sLine(i:i).eq.' ') then
			iColnum=iColnum+1
		  endif
		enddo
		flag = .true.
	  endif
	enddo
	i = 1
	if (verbose) then
	  write(*,*) cComment,iColnum, ' columns detected  from line ', iLine
	endif
	!Checking iColnum
	if (iColnum.gt.MAX_COLUMN) then
	  write(sErrorMsg, *) cComment//' Warning! Too many columns in file, reducing to max. allowed:', MAX_COLUMN
	  write(*,*) sErrorMsg
	  iColnum = MAX_COLUMN
	endif
	!Reading data
	!TODO: add checks for non-numeric data in file, replace/store elsewhere    
	do while ((istat.eq.0).and.(i.le.MAX_ROW))
	  if ((sLine(1:1).ne.cComment).and.(len(trim(sLine)).gt.0)) then
      read(sLine, fmt=*, iostat=iTemp) datatable(i, 1:iColnum)
      if (iTemp.gt.0) then
        if (verbose) then
          write(*,*) cComment//' Warning! Bad line: ', iLine, ', skipped'
        endif
      else
        datatable(i, 0) = dble(i)
        i = i + 1
      endif
	  endif
	  read(unit_id, '(a)', iostat=istat) sLine
	  iLine = iLine + 1
	enddo
  if (unit_id.ne.5) then
    close(unit_id)
  endif
  iSize = i - 1
end subroutine LoadFromFile


subroutine LoadFromFileExt(sFilename, datatable, iColNum, iSize)
!Load data from ASCII file with possible character fields
!                               Input parameters
character(*), intent(in) :: sFilename
!                               Output parameters
real*8, intent(out) :: datatable(MAX_ROW, 0:MAX_COLUMN) !Data from file will be placed here
integer, intent(out) :: iColNum !Number of columns in file
integer, intent(out) :: iSize   !Number of lines in file

integer unit_id
logical file_exists, flag
integer i, istat, iTemp, iLine
character*(LINE_LENGTH) sLine  !! one line from file
type(TStringArray) :: aFields
character*(512) sErrorMsg
  datatable(:,:)=0D0

  if (trim(sFilename).eq.'') then
    if (verbose) then
      write(*,*) cComment//' No filename specified, proceeding with STDIN'
    endif
    unit_id = 5
  else
    !checking for input file existance
    inquire(file=trim(sFilename), exist=file_exists)
    if (file_exists) then
      unit_id = find_io(7)
      open(unit=unit_id, file=trim(sFilename), status='UNKNOWN')
    else
      write(*, '(a)') 'Error! File "'//trim(sFilename)//'" does not exists! '
      stop
    endif
  endif

	!loading data from file======================================
	i = 0
	istat = 0
	!detect number of columns
	flag = .false.
	iLine = 0
	do while (flag.eqv..false.)
	  read(unit_id, '(a)', iostat=istat) sLine
	  iLine = iLine + 1
	  if (istat.ne.0) then
  		write(*,*) 'Error! No data to proceed!'
	  	stop
	  endif
	  if (sLine(1:1) .ne. cComment) then !Looking for first non-commented line
	    call RemoveDelimiters(sLine, bRemoveDuplicateDelimiters, bTabsToSpaces)
		  if (cDelimiter.eq.' ') then
		    call TrimLeft(sLine, sLine)
		  endif 
			if (len(trim(sLine)).gt.0) then
			  iColnum = 1 !first column
			endif
			do i = 1, len(trim(sLine)) !counting remaining spaces
			  if (sLine(i:i).eq.cDelimiter) then
				  iColnum = iColnum + 1
			  endif
			enddo
			flag = .true.
	  endif
	enddo
  istat = 0
  iLine = 0
  do while ((istat .eq. 0).and.(i.le.MAX_ROW))
    if (istat.eq.0) then
      if ((sLine(1:1).eq.cComment).or.(trim(sLine).eq.'')) then
        read(unit_id, '(a)', iostat=istat) sLine
        cycle !skip comments and empty lines
      endif
      iLine = iLine + 1
      datatable(iLine, 0) = iLine
      call RemoveDelimiters(sLine, bRemoveDuplicateDelimiters, bTabsToSpaces)
      !write(*,*) trim(sLine)
      aFields = TStringArraySplitX(sLine, cDelimiter)
      do iTemp = 1, iColnum
        if (.not.(iTemp.in.xcol_ignore(1:xcol_ignore_num))) then
          read(aFields%member(iTemp)%chars, *, iostat=istat) datatable(iLine, iTemp)
          if (istat.ne.0) then
            datatable(iLine, iTemp) = get_empty_value()
            istat = 0
          endif
        else 
          datatable(iLine, iTemp) = get_empty_value()
        endif
      enddo
      read(unit_id,'(a)', iostat=istat) sLine
    endif
  enddo
  iColNum = iColnum
  iSize   = iLine
end subroutine LoadFromFileExt

subroutine RemoveDelimiters(sLine, bRemoveRepeat, bExpandTabs)
character*(*), intent(inout) :: sLine
logical, intent(in) :: bRemoveRepeat
logical, intent(in) :: bExpandTabs
integer ipos, ilength
	if (bExpandTabs) then !replacing TABS with SPACEs
  	call replace_substring(sLine, char(9),' ') 
	endif
	if (bRemoveRepeat) then !removing duplicate delimiters
	  ipos = 1
	  ilength = len(trim(sLine))
	  do while (ipos.le.ilength-1)
	    if ((sLine(ipos:ipos).eq.cDelimiter).and. &
	        (sLine(ipos+1:ipos+1).eq.cDelimiter)) then
	      sLine(ipos:ilength) = sLine(ipos+1:ilength+1)
	      ilength = ilength - 1
	    else
	      ipos = ipos + 1
	    endif
	  enddo
	endif
end subroutine RemoveDelimiters

!! Subroutine to create an array, containing number sequence (like 0,1,2,3,.. etc.)
subroutine ArraySequence(x, n, x0, step)
real*8, intent(out) :: x(n)  !! output data array
integer, intent(in) :: n     !! number of elements
real*8, intent(in)  :: x0    !! minimum value
real*8, intent(in)  :: step  !! step value
!local variables:
integer i
real*8 q
  do i=0, n-1
    q = x0+step*real(i)
    x(i+1) = q
  enddo
end subroutine ArraySequence

real*8 function get_empty_value()
  if (iIgnoranceMode.eq.0) then
    get_empty_value = 0d0
  else
    get_empty_value = NANVALUE
  endif
end function get_empty_value

subroutine LoadFromFileAligned(sFilename, iColNum, iSize) !Load data from ASCII file
!                               Input parameters
character(*), intent(in) :: sFilename
!                               Output parameters
integer, intent(out) :: iColNum !Number of columns in file
integer, intent(out) :: iSize   !Number of lines in file

integer unit_id
logical file_exists, flag
integer i, istat, iTemp, iLine, iColTotalEstimate
character*(LINE_LENGTH) sLine   !! one line from file
character*(512) sErrorMsg
character*(50) sField
  if (trim(sFilename).eq.'') then
    if (verbose) then
      write(*,*) cComment//' No filename specified, proceeding with STDIN'
    endif
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
  endif
  !loading data from file======================================
  iLine = 0
  istat = 0
  !detect number of columns
  flag = .false.
  iLine = 0
  datatable(:,:) = 0D0
  iFieldBreaks(1) = 1
  iColTotalEstimate = 1
  !Parsing first non-comment line - should contain field separators
  do while (istat.eq.0)
    read(unit_id, '(a)', iostat=istat) sLine
    if (istat.eq.0) then
      if ((sLine(1:1).ne.cComment).and.(len(trim(sLine)).gt.0)) then
        do iTemp = 1, len(trim(sLine))
          if (sLine(iTemp:iTemp).eq.cDelimiter) then
            iColTotalEstimate = iColTotalEstimate + 1
            iFieldBreaks(iColTotalEstimate) = iTemp
          endif
        enddo
        iColTotalEstimate = iColTotalEstimate - 1
        ! iFieldBreaks(iColTotalEstimate+1) = len(trim(sLine))
        istat = 1
      endif
    endif
  enddo
  istat = 0
  do while ((istat .eq. 0).and.(i.le.MAX_ROW))
    read(unit_id, '(a)', iostat=istat) sLine
    if (istat.eq.0) then
      if ((sLine(1:1).eq.cComment).or.(trim(sLine).eq.'')) then
        cycle !skip comments and empty lines
      endif
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
          endif
        endif
      enddo
    endif
  enddo
  iColNum = iColTotalEstimate
  iSize   = iLine
end subroutine LoadFromFileAligned

end module array_works

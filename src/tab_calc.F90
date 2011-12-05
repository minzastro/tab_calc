!@author Alexey A.Mints
!>----------------------------------------
! |       Alexey A. Mints                |
! |  mailto: minzastro@yandex.ru         |
! | Special utility for obtaining data   |
! | statistics. Uses columned text files |
!<----------------------------------------
!TODO:
!  + 'smooth' must leave unaffected data (other columns) by option (-u?)
program tab_calc

use tcGlobals
use tcUtils
use tcOutput
use tcPower
use operators
use quickSort
use array_works
use stringUtils
use ini_file
use StringArray
use histograms
use comline

implicit none

  !GLOBAL variables
  character*(200) sInstallPath             ! directory with all the files
  character*(40) sCommand                  !command to proceed
  character*(20) sKey                      ! Command-line key
  character*(120) sKeyValue                ! input key & it's value
  character*(20) sKeyPart
  character*9 dtToday                      ! Today's date
  real*8 rTemp, x_1, x_2, y_1, y_2
  integer iTemp
  character*(500) sLine                    ! one line from file
  integer unit_id                          ! unit to read from (file or $stdin)
  real*8 sum_dat, count_dat, density
  integer i, j, istat, k, k2, pos(2)
  logical bSingleValue               ! Print only xcol_add values
  logical bAligned                   ! deal with bAligned files properly
  logical*4 file_exists              ! flag of input file existance
  logical flag
  ! same for 2d-histograms
  real*8  steps_2(0:MAX_STEPS)         ! positions of each step for 2d
  real*8  range_min_2, range_max_2     ! minimum & maximum values of data
  real*8  step_size_2                  ! step size for the 2nd dimenstion
  integer hist_data_2d(0:MAX_STEPS,0:MAX_STEPS)  !! number of points in each bin for 2d hist/distr
  real*8  distr_sum_2d(0:MAX_STEPS,0:MAX_STEPS)   !! sum data per bin

  !! variables for fitting
  character*(20) sFitFormula         !! fitting formula (not implemented yet)
  real*8 fit_a, fit_b                !! best-fit parameters

  !!root-finder

  character*(100) sIniFileName       !! name of .ini file
  type (TIniFile) xIni !! File with ini_values must be named 'tab_calc2.ini'

  type (TStringArray) xArray !! For parsing comma-separated arguments

  type (TStringArray) xSubCommands

#ifdef INSTALL_PATH
  sInstallPath=INSTALL_PATH
#else
  sInstallPath=''
#endif

  if (COMMAND_ARGUMENT_COUNT().eq.0) then
    print *, 'Error: not enough parameters!'
    call PrintInfo
  endif

  !SETTING DEFAULTS-------------------
  !General
  sIniFileName=trim(sInstallPath)//'tab_calc.ini'

  !checking for --ini option first...
  do i = 1, COMMAND_ARGUMENT_COUNT()-1 !iargc()-1
    call GetArg(i, sKey)
    if (trim(sKey) .eq. '--ini') then
      call GetArg(i,sIniFileName)
    endif
  enddo

! setting default values
  sCommand   = 'help'
  xcol_add(:)= XCOL_NULL
  xcol_num = 0
  xcol_ignore(:)= XCOL_NULL
  xcol_ignore_num = 0
  ycol_add(:)= XCOL_NULL
  ycol_num = 0
  int_columns(:) = XCOL_NULL
  int_col_num = 0

  colnum  = 0
  bSingleValue = .false.
  bAligned     = .false.
  !IO constants
  verbose    = .false.
  filename   = ''
  sRealFormat= 'F16.6'
  sIntegerFormat = 'I4'
  bFormattedOutput = .false.
  !Histogram
  mode = 0
  range_min  = 1D100
  range_max  = -1D100
  step_num   = 10
  !roots
  threshold  = 0D0

! reading .ini file (see description of ini_file.f90 for details)
  if (TIniFileOpen(xIni, trim(sIniFileName))) then
    call TIniFileGetCharacter(xIni, 'COMMAND',     sCommand,    sCommand)
    call TIniFileGetCharacter(xIni, 'DATA_FILE',   filename,    filename)
    call TIniFileGetCharacter(xIni, 'COMMENT',     cComment,    cComment)
    call TIniFileGetCharacter(xIni, 'DELIMITER',   cDelimiter,  cDelimiter)
    call TIniFileGetCharacter(xIni, 'REAL_FORMAT', sRealFormat, sRealFormat)
    call TIniFileGetCharacter(xIni, 'INTEGER_FORMAT', sIntegerFormat, sIntegerFormat)
    xcol_add(1) = TIniFileGetInteger(xIni, 'XCOL', xcol_add(1))
    if (xcol_add(1).ne.XCOL_NULL) then
      xcol_num = 1
    endif
    ycol_add(1) = TIniFileGetInteger(xIni, 'YCOL', ycol_add(1))
    if (ycol_add(1).ne.XCOL_NULL) then
      ycol_num = 1
    endif
    step_num    = TIniFileGetInteger(xIni, 'STEP_NUMBER', step_num)
    range_min = TIniFileGetReal(xIni, 'MIN_VALUE', range_min)
    range_max = TIniFileGetReal(xIni, 'MAX_VALUE', range_max)
    threshold = TIniFileGetReal(xIni, 'THRESHOLD', threshold)
  endif

  !parsing parameters=========================================
  i = 1
  call clReadParams()
  bAligned = clCheckParam('-a', '--aligned')
  cDelimiter = clGetParamValue('-d', '--delimiter', cDelimiter)
  filename = clGetParamValue('-f')
  if (clCheckParam('-g', '--groupby')) then
    bGroupByMode = .true.
    call paramToArrayData(clGetParamValue('-g', '--groupby', ''), ARRAY_SIZE, aGroupByColumns, iGroupByColumns)
  endif
  sCommand = clGetParamValue('-c', 'none')
  cComment = clGetParamValue('-comment', cComment)
  bSingleValue = clCheckParam('-s')
  iSkipAmount = clGetParamValue('--skip')
  if (clCheckParam('-S')) then
    call TStringArraySplit(clGetParamValue('-S'), ',', xSubCommands)
  endif
  call paramToArrayData(clGetParamValue('-x'), MAX_COLUMN, xcol_add, xcol_num)
  call paramToArrayData(clGetParamValue('-xi'), MAX_COLUMN, xcol_ignore, xcol_ignore_num)
  call paramToArrayData(clGetParamValue('-y'), MAX_COLUMN, ycol_add, ycol_num)
  call paramToArrayData(clGetParamValue('-i', '--int', ''), MAX_COLUMN, int_columns, int_col_num)
  range_min = clGetParamValue('-m', '--min', range_min)
  range_max = clGetParamValue('-M', '--max', range_max)
  step_num  = clGetParamValue('-t', step_num)
  threshold = clGetParamValue('-thr', threshold)
  mode      = clGetParamValue('-o', '--mode', mode)
  if (clCheckParam('-v', '--version')) then
    call PrintVersion()
  endif
  verbose = clCheckParam('-V', '--verbose')
  if (clCheckParam('-h', '/?').or.clCheckParam('-?', '--help')) then
    call PrintInfo()
  endif
#include "params.i"

  if (verbose) then
    print *, cComment//' Verbose mode on'
  endif

  !resetting data arrays
  temp_values(:)=0D0
  temp_values2(:,:)=0D0
  datatable(:,:)=0D0

  if (cDelimiter.eq.' ') then
    bRemoveDuplicateDelimiters = .true.
    bTabsToSpaces = .true.
  else
    bRemoveDuplicateDelimiters = .false.
    bTabsToSpaces = .false.
  endif

  if (bAligned) then
    if (verbose) then
      print *, cComment//'Reading aligned data'
    endif
    call LoadFromFileAligned(filename, colnum, rownum)
  else if ((xcol_ignore_num.gt.0).or.(cDelimiter.ne.' ')) then
    call LoadFromFileExt(filename, datatable, colnum, rownum )
  else
    call LoadFromFile(filename, datatable, colnum, rownum )
  endif

  if (verbose) then
    call date_and_time(dtToday)
    write(*,*) cComment//' Created:', dtToday(7:8)//'/'//dtToday(5:6)//'/'//dtToday(1:4)
    write(*,*) cComment//' ', rownum, ' data lines. ', colnum, ' data fields'
  endif

  !++++++++++++++++++++++++++ Parameter checking +++++++++++++++++++++
  !check for proper colnum number
  if((maxval(xcol_add).gt.colnum))then
    write(*,*) cComment//' WARNING! Number of column to proceed is greater, than total number of columns in the file'
  endif

  if (bGroupByMode.and.(xcol_num.gt.1)) then
    write(*,*) cComment//' WARNING! Only one column can be processed in groupby mode'
  endif

  !If no xcol specified - use all
  if (xcol_num.eq.0) then
    if (verbose) then
      write(*,*) cComment//' WARNING! No columns specified, all columns accepted to be active'
    endif
    do j = 1, colnum
      xcol_add(j) = j
    enddo
    xcol_num = colnum
  endif

  !Remove columns marked as ignored
  if (xcol_ignore_num.gt.0) then
    j = 1
    do while (j.le.xcol_num)
      if (xcol_add(j).in.xcol_ignore(1:xcol_ignore_num)) then
        xcol_add(j:xcol_num-1) = xcol_add(j+1:xcol_num)
        xcol_num = xcol_num - 1
      else
        j = j + 1
      endif
    enddo
  endif

  ! Removing empty rows:
  call RemoveNanRows()
  if (bAligned) then
    if (rownum.eq.0) then
      write(*,*) cComment//'Error! No data left after empty records removal'
      stop
    endif
  endif

  ! Applying data cuts (if set)
  if (range_min.eq.1D100) then
    range_min = minval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
  end if
  if (range_max.eq.-1D100) then
    range_max = maxval(datatable(1:rownum, xcol_add(1):xcol_add(1)))
  end if
  call ApplyDataCuts(range_min, range_max, xcol_add(1))

  !++++++++++++++++++++++++++ MAIN PART ++++++++++++++++++++++++++++++
  my_case: select case (trim(sCommand))
    case('none')
      call PrepareFormatXcol()
      do j = 1,rownum
        call WriteFormattedLineX(datatable(j, xcol_add(1:xcol_num)), xFormat)
      enddo

    case('avg') !Average value by columns
      if (.not.bGroupByMode) then
        if (xcol_num.gt.0) then
          datatable(1:rownum, 1:xcol_num) = datatable(1:rownum, xcol_add(1:xcol_num))
          call PrepareRealFormat(xcol_num)
          write(*,sFormat) &
                sum(transpose(datatable(1:rownum, 1:xcol_num)), dim=2)/rownum
        endif
      else
        call FillGroupBySums()
        sFormat = '('//trim(GetRealFormat(iGroupByColumns))//',1X,'//sIntegerFormat//',1X,'//sRealFormat//')'
        do j = 1, iGroupByCount
          write(*,sFormat) aGroupByValues(j, 1:iGroupByColumns), int(aGroupByValues(j, -1)), &
                           aGroupByValues(j, -2)/aGroupByValues(j, -1)
        enddo
      endif

    case('sum') !Sum of columns
      if (.not.bGroupByMode) then
        if (xcol_num.gt.0) then
          datatable(1:rownum, 1:xcol_num) = datatable(1:rownum, xcol_add(1:xcol_num))
          call PrepareFormatXcolFixed()
          call WriteFormattedLineX(sum(transpose(datatable(1:rownum, 1:xcol_num)), dim=2), xFormat)
        endif
      else
        call FillGroupBySums()
        sFormat = '('//trim(GetRealFormat(iGroupByColumns))//',1X,'//sIntegerFormat//',1X,'//sRealFormat//')'
        do j = 1, iGroupByCount
          write(*, sFormat) aGroupByValues(j, 1:iGroupByColumns), &
                            int(aGroupByValues(j, -1)), &
                            aGroupByValues(j, -2)
        enddo
      endif

    case ('med') !Median value
      do j = 1, xcol_num
        call quick_sort(datatable(1:rownum, xcol_add(j)), long_values(1:rownum))
        temp_values(j) = long_values((rownum+1)/2)
      enddo
      call PrepareFormatXcol()
      call WriteFormattedLineX(temp_values(1:xcol_num), xFormat)

    case ('med_q') !Median value with quartiles
      do j = 1, xcol_num
        call quick_sort(datatable(1:rownum, xcol_add(j)), long_values(1:rownum))
        temp_values2(1, j) = long_values((rownum + 1)*3/4) !25%
        temp_values2(2, j) = long_values((rownum + 1)/2)   !50%
        temp_values2(3, j) = long_values((rownum + 1)/4)   !75%
      enddo
      if (bSingleValue) then
        sFormat = '('//trim(GetRealFormat(3))//')'
        write(*,sFormat) temp_values2(1:3, xcol_add(1):xcol_add(1))
      else
        sFormat = '('//trim(GetRealFormat(3*colnum))//')'
        write(*,sFormat) temp_values2(1:3, 1:colnum)
      endif

    case ('sort_by') !sorting
      call quick_sort_index(datatable(1:rownum, xcol_add(1)), &
                            long_values(1:rownum), &
                            index_values(1:rownum))
      sFormat = GetPreFormatSymForAll()
      do j = 1, rownum
        call WriteFormattedLine(datatable(index_values(j), 1:colnum), sFormat)
      enddo

    case ('sort') !sorting
      call quick_sort(datatable(1:rownum, xcol_add(1)), long_values(1:rownum))
      sFormat = GetColumnPreFormat(xcol_add(1))
      do j = 1, rownum
        call WriteFormattedLine(long_values(j:j), sFormat)
      enddo

    case ('smooth') !smooth with window size = step_num
      do k = 1, xcol_num
        do j = 1, step_num
          long_values(j) = sum(datatable(1:2*j-1, xcol_add(k):xcol_add(k)))/(2*j-1)
        enddo
        do j = 1+step_num, rownum-step_num
          long_values(j) = sum(datatable(j-step_num:j+step_num, xcol_add(k):xcol_add(k))) &
                           /(2*step_num + 1)
        enddo
        do j = 1, step_num
          k2 = step_num-j
          long_values(rownum-k2) = sum(datatable(rownum-2*k2:rownum, xcol_add(k):xcol_add(k))) &
                                  /(2*k2 + 1)
        enddo
        datatable(1:rownum, xcol_add(k)) = long_values(1:rownum)
      enddo
      if (bSingleValue) then
        sFormat = GetColumnPreFormat(xcol_add(1))
        do j = 1, rownum
          call WriteFormattedLine(long_values(j:j), sFormat)
        enddo
      else
        call PrepareFormatAllExt()
        do j = 1, rownum
          call WriteFormattedLineX(datatable(j, 1:colnum), xFormat)
        enddo
      endif

    case ('sqrt') !getting vector length
      do j=1,rownum
        k = 1
        temp_values(1) = 0D0
        do while (xcol_add(k).ne.XCOL_NULL)
          temp_values(1) = temp_values(1) + datatable(j, xcol_add(k))**2
          k = k + 1
        enddo
        write(*,*) dsqrt(temp_values(1))
      enddo

    case ('help')
      call PrintInfo

#include "commands.i"

    case default
      write(*,*) 'Illegal command: ', trim(sCommand)
      call PrintInfo
  end select my_case


contains

subroutine RequireXCols(i)
integer, intent(in) :: i
  if (xcol_num.ne.i) then
    write(*,*) cComment//'Error! ', i,' columns required!'
    stop
  end if
end subroutine RequireXCols

subroutine paramToArrayData(sParam, iMaxSize, iArr, iArrSize)
character*(*), intent(in) :: sParam
integer, intent(in) :: iMaxSize
integer, intent(inout) :: iArr(1:iMaxSize)
integer, intent(out) :: iArrSize
type(TStringArray) :: xArray
  call TStringArraySplit(sParam, ',', xArray)
  call toIntegerArray(xArray, iMaxSize, iArr, iArrSize)
end subroutine paramToArrayData

subroutine PrintInfo()
#ifdef INCLUDE_TEXTFILES
#include "USAGE.wrap"
#else
  use stringUtils
  call PrintFile(trim(INSTALL_PATH)//'USAGE')
#endif
  stop
end subroutine PrintInfo

subroutine PrintVersion()
#ifdef INCLUDE_TEXTFILES
#include "VERSION.wrap"
#else
  use stringUtils
  call PrintFile(trim(INSTALL_PATH)//'VERSION')
#endif
  stop
end subroutine PrintVersion
end program tab_calc

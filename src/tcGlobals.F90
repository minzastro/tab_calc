module tcGlobals
use StringArray
implicit none

real*8, parameter :: PI = 3.1415926535897932d0
real*8, parameter :: HUGE_REAL = huge(1d0)

integer, parameter :: MAX_COLUMN = ARRAY_SIZE     ! maximum number of columns in file
#ifdef HUGE
integer, parameter :: MAX_ROW    = 400000 ! maximum number of rows in file
#else
integer, parameter :: MAX_ROW    = 20000 ! maximum number of rows in file
#endif
integer, parameter :: MAX_STEPS  = 200    ! maximum number of steps in histogram/smoothing range
integer, parameter :: XCOL_NULL = -100    ! Value for xcol_add(), treated as "not set"

integer, parameter :: LINE_LENGTH = 2000   ! Maximum string length

character*(120) filename                 ! data filename
real*8, save :: datatable(MAX_ROW, 0:MAX_COLUMN)              !! main array with data
integer, save :: rownum !Number of rows in file
integer, save :: colnum !Number of columns in file
integer, save :: mode   ! 0=absolute; 1={max=1}; 2={sum=1};

integer, save :: xcol_add(MAX_COLUMN), xcol_num !Columns to proceed and their count
integer, save :: xcol_ignore(MAX_COLUMN), xcol_ignore_num !Columns to ignore and their count (textual columns go here)
integer, save :: ycol_add(MAX_COLUMN), ycol_num !Y-columns to proceed (deprecated)
integer, save :: int_columns(MAX_COLUMN), int_col_num !Integer columns (for formatted output)
integer, save :: angular_columns(MAX_COLUMN), ang_col_num !Angle-like or Time-like columns (so far for input only, converted to seconds)

logical       :: verbose                 ! Flag for extra output
character*(1) :: cComment = ';'          ! Default comment character
character*(1) :: cDelimiter = ' '        ! Default field delimiter
logical, save :: bTabsToSpaces           ! Wether to expand tabs to spaces (1 tab = 1 space)
logical, save :: bRemoveDuplicateDelimiters ! Wether to remove duplicate delimiters
logical, save :: bFormattedOutput  ! Flag for formatted output (UNUSED)

real*8, save  :: threshold                   !!root threshold (0.0 by default)

!For histograms and functions
real*8, save :: steps(0:MAX_STEPS)         ! positions of each step
real*8, save :: range_min, range_max       ! minimum & maximum values of data
real*8, save :: step_size                  ! size of 1 step = (range_max-range_min)/step_num
integer, save :: step_num                   ! number of steps

!TEMPORARY variables
real*8, save  :: temp_value !temporary value
real*8, save  :: temp_values(MAX_COLUMN), temp_values2(10,MAX_COLUMN), long_values(MAX_ROW) !! temporary arrays
integer, save :: index_values(MAX_ROW) !index for sorting

!Variables for GROUP BY mode (-g option)
logical , save :: bGroupByMode =  .false. !use grouping
integer , save :: iGroupByColumns ! Number of distinct coluumns
integer , save :: aGroupByColumns(ARRAY_SIZE) !Limited to maximum size of StringArray
real*8  , save :: aGroupByValues(MAX_ROW, -ARRAY_SIZE-1:ARRAY_SIZE) !1st index is group, 2nd = value
integer , save :: iGroupByIndex(1:MAX_ROW)
integer , save :: iGroupByCount ! Number of distinct values

!Variables for formatted output
character*(300), save :: sFormat            !!FORMAT string
character*(6), save :: sRealFormat          !!FORMAT for real values
character*(4), save :: sIntegerFormat       !!FORMAT for integer values
type(TStringArray), save :: xFormat


!Additional variables definitions go here
#include "vars.i"

end module tcGlobals

module fits_reader
#include "longnam.h"
use tcGlobals

contains
! Subroutine to load data from text file into double-precision array.
! Number of lines/columns in file is detected,
! Comments are also supported as lines, starting with 'cComment' character (default to ';')
subroutine LoadFromFileFITS(sFilename, iColNum, iSize) !Load data from ASCII file
!                               Input parameters
character(*), intent(in) :: sFilename
!                               Output parameters
integer, intent(out) :: iColNum !Number of columns in file
integer, intent(out) :: iSize   !Number of lines in file

character*(40) sColName
integer iStatus, iUnit, irow, iType, iRepeat, iWidth
integer blocksize, hdutype, ntable
logical anynull
real*4 xReal
real*8 xReal2

    !  The STATUS parameter must always be initialized.
    iStatus=0

    !  Get an unused Logical Unit Number to use to open the FITS file.
    call ftgiou(iUnit, iStatus)

    !Open the FITS file previously created by WRITEIMAGE
    call ftopen(iUnit, sFilename, 0, blocksize, iStatus)

    call fits_movabs_hdu(iUnit, 2, hdutype, iStatus)
    if ((hdutype .ne. 1) .and. (hdutype .ne. 2)) then
      write(*, *) 'HDU type not supported!', hdutype
    end if

    call fits_get_num_cols(iUnit, iColNum, iStatus)
    call fits_get_num_rows(iUnit, iSize, iStatus)
    do i = 1, iSize
      datatable(i, 0) = i
    enddo
    do i = 1, xcol_num
      call fits_get_coltype(iUnit, xcol_add(i), iType, iRepeat, iWidth, iStatus)
      if (iType.eq.42) then
        do irow = 1, iSize
          call fits_read_col_flt(iUnit, xcol_add(i), irow, 1, 1, ' ', xReal, anynull, iStatus)
          datatable(irow, xcol_add(i)) = xReal
        enddo
      else if (iType.eq.82) then
        do irow = 1, iSize
          call fits_read_col_dbl(iUnit, xcol_add(i), irow, 1, 1, ' ', xReal2, anynull, iStatus)
          datatable(irow, xcol_add(i)) = xReal2
        enddo
      else
        write(*, *) 'Column type ', iType, ' in column ', xcol_add(i), ' is not supported and will be ignored'
      endif
    enddo
end subroutine LoadFromFileFITS

end module fits_reader

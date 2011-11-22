  case('polar')
    if ((xcol_num.ne.2).and.(xcol_num.ne.4)) then
      write(*,*) 'Please specify 2 or 4 columns with -x switch'
    end if
    if (bSingleValue) then
      call PrepareRealFormat(xcol_num)
    else
      call PrepareFormatAllExt()
    endif
    temp_values(9) = 0d0
    temp_values(10) = HUGE_REAL
    do i = 1, rownum
      temp_values(1:xcol_num) = datatable(i, xcol_add(1:xcol_num))
      temp_values(5) = dsqrt(temp_values(1)**2 + temp_values(2)**2)
      datatable(i, xcol_add(1)) = temp_values(5)
      temp_values(15) = temp_values(1)/temp_values(5) ! cosinus
      temp_values(16) = temp_values(2)/temp_values(5) ! sinus
      datatable(i, xcol_add(2)) = datan2(temp_values(15), temp_values(16))
      if (mode.eq.1) then ! Special mode for gradual angle increase
        if (temp_values(10).eq.HUGE_REAL) then !first set the temporary variable
          temp_values(10) = datatable(i, xcol_add(2))
        else
          if ((datatable(i, xcol_add(2)) - temp_values(10)).gt.PI) then ! Count as loop forward
            temp_values(9) = temp_values(9) - 2d0*PI
          else if (( temp_values(10)) - datatable(i, xcol_add(2)).gt.PI) then ! loop backward
            temp_values(9) = temp_values(9) + 2d0*PI
          end if
          temp_values(10) = datatable(i, xcol_add(2)) ! Save the old, uncorrected value
          datatable(i, xcol_add(2)) = datatable(i, xcol_add(2)) + temp_values(9)
        endif
      endif
      if (xcol_num.eq.4) then
        temp_values(7) = datatable(i, xcol_add(3))*temp_values(15) -   &
                         datatable(i, xcol_add(4))*temp_values(16)
        temp_values(8) =  datatable(i, xcol_add(3))*temp_values(16) + &
                          datatable(i, xcol_add(4))*temp_values(15)
        datatable(i, xcol_add(3)) = temp_values(7)
        datatable(i, xcol_add(4)) = temp_values(8)
      endif
    enddo
    if (mode.eq.2) then
      if ((maxval(datatable(1:rownum, xcol_add(2))) - minval(datatable(1:rownum, xcol_add(2)))).gt.5d0) then
        do i = 1, rownum
          if (datatable(i, xcol_add(2)).lt.0d0) then
            datatable(i, xcol_add(2)) = datatable(i, xcol_add(2)) + 2d0*PI
          endif
        enddo
      endif
    endif
    do i = 1, rownum
      if (bSingleValue) then
        write(*, sFormat) datatable(i, xcol_add(1:xcol_num))
      else
        call WriteFormattedLineX(datatable(i, 1:colnum), xFormat)
      endif
    enddo


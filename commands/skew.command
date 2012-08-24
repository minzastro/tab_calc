case ('skew')
  if (.not.bGroupByMode) then
    temp_values(1:xcol_num)=sum(datatable(1:rownum,xcol_add(1:xcol_num)),dim=1)/rownum
    temp_values2(1, 1:xcol_num) = moment(datatable(1:rownum, xcol_add(1:xcol_num)), &
                                         rownum, xcol_num, 2, temp_values(1:xcol_num))
    temp_values2(2, 1:xcol_num) = moment(datatable(1:rownum, xcol_add(1:xcol_num)), &
                                         rownum, xcol_num, 3, temp_values(1:xcol_num))
    call PrepareRealFormat(xcol_num)
    write(*,sFormat) temp_values2(2, 1:xcol_num) / (temp_values2(1, 1:xcol_num))**1.5d0
  else
    call FillGroupBySums()
    long_values(:) = aGroupByValues(:, -2)/aGroupByValues(:, -1)
    aGroupByValues(:, -2) = 0D0
    aGroupByValues(:, -3) = 0D0
    do j = 1, rownum
      k = iGroupByIndex(j)
      aGroupByValues(k, -2) = aGroupByValues(k, -2) + (datatable(j, xcol_add(1))-long_values(k))**2
      aGroupByValues(k, -3) = aGroupByValues(k, -3) + (datatable(j, xcol_add(1))-long_values(k))**3
    enddo
    aGroupByValues(:, -2) = dsqrt(dble(rownum)) * aGroupByValues(:, -3)/ (aGroupByValues(:, -2))**1.5d0
    sFormat = '('//trim(GetRealFormat(iGroupByColumns))//',1X,'//sIntegerFormat//',1X,'//sRealFormat//')'
    do j = 1, iGroupByCount
      write(*,sFormat) aGroupByValues(j, 1:iGroupByColumns), int(aGroupByValues(j,-1)), aGroupByValues(j,-2)
    enddo
  end if

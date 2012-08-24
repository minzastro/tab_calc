!dispersion
case ('dis')
  if (.not.bGroupByMode) then
    temp_values(1:xcol_num) = dsqrt(moment(datatable(1:rownum, xcol_add(1:xcol_num)), rownum, xcol_num, 2))
    call PrepareRealFormat(xcol_num)
    write(*,sFormat) temp_values(1:xcol_num)
  else
    call FillGroupBySums()
    long_values(:) = aGroupByValues(:, -2)/aGroupByValues(:, -1)
    aGroupByValues(:, -2) = 0D0
    do j = 1, rownum
      k = iGroupByIndex(j)
      aGroupByValues(k, -2) = aGroupByValues(k, -2) + (datatable(j, xcol_add(1))-long_values(k))**2
    enddo
    aGroupByValues(:, -2) = dsqrt(aGroupByValues(:, -2)/aGroupByValues(:, -1))
    sFormat = '('//trim(GetRealFormat(iGroupByColumns))//',1X,'//sIntegerFormat//',1X,'//sRealFormat//')'
    do j = 1, iGroupByCount
      write(*,sFormat) aGroupByValues(j, 1:iGroupByColumns), int(aGroupByValues(j,-1)), aGroupByValues(j,-2)
    enddo
  end if

!relative dispersion
case ('rel_dis')
  if (.not.bGroupByMode) then
    temp_values2(1,1:xcol_num)=sum(datatable(1:rownum,xcol_add(1:xcol_num)),dim=1)/rownum
    temp_values(1:xcol_num) = dsqrt(moment(datatable(1:rownum, xcol_add(1:xcol_num)), &
                                           rownum, xcol_num, 2, temp_values2(1, 1:xcol_num)))
    call PrepareRealFormat(xcol_num)
    write(*,sFormat) temp_values(1:xcol_num)/temp_values2(1,1:xcol_num)
  else
    call FillGroupBySums()
    long_values(:) = aGroupByValues(:, -2)/aGroupByValues(:, -1)
    aGroupByValues(:, -2) = 0D0
    do j = 1, rownum
      k = iGroupByIndex(j)
      aGroupByValues(k, -2) = aGroupByValues(k, -2) + (datatable(j, xcol_add(1))-long_values(k))**2
    enddo
    aGroupByValues(:, -1) = dsqrt(aGroupByValues(:, -2)/(aGroupByValues(:, -1)*long_values(:)))
    sFormat = '('//trim(GetRealFormat(iGroupByColumns))//',1X,'//sIntegerFormat//',1X,'//sRealFormat//')'
    do j = 1, iGroupByCount
      write(*,sFormat) aGroupByValues(j, 1:iGroupByColumns), int(aGroupByValues(j,-1)), aGroupByValues(j,-2)
    enddo
  end if

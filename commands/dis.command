!dispersion
case ('dis')
  if (.not.bGroupByMode) then
    temp_values2(1,1:xcol_num)=sum(transpose(datatable(1:rownum,xcol_add(1:xcol_num))),dim=2)/rownum
    do j = 1, rownum
      temp_values(1:xcol_num) = temp_values(1:xcol_num)+(datatable(j, xcol_add(1:xcol_num))-temp_values2(1,1:xcol_num))**2
    enddo !rownum
    temp_values(1:xcol_num) = dsqrt(temp_values(1:xcol_num)/rownum)
    call PrepareRealFormat(xcol_num)
    write(*,sFormat) temp_values(1:xcol_num)
  else
    call FillGroupBySums()
    long_values(:) = aGroupByValues(:, -2)/aGroupByValues(:, -1)
    aGroupByValues(:, -2) = 0D0
    do j = 1, rownum
      do k = 1, iGroupByCount
        if (count(aGroupByValues(k, 1:iGroupByColumns) .ne. datatable(j, aGroupByColumns(1:iGroupByColumns))).eq.0) then
          aGroupByValues(k, -2) = aGroupByValues(k, -2) + (datatable(j, xcol_add(1))-long_values(k))**2
          exit
        end if
      enddo
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
    temp_values2(1,1:xcol_num)=sum(transpose(datatable(1:rownum,xcol_add(1:xcol_num))),dim=2)/rownum
    do j = 1, rownum
      temp_values(1:xcol_num) = temp_values(1:xcol_num)+(datatable(j, xcol_add(1:xcol_num))-temp_values2(1,1:xcol_num))**2
    enddo !rownum
    temp_values(1:xcol_num) = dsqrt(temp_values(1:xcol_num)/rownum)
    write(*,*) temp_values(1:xcol_num)/temp_values2(1,1:xcol_num)
  else
    call FillGroupBySums()
    long_values(:) = aGroupByValues(:, -2)/aGroupByValues(:, -1)
    aGroupByValues(:, -2) = 0D0
    do j = 1, rownum
      do k = 1, iGroupByCount
        if (count(aGroupByValues(k, 1:iGroupByColumns) .ne. datatable(j, aGroupByColumns(1:iGroupByColumns))).eq.0) then
          aGroupByValues(k, -2) = aGroupByValues(k, -2) + (datatable(j, xcol_add(1))-long_values(k))**2
          exit
        end if
      enddo
    enddo
    aGroupByValues(:, -1) = dsqrt(aGroupByValues(:, -2)/(aGroupByValues(:, -1)*long_values(:)))
    do j = 1, iGroupByCount
      write(*,*) aGroupByValues(j, 1:iGroupByColumns), int(aGroupByValues(j,-1)), aGroupByValues(j,-2)
    enddo
  end if

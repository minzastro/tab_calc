!+ hmean # harmonic mean of the data
    case('hmean') !harmonic mean
      if (.not.bGroupByMode) then
        temp_values(:) = 0d0
        do i = 1, rownum
          temp_values(1:xcol_num) = temp_values(1:xcol_num) + 1d0/datatable(i, xcol_add(1:xcol_num))
        enddo
        temp_values(1:xcol_num) = rownum/temp_values(1:xcol_num)
        call PrepareRealFormat(xcol_num)
        write(*,sFormat) temp_values(1:xcol_num)
      else
        aGroupByValues(:,:) = 0D0
        iGroupByCount = 0
        do j = 1, rownum
          flag = .false.
          do k = 1, iGroupByCount
            !See if already in some group-by set
            if (count(aGroupByValues(k, 1:iGroupByColumns) .ne. datatable(j, aGroupByColumns(1:iGroupByColumns))).eq.0) then
              aGroupByValues(k, -xcol_add(1:xcol_num)) = aGroupByValues(k, -xcol_add(1:xcol_num)) &
                                                       + 1d0/datatable(j, xcol_add(1:xcol_num))
              aGroupByValues(k, 0) = aGroupByValues(k, 0) + 1
              flag = .true.
            endif
          enddo
          if (.not.flag) then
            !new group-by set
            iGroupByCount = iGroupByCount + 1
            aGroupByValues(iGroupByCount, 1:iGroupByColumns) = datatable(j, aGroupByColumns(1:iGroupByColumns))
            aGroupByValues(k, -xcol_add(1:xcol_num)) = 1d0/datatable(j, xcol_add(1:xcol_num))
            aGroupByValues(k, 0) = 1
          endif
        enddo
        call PrepareRealFormat(xcol_num + iGroupByColumns)
        do j = 1, iGroupByCount
          aGroupByValues(j, -xcol_add(1:xcol_num)) = aGroupByValues(k, 0)/aGroupByValues(j, -xcol_add(1:xcol_num))
          write(*, sFormat) aGroupByValues(j, 1:iGroupByColumns), aGroupByValues(j, -xcol_add(1:xcol_num))
        enddo
      endif

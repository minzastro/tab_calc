    case('count') !Minimum value
      if (.not.bGroupByMode) then
        write(*,*) rownum
      else
        aGroupByValues(:,:) = 0D0
        iGroupByCount = 0
        do j = 1, rownum
          flag = .false.
          do k = 1, iGroupByCount
            if (count(aGroupByValues(k, 1:iGroupByColumns) .ne. datatable(j, aGroupByColumns(1:iGroupByColumns))).eq.0) then
              aGroupByValues(k, -1) = aGroupByValues(k, -1) + 1
              flag = .true.
            end if
          enddo
          if (.not.flag) then
            iGroupByCount = iGroupByCount + 1
            aGroupByValues(iGroupByCount, 1:iGroupByColumns) = datatable(j, aGroupByColumns(1:iGroupByColumns))
            aGroupByValues(iGroupByCount, -1) = 1
          end if
        enddo
        do j = 1, iGroupByCount
          write(*,*) aGroupByValues(j, 1:iGroupByColumns), int(aGroupByValues(j, -1))
        enddo
      end if

!+ max # data maximum
    case('max') !Minimum value
      if (.not.bGroupByMode) then
        if (xcol_num.gt.1) then
          bSingleValue = .true.
        end if
        if (bSingleValue) then
	        do j = 1, xcol_num
	          pos = maxloc(datatable(1:rownum, xcol_add(j):xcol_add(j)))
	          temp_values(j) = datatable(pos(1), xcol_add(j))
	        enddo
          write(*,*) temp_values(1:xcol_num)
        else
          call PrepareFormatAllExt()
          pos = maxloc(datatable(1:rownum, xcol_add(1):xcol_add(1)))
          call WriteFormattedLineX(datatable(pos(1), 1:colnum), xFormat)
        end if
      else
        aGroupByValues(:,:) = 0D0
        aGroupByValues(:,-1) = -huge(1d0)
        iGroupByCount = 0
        do j = 1, rownum
          flag = .false.
          do k = 1, iGroupByCount
            if (count(aGroupByValues(k, 1:iGroupByColumns) .ne. datatable(j, aGroupByColumns(1:iGroupByColumns))).eq.0) then
              if (aGroupByValues(k, -1).lt.datatable(j, xcol_add(1))) then
                aGroupByValues(k, -1) = datatable(j, xcol_add(1))
              end if
              flag = .true.
            end if
          enddo
          if (.not.flag) then
            iGroupByCount = iGroupByCount + 1
            aGroupByValues(iGroupByCount, 1:iGroupByColumns) = datatable(j, aGroupByColumns(1:iGroupByColumns))
            aGroupByValues(iGroupByCount, -1) = datatable(j, xcol_add(1))
          end if
        enddo
        do j = 1, iGroupByCount
          write(*,*) aGroupByValues(j, 1:iGroupByColumns), aGroupByValues(j, -1)
        enddo
      end if

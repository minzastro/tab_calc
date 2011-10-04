module quickSort
contains

subroutine quick_sort(list_in, list_out)
IMPLICIT NONE
real*8, DIMENSION (:), INTENT(IN)  :: list_in
real*8, DIMENSION (:), INTENT(OUT)  :: list_out
INTEGER order(size(list_in))
  call quick_sort_index(list_in, list_out, order)
end subroutine quick_sort

SUBROUTINE quick_sort_index(list_in, list_out, order)

! Quick sort routine from:
! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
! Modified by Alan Miller to include an associated integer array which gives
! the positions of the elements in the original order.

IMPLICIT NONE
real*8, DIMENSION (:), INTENT(IN)  :: list_in
real*8, DIMENSION (:), INTENT(OUT)  :: list_out
INTEGER, DIMENSION (:), INTENT(OUT)  :: order

! Local variable
INTEGER :: i

list_out = list_in
DO i = 1, SIZE(list_out)
  order(i) = i
END DO

CALL quick_sort_1(1, SIZE(list_out))

contains

RECURSIVE SUBROUTINE quick_sort_1(left_end, right_end)

INTEGER, INTENT(IN) :: left_end, right_end

!     Local variables
INTEGER             :: i, j, itemp
real*8                :: reference, temp
INTEGER, PARAMETER  :: max_simple_sort_size = 6

IF (right_end < left_end + max_simple_sort_size) THEN
  ! Use interchange sort for small lists
  CALL interchange_sort(left_end, right_end)

ELSE
  ! Use partition ("quick") sort
  reference = list_out((left_end + right_end)/2)
  i = left_end - 1; j = right_end + 1

  DO
    ! Scan list from left end until element >= reference is found
    DO
      i = i + 1
      IF (list_out(i) >= reference) EXIT
    END DO
    ! Scan list from right end until element <= reference is found
    DO
      j = j - 1
      IF (list_out(j) <= reference) EXIT
    END DO


    IF (i < j) THEN
      ! Swap two out-of-order elements
      temp = list_out(i); list_out(i) = list_out(j); list_out(j) = temp
      itemp = order(i); order(i) = order(j); order(j) = itemp
    ELSE IF (i == j) THEN
      i = i + 1
      EXIT
    ELSE
      EXIT
    END IF
  END DO

  IF (left_end < j) CALL quick_sort_1(left_end, j)
  IF (i < right_end) CALL quick_sort_1(i, right_end)
END IF

END SUBROUTINE quick_sort_1


SUBROUTINE interchange_sort(left_end, right_end)

INTEGER, INTENT(IN) :: left_end, right_end

!     Local variables
INTEGER             :: i, j, itemp
real*8                :: temp

DO i = left_end, right_end - 1
  DO j = i+1, right_end
    IF (list_out(i) > list_out(j)) THEN
      temp = list_out(i); list_out(i) = list_out(j); list_out(j) = temp
      itemp = order(i); order(i) = order(j); order(j) = itemp
    END IF
  END DO
END DO

END SUBROUTINE interchange_sort
end subroutine quick_sort_index
END module quickSort
      MODULE SORTING
      CONTAINS
      SUBROUTINE SSORT (X, Y, N)
      IMPLICIT NONE
!
!    Example of an Insertion Sort, Modified to Fortran90
!    Function in shifting contents of X
!
!***BEGIN PROLOGUE  SSORT
!***PURPOSE  Sort an array and make the same interchanges in
!            an auxiliary array.  The array is sorted in
!            decreasing order.
!***TYPE      SINGLE PRECISION
!***KEYWORDS  SORT, SORTING
!
!   Description of Parameters
!      X - array of values to be sorted   (usually abscissas)
!      IY - array to be carried with X (all swaps of X elements are
!          matched in IY .  After the sort IY(J) contains the original
!          postition of the value X(J) in the unsorted X array.
!      N - number of values in array X to be sorted
!
!***REVISION HISTORY  (YYMMDD)
!   950310  DATE WRITTEN
!   John Mahaffy
!***END PROLOGUE  SSORT
!     .. Scalar Arguments ..
      INTEGER N
!     .. Array Arguments ..
      REAL*8 X(*), Y(*)
!      INTEGER IY(*)
!     .. Local Scalars ..
      REAL TEMP
      INTEGER I, J, K, ITEMP
!***FIRST EXECUTABLE STATEMENT  SSORT
      Y(1:N) = X(1:N)
      DO 100 I=2,N
!    If the Ith element is out of order with the preceeding
!    element search for its position in the portion of the
!    array that is already ordered.
         IF ( Y(I).GT.Y(I-1) ) THEN
            DO 50 J=I-2,1,-1
              IF(Y(I).LT.Y(J)) go to 70
  50          CONTINUE
            J=0
!     Use Fortran 90 Intrinsic Function to
!     Shift   array elements and insert
!     cshift is a circular shift.  With -1 as the second
!     argument, it shifts all listed array elements up one
!     element in the array, except the last listed element, which
!     circles around to the first listed array element.
!
  70        y(j+1:i) = cshift(y(j+1:i),-1)
!            iy(j+1:i) = cshift(iy(j+1:i),-1)
         ENDIF
  100 CONTINUE
      RETURN
      END SUBROUTINE SSORT

      SUBROUTINE SSORT_INDEX (X, Y, iY, N)
      IMPLICIT NONE
!
!    Example of an Insertion Sort, Modified to Fortran90
!    Function in shifting contents of X
!
!***BEGIN PROLOGUE  SSORT
!***PURPOSE  Sort an array and make the same interchanges in
!            an auxiliary array.  The array is sorted in
!            decreasing order.
!***TYPE      SINGLE PRECISION
!***KEYWORDS  SORT, SORTING
!
!   Description of Parameters
!      X - array of values to be sorted   (usually abscissas)
!      IY - array to be carried with X (all swaps of X elements are
!          matched in IY .  After the sort IY(J) contains the original
!          postition of the value X(J) in the unsorted X array.
!      N - number of values in array X to be sorted
!
!***REVISION HISTORY  (YYMMDD)
!   950310  DATE WRITTEN
!   John Mahaffy
!***END PROLOGUE  SSORT
!     .. Scalar Arguments ..
      INTEGER N
!     .. Array Arguments ..
      REAL*8 X(*), Y(*)
      INTEGER IY(*)
!     .. Local Scalars ..
      REAL TEMP
      INTEGER I, J, K, ITEMP
      do i = 1, N
        iY(i) = i
      enddo
!***FIRST EXECUTABLE STATEMENT  SSORT
      Y(1:N) = X(1:N)
      DO 100 I=2,N
!    If the Ith element is out of order with the preceeding
!    element search for its position in the portion of the
!    array that is already ordered.
         IF ( Y(I).GT.Y(I-1) ) THEN
            DO 50 J=I-2,1,-1
              IF(Y(I).LT.Y(J)) go to 70
  50          CONTINUE
            J=0
!     Use Fortran 90 Intrinsic Function to
!     Shift   array elements and insert
!     cshift is a circular shift.  With -1 as the second
!     argument, it shifts all listed array elements up one
!     element in the array, except the last listed element, which
!     circles around to the first listed array element.
!
  70        y(j+1:i) = cshift(y(j+1:i),-1)
            iy(j+1:i) = cshift(iy(j+1:i),-1)
         ENDIF
  100 CONTINUE
      RETURN
      END SUBROUTINE SSORT_INDEX


      END MODULE SORTING
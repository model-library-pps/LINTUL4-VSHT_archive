* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE LOWERC (STRING)
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER I,IC,L
      SAVE
      L = LEN (STRING)
      DO 10 I=1,L
      IC = ICHAR(STRING(I:I))
      IF (IC.GE.65.AND.IC.LE.90) STRING(I:I) = CHAR(IC+32)
10    CONTINUE
      RETURN
      END

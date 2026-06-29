* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE UPPERC (STRING)
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER I,IC,L
      SAVE
      L = LEN (STRING)
      DO 10 I=1,L
      IC = ICHAR(STRING(I:I))
      IF (IC.GE.97.AND.IC.LE.122) STRING(I:I) = CHAR(IC-32)
10    CONTINUE
      RETURN
      END

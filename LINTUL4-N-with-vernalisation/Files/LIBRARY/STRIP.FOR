* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE STRIP (STRING,CHR)
      IMPLICIT NONE
      CHARACTER STRING*(*),CHR*1
      INTEGER I,J,ILEN
      SAVE
      J = ILEN (STRING)
      IF (J.EQ.0) RETURN
10    I = INDEX (STRING(1:J),CHR)
      IF (I.GT.0) THEN
      STRING(I:J-1) = STRING(I+1:J)
      STRING(J:J) = ' '
      J = J-1
      GOTO 10
      END IF
      RETURN
      END

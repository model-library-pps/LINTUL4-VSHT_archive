* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE REMOVE (STRING,CHR)
      IMPLICIT NONE
      CHARACTER STRING*(*),CHR*1
      INTEGER I
      SAVE
10    I = INDEX (STRING,CHR)
      IF (I.GT.0) THEN
      STRING(I:I) = ' '
      GOTO 10
      END IF
      RETURN
      END

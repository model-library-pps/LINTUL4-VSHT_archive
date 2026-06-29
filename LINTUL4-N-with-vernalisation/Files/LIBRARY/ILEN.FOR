* This file was intentionally removed of
* comment lines and indentation
      INTEGER FUNCTION ILEN (STRING)
      IMPLICIT NONE
      CHARACTER*(*) STRING
      INTEGER I, L
      SAVE
      L = LEN (STRING)
      DO 10 I=L,1,-1
      IF (STRING(I:I).NE.' ') THEN
      ILEN = I
      RETURN
      END IF
10    CONTINUE
      ILEN = 0
      RETURN
      END

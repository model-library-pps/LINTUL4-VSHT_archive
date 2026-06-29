* This file was intentionally removed of
* comment lines and indentation
      REAL FUNCTION LIMIT (MIN,MAX,X)
      IMPLICIT NONE
      REAL MIN,MAX,X
      CHARACTER MESSAG*52
      SAVE
      IF (MAX.LT.MIN) THEN
      WRITE (MESSAG, '(2(A,G11.5))')
     &'argument error, MIN = ',MIN,', MAX = ',MAX
      CALL FATALERR ('LIMIT',MESSAG)
      END IF
      IF (X.LT.MIN) THEN
      LIMIT = MIN
      ELSE IF (X.LE.MAX) THEN
      LIMIT = X
      ELSE
      LIMIT = MAX
      END IF
      RETURN
      END

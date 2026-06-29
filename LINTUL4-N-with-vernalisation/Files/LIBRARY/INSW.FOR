* This file was intentionally removed of
* comment lines and indentation
      REAL FUNCTION INSW (X1,X2,X3)
      IMPLICIT NONE
      REAL X1,X2,X3
      SAVE
      IF (X1.LT.0.) THEN
      INSW = X2
      ELSE
      INSW = X3
      END IF
      RETURN
      END

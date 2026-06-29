* This file was intentionally removed of
* comment lines and indentation
      REAL FUNCTION REAAND (X1, X2)
      IMPLICIT NONE
      REAL X1, X2
      SAVE
      IF (X1.GT.0. .AND. X2.GT.0.) THEN
      REAAND = 1.
      ELSE
      REAAND = 0.
      END IF
      RETURN
      END

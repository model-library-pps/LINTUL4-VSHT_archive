** $Id: fcnsw.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      REAL FUNCTION FCNSW (X1,X2,X3,X4)
      IMPLICIT NONE
      REAL X1,X2,X3,X4
      SAVE
      IF (X1.LT.0.) THEN
      FCNSW = X2
      ELSE IF (X1.EQ.0.) THEN
      FCNSW = X3
      ELSE
      FCNSW = X4
      END IF
      RETURN
      END

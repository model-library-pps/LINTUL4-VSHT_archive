** $Id: notnul.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      REAL FUNCTION NOTNUL (X)
      IMPLICIT NONE
      REAL X
      SAVE
      IF (X.NE.0.) THEN
      NOTNUL = X
      ELSE
      NOTNUL = 1.
      END IF
      RETURN
      END

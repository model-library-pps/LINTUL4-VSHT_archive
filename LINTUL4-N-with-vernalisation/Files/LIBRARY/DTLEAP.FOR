** $Id: dtleap.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      LOGICAL FUNCTION DTLEAP (YEAR)
      IMPLICIT NONE
      INTEGER YEAR
      SAVE
      DTLEAP = MOD (YEAR,4).EQ.0
      IF (MOD (YEAR,100).EQ.0) DTLEAP = .FALSE.
      IF (MOD (YEAR,400).EQ.0) DTLEAP = .TRUE.
      RETURN
      END

** $Id: iunifl.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      INTEGER FUNCTION IUNIFL (LOWB,UPPB)
      IMPLICIT NONE
      INTEGER LOWB, UPPB
      REAL UNIFL, DIFF, RAN
      LOGICAL DRAW
      SAVE
      DIFF   = 1./(1.+REAL(UPPB)-REAL(LOWB))
      DRAW = .TRUE.
10    IF (DRAW) THEN
      RAN  = UNIFL()
      DRAW = RAN.EQ.1.
      GOTO 10
      END IF
      IUNIFL = LOWB+INT(RAN/DIFF)
      RETURN
      END

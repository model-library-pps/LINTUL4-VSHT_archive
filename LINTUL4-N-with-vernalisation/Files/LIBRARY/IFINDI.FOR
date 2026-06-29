** $Id: ifindi.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      INTEGER FUNCTION IFINDI (ILIS,ILDEC,IST,IEND,IINP)
      IMPLICIT NONE
      INTEGER ILIS,ILDEC,IST,IEND,IINP
      DIMENSION ILIS(ILDEC)
      INTEGER IN, IM, STEP
      SAVE
      IF (IST.LT.1.OR.IST.GT.ILDEC.OR.
     &IEND.LT.0.OR.IEND.GT.ILDEC) CALL FATALERR ('IFINDI',
     &'search outside array bounds')
      IF (IEND.EQ.0) THEN
      IFINDI = 0
      RETURN
      END IF
      STEP = 1
      IF (IEND.LT.IST) STEP = -1
      IM = 0
      DO IN=IST,IEND,STEP
      IF (IINP.EQ.ILIS(IN)) THEN
      IM = IN
      GOTO 10
      END IF
      END DO
10    CONTINUE
      IFINDI = IM
      RETURN
      END

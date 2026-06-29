** $Id: getun.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      INTEGER FUNCTION GETUN (IST,IEND)
      IMPLICIT NONE
      INTEGER IST,IEND
      INTEGER I1
      LOGICAL OPEN
      SAVE
      IF (IST.GT.IEND.OR.
     &IST.LT.10.OR.IST.GT.99.OR.IEND.LT.10.OR.IEND.GT.99)
     &CALL FATALERR ('GETUN','unit number outside range')
      GETUN = -1
      I1    = IST
      OPEN  = .TRUE.
10    IF (OPEN.AND.I1.NE.IEND) THEN
      INQUIRE (UNIT=I1,OPENED=OPEN)
      IF (.NOT.OPEN) GETUN = I1
      I1 = I1+1
      GOTO 10
      END IF
      IF (OPEN) CALL FATALERR ('GETUN','cannot find free unit number')
      RETURN
      END

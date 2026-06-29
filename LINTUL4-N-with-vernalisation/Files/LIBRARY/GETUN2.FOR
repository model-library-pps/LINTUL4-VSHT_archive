** $Id: getun2.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      INTEGER FUNCTION GETUN2 (IST,IEND,NUM)
      IMPLICIT NONE
      INTEGER IST,IEND,NUM
      INTEGER UNIT1,UNIT2
      LOGICAL OPEN_FIRST,FOUND,TMP,OPEN_REST
      SAVE
      IF (IST.GT.IEND.OR.
     &IST.LT.10.OR.IST.GT.99.OR.IEND.LT.10.OR.IEND.GT.99)
     &CALL FATALERR ('GETUN2','unit number range invalid')
      IF (NUM.LT.1) CALL FATALERR ('GETUN2','invalid value of NUM')
      GETUN2 = -1
      UNIT1  = IST
      FOUND  = .FALSE.
10    IF (.NOT.FOUND.AND.UNIT1.NE.IEND) THEN
      INQUIRE (UNIT=UNIT1,OPENED=OPEN_FIRST)
      IF (.NOT.OPEN_FIRST) THEN
      IF (NUM.GE.2) THEN
      OPEN_REST = .FALSE.
      DO UNIT2=UNIT1+1,UNIT1+NUM-1
      INQUIRE (UNIT=UNIT2,OPENED=TMP)
      IF (TMP) OPEN_REST = .TRUE.
      END DO
      IF (OPEN_REST) THEN
      UNIT1 = UNIT1+NUM
      ELSE
      FOUND = .TRUE.
      END IF
      ELSE
      FOUND = .TRUE.
      END IF
      ELSE
      UNIT1 = UNIT1+1
      END IF
      GOTO 10
      END IF
      IF (.NOT.FOUND) CALL FATALERR
     &('GETUN2','cannot find free unit number')
      GETUN2 = UNIT1
      RETURN
      END

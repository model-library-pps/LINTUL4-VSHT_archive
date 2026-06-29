* This file was intentionally removed of
* comment lines and indentation
      INTEGER FUNCTION IFINDC (NAMLIS,ILDEC,IST,IEND,NAME)
      IMPLICIT NONE
      INTEGER ILDEC, IST, IEND
      CHARACTER*(*) NAMLIS, NAME
      DIMENSION NAMLIS(ILDEC)
      INTEGER IN, IM, IL1, IL2, STEP
      CHARACTER*1 C
      SAVE
      IL1 = LEN (NAMLIS(1))
      IL2 = LEN (NAME)
      IF (IL1.NE.IL2) CALL FATALERR ('IFINDC','string size mismatch')
      IF (IST.LT.1.OR.IST.GT.ILDEC.OR.
     &IEND.LT.0.OR.IEND.GT.ILDEC) CALL FATALERR ('IFINDC',
     &'search outside array bounds')
      IF (IEND.EQ.0) THEN
      IFINDC = 0
      RETURN
      END IF
      STEP = 1
      IF (IEND.LT.IST) STEP = -1
      C  = NAME(1:1)
      IM = 0
      DO IN=IST,IEND,STEP
      IF (C.EQ.NAMLIS(IN)(1:1)) THEN
      IF (NAME.EQ.NAMLIS(IN)) THEN
      IM = IN
      GOTO 10
      END IF
      END IF
      END DO
10    CONTINUE
      IFINDC = IM
      RETURN
      END

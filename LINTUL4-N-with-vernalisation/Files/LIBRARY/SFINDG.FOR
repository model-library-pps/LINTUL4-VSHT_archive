** $Id: sfindg.for 1.1 1997/10/02 16:08:14 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE SFINDG (NAMLIS,ILDEC,IST,IEND,NAME,ISTYPE,
     &IFINDG,IMATCH)
      IMPLICIT NONE
      INTEGER ILDEC, IST, IEND, ISTYPE, IFINDG, IMATCH
      CHARACTER*(*) NAMLIS(ILDEC), NAME
      INTEGER IN, IM, IL1, IL2, IL3, INCR,ILEN
      SAVE
      IL1 = LEN (NAMLIS(1))
      IL2 = LEN (NAME)
      IL3 = IL2
10    IF (NAME(IL3:IL3).EQ.' '.AND.IL3.GT.0) THEN
      IL3 = IL3-1
      GOTO 10
      END IF
      IF (IL3.GT.IL1) CALL FATALERR ('SFINDG','search string too long')
      IF (IEND.GT.IST) THEN
      IF (IST.LT.1.OR.IEND.GT.ILDEC)
     &CALL FATALERR ('SFINDG','search outside array bounds')
      INCR = +1
      ELSE IF (IEND.LT.IST) THEN
      IF (IEND.LT.1.OR.IST.GT.ILDEC)
     &CALL FATALERR ('SFINDG','search outside array bounds')
      INCR = -1
      END IF
      IFINDG = 0
      DO 20 IN=IST,IEND,INCR
      IM = INDEX (NAMLIS(IN),NAME(1:IL3))
      IF (IM.GT.0) THEN
      IF (ISTYPE.EQ.1) THEN
      IF (IM.EQ.1) THEN
      IF (IL3.EQ.ILEN(NAMLIS(1))) THEN
      IFINDG = IN
      GOTO 30
      END IF
      END IF
      ELSE IF (ISTYPE.EQ.2) THEN
      IF (IM.EQ.1) THEN
      IFINDG = IN
      GOTO 30
      END IF
      ELSE IF (ISTYPE.EQ.3) THEN
      IF (IM.EQ.IL1-IL3+1) THEN
      IFINDG = IN
      GOTO 30
      END IF
      ELSE IF (ISTYPE.EQ.4) THEN
      IFINDG = IN
      GOTO 30
      END IF
      END IF
20    CONTINUE
      IM = 0
30    CONTINUE
      IMATCH = IM
      RETURN
      END

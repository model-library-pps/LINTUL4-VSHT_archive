* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE WORDS (RECORD,ILW,SEPARS,IWBEG,IWEND,IFND)
      IMPLICIT NONE
      INTEGER ILW,IWBEG,IWEND,IFND
      DIMENSION IWBEG(ILW),IWEND(ILW)
      CHARACTER*(*) RECORD,SEPARS
      INTEGER I,ICHR
      CHARACTER CHAR*1
      LOGICAL SEPAR,GOING
      SAVE
      ICHR = LEN (RECORD)
      IFND = 0
      DO 10 I=1,ILW
      IWBEG(I) = 0
      IWEND(I) = 0
10    CONTINUE
      GOING = .FALSE.
      DO 20 I=1,ICHR
      CHAR  = RECORD(I:I)
      SEPAR = (INDEX(SEPARS,CHAR).NE.0)
      IF (.NOT.SEPAR .AND. .NOT.GOING) THEN
      IFND = IFND + 1
      IWBEG(IFND) = I
      GOING = .TRUE.
      END IF
      IF (SEPAR .AND. GOING) THEN
      IWEND(IFND) = I - 1
      IF (IFND.EQ.ILW) RETURN
      GOING = .FALSE.
      END IF
20    CONTINUE
      IF (GOING) IWEND(IFND)=ICHR
      RETURN
      END

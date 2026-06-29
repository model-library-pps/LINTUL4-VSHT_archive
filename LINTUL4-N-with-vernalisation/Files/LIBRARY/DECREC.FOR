* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE DECREC (RECORD,ILX,X)
      IMPLICIT NONE
      INTEGER ILX
      CHARACTER*(*) RECORD
      REAL X
      DIMENSION X(ILX)
      INTEGER I,ICHR,IFND,IW1,IW2,IWAR
      CHARACTER CHAR*1
      LOGICAL SEPAR
      SAVE
      IFND = 0
      IW1  = 0
      IW2  = 0
      ICHR = LEN (RECORD)
      DO 10 I=1,ICHR
      CHAR  = RECORD(I:I)
      SEPAR = (CHAR.EQ.' ' .OR. CHAR.EQ.',')
      IF (IW1.EQ.0 .AND. .NOT.SEPAR) IW1 = I
      IF (IW1.GT.0) THEN
      IF (SEPAR) THEN
      IW2 = I - 1
      ELSE IF (I.EQ.ICHR) THEN
      IW2 = I
      END IF
      END IF
      IF (IW2.GT.0) THEN
      IFND = IFND + 1
      CALL DECREA (IWAR,RECORD(IW1:IW2),X(IFND))
      IF  (IWAR.NE.0)  GOTO 20
      IF (IFND.EQ.ILX) RETURN
      IW1 = 0
      IW2 = 0
      END IF
10    CONTINUE
20    CONTINUE
      IF (IFND.EQ.0) THEN
      CALL FATALERR ('DECREC','Record empty')
      ELSE
      WRITE (*,'(1X,A,I3,A,/,1X,A)')
     $'Cannot decode as',ILX,' numbers record:',RECORD(1:ICHR)
      CALL FATALERR ('DECREC','Execution terminated')
      END IF
      END

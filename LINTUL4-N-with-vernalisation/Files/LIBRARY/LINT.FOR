* This file was intentionally removed of
* comment lines and indentation
      REAL FUNCTION LINT (TABLE,ILTAB,X)
      IMPLICIT NONE
      INTEGER ILTAB
      REAL TABLE(ILTAB), X
      INTEGER I1, ILT, IUP
      REAL SLOPE, TINY
      PARAMETER (TINY=1.E-7)
      LOGICAL ERR, WARN
      SAVE
      ERR  = .FALSE.
      WARN = .FALSE.
      IF (MOD(ILTAB,2).NE.0 .OR. ILTAB.LE.2) THEN
      WRITE (*,'(A)')
     $' Number of elements in LINT table not correct !'
      ERR = .TRUE.
      ELSE
      IUP = 0
      DO 10 I1=3,ILTAB,2
      IF (TABLE(I1).LE.TABLE(I1-2)) THEN
      WRITE (*,'(2A,I4)')
     $' X-coordinates in LINT table not in',
     $' ascending order at point',I1
      ERR = .TRUE.
      END IF
      IF (IUP.EQ.0 .AND. TABLE(I1).GE.X) IUP = I1
10    CONTINUE
      END IF
      IF (.NOT.ERR .AND. X.LT.TABLE(1)) THEN
      IUP = 3
      IF ((TABLE(1)-X) .GT. ABS(X)*TINY) THEN
      WARN = .TRUE.
      WRITE (*,'(A,G13.5)')
     $' WARNING in LINT: X-value below defined region at X=',X
      END IF
      ELSE IF (.NOT.ERR .AND. X.GT.TABLE(ILTAB-1)) THEN
      IUP = ILTAB-1
      IF ((X-TABLE(ILTAB-1)) .GT. ABS(X)*TINY) THEN
      WARN = .TRUE.
      WRITE (*,'(A,G13.5)')
     $' WARNING in LINT: X-value above defined region at X=',X
      END IF
      END IF
      IF (WARN.OR.ERR) THEN
      ILT = MIN (ILTAB/2, 15)
      WRITE (*,'(A,I4,/,A,I2,A)')
     $' Number of table elements is',ILTAB,
     $' First ',ILT,' pairs are:'
      IF (ILT.GT.0) WRITE (*,'(2G13.5)')
     $(TABLE(I1),TABLE(I1+1),I1=1,2*ILT,2)
      IF (ERR) CALL FATALERR ('LINT','execution terminated')
      END IF
      SLOPE = (TABLE(IUP+1)-TABLE(IUP-1))/(TABLE(IUP)-TABLE(IUP-2))
      LINT  = TABLE(IUP-1)+(X-TABLE(IUP-2))*SLOPE
      RETURN
      END

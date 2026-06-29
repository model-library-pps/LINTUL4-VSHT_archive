** $Id: lint2.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      REAL FUNCTION LINT2 (TABNAM,TABLE,ILTAB,X)
      IMPLICIT NONE
      CHARACTER*(*) TABNAM
      INTEGER ILTAB
      REAL TABLE(ILTAB), X
      INTEGER I1, IUP, IL, ILEN
      REAL SLOPE, TINY
      PARAMETER (TINY=1.E-7)
      LOGICAL ERR
      SAVE
      ERR  = .FALSE.
      IF (MOD(ILTAB,2).NE.0 .OR. ILTAB.LE.2) THEN
      IL = MAX (1, ILEN (TABNAM))
      WRITE (*,'(2A,/,A)')
     &' Number of elements in interpolation table: ',TABNAM(1:IL),
     &' not correct !'
      ERR = .TRUE.
      ELSE
      IUP = 0
      DO 10 I1=3,ILTAB,2
      IF (TABLE(I1).LE.TABLE(I1-2)) THEN
      IL = MAX (1, ILEN (TABNAM))
      WRITE (*,'(2A,/,A,I4)')
     &' X-coordinates in interpolation table: ',TABNAM(1:IL),
     &' not in ascending order at point',I1
      ERR = .TRUE.
      END IF
      IF (IUP.EQ.0 .AND. TABLE(I1).GE.X) IUP = I1
10    CONTINUE
      END IF
      IF (.NOT.ERR .AND. X.LT.TABLE(1)) THEN
      IUP = 3
      IF ((TABLE(1)-X) .GT. ABS(X)*TINY) THEN
      IL = MAX (1, ILEN (TABNAM))
      WRITE (*,'(A,G13.5,/,2A)')
     &' WARNING in LINT2: X-value below defined region at X=',X,
     &' in interpolation table: ',TABNAM(1:IL)
      END IF
      ELSE IF (.NOT.ERR .AND. X.GT.TABLE(ILTAB-1)) THEN
      IUP = ILTAB-1
      IF ((X-TABLE(ILTAB-1)) .GT. ABS(X)*TINY) THEN
      IL = MAX (1, ILEN (TABNAM))
      WRITE (*,'(A,G13.5,/,2A)')
     &' WARNING in LINT2: X-value above defined region at X=',X,
     &' in interpolation table: ',TABNAM(1:IL)
      END IF
      END IF
      IF (ERR) CALL FATALERR ('LINT2',' ')
      SLOPE = (TABLE(IUP+1)-TABLE(IUP-1))/(TABLE(IUP)-TABLE(IUP-2))
      LINT2 = TABLE(IUP-1)+(X-TABLE(IUP-2))*SLOPE
      RETURN
      END

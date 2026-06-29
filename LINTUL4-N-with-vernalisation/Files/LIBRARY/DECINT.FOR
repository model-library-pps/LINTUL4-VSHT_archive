* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE DECINT (IWAR,STRING,IVALUE)
      IMPLICIT NONE
      INTEGER IWAR,IVALUE
      CHARACTER*(*) STRING
      INTEGER I,IC,IS,ILR,IDEC,ID1,ILIM,ITYP,JUMPTB(0:3,0:2)
      CHARACTER HELP*12,FORM*5,VALMAX*10
      DIMENSION VALMAX(2)
      LOGICAL OVERFL
      SAVE
      DATA JUMPTB /0,0,0,0, 0,2,0,0, 0,3,3,3/
      DATA VALMAX /'2147483648','2147483647'/
      ILR  = LEN (STRING)
      I    = 0
      IS   = 1
      IDEC = 0
      ID1  = 1
      ILIM = 2
10    IF (IS.GT.0 .AND. I.LT.ILR) THEN
      I  = I + 1
      IC = ICHAR(STRING(I:I))
      IF (IC.GE.48 .AND. IC.LE.57) THEN
      ITYP = 2
      IDEC = IDEC + 1
      ELSE IF (IC.EQ.43 .OR. IC.EQ.45) THEN
      ITYP = 1
      ID1  = 2
      IF (IC.EQ.45) ILIM=1
      ELSE
      ITYP = 0
      END IF
      IS = JUMPTB (IS,ITYP)
      GOTO 10
      END IF
      OVERFL = IDEC.GT.10
      IF (IDEC.EQ.10) OVERFL=LGT (STRING(ID1:ID1+9),VALMAX(ILIM))
      IF (IS.EQ.3 .AND. .NOT.OVERFL) THEN
      HELP = ' '
      HELP = ' '//STRING
      WRITE (FORM,'(A,I2,A)') '(I',ILR+1,')'
      READ (HELP,FORM,ERR=20) IVALUE
      IWAR = 0
      RETURN
      END IF
20    CONTINUE
      IWAR   = 1
      IVALUE = 0
      RETURN
      END

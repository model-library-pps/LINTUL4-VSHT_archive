** $Id: outar2.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE OUTAR2 (NAME, ARRAY, LDEC, UDEC, I1, I2)
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER LDEC,UDEC,I1,I2
      REAL ARRAY(LDEC:UDEC)
      CHARACTER IND(0:99)*5, TEMP*36
      INTEGER I3, IL, IL2, ILEN, STEP
      LOGICAL INIT
      SAVE
      DATA INIT /.FALSE./
      IF (.NOT.INIT) THEN
      DO 10 I3=0,99
      IF (I3.GE.0.AND.I3.LE.9)
     &WRITE (IND(I3),'(A,I1,A)') '(',I3,')'
      IF (I3.GT.9.AND.I3.LE.99)
     &WRITE (IND(I3),'(A,I2,A)') '(',I3,')'
10    CONTINUE
      INIT = .TRUE.
      END IF
      IF (I1.LT.LDEC.OR.I1.GT.UDEC.OR.
     &I2.LT.LDEC.OR.I2.GT.UDEC) THEN
      WRITE (*,*) 'ERROR in OUTAR2: ',
     &'output range of ',NAME,' outside declared size'
      CALL FATALERR (' ',' ')
      END IF
      STEP = 1
      IF (I2.LT.I1) STEP = -1
      IL = ILEN (NAME)
      DO 20 I3=I1,I2,STEP
      IF (I3.GE.0.AND.I3.LE.9) THEN
      TEMP = NAME(1:IL)//IND(I3)
      IL2  = IL+3
      ELSE IF (I3.GE.10.AND.I3.LE.99) THEN
      TEMP = NAME(1:IL)//IND(I3)
      IL2 = IL+4
      ELSE
      IF (I3.GE.100.AND.I3.LE.999) THEN
      WRITE (TEMP,'(2A,I3,A)') NAME(1:IL),'(',I3,')'
      IL2 = IL+5
      ELSE IF (I3.GE.-99.AND.I3.LE.-10) THEN
      WRITE (TEMP,'(2A,I3,A)') NAME(1:IL),'(',I3,')'
      IL2 = IL+5
      ELSE IF (I3.GE.-9.AND.I3.LE.-1) THEN
      WRITE (TEMP,'(2A,I2,A)') NAME(1:IL),'(',I3,')'
      IL2 = IL+4
      ELSE
      CALL FATALERR ('OUTAR2', 'subscript out of range')
      END IF
      END IF
      IF (I1.NE.I2) THEN
      IF (TEMP(36:36).NE.' ') CALL FATALERR
     &('OUTAR2','string too long')
      IF (I3.EQ.I1) THEN
      TEMP(36:36) = 'B'
      ELSE IF (I3.EQ.I2) THEN
      TEMP(36:36) = 'E'
      ELSE
      TEMP(36:36) = '.'
      END IF
      END IF
      CALL OUTDAT (2, 0, TEMP, ARRAY(I3))
20    CONTINUE
      RETURN
      END

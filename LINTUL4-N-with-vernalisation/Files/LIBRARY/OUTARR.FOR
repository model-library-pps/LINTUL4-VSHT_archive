* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE OUTARR (NAME, ARRAY, I1, I2)
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER I1, I2
      REAL ARRAY(I1:I2)
      CHARACTER IND(0:99)*5, TEMP*80
      INTEGER I3, IL, IL2, ILEN
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
      IL = ILEN (NAME)
      DO 20 I3=I1,I2
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
      CALL FATALERR ('OUTARR', 'subscript out of range')
      END IF
      END IF
      CALL OUTDAT (2, 0, TEMP(1:IL2), ARRAY(I3))
20    CONTINUE
      RETURN
      END

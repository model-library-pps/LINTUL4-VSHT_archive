* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE COPFIL (IIN, FILE, IOUT)
      IMPLICIT NONE
      INTEGER IIN, IOUT
      CHARACTER*(*) FILE
      INTEGER IOS, I1, I2, ILEN
      LOGICAL OPENT, INIT
      CHARACTER LINE*80, LINE2*72
      SAVE
      DATA INIT /.FALSE./
      IF (.NOT.INIT) THEN
      DO 5 I1=2,71
      LINE2(I1:I1) = '-'
5     CONTINUE
      LINE2(1:1) = '*'
      LINE2(72:72) = '*'
      INIT = .TRUE.
      END IF
      I1 = ILEN (FILE)
      IF (I1.EQ.0) CALL FATALERR ('COPFIL','no input file name')
      INQUIRE (UNIT=IOUT,OPENED=OPENT)
      IF (.NOT.OPENT) CALL FATALERR ('COPFIL','output file not open')
      CALL FOPENG (IIN, FILE, 'OLD','SF',0,' ')
      WRITE (IOUT, '(A)') LINE2
      WRITE (IOUT, '(80A)')
     &'* Contents of input file: ', FILE(1:I1),(' ',I2=27+I1,71),'*'
      WRITE (IOUT, '(A,/)') LINE2
      IOS = 0
10    IF (IOS.EQ.0) THEN
      READ  (IIN, '(A)', IOSTAT=IOS) LINE
      I1 = MAX (1, ILEN (LINE))
      IF (IOS.EQ.0) WRITE (IOUT, '(A)') LINE(1:I1)
      GOTO 10
      END IF
      WRITE (IOUT,'(A)') ' '
      CLOSE (IIN)
      RETURN
      END

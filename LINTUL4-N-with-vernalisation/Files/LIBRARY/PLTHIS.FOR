* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE PLTHIS (IUNIT,HIST,ILH,ILINE,IMARK,FORM,LEGEND,IDOWN)
      IMPLICIT NONE
      INTEGER IUNIT,ILH,ILINE,IMARK,IDOWN
      REAL HIST
      DIMENSION HIST(2,ILH/2)
      CHARACTER*(*) FORM,LEGEND
      INTEGER I,LFORM,LLEGEN
      REAL X1,X2,YBASE
      PARAMETER (YBASE=0.0)
      SAVE
      LFORM  = LEN (FORM)
      LLEGEN = LEN (LEGEND)
      WRITE (IUNIT,'(A,/,A,I2,A,I2,/,A)',ERR=20)
     $'*',
     $'1,',ILINE,',',IMARK,
     $LEGEND(1:LLEGEN)
      DO 10 I=1,ILH/2
      IF (I.EQ.1) THEN
      X2 = (HIST(1,1) + HIST(1,2)) / 2.0
      X1 = 2.0 * HIST(1,1) - X2
      IF (IDOWN.EQ.1) WRITE (IUNIT,FMT=FORM,ERR=20) X1,YBASE
      ELSE IF (I.LT.(ILH/2)) THEN
      X1 = X2
      X2 = (HIST(1,I) + HIST(1,I+1)) / 2.0
      ELSE
      X1 = X2
      X2 = 2.0 * HIST(1,I) - X1
      END IF
      WRITE (IUNIT,FMT=FORM,ERR=20) X1,HIST(2,I)
      WRITE (IUNIT,FMT=FORM,ERR=20) X2,HIST(2,I)
      IF (IDOWN.EQ.1) THEN
      WRITE (IUNIT,FORM(1:LFORM),ERR=20) X2,YBASE
      END IF
10    CONTINUE
      RETURN
20    CONTINUE
      CALL FATALERR ('PLTHIS','writing to output file, check CALL')
      END

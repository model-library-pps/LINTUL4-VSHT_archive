* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE PLTFUN (IUNIT,FUN,ILF,ILINE,IMARK,FORM,LEGEND)
      IMPLICIT NONE
      INTEGER IUNIT,ILF,IMARK,ILINE
      REAL FUN
      DIMENSION FUN(2,ILF/2)
      CHARACTER*(*) FORM,LEGEND
      INTEGER I,LFORM,LLEGEN
      SAVE
      LFORM  = LEN (FORM)
      LLEGEN = LEN (LEGEND)
      WRITE (IUNIT,'(A,/,A,I2,A,I2,/,A)',ERR=20)
     $'*',
     $'1,',ILINE,',',IMARK,
     $LEGEND(1:LLEGEN)
      DO 10 I=1,ILF/2
      WRITE (IUNIT,FORM(1:LFORM),ERR=20) FUN(1,I),FUN(2,I)
10    CONTINUE
      RETURN
20    CONTINUE
      CALL FATALERR ('PLTFUN','writing to output file, check CALL')
      END

* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE DECREA (IWAR,STRING,VALUE)
      IMPLICIT NONE
      INTEGER IWAR
      REAL VALUE
      CHARACTER*(*) STRING
      INTEGER ILR
      CHARACTER HELP*21,FORM*7
      LOGICAL DECCHK
      SAVE
      ILR = LEN (STRING)
      IF (DECCHK(STRING) .AND. ILR.LE.20) THEN
      HELP = ' '
      HELP = ' '//STRING
      WRITE (FORM,'(A,I2,A)') '(F',ILR+1,'.0)'
      READ (HELP,FORM,ERR=10) VALUE
      IWAR = 0
      RETURN
      END IF
10    CONTINUE
      IWAR  = 1
      VALUE = 0.0
      RETURN
      END

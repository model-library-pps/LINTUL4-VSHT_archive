* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE MOFILP (IUNIT)
      IMPLICIT NONE
      INTEGER IUNIT
      CHARACTER LINE*80
      LOGICAL COM
      SAVE
      COM = .TRUE.
10    IF (COM) THEN
      READ (IUNIT,'(A)') LINE
      COM = LINE(1:1).EQ.'*' .OR. LINE(1:2).EQ.' *'
      GOTO 10
      END IF
      BACKSPACE (IUNIT)
      RETURN
      END

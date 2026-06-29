* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE ENTINT (QUEST,IX)
      IMPLICIT NONE
      INTEGER IX
      CHARACTER QUEST*(*)
      INTEGER IS,IL,IWAR
      CHARACTER INPUT*20
      INTEGER ILEN, ISTART
      SAVE
10    CONTINUE
      CALL ENTCHA (QUEST,INPUT)
      IS = ISTART (INPUT)
      IL = ILEN (INPUT)
      IS = MAX (1,IS)
      IL = MAX (1,IL)
      CALL DECINT (IWAR,INPUT(IS:IL),IX)
      IF (IWAR.NE.0) THEN
      WRITE (*,'(/,A,/,A,/)')
     $' Enter an INTEGER value !!',
     $' or <CTRL> Z  to STOP (<Command>. on Mac)'
      GOTO 10
      END IF
      RETURN
      END
